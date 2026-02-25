;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; "chess-tree-search.scm"
;;
;;  Creating a Chess engine in Lisp (using Scheme in Racket)
;;
;;  version 2.01a   2026-02-09    A very first draft for version two.
;;                                A new start with a more efficient datastructure
;;  version 2.02a   2026-02-10    Pretty print board is 0K.
;;                                Started the 'all-moves' function
;;  version 2.02b   2026-02-11    More work on 'all-moves' function
;;  version 2.02c   2026-02-12    Made the 'play-move' function - first as helper for the 'all-moves' function
;;  version 2.10a   2026-02-14    A complete 'all-moves' function - but not tot yet thoroughly tested yet
;;  version 2.10b   2026-02-15    Added Pawn-promotion in 'play-move'
;;  version 2.10c   2026-02-15    Added minimal game play
;;  version 2.10d   2026-02-16    Added a fast 'check' test - fixed an endless loop...
;;  version 2.20a   2026-02-16    Added the 'evaluate' function 
;;  version 2.21a   2026-02-17    More work on move generation - added the tree search code
;;  version 2.30a   2026-02-19    Completed the brute force min-max-method tree search for the best computer move
;;                                (no alpha-beta pruning)
;;                                Lots of cosmetics
;;  version 2.40a   2026-02-22    Added a first FEN functions
;;  version 2.41a   2026-02-22    Added a first open-library functions
;;  version 2.41b   2026-02-22    Working open-library and FEN functions
;;  version 2.41s   2026-02-23    Conversion to Chez Scheme code
;;  version 2.42s   2026-02-24    Added colour displaying the board for a standard Mac OS shell
;;  version 2.43s   2026-02-25    Fix in FEN parser
;;
;; run in terminal
;; $ chez chess.scm
;;
;;  (cl) 2026-02-25 by Arno Jacobs
;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;;
;; A small speed increase (~7% ?)
(optimize-level 3)

(load "chess-initials.scm")
(load "chess-legal-moves.scm")
(load "chess-evaluate.scm")
(load "chess-pretty-print.scm")
(load "chess-open-library.scm")


;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; Brute force min-max method
;;

(define (max-scores scores)
  (let ((max-score (apply max (map first scores))))
    (filter (lambda (score) (= (first score) max-score)) scores)))

(define (pretty-score play-state score)
  (let* ((from-x (first  (first (second score))))
         (from-y (second (first (second score))))
         (piece-type (abs (location-value (first play-state) from-x from-y))))
    (string-append "    " (pretty-type-plus piece-type) "   " (pretty-move (second score)) " :   " (number->string (first score)) "\n")))

;; No alpha-beta pruning in this function yet...
(define (search-tree play-state move depth)
  (let ((board (first play-state))
        (player-colour (second play-state)))
    (if (null? move)
        Stalemate-value ;; Either this or the negative value ???
        (let* ((next-state (play-move play-state move))
               (board-score (evaluate next-state)))
          (if (= board-score (- Checkmate-value))
              (* depth Checkmate-value)
              (if (= depth 1)
                  (- board-score)
                  (let ((next-opponents-moves (all-moves next-state)))
                    (if (null? next-opponents-moves)
                        (- Stalemate-value)
                        (- (apply max
                                  (map (lambda (next-move)
                                         (search-tree next-state
                                                      next-move 
                                                      (- depth 1))
                                         ) next-opponents-moves )))))))))))

(define (find-and-show-score-for-best-from-move play-state moves search-depth show-scores)
  (if (null? moves)
      No-Moves
      (let* ((move (first moves))
             (score (search-tree play-state move search-depth))
             (move-score (list score move))
             (mate-in-2-score (* (- search-depth 2) Checkmate-value)))
        (if show-scores
            (display (pretty-score play-state move-score))
            (display ""))
        (if (= score mate-in-2-score) 
            (list move-score)
            (cons move-score (find-and-show-score-for-best-from-move play-state (rest moves) search-depth show-scores))))))

(define (find-and-show-score-for-best-from-moves play-state moves search-depth show-scores)
  (if show-scores
      (display "\n * Scores per possible move:\n")
      (display ""))
  (max-scores (find-and-show-score-for-best-from-move play-state moves search-depth show-scores)))

(define (find-best-from-moves play-state moves search-depth show-scores)
  (second (random-element
           (find-and-show-score-for-best-from-moves play-state moves search-depth show-scores))))       

(define (get-check-mate-move play-state moves)
  (if (null? moves)
      No-Move
      (let* ((move (first moves))
             (next-state (play-move play-state move)))
        (if (is-checked? next-state)
            (if (null? (all-moves next-state))
                move
                (get-check-mate-move play-state (rest moves)))
            (get-check-mate-move play-state (rest moves))))))
        
(define (find-best-move play-state search-depth show-scores)  
  (let* ((first-moves (all-moves play-state))
         (check-mate-move (get-check-mate-move play-state first-moves)))
    (if (equal? check-mate-move No-Move)
        (if (= (length first-moves) 1)
            (first first-moves)
            (find-best-from-moves play-state first-moves search-depth show-scores))
        check-mate-move)))  ;; mate in one

;; Look up board position in opening library
(define (get-opening-move game library-game)
  (let ((gln (length game))
        (lln (length library-game)))
    (if (< gln lln)
        (if (equal? game (list-head library-game gln))
            (first (list-tail library-game gln))
            null)
        null)))

(define (get-opening-library-moves game library)
  (if (null? game)
      (list (list '(4 1) '(4 3)))
      (filter (lambda (ls) (not (null? ls)))
              (map (lambda (library-game) (get-opening-move game library-game)) library))))

(define (computer-opening-library-move play-state open-moves)
  (let* ((open-move (random-element open-moves))
         (from-move (first open-move))
         (piece-type (abs (location-value (first play-state) (first from-move) (second from-move)))))
    (display "\n * move from opening library")
    (pretty-move-plus piece-type open-move)
    open-move))

(define (computers-move play-state search-depth game show-scores)
  (let* ((players-colour (second play-state))
         (is-full-game? (nth play-state 9))
         (opening-library (if (= players-colour black)
                              opening-library-black
                              opening-library-white))
         (open-moves (get-opening-library-moves (reverse game) opening-library)))
    (if (or (null? open-moves) (not is-full-game?))                       
        (let* ((move (find-best-move play-state search-depth show-scores))
               (from-move (first move))
               (piece-type (abs (location-value (first play-state) (first from-move) (second from-move)))))
          (pretty-move-plus piece-type move)
          move)
        (computer-opening-library-move play-state open-moves))))

;; NO mate in one branch
(define (analyse-best-move play-state search-depth show-scores)  
  (let ((first-moves (all-moves play-state)))
    (if (= (length first-moves) 1)
        (first first-moves)
        (find-best-from-moves play-state first-moves search-depth show-scores))))

;; Just show the list of possible moves and its best score
(define (computers-analysis play-state search-depth)
  (display "\n * analysis . . . \n")      
  (let* ((move (analyse-best-move play-state search-depth #t))
         (from-move (first move))
         (piece-type (abs (location-value (first play-state) (first from-move) (second from-move)))))
    (if (= (length (all-moves play-state)) 1)
        (display
         (string-append
          "\n   only one legal computers move:  " (pretty-type-plus piece-type) "  " (pretty-move move) "\n" ))
        (display ""))
    No-Move))

(define (random-move play-state)
  (let* ((move (random-element (all-moves play-state)))
         (from-move (first move))
         (piece-type (abs (location-value (first play-state) (first from-move) (second from-move)))))
    (pretty-move-plus piece-type move)
    move))





;;
;; End of code.
;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---


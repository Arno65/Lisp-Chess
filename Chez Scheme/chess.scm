;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; "chess.scm"
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
;;
;; run in terminal
;; $ chez chess.scm
;;
;;  (cl) 2026-02-24 by Arno Jacobs
;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;;
;; A small speed increase (~7% ?)
(optimize-level 3)

(load "chess-initials.scm")
(load "chess-boards.scm")
(load "chess-legal-moves.scm")
(load "chess-evaluate.scm")
(load "chess-tree-search.scm")
(load "chess-pretty-print.scm")
(load "chess-FEN-library.scm")
(load "chess-open-library.scm")


;; mate-in-#N needs 2 * N - 1 depth
;; So mate-in-4 needs search-depth 7
;;
(define search-depth 3)
;;(define search-depth 5)
;;(define search-depth 7)


(define introduction
  (string-append
   "\n\n ( ( ( a tiny and simple  ( Chez Scheme ( Lisp ))  chess engine ) ) )\n\n"
   "   ( (cl)  2026-02-24  by Arno Jacobs )\n\n"
   "   ( version 2.42s )\n"))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; ps  is play state
;;

;;
(define ps initial-board)
;;(define ps Mate-in-2-white-01)
;;(define ps Mate-in-4-white-01)


;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; Read commands or moves from the keyboard
;;
;; First the simple parse only for format 'e2e4'
;; (for now 'e2e4' is the working format)
;;

(define (pretty-helper-information)
  (display introduction)
  (display "helper information\n\n")
  (display "  the input format for a move (like): e2e4\n\n")
  (display "  commands:\n")
  (display "    A     show a Lisp-code-generated analysis list of moves and individual scores\n")
  (display "    b     show the current board\n")
  (display "    c/C   play a Lisp-code-generated move, show individual scores with option 'C'\n")
  (display "    e     show the evaluation score of the current board position\n")
  (display "    f     show the FEN string of the current board position\n")
  (display "    F     read a FEN string and convert to the current board position\n")
  (display "    g     show all the previous moves\n")
  (display "    h     show this helper information\n")
  (display "    m/M   show all possible moves for the current player (m) or opponent (M)\n")
  (display "    p#    set promotion piece (pq - Queen, pb - Bishop, pn - Knight, pr - Rook)\n")
  (display "    P/p   show the currently set promotion piece\n")
  (display "    r     play a random legal move\n")
  (display "    +/-   increment or decrement the search depth\n")
  (display "    q     quit the game.\n\n")
  null)

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----

(define (parse-move play-state entered-move game)
  (let ((move (string->list entered-move)))
    (if (null? move)
        No-Moves
        (if (= 4 (length move))
            (move-to-list entered-move)
            (let ((cmd (first move)))              
              (cond ((equal? cmd #\A)  (computers-analysis play-state search-depth)) ;; only show individual scores - NO move
                    ((equal? cmd #\b)  (pretty-board play-state))
                    ((equal? cmd #\c)  (computers-move play-state search-depth game #f))
                    ((equal? cmd #\C)  (computers-move play-state search-depth game #t)) ;; show individual scores
                    ((equal? cmd #\e)  (pretty-evaluation-score play-state))
                    ((equal? cmd #\f)  (pretty-FEN play-state))
                    ((equal? cmd #\F)  Read-FEN-string)
                    ((equal? cmd #\g)  (pretty-game game))
                    ((equal? cmd #\h)  (pretty-helper-information))
                    ((equal? cmd #\m)  (pretty-moves-list play-state (all-moves play-state)))
                    ((equal? cmd #\M)  (pretty-moves-list play-state (all-opponent-moves play-state)))
                    ((equal? cmd #\p)  (list Set-promotion-piece (safe-second move)))
                    ((equal? cmd #\P)  (pretty-promotion-piece (nth play-state 8)))
                    ((equal? cmd #\r)  (random-move play-state))
                    ((equal? cmd #\+)  (set! search-depth (pretty-change-search-depth search-depth 1))
                                       No-Moves)
                    ((equal? cmd #\-)  (set! search-depth (pretty-change-search-depth search-depth -1))
                                       No-Moves)
                    ((equal? cmd #\q)  Quit-game)                    
                    (else              No-Moves)))))))


;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; Add game-play and opening-library...
;;

(define (play-game play-state game)
  (pretty-check-info play-state)
  (display "\n $ Next move for player ")
  (display (pretty-colour-plus (second play-state)))
  (display ": ")
  (flush-output-port (current-output-port))
  (let* ((input-move (get-line (current-input-port))) ;; Chez Scheme console input
         (move (parse-move play-state input-move game))
         (legal-moves (all-moves play-state)))
    (if (or (equal? move Quit-game) (null? legal-moves))
        (pretty-Quit game)
        (if (equal? move Read-FEN-string)
            (let ((FEN-play-state (parse-FEN input-move play-state)))
              (pretty-board FEN-play-state)
              (play-game FEN-play-state No-Moves)) ;; ONLY No-Moves is FEN string is correct.                
            (if (or (equal? move void) (null? move))
                (play-game play-state game)
                (if (member Set-promotion-piece move)
                    (play-game (manual-set-promotion-piece play-state (second move)) game)
                    (if (member move legal-moves)
                        (let* ((next-state (play-move play-state move))
                               (next-moves (all-moves next-state)))
                          ;; (game-sequence (cons move game))
                          (pretty-board next-state)
                          ;; Check - Checkmate - Draft  tests
                          (let ((next-game (cons move game)))
                            (cond ((and (is-checked? next-state)
                                        (null? next-moves))
                                   (pretty-checkmate (opponent-colour (second next-state)) next-game))
                                  ((null? next-moves)         (display "\n - - - a draw. \n"))
                                  (else                       (play-game next-state next-game)))))
                        (play-game play-state game))))))))


;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; game loop
;;

(define (game-loop)
  (display introduction)
  (random-seed (time-second (current-time)))
  (pretty-board ps)  
  (play-game ps
             (if (equal? (second ps) white)
                 No-Moves
                 (list No-Move)))
  (display "\n0K.\n\n"))


(game-loop)
(exit)

;; FEN: initial board
;; FEN: rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 0 

;; FEN: mate in 4
;; FEN: 8/8/8/1p3P2/8/kPpN4/1pB5/1K3R2 w - -

;; FEN: mate in 3
;; FEN: 8/k1P5/8/1KR5/8/8/8/8 w - -

;;
;; End of code.
;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---


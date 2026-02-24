;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; "chess-pretty-print.scm"
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
(load "chess-legal-moves.scm")
(load "chess-evaluate.scm")

(define *BW-indicator* white)
(define reset-string "\x1b;[0m")

;; No range checking
(define (pretty-position position)
  (let ((position-letter (string (integer->char (+ 97 (first  position)))))
        (position-number (string (integer->char (+ 49 (second position))))))
    (string-append position-letter position-number)))

(define (pretty-positions positions)
  (apply
   string-append
   (map (lambda (p) (string-append (pretty-position p) " ")) positions )))

  
(define (pretty-move move)
  (let ((from-position (first move))
        (to-position (second move)))
    (string-append (pretty-position from-position) " - " (pretty-position to-position))))

;; All pretty functions convert board & piece data to strings
(define (pretty-colour piece-value)
  (if (= empty piece-value)
      "  "
      (if (= (colour piece-value) black)
          (if (= *BW-indicator* white)
              "\x1b;[1m\x1b;[102m\x1b;[30m "
              "\x1b;[1m\x1b;[42m\x1b;[30m ")
          " ")))

(define (pretty-colour-plus piece-value)
  (cond ((= (sgn piece-value) white) "white")
        ((= (sgn piece-value) black) "black")
        (else                        "{none}")))
        

;; (case) won't work here and on more places, so going for a (cond) list
;; very readable
(define (pretty-type piece-value)
  (let ((piece-type (abs piece-value)))
    (cond ((= piece-type King)   "K ")
          ((= piece-type Queen)  "Q ")
          ((= piece-type Bishop) "B ")
          ((= piece-type Knight) "N ")
          ((= piece-type Rook)   "R ")
          ((= piece-type Pawn)   "p ")
          (else                  " "))))

(define (pretty-type-plus piece-type)
  (cond ((= piece-type King)   "King  ")
        ((= piece-type Queen)  "Queen ")
        ((= piece-type Bishop) "Bishop")
        ((= piece-type Knight) "Knight")
        ((= piece-type Rook)   "Rook  ")
        ((= piece-type Pawn)   "Pawn  ")
        (else                  " _    ")))

(define (pretty-piece piece-value)
  (set! *BW-indicator* (- *BW-indicator*))
  (let ((piece-colour (pretty-colour piece-value))
        (piece-type   (pretty-type   piece-value))
        (BW-string    (if (= *BW-indicator* white)
                          "\x1b;[100m\x1b;[97m"    ;; grey background
                          "\x1b;[40m\x1b;[97m")))  ;; black background
    (string-append BW-string piece-colour piece-type reset-string )))

(define (pretty-line-of-pieces piece-values)
  (set! *BW-indicator* (- *BW-indicator*))
  (if (null? piece-values)
      "\n\n"
      (string-append (pretty-piece (first piece-values))
                     "  "
                     (pretty-line-of-pieces (rest piece-values)))))

(define (pretty-En-Passant-target)
  (set! *BW-indicator* (- *BW-indicator*))
  (let ((BW-string (if (= *BW-indicator* white)
                       "\x1b;[100m\x1b;[97m"    ;; grey background
                       "\x1b;[40m\x1b;[97m")))  ;; black background
    (string-append BW-string " % " reset-string)))

(define (pretty-line-with-en-passant-target piece-values x ept-x)
  (set! *BW-indicator* (- *BW-indicator*))
  (if (null? piece-values)
      "\n\n"
      (string-append (if (= x ept-x)
                         (pretty-En-Passant-target)
                         (pretty-piece (first piece-values)))
                     "  "
                     (pretty-line-with-en-passant-target (rest piece-values) (+ x 1) ept-x ))))
                     
(define (pretty-line-of-pieces-plus-en-passant-target piece-values y en-passant-target)
  (let ((ept-x (first en-passant-target))
        (ept-y (second en-passant-target)))
    (if (= y ept-y)
        (pretty-line-with-en-passant-target piece-values 0 ept-x)
        (pretty-line-of-pieces piece-values))))

(define (pretty-board-lines board line en-passant-target)
  (set! *BW-indicator* white)
  (if (null? board)
      ""
      (string-append " "
                     (number->string line)
                     "   "
                     (if (equal? en-passant-target No-Position)
                         (pretty-line-of-pieces (first board))
                         (pretty-line-of-pieces-plus-en-passant-target
                          (first board) (- line 1) en-passant-target ))
                     (pretty-board-lines (rest board) (- line 1) en-passant-target))))

(define (pretty-castling-info castling-info)
  (let ((hasShortWhite (member (* white Short-Castling) castling-info))
        (hasLongWhite  (member (* white Long-Castling)  castling-info))
        (hasShortBlack (member (* black Short-Castling) castling-info))
        (hasLongBlack  (member (* black Long-Castling)  castling-info)))
    (display "\n Castling availability")
    (if (and hasShortWhite hasLongWhite)
        (display "\n  white:   O-O   O-O-O")
        (if hasShortWhite
            (display "\n  white:   O-O")
            (if hasLongWhite
                (display "\n  white:         O-O-O")
                (display ""))))
    (if (and hasShortBlack hasLongBlack)
        (display "\n  black:   O-O   O-O-O")
        (if hasShortBlack
            (display "\n  black:   O-O")
            (if hasLongBlack
                (display "\n  black:         O-O-O")
                (display ""))))))

(define (pretty-en-passant-target en-passant-target)
  (display "\n En Passant target: ")
  (display (pretty-position en-passant-target)))

(define (pretty-moves-counter half-moves full-moves)
  (display "\n Half move clock and full move counter: ")
  (display half-moves)
  (display " / ")
  (display full-moves))

(define (pretty-promotion-piece piece-type)
  (display "\n Promotion piece is set to ")
  (display (pretty-type-plus piece-type))
  null)

(define (piece-counter-per-row board row piece)
  (do ((x (- width 1) (- x 1))
       (ppc 0 (+ (if (= (location-value board x row) piece) 1 0) ppc )))
    ((< x 0) ppc )))
     
(define (promotion-possibility? board players-colour)
  (if (= players-colour white)
      (> (piece-counter-per-row board 6 (* white Pawn)) 0)
      (> (piece-counter-per-row board 1 (* black Pawn)) 0)))

(define (print-correct-board play-state)
  (let ((board (first play-state))
        (players-colour (second play-state))
        (castling-info (third play-state))
        (en-passant-target (fourth play-state))
        (half-moves (nth play-state 4))  ;; fifth has index '4'
        (full-moves (nth play-state 5))
        ;; (white-Kings-position (nt play-state 6))
        ;; (black-Kings-position (nt play-state 7))
        (promotion-piece (nth play-state 8))
        (is-full-game? (nth play-state 9)))
    (display "\n\n\n")    
    ;; reverse so the first row is at the bottom 
    (display (pretty-board-lines (reverse board) height en-passant-target))
    (display "       a    b    c    d    e    f    g    h\n")    
    (if is-full-game?
        (pretty-moves-counter half-moves full-moves)
        null)
    (if (promotion-possibility? board players-colour)
        (pretty-promotion-piece promotion-piece)
        null)
    (if (not (null? castling-info))
        (pretty-castling-info castling-info)
        (display ""))
    (if (not (equal? en-passant-target No-Position))
        (pretty-en-passant-target en-passant-target)
        (display ""))
    (display "\n Current player colour is ")
    (display (pretty-colour-plus players-colour))
    (display "\n")))
    
      
;; Convert the board data to a string
(define (pretty-board play-state)
  (let ((board (first play-state)))
    (if (and (= (length board) width)
             (= (apply + (map length board)) (* height width)))
        (print-correct-board play-state)
        (display "Incorrect 'board' data...\n\n")))
  null)

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; Pretty printing the list of moves per piece
;;
(define (pretty-move-plus piece-type move)
  (display
   (string-append
    "\n * computers move: " (pretty-type-plus piece-type) "  " (pretty-move move))))

(define (pretty-moves-lines board moves current-from)
  (if (null? moves)
      ""
      (let* ((move (first moves))
             (next-moves (rest moves))
             (from-move (first move))
             (to-move (second move))
             (piece-type (abs (location-value board (first from-move) (second from-move)))))
        (if (equal? current-from from-move)
            (string-append
             " "
             (pretty-position to-move)             
             (pretty-moves-lines board next-moves from-move))
            (string-append
             "\n"
             (pretty-type-plus piece-type)
             "  "
             (pretty-move move)
             (pretty-moves-lines board next-moves from-move))))))

(define (sort-pieces moves)
  (if (null? moves)
      null
      (let* ((piece-location (first (first moves)))
             (equal-pieces (filter (lambda (from) (equal? piece-location (first from))) moves))
             (remaining-moves (filter (lambda (from) (not (equal? piece-location (first from)))) moves)))
        (append equal-pieces (sort-pieces remaining-moves)))))

(define (pretty-moves-list play-state moves)
  (if (null? moves)
      (display "\n * No legal moves possible...\n")
      (let* ((board (first play-state))
             (move (first moves))
             (from-move (first move))
             (piece-colour (colour (location-value board (first from-move) (second from-move)))))
        (display (string-append "The moves for " (pretty-colour-plus piece-colour) " are:" ))
        (display (pretty-moves-lines board (sort-pieces moves) No-Position))
        (display "\n")))
  null)

(define (pretty-game-one-move moves player-colour)
  (if (= player-colour white)
      (display "\n   ")
      (display "     "))
  (display (pretty-move (first moves)))
  (pretty-game-per-move (rest moves) (opponent-colour player-colour)))

(define (pretty-game-per-move moves player-colour)
  (if (null? moves)
      (display "\n\n")
      (pretty-game-one-move moves player-colour)))
      
(define (pretty-game-list game)
  (let* ((move (first game))
         (start-player-colour (if (equal? move No-Move) black white))
         (moves (if (equal? move No-Move)
                    (rest game)
                    game)))
    (display "\n * The played game:")
    (if (= start-player-colour black)
        (display "\n          ")
        (display ""))
    (pretty-game-per-move moves start-player-colour))
  null)


;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---

(define (pretty-check-info play-state)
  (let ((check-message (string-append  "\n * Player " (pretty-colour-plus (second play-state)) " is checked!\n")))
    (if (is-checked? play-state)
        (display check-message)
        (display ""))))

(define (pretty-change-search-depth search-depth delta)
  (let* ((next-search-depth (max 1 (min 9 (+ search-depth delta))))
         (next-search-depth-string
          (string-append "\n * The search depth is set to "
                         (number->string next-search-depth)
                         " ply.\n" )))
    (display next-search-depth-string)
    next-search-depth))

(define (pretty-evaluation-score play-state)
  (display "\n * The evaluation value of the current board: ")
  (display (evaluate play-state))
  (display "\n")
  null)

(define (pretty-game game)  
  (if (or (null? game)
          (equal? game (list No-Move)))
      (display "\n * No moves yet!\n")
      (pretty-game-list (reverse game)))
  null)

(define (pretty-checkmate players-colour game)
  (pretty-game game)
  (display "\n * * * Checkmate! * * * \n  The winning player is ")
  (display (pretty-colour-plus players-colour))
  (display "!\n\n"))

(define (pretty-Quit game)
  (pretty-game game)
  (display "\n Quit game - bye bye ... \n\n"))

(define (pretty-draw game)
  (pretty-game game)  
  (display "\n - - - a draw. \n\n"))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; Forsyth–Edwards Notation
;;

(define (pretty-FEN-type piece-value)
  (let ((piece-type (abs piece-value)))
    (cond ((= piece-value (* white King))   "K")
          ((= piece-value (* white Queen))  "Q")
          ((= piece-value (* white Bishop)) "B")
          ((= piece-value (* white Knight)) "N")
          ((= piece-value (* white Rook))   "R")
          ((= piece-value (* white Pawn))   "P")
          ((= piece-value (* black King))   "k")
          ((= piece-value (* black Queen))  "q")
          ((= piece-value (* black Bishop)) "b")
          ((= piece-value (* black Knight)) "n")
          ((= piece-value (* black Rook))   "r")
          ((= piece-value (* black Pawn))   "p")
          (else                             "?"))))

(define (count-empties row empties)
  (if (null? row)
      (string-append (number->string empties) "/")
      (if (= (first row) 0)
          (count-empties (rest row) (+ empties 1))
          (string-append (number->string empties) (get-row-FEN-string row)))))
          
(define (get-row-FEN-string row)
  (if (null? row)
      "/"
      (let ((head (first row))
            (tail (rest row)))
        (if (= head 0)
            (count-empties row 0)
            (string-append (pretty-FEN-type head) (get-row-FEN-string tail))))))

(define (get-board-FEN-string-loop board)
  (if (null? board)
      ""
      (string-append
       (get-row-FEN-string (first board))
       (get-board-FEN-string-loop (rest board)))))

(define (get-board-FEN-string board) 
  (remove-last-character (get-board-FEN-string-loop (reverse board))))

(define (get-castling-FEN-string castling-state)
  (if (null? castling-state)
      "-"
      (string-append
       (if (member (* white Short-Castling) castling-state) "K" "")
       (if (member (* white Long-Castling)  castling-state) "Q" "")
       (if (member (* black Short-Castling) castling-state) "k" "")
       (if (member (* black Long-Castling)  castling-state) "q" ""))))

(define (remove-last-character str)
  (substring str 0 (- (string-length str) 1)))

(define (pretty-FEN play-state)
  (let* ((board-FEN-string (get-board-FEN-string (first play-state)))
         (player-colour-FEN-string (if (= (second play-state) black) "b" "w"))
         (castling-FEN-string (get-castling-FEN-string (third play-state)))
         (En-Passant-target-position (fourth play-state))
         (En-Passant-target-FEN-string (if (equal? En-Passant-target-position No-Position)
                                           "-"
                                           (pretty-position En-Passant-target-position)))
         (half-moves (number->string (nth play-state 4)))
         (full-moves (number->string (nth play-state 5)))
         (is-full-game? (nth play-state 9))
         (moves-counter-FEN-string (if is-full-game? (string-append half-moves " " full-moves) "")))
    ;;
    (display (string-append "\n # FEN: " board-FEN-string
                            " " player-colour-FEN-string
                            " " castling-FEN-string
                            " " En-Passant-target-FEN-string
                            " " moves-counter-FEN-string "\n" )))
  null)
        
;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---

(define (manual-set-promotion-piece play-state choice)
  (let* ((current-promotion-piece (nth play-state 8))
         (new-promotion-piece 
          (cond ((equal? choice #\q) Queen)
                ((equal? choice #\b) Bishop)
                ((equal? choice #\n) Knight)
                ((equal? choice #\r) Rook)
                (else                current-promotion-piece))))
    (pretty-promotion-piece new-promotion-piece)
    (list-set play-state 8 new-promotion-piece)))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;;
;; End of this code

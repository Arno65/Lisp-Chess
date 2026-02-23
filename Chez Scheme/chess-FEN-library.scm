;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; "chess-FEN-library.scm"
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
;;
;; run in terminal
;; $ chez chess.scm
;;
;;  (cl) 2026-02-23 by Arno Jacobs
;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;;
;; A small speed increase (~7% ?)
(optimize-level 3)

(load "chess-initials.scm")
(load "chess-boards.scm")
(load "chess-legal-moves.scm")


;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---

;; FEN: 8/8/8/1p3P2/8/kPpN4/1pB5/1K3R2 w - -

              
(define (string-split str delim)
  ;; delim is a single character, e.g. #\,
  (let loop ((chars (string->list str))
             (word '())
             (words '()))
    (cond
      ((null? chars)
       ;; finished – add last word if any
       (if (null? word)
           (reverse words)
           (reverse (cons (list->string (reverse word)) words))))
      ((char=? (car chars) delim)
       ;; delimiter → finish current word, start new one
       (loop (cdr chars)
             '()
             (if (null? word)
                 words                  ; consecutive delimiters → no empty string
                 (cons (list->string (reverse word)) words))))
      (else
       ;; normal char → add to current word
       (loop (cdr chars)
             (cons (car chars) word)
             words)))))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---

(define (is-any-board-setup? board)
  (if (list? board)
      (if (= (length board) 8)
           (if (andmap (lambda (row) (= (length row) 8)) board)
               (let ((flatten (apply append board)))
                 (andmap (lambda (field) (and (>= field (* black King)) (<= field (* white King)))) flatten))
               #f)
          #f)
      #f))
  
(define (create-empties n)
  (if (= n 0)
      No-Moves
      (cons empty (create-empties (- n 1)))))

(define (FEN-char-to-piece-value FEN-char)
  (let ((empties (max 1 (min 8 (- (char->integer FEN-char) 48))))) ;; Only in range [1..8]
    (cond ((equal? FEN-char #\p) (list (* black Pawn)))
          ((equal? FEN-char #\r) (list (* black Rook)))
          ((equal? FEN-char #\n) (list (* black Knight)))
          ((equal? FEN-char #\b) (list (* black Bishop)))
          ((equal? FEN-char #\q) (list (* black Queen)))
          ((equal? FEN-char #\k) (list (* black King)))
          ((equal? FEN-char #\P) (list (* white Pawn)))
          ((equal? FEN-char #\R) (list (* white Rook)))
          ((equal? FEN-char #\N) (list (* white Knight)))
          ((equal? FEN-char #\B) (list (* white Bishop)))
          ((equal? FEN-char #\Q) (list (* white Queen)))
          ((equal? FEN-char #\K) (list (* white King)))
          (else                  (create-empties empties)))))

(define (FEN-board-data-to-board board-data)
  (if (= (length board-data) 8)
      (let ((board (reverse 
                    (map (lambda (row)
                           (apply append 
                                  (map FEN-char-to-piece-value row)))
                         board-data))))
        (if (is-any-board-setup? board)
            board
            No-Moves))
      No-Moves))
  

(define (parse-castling-states FEN-castling)
  (if (list? FEN-castling)
      (if (null? FEN-castling)
          No-Moves
          (let ((FEN-cc (first FEN-castling)))
            (cond ((equal? FEN-cc #\k) (cons (* black Short-Castling) (parse-castling-states (rest FEN-castling))))
                  ((equal? FEN-cc #\q) (cons (* black Long-Castling)  (parse-castling-states (rest FEN-castling))))
                  ((equal? FEN-cc #\K) (cons (* white Short-Castling) (parse-castling-states (rest FEN-castling))))
                  ((equal? FEN-cc #\Q) (cons (* white Long-Castling)  (parse-castling-states (rest FEN-castling))))
                  (else                                                No-Moves))))
      No-Moves))

;; Make sure the parsed position is on the board
(define (safe-position position)
  (if (list? position)
      (if (= (length position) 2)
          (let ((column (first  position))
                (row    (second position)))
            (if (and (>= column 0)
                     (<  column width)
                     (>= row    0)
                     (<  row    height))
                position
                No-Position))
          No-Position)
      No-Position))

(define (position-to-list position)  
  (let ((position-as-list (string->list position)))
    (if (= (length position-as-list) 2)
        (safe-position
         (list (- (char->integer (car  position-as-list)) 97)
               (- (char->integer (cadr position-as-list)) 49)))
        No-Position)))

;; x,y in the range [0..7] for a standard chess board
(define (get-Kings-positions-x-y board y)
  (do ((x (- width 1) (- x 1))
       (rv null (cons (if (= (abs (location-value board x y)) King)
                          (list (safe-position (list x y)))
                          null)
                      rv)))
    ((< x 0) rv)))

(define (get-Kings-positions board)
    (do ((y (- height 1) (- y 1))
         (rv null (append (get-Kings-positions-x-y board y) rv)))
      ((< y 0) (apply append rv))))

(define (get-Kings-position board King-colour)
  (let ((Kings-positions (get-Kings-positions board)))
    (if (= (length Kings-positions) 2)
        (let* ((x1 (first  (first  Kings-positions)))
               (y1 (second (first  Kings-positions)))
               (x2 (first  (second Kings-positions)))
               (y2 (second (second Kings-positions)))
               (first-colour (colour (nth (nth board y1) x1)))
               (second-colour (colour (nth (nth board y2) x2))))
          (cond ((and (= King-colour first-colour))  (list x1 y1))
                ((and (= King-colour second-colour)) (list x2 y2))
                (else                                 No-Position)))
        No-Position)))

(define (incorrect-FEN-string play-state)
  (display "\n # The FEN string is incorrect!\n")
  play-state)

(define (parse-FEN FEN-string play-state)
  (let* ((FEN-list (rest (string-split (string-append FEN-string " . . . . . ") #\space )))
         (board-data (map string->list (string-split (first FEN-list) #\/ )))
         (board (FEN-board-data-to-board board-data)))
    (if (null? board)
        (incorrect-FEN-string play-state)
        (let* ((player-colour (if (equal? (string->list (second FEN-list)) #\b)
                                  black
                                  white))
               (castling-states-string (third FEN-list))
               (castling-states (parse-castling-states (string->list castling-states-string)))
               (En-Passant-target (position-to-list (fourth FEN-list)))
               (white-King-position (get-Kings-position board white))
               (black-King-position (get-Kings-position board black)))
          ;; Create the play state from the FEN string
          (if (and (not (equal? white-King-position No-Position))
                   (not (equal? black-King-position No-Position)))
              (list board
                    player-colour
                    castling-states
                    En-Passant-target
                    0
                    0
                    white-King-position
                    black-King-position
                    Queen
                    (and (equal? board (first initial-board))  ;; Check for full game - from the start (no puzzle)
                         (= player-colour white)
                         (equal? castling-states-string "KQkq")
                         (equal? En-Passant-target No-Position)))
              (incorrect-FEN-string play-state))))))
        


;; End of this code
;;


;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; "chess-initials.scm"
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

;; The definitions ---------------------------------------------------------
;;
(define null '())

(define (sgn i)
  (cond ((< i 0) -1)
        ((> i 0)  1)
        (else     0)))

(define (list-set lst k val)
  (cond ((null? lst)  null)
        ((= k 0)     (cons val (cdr lst)))
        (else        (cons (car lst) (list-set (cdr lst) (- k 1) val)))))

;; Chess board dimensions
(define width  8)
(define height 8)

;; A check piece in this code is the product of the colour and type value
;; A black Queen has value -5
;; A white Rook has value   2

(define (colour piece-value)
  (cond ((> piece-value empty) white)
        ((< piece-value empty) black)
        (else empty)))

(define (opponent-colour piece-value)
  (-(colour piece-value)))
  
(define white    1)
(define black   -1)

(define King     6)
(define Queen    5)
(define Bishop   4)
(define Knight   3)
(define Rook     2)
(define Pawn     1)

(define empty    0)
(define outside -9) ;; Outside of the board

;; Piece and state values
(define Checkmate-value  999999)
(define Stalemate-value     999) ;; ???
(define Check-value         420) ;; Still not sure about this one
(define Queen-value         900)
(define Rook-value          500)
(define Bishop-value        330)
(define Knight-value        320)
(define Pawn-value           25) ;; position dependent

;; Piece-square table
;; Knight position bonus
;;
(define Knight-position-bonus-white
  (list
   (list -50 -40 -30 -30 -30 -30 -40 -50)
   (list -40 -20   0   5   5   0 -20 -40)
   (list -30   5  10  15  15  10   5 -30)  
   (list -30   0  15  20  20  15   0 -30)
   (list -30   5  15  20  20  15   5 -30)
   (list -30   0  10  15  15  10   0 -30)
   (list -40 -20   0   0   0   0 -20 -40)
   (list -50 -40 -30 -30 -30 -30 -40 -50)))

(define Knight-position-bonus-black
  (reverse Knight-position-bonus-white))

;; Board helpers
(define No-Position (list -1 -1))
(define No-Move     (list No-Position No-Position))
(define No-Moves    null)

(define Set-promotion-piece   (list '(19 19) '(19 19))) ;; format as a move
(define Read-FEN-string       (list '(29 29) '(29 29))) ;; format as a move
(define Quit-game             (list '(99 99) '(99 99))) ;; format as a move


;; Piece state helpers
(define En-Passant-target  No-Position)
(define Short-Castling     20)
(define Long-Castling      30)

;; Helper for a promotion piece
(define promotion-pieces (list Queen Bishop Knight Rook))


;; Some list helper code
(define (first lst)  (car    lst))
(define (second lst) (cadr   lst))
(define (third lst)  (caddr  lst))
(define (fourth lst) (cadddr lst))
;;
(define (rest lst)   (cdr    lst))

;; Here n as index, so (nth lst 0) is the same as (first lst)
(define (nth lst n) (list-ref lst n))

(define (safe-nth lst n)
  (if (or (< n 0) (>= n (length lst)))
      null
      (nth lst n)))

;; Pick a random element from a list
(define (random-element ls)
  (nth ls (random (length ls))))


(define (safe-second lst)
  (if (> (length lst) 1)
      (second lst)
      lst))

 
;; End of this code

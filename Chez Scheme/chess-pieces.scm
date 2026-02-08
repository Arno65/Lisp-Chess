;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; "chess-pieces.scm"
;;
;;  Creating a Chess engine in Lisp (using Chez Scheme)
;;
;;  version 0.02a   2026-01-10    From a first draft of 'chess.scm', pieces data
;;  version 0.02b   2026-01-12    Added Piece & state values for the 'evaluate' function
;;  version 0.03a   2026-01-18    Removing the 'First-move' indicator
;;  version 0.03b   2026-01-21    Changes in piece valuation
;;  version 0.04a   2026-01-22    Changes in Knight-position-bonus, now dependent on players colour
;; 
;;  (cl) 2025-12-31, 2026-01-22 by Arno Jacobs
;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---

;;; Some definitions
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
(define Checkmate-value 9999999)
(define Checkmate-range  999999)
(define Stalemate-value    9999)
(define Check-value         420)
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
   (list -30   5  15  20  20  15   5 -30)  
   (list -30   0  10  15  15  10   0 -30)
   (list -30   5  15  20  20  15   5 -30)
   (list -30   0  10  15  15  10   0 -30)
   (list -40 -20   0   0   0   0 -20 -40)
   (list -50 -40 -30 -30 -30 -30 -40 -50)))

(define Knight-position-bonus-black
  (reverse Knight-position-bonus-white))

;; Piece states
(define En-Passant 10)
(define Castling   20)

(define (first lst)  (car lst))
(define (second lst) (cadr lst))
(define (third lst)  (caddr lst))
(define (fourth lst) (cadddr lst))

(define (rest lst)   (cdr lst))

;; A helper function for creating a piece
;; Either 2 or 3 parameters
(define (piece . characteristics)
  (if (= 2 (length characteristics))
      (* (first characteristics) (second characteristics))
      (if (= 3 (length characteristics))
          (* (first characteristics) (+ (second characteristics) (third characteristics)))
          empty)))

;; End of this code

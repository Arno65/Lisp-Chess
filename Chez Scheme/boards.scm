;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; "boards.scm"
;;
;;  Creating a Chess engine in Lisp (using Chez Scheme)
;;
;;  version 0.02a   2026-01-10    A version with two boards, incl. 'initial-board'
;;  version 0.02b   2026-01-11    Added 'Mate in 2' boards
;;  version 0.02c   2026-01-16    Added 'Mate in 2' boards
;;  version 0.02d   2026-01-17    Added 'Mate in 2' boards (and a mate in N)
;;  version 0.02e   2026-01-18    Removing the 'First-move' indicator
;;  version 0.02f   2026-01-20    Added 'Mate in 2' boards
;;  version 0.02g   2026-01-21    Added a test board
;;  version 0.02h   2026-01-22    Added 'Mate in 2' boards
;;  version 0.02i   2026-01-27    Added a test setup for illegal castling moves
;; 
;;  (cl) 2025-12-31, 2026-01-27 by Arno Jacobs
;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---

(load "chess-pieces.scm")

;; FEN: rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -
(define initial-board
  (list
   (list (piece white Rook Castling) (piece white Knight) (piece white Bishop) (piece white Queen)
         (piece white King Castling) (piece white Bishop) (piece white Knight) (piece white Rook Castling))
   (list (piece white Pawn) (piece white Pawn) (piece white Pawn) (piece white Pawn)
         (piece white Pawn) (piece white Pawn) (piece white Pawn) (piece white Pawn))
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list (piece black Pawn) (piece black Pawn) (piece black Pawn) (piece black Pawn)
         (piece black Pawn) (piece black Pawn) (piece black Pawn) (piece black Pawn))
   (list (piece black Rook Castling) (piece black Knight) (piece black Bishop) (piece black Queen)
         (piece black King Castling) (piece black Bishop) (piece black Knight) (piece black Rook Castling))))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; Mate in 2 - white to play
;; FEN: 5Kbk/6pp/6P1/8/8/8/8/7R w - - 
;; Stockfish:  1. Rh6 Bf7  2. Rxh7#  
(define Mate-in-2-white-01
  (list
   (list empty empty empty empty empty empty empty (piece white Rook))
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty (piece white Pawn) empty)
   (list empty empty empty empty empty empty (piece black Pawn) (piece black Pawn))
   (list empty empty empty empty
         empty (piece white King) (piece black Bishop) (piece black King))))

;; Mate in 2 - white to play
;; FEN: 3k4/R6R/3n4/8/8/8/8/K7 w - -
;; Stockfish:  1. Rhg7 Nc4  2. Ra8# 
(define Mate-in-2-white-02
  (list
   (list (piece white King) empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty (piece black Knight) empty empty empty empty)
   (list (piece white Rook) empty empty empty empty empty empty (piece white Rook))
   (list empty empty empty (piece black King) empty empty empty empty)))

;; Mate in 2 - white to play
;; FEN: R1B4k/8/8/7K/8/8/p1pppppp/1Q6 w - -
;; Stockfish:  1. Kh6 cxb1=Q  2. Bb7# 
(define Mate-in-2-white-03
  (list
   (list empty (piece white Queen) empty empty empty empty empty empty)
   (list (piece black Pawn) empty (piece black Pawn) (piece black Pawn)
         (piece black Pawn) (piece black Pawn) (piece black Pawn) (piece black Pawn))
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty (piece white King))
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list (piece white Rook) empty (piece white Bishop) empty empty  empty empty (piece black King))))


;; Mate in 2 - white to play
;; FEN: R5rk/6pr/1K6/8/3Q4/8/8/7R w - -
;; Stockfish:  1. Qa1 Rf8   2. Rxf8# 
(define Mate-in-2-white-04
  (list
   (list empty empty empty empty empty empty empty (piece white Rook))
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty (piece white Queen) empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty (piece white King) empty empty empty empty empty empty)
   (list empty empty empty empty empty empty (piece black Pawn) (piece black Rook))
   (list (piece white Rook) empty empty empty
         empty empty (piece black Pawn) (piece black King))))

;; Mate in 2 - white to play
;; FEN: 6K1/8/6R1/6R1/8/8/8/k5qQ w - -
;; Stockfish: 1. Rb6 Qb1   2. Qxb1# 
;;
(define Mate-in-2-white-05
  (list
   (list (piece black King) empty empty empty empty empty (piece black Queen) (piece white Queen))
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty (piece white Rook) empty)
   (list empty empty empty empty empty empty (piece white Rook) empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty (piece white King) empty)))

;; Mate in 2 - white to play
;; FEN: 1r6/kp6/pR6/Q7/1K6/8/8/8 w - -
;; Stockfish: 1. Qd5 ...
;;
(define Mate-in-2-white-06
  (list
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty (piece white King) empty empty empty empty empty empty)
   (list (piece white Queen) empty empty empty empty empty empty empty)
   (list (piece black Pawn) (piece white Rook) empty empty empty empty empty empty)
   (list (piece black King) (piece black Pawn) empty empty empty empty empty empty)
   (list empty (piece black Rook) empty empty empty empty empty empty)))

;; Mate in 2 - white to play
;; FEN: kB6/8/8/3B4/4K3/8/4NK2/7k w - -
;;
(define Mate-in-2-white-07
  (list
   (list empty empty empty empty empty empty empty (piece black King))
   (list empty empty empty empty (piece white Knight) (piece white King) empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty (piece white Knight) empty empty empty)
   (list empty empty empty (piece white Bishop) empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list (piece black Queen) (piece white Bishop) empty empty empty empty empty empty)))

;; Mate in 2 - black to play
;; FEN: 5kBK/6rP/7n/8/8/8/8/8 b - -
;; Stockfish: 1... Rf7 2. Bxf7 Nxf7# 
(define Mate-in-2-black-01
  (list
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty
         empty empty empty (piece black Knight))
   (list empty empty empty empty empty
         empty (piece black Rook) (piece white Pawn))
   (list empty empty empty empty
         empty (piece black King) (piece white Bishop) (piece white King))))


;; Mate in 4 - white to play
;; FEN: 8/8/8/1p3P2/8/kPpN4/1pB5/1K3R2 w - -
;; Stockfish: 1. Rf3 b4  2. Nxb2 cxb2  3. Bd3 Kxb3  4. Bb5# 
;;
(define Mate-in-4-white-01
  (list
   (list empty (piece white King) empty empty empty (piece white Rook) empty empty)
   (list empty (piece black Pawn) (piece white Bishop) empty empty empty empty empty)
   (list (piece black King) (piece white Pawn) (piece black Pawn) (piece white Knight)
         empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty (piece black Pawn) empty empty empty (piece white Pawn) empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)))

;; Mate in # - white to play
;; FEN: 2b1k3/K7/2PP4/8/8/8/8/8 w - -
;; Stockfish:
;;  1. Ka8 Kd8    2. Kb8 Be6    3. c7+ Kd7    4. c8=Q+ Kxd6   5. Qf8+ Ke5
;;  6. Kc7 Bf5    7. Qc5+ Kf4   8. Qd4+ Be4   9. Kd6 Kf3     10. Ke5 Bg6
;; 11. Qf4+ Ke2  12. Kd4 Bh5   13. Qg3 Bf3   14. Qh2+ Kf1    15. Ke3 Bg2
;; 16. Qf4+ Kg1  17. Qg3 Kh1   18. Kf2 Bf1   19. Qg1#
;;
(define Mate-in-N-white-01
  (list
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty (piece white Pawn) (piece white Pawn)
         empty empty empty empty)
   (list (piece white King) empty empty empty empty empty empty empty)
   (list empty empty (piece black Bishop) empty 
         (piece black King) empty empty empty)))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; A setup for testing all castling rules
;; White to move, test O-O and O-O-O
(define castling-tests
 (list
   (list (piece white Rook Castling) empty empty empty
         (piece white King Castling) empty empty (piece white Rook Castling))
   (list (piece white Pawn) (piece white Pawn) (piece white Pawn) (piece white Pawn)
         (piece white Pawn) (piece white Pawn) (piece white Pawn) (piece white Pawn))
   (list empty (piece black Knight) empty empty empty empty (piece black Knight) empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list (piece black Pawn) (piece black Pawn) (piece black Pawn) empty
         (piece black Pawn) (piece black Pawn) (piece black Pawn) (piece black Pawn))
   (list (piece black Rook Castling) empty (piece black Bishop) (piece black Queen)
         (piece black King Castling) (piece black Bishop) empty (piece black Rook Castling))))


;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; A pre-draw situation
;; White to move: a1-g6 
;; Black is NOT checked, but can't move
(define pre-draw
  (list
   (list empty (piece white Bishop) empty empty empty empty (piece white King) empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list (piece white Rook) empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty (piece black Pawn) empty)
   (list (piece white Rook) empty empty empty
         empty empty (piece black Knight) (piece black King))))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; Empty helper board - for endgame setup
(define empty-board
  (list
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)
   (list empty empty empty empty empty empty empty empty)))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;;
;; End of this code

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; "chess-boards.scm"
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

;; The structure for the state chess data
;; Let 'C' be the list with all the state data
;; The (length C) must be 8
;;
;;   (car C)    or  (first C)    is the 8x8 board - first row is the '1' row of the board
;;   (cadr C)   or  (second C)   is the players colour value
;;   (caddr C)  or  (third C)    is a list of castling availability - 'null' is no more castling
;;   (cadddr C) or  (fourth C)   is the target En Passant move - 'NoPosition' means no target 
;;  after defining nth function...
;;   (nth C 5)                   is the number for the half move clock
;;   (nth C 6)                   is the number for the full move counter
;; Helpers, not in the FEN list
;;   (nth C 7)                   is the white King's position
;;   (nth C 8)                   is the black King's position
;;

;; FEN:
;; F    rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 0 
(define initial-board
  (list
   (list
    (list (* white Rook) (* white Knight) (* white Bishop) (* white Queen)
          (* white King) (* white Bishop) (* white Knight) (* white Rook))
    (list (* white Pawn) (* white Pawn) (* white Pawn) (* white Pawn)
          (* white Pawn) (* white Pawn) (* white Pawn) (* white Pawn))   
    (list empty empty empty empty empty empty empty empty)
    (list empty empty empty empty empty empty empty empty)
    (list empty empty empty empty empty empty empty empty)
    (list empty empty empty empty empty empty empty empty)
    (list (* black Pawn) (* black Pawn) (* black Pawn) (* black Pawn)
          (* black Pawn) (* black Pawn) (* black Pawn) (* black Pawn))
    (list (* black Rook) (* black Knight) (* black Bishop) (* black Queen)
          (* black King) (* black Bishop) (* black Knight) (* black Rook)))
   white ;; players colour
   (list (* white Short-Castling) (* white Long-Castling)  ;; castling availability
         (* black Short-Castling) (* black Long-Castling))
   No-Position ;; En Passant target position, for example (list 4 2) for 'e3'
   0 ;; half moves
   0 ;; moves
   (list 4 0)  ;; position white King
   (list 4 7)  ;; position black King
   Queen
   #t ))       ;; set for a full game - not a chess puzzle


;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; Mate in 2 - white to play
;; FEN:
;; F    5Kbk/6pp/6P1/8/8/8/8/7R w - - 
;; Stockfish:  1. Rh6 Bf7  2. Rxh7#  
(define Mate-in-2-white-01
  (list
   (list
    (list empty empty empty empty empty empty empty (* white Rook))
    (list empty empty empty empty empty empty empty empty)
    (list empty empty empty empty empty empty empty empty)
    (list empty empty empty empty empty empty empty empty)
    (list empty empty empty empty empty empty empty empty)
    (list empty empty empty empty empty empty (* white Pawn) empty)
    (list empty empty empty empty empty empty (* black Pawn) (* black Pawn))
    (list empty empty empty empty
          empty (* white King) (* black Bishop) (* black King)))
   ;;
    white ;; players colour
    null ;; NO castling options
    No-Position ;; En Passant target position
    0 ;; half moves
    0 ;; moves
    (list 5 7)      ;; position white King
    (list 7 7)      ;; position black King
    Queen
    #f ))           ;; set for a puzzel - not a full game

;; Mate in 2 - white to play
;; FEN:
;; F    3k4/R6R/3n4/8/8/8/8/K7 w - -
;; Stockfish:  1. Rhg7 Nc4  2. Ra8# 
(define Mate-in-2-white-02
  (list
   (list
    (list (* white King) empty empty empty empty empty empty empty)
    (list empty empty empty empty empty empty empty empty)
    (list empty empty empty empty empty empty empty empty)
    (list empty empty empty empty empty empty empty empty)
    (list empty empty empty empty empty empty empty empty)
    (list empty empty empty (* black Knight) empty empty empty empty)
    (list (* white Rook) empty empty empty empty empty empty (* white Rook))
    (list empty empty empty (* black King) empty empty empty empty))
   white ;; players colour
   null ;; NO castling options
   No-Position ;; En Passant target position
   0 ;; half moves
   0 ;; moves
   (list 0 0)      ;; position white King
   (list 3 7)      ;; position black King
   Queen
   #f ))           ;; set for a puzzel - not a full game


;; Mate in 2 - white to play
;; FEN:
;; F    R1B4k/8/8/7K/8/8/p1pppppp/1Q6 w - -
;; Stockfish:  1. Kh6 cxb1=Q  2. Bb7# 
(define Mate-in-2-white-03
  (list
   (list
    (list empty (* white Queen) empty empty empty empty empty empty)
    (list (* black Pawn) empty (* black Pawn) (* black Pawn)
          (* black Pawn) (* black Pawn) (* black Pawn) (* black Pawn))
    (list empty empty empty empty empty empty empty empty)
    (list empty empty empty empty empty empty empty empty)
    (list empty empty empty empty empty empty empty (* white King))
    (list empty empty empty empty empty empty empty empty)
    (list empty empty empty empty empty empty empty empty)
    (list (* white Rook) empty (* white Bishop) empty empty  empty empty (* black King)))
   ;;
   white ;; players colour
   null ;; NO castling options
   No-Position ;; En Passant target position
   0 ;; half moves
   0 ;; moves
   (list 4 7)      ;; position white King
   (list 7 7)      ;; position black King
   Queen
   #f ))           ;; set for a puzzel - not a full game


;; Mate in 2 - white to play
;; FEN:
;; F    R5rk/6pr/1K6/8/3Q4/8/8/7R w - -
;; Stockfish:  1. Qa1 Rf8   2. Rxf8# 

;; Mate in 2 - white to play
;; FEN:
;; F    6K1/8/6R1/6R1/8/8/8/k5qQ w - -
;; Stockfish: 1. Rb6 Qb1   2. Qxb1# 
;;

;; Mate in 2 - white to play
;; FEN:
;; F    1r6/kp6/pR6/Q7/1K6/8/8/8 w - -
;; Stockfish: 1. Qd5 ...
;;

;; Mate in 2 - white to play
;; FEN:
;; F    kB6/8/8/3B4/4K3/8/4NK2/7k w - -
;;

;; Mate in 2 - black to play
;; FEN:
;; F    5kBK/6rP/7n/8/8/8/8/8 b - -
;; Stockfish: 1... Rf7 2. Bxf7 Nxf7# 
(define Mate-in-2-black-01
  (list
   (list
    (list empty empty empty empty empty empty empty empty)
    (list empty empty empty empty empty empty empty empty)
    (list empty empty empty empty empty empty empty empty)
    (list empty empty empty empty empty empty empty empty)
    (list empty empty empty empty empty empty empty empty)
    (list empty empty empty empty
          empty empty empty (* black Knight))
    (list empty empty empty empty empty
          empty (* black Rook) (* white Pawn))
    (list empty empty empty empty
          empty (* black King) (* white Bishop) (* white King)))
   black ;; players colour
   null ;; NO castling options
   No-Position ;; En Passant target position
   0 ;; half moves
   0 ;; moves    
   (list 7 7)      ;; position white King
   (list 5 7)      ;; position black King
   Queen
   #f ))           ;; set for a puzzel - not a full game


;; Mate in 4 - white to play
;; FEN:
;; F    8/8/8/1p3P2/8/kPpN4/1pB5/1K3R2 w - -
;; Stockfish: 1. Rf3 b4  2. Nxb2 cxb2  3. Bd3 Kxb3  4. Bb5# 
;;
(define Mate-in-4-white-01
  (list
   (list
    (list empty (* white King) empty empty empty (* white Rook) empty empty)
    (list empty (* black Pawn) (* white Bishop) empty empty empty empty empty)
    (list (* black King) (* white Pawn) (* black Pawn) (* white Knight)
          empty empty empty empty)
    (list empty empty empty empty empty empty empty empty)
    (list empty (* black Pawn) empty empty empty (* white Pawn) empty empty)
    (list empty empty empty empty empty empty empty empty)
    (list empty empty empty empty empty empty empty empty)
    (list empty empty empty empty empty empty empty empty))
   white ;; players colour
   null ;; NO castling options
   No-Position ;; En Passant target position
   0 ;; half moves
   0 ;; moves
   (list 1 0)      ;; position white King
   (list 0 2)      ;; position black King
   Queen
   #f ))           ;; set for a puzzel - not a full game

;; Mate in # - white to play
;; FEN:
;; F    2b1k3/K7/2PP4/8/8/8/8/8 w - -
;; Stockfish:
;;  1. Ka8 Kd8    2. Kb8 Be6    3. c7+ Kd7    4. c8=Q+ Kxd6   5. Qf8+ Ke5
;;  6. Kc7 Bf5    7. Qc5+ Kf4   8. Qd4+ Be4   9. Kd6 Kf3     10. Ke5 Bg6
;; 11. Qf4+ Ke2  12. Kd4 Bh5   13. Qg3 Bf3   14. Qh2+ Kf1    15. Ke3 Bg2
;; 16. Qf4+ Kg1  17. Qg3 Kh1   18. Kf2 Bf1   19. Qg1#
;;

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; Empty helper board - for endgame setup
(define empty-board
  (list
   (list
    (list empty empty empty empty empty empty empty empty)
    (list empty empty empty empty empty empty empty empty)
    (list empty empty empty empty empty empty empty empty)
    (list empty empty empty empty empty empty empty empty)
    (list empty empty empty empty empty empty empty empty)
    (list empty empty empty empty empty empty empty empty)
    (list empty empty empty empty empty empty empty empty)
    (list empty empty empty empty empty empty empty empty))
   white ;; players colour
   (list (* white Short-Castling) (* white Long-Castling)  ;; castling options per colour
         (* black Short-Castling) (* black Long-Castling))
   ;; null
   No-Position ;; En Passant target position
   0 ;; half moves
   0 ;; moves
   (list 0 0)      ;; position white King
   (list 0 0)      ;; position black King
   Queen
   #f ))           ;; set for a puzzel - not a full game

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;;
;; End of this code

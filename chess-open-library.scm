;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; "chess-open-library.scm"
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


;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; Helper functions to convert readable string formatted moves to position lists
;;
;; A string "e2e4" to ((4 1) (4 3))
;; NO full range checking
;;
(define (moves-to-lists olsl)
  (map move-to-list olsl))


;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; The actual opening library in readable string format
;;

(define opening-library-strings-white
  ;; This list is for optimal white games
  (list
   (list "e2e4" "e7e5" "g1f3" "b8c6" "f1b5" "g8f6" "d2d3" "f8c5" "c2c3"
         "e8g8" ;; O-O
         "e1g1" ;; O-O
         "d7d5" "b1d2" "d5e4" "d3e4")
   (list "d2d4" "d7d5" )
   (list "c2c4" "e7e5" )
   (list "g1f3" "g8f6" )
   ))
   
(define opening-library-strings-black
  ;; This list is for optimal black games
  (list
   (list "e2e4" "e7e5" "g1f3" "b8c6" "f1b5" "g8f6" "d2d3" "f8c5" "c2c3"
         "e8g8" ;; O-O
         "e1g1" ;; O-O
         "d7d5" "b1d2" "d5e4" "d3e4")
   (list "d2d4" "d7d5" )
   (list "c2c4" "e7e5" )
   (list "g1f3" "g8f6" )
   ))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; This one needed to be after 'opening-library-strings'
;;

(define opening-library-white
  (map moves-to-lists opening-library-strings-white))

(define opening-library-black
  (map moves-to-lists opening-library-strings-black))

;; End of this code

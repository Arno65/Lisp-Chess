;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; "opening-library.scm"
;;
;;  Creating a Chess engine in Lisp (using Chez Scheme)
;;
;;  version 0.02a   2026-01-10    An opening library for my chess engine
;;                                A first minimal version, working jusr fine
;;  version 0.02b   2026-01-11    Added a few extra moves
;;  version 0.02c   2026-01-12    Swapped colour format (same as FEN)
;;  version 0.03a   2026-01-14    Completely changes opening library format
;; 
;;  (cl) 2025-12-31, 2026-01-14 by Arno Jacobs
;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---

;; Helper functions to convert readable string formatted moves to position lists
;;
;; A string "e2e4"  to  ((5 2) (5 4))
;; NO full range checking
;;
(define (move-to-list move)
  (let ((move-as-list (string->list move)))
    (if (= (length move-as-list) 4)
        (list (list (- (char->integer (car    move-as-list)) 96)
                    (- (char->integer (cadr   move-as-list)) 48))
              (list (- (char->integer (caddr  move-as-list)) 96)
                    (- (char->integer (cadddr move-as-list)) 48)))
        null)))

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

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; "chess-evaluate.scm"
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
(load "chess-legal-moves.scm")

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; Evaluate and score a board position for a given player
;;

(define (get-piece-value-2 abs-piece-type pawn-factor)
  (cond ((= abs-piece-type Queen)  Queen-value)
        ((= abs-piece-type Bishop) Bishop-value)
        ((= abs-piece-type Knight) Knight-value)
        ((= abs-piece-type Rook)   Rook-value)
        ((= abs-piece-type Pawn)   (* pawn-factor Pawn-value))
        (else                      0)))

(define (get-piece-value-x-y board players-colour x y)
  (let ((piece-value  (location-value board x y))
        (pawn-factor (if (= players-colour white)
                         y
                         (- 7 y))))
    (if (or (= piece-value empty) (= (opponent-colour players-colour) (colour piece-value)))
        0
        (get-piece-value-2 (abs piece-value) pawn-factor))))
        
(define (get-pieces-value-y board players-colour y)
  (do ((x (- width 1) (- x 1))
       (rv null (cons (get-piece-value-x-y board players-colour x y) rv)))
    ((< x 0) rv )))

(define (get-pieces-value board players-colour)
  (do ((y (- height 1) (- y 1))
       (rv null (append (get-pieces-value-y board players-colour y) rv)))
    ((< y 0) rv)))

;; --- --- Knight bonusses code --- --- --- --- --- --- --- --- ---
;;
;; No range checking...
(define (knight-bonus kx ky player-colour)
  (if (= player-colour white)
      (nth (nth Knight-position-bonus-white ky) kx)
      (nth (nth Knight-position-bonus-black ky) kx)))

(define (knight-bonus-xy board player-colour kx ky)
  (let ((piece-type (location-value board kx ky)))
    (if (= piece-type (* player-colour Knight))
        (knight-bonus kx ky player-colour)
        0)))
    
(define (knight-bonusses-y board player-colour y)
  (do ((x (- width 1) (- x 1))
       (rv null (cons (knight-bonus-xy board player-colour x y) rv)))
    ((< x 0) rv )))

(define (knight-bonusses board player-colour)
  (apply + (do ((y (- height 1) (- y 1))
                (rv null (append (knight-bonusses-y board player-colour y) rv )))
             ((< y 0) rv))))


(define (is-checkmate-by? play-state players-colour)
    (let ((next-moves (all-moves play-state)))
      (null? next-moves)))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; My evaluate function - and yes, I'm not a good chess player

                          
(define (evaluate play-state)
  (let* ((board                   (first play-state))
         (players-colour          (second play-state))
         (opponents-colour        (opponent-colour players-colour)) 
         (player-pieces-values    (get-pieces-value board players-colour))
         (opponents-pieces-values (get-pieces-value board opponents-colour))
         (sum-knight-bonus        (-  (knight-bonusses board players-colour)
                                      (knight-bonusses board opponents-colour)))
         (players-King-position   (if (= players-colour white)
                                      (nth play-state 6)
                                      (nth play-state 7)))
         (opponents-King-position (if (= opponents-colour white)
                                      (nth play-state 6)
                                      (nth play-state 7)))
         (checked-by-player?      (is-player-checked? board opponents-colour players-colour opponents-King-position))
         (checked-by-opponent?    (is-player-checked? board players-colour opponents-colour players-King-position)))
    (if (and checked-by-opponent? (is-checkmate-by? play-state opponents-colour))
        (- Checkmate-value)
        (apply + (list 
                  (apply + player-pieces-values)
                  (-(apply + opponents-pieces-values))
                  sum-knight-bonus
                  (if checked-by-player?
                      Check-value
                      0)
                  (if checked-by-opponent?
                      (- Check-value)
                      0))))))



;;
;; End of code.
;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---


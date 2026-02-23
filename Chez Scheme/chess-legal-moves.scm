;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; "chess-legal-moves.scm"
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


;; functions

(define (location-value board x y)
  (nth (nth board y) x))

(define (safe-location-value board x y)
    (if (or (< x 0) (> x (- width 1)) (< y 0) (> y (- height 1)))
      outside
      (nth (nth board y) x)))

(define (is-empty? board x y)
  (= empty (location-value board x y)))

(define (safe-is-empty? board x y)
  (= empty (safe-location-value board x y)))

(define (set-promotion-piece play-state promotion-piece)
  (list-set play-state 8 promotion-piece))

(define (remove-nulls lst)
    (filter (lambda (e) (not (null? e))) lst))   ;; remove all 'null' moves

;; A string "e2e4" to ((5 2) (5 4))
;; NO full range checking
;;
(define (move-to-list move)
  (let ((move-as-list (string->list move)))
    (if (= (length move-as-list) 4)
        (list (list (- (char->integer (car    move-as-list)) 97)
                    (- (char->integer (cadr   move-as-list)) 49))
              (list (- (char->integer (caddr  move-as-list)) 97)
                    (- (char->integer (cadddr move-as-list)) 49)))
        null)))

(define (King-position play-state)
  (let ((player-colour (second play-state))
        (white-King-position (nth play-state 6))
        (black-King-position (nth play-state 7)))
    (if (= player-colour white)
        white-King-position
        black-King-position)))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; Pawn moves
;;
;; Check for all possible Pawn moves 

(define (reverse-direction move)
  (map (lambda (x) (- x)) move))

(define (direct-moves moves direction)
  (if (= direction 1)
      moves
      (map (lambda (move) (reverse-direction move)) moves)))

(define (check-pawn-move board x y move)
  (let ((move-piece-value
         (safe-location-value board
                              (+ x (first  move))
                              (+ y (second move)))))
    (and (not (= move-piece-value outside))
         (= move-piece-value empty))))

;; Standard take check
(define (check-pawn-take board piece-colour x y take)
  (let ((take-piece-value
          (safe-location-value board
                               (+ x (first  take))
                               (+ y (second take))))) 
    (and (not (= take-piece-value outside))
         (= piece-colour (opponent-colour take-piece-value)))))

;; Is it possible to take via En Passant?
(define (check-en-passant en-passant-target x y take)
  (let ((take-position (list (+ x (first take))
                             (+ y (second take)))))
    (equal? en-passant-target take-position)))

(define (check-pawn-moves board piece-colour step-moves take-moves direction first-move en-passant-target x y)
  (let ((moves (direct-moves
                (if first-move
                    step-moves
                    (list (first step-moves))) direction))
        (takes (direct-moves take-moves direction)))
    (append (filter (lambda (move) (check-pawn-move board x y move)) moves)
            (filter (lambda (take) (check-en-passant en-passant-target x y take)) takes)
            (filter (lambda (take) (check-pawn-take board piece-colour x y take)) takes))))

(define (pawn-moves board piece-colour en-passant-target x y)
  (let ((step-moves '((0 1) (0 2)))
        (take-moves '((-1 1) (1 1)))
        (direction  (if (= piece-colour white) 1 -1)))
    (define first-move
      (or (and (= y 1) (= piece-colour white) (is-empty? board x 2))
          (and (= y 6) (= piece-colour black) (is-empty? board x 5))))
    (check-pawn-moves board piece-colour step-moves take-moves direction first-move en-passant-target x y)))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; A standard check for a move of King or Knight - they will only move one step
;; Either move to an empty spot or move for a take of opponents piece 
(define (check-single-step-move board piece-colour x y possible-move)
  (let ((next-piece-value
         (safe-location-value board
                              (+ x (first  possible-move))
                              (+ y (second possible-move)))))
    (and (not (= next-piece-value outside))
         (or (= next-piece-value empty)
             (= piece-colour (- (colour next-piece-value)))))))

(define (check-single-step-moves board piece-colour x y possible-moves)
  (filter (lambda (move) (check-single-step-move board piece-colour x y move)) possible-moves))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; Knight moves
;;
;; Check for all possible Knight moves  
(define (knight-moves board piece-colour x y)
  (let ((all-knight-moves '( (-1 -2) (-1  2) (-2 -1) (-2 1)
                             ( 1 -2) ( 1  2) ( 2 -1) ( 2 1))))
    (check-single-step-moves board piece-colour x y all-knight-moves)))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; helper for Queen, Bishop and Rook moves
;;
(define (one-moves-list moves)
  (apply append (map (lambda (step) (safe-nth moves step)) '(0 1 2 3 4 5 6 7))))

(define (walk-directional-step board piece-colour x y dx dy step)
  (let ((px (+ x (* dx step)))
        (py (+ y (* dy step))))
    (append
     (cond ((= (safe-location-value board px py) outside) No-Moves)
           ((safe-is-empty? board px py)
            (cons (list (* dx step) (* dy step))
                  (walk-directional-step board piece-colour x y dx dy (+ step 1))))
           ((not (= piece-colour (colour (safe-location-value board px py))))
            (list (list (* dx step) (* dy step))))
           (else No-Moves)))))

(define (moves-per-direction board piece-colour x y direction)
  (let ((dx (first  direction))
        (dy (second direction)))
    (walk-directional-step board piece-colour x y dx dy 1)))

(define (moves-for-all-directions board piece-colour x y directions)
  (one-moves-list
   (map
    (lambda (direction) (moves-per-direction board piece-colour x y direction))
    directions)))



;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; Queen moves
;;
(define (queen-moves board piece-colour x y)
  (let ((directions '((-1 -1) (0 -1) (1 -1)
                      (-1  0)        (1  0)
                      (-1  1) (0  1) (1  1))))
    (moves-for-all-directions board piece-colour x y directions)))
    
;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; Bishop moves
;;
(define (bishop-moves board piece-colour x y)
  (let ((directions '((-1 -1) (1 -1) (-1  1) (1  1))))
    (moves-for-all-directions board piece-colour x y directions )))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; Rook moves
;;
(define (rook-moves board piece-colour x y)
  (let ((directions '((0 -1) (-1  0) (1  0) (0  1))))
    (moves-for-all-directions board piece-colour x y directions )))


;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; The 'regular' King moves
;; NO casting and NO legality by 'non-check'
;;
(define (king-moves board piece-colour x y)
  (let ((regular-knight-moves '((-1 -1) (0 -1) (1 -1)
                                (-1  0)        (1  0)
                                (-1  1) (0  1) (1  1))))
    (check-single-step-moves board piece-colour x y regular-knight-moves)))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; All moves for an arbitrary piece on a arbitrary position - also for the King but
;;      -  NO castling and
;;      -  NO checks on legality by 'non-check'
;;
;; (x,y) is safe by loop ranges
;;
(define (moves-for-location board piece-value castling-info en-passant-target x y)
  (let ((piece-type (abs piece-value))
        (piece-colour (colour piece-value)))
    (cond ((= piece-type King)   (king-moves   board piece-colour x y))
          ((= piece-type Queen)  (queen-moves  board piece-colour x y))
          ((= piece-type Bishop) (bishop-moves board piece-colour x y))
          ((= piece-type Knight) (knight-moves board piece-colour x y))
          ((= piece-type Rook)   (rook-moves   board piece-colour x y))
          ((= piece-type Pawn)   (pawn-moves   board piece-colour en-passant-target x y))
          (else                  No-Moves))))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; All moves for all pieces by colour
;;
;; example: ( ((0 1) (0 2))
;;            ((0 1) (0 3))
;;                    . . . )
;;
(define (to-moves x y moves)
  (if (null? moves)
      No-Moves
      (let ((move (first moves)))
        (cons (list (+ x (first move)) (+ y (second move))) 
              (to-moves x y (rest moves))))))

(define (vector-sum v1 v2)
  (let ((v1x (first v1))
        (v1y (second v1))
        (v2x (first v2))
        (v2y (second v2)))
  (list (+ v1x v2x) (+ v1y v2y))))
  

(define (all-moves-x-y board players-colour castling-info en-passant-target x y)
   (let ((piece-value (location-value board x y))   ;; The (x,y) range is safe
        (from-move (list x y))) 
    (if (= players-colour (colour piece-value))
        (let ((next-moves (moves-for-location board piece-value castling-info en-passant-target x y)))
          (if (null? next-moves)
              No-Moves
               (map (lambda (to-move) (list from-move (vector-sum from-move to-move))) next-moves)))
        No-Moves )))

;; x,y in the range [0..7] for a standard chess board
(define (all-moves-y board players-colour castling-info en-passant-target y)
  (do ((x (- width 1) (- x 1))
       (rv null (cons (all-moves-x-y board players-colour castling-info en-passant-target x y) rv)))
    ((< x 0) (apply append rv) )))

(define (all-moves-loop play-state)
  (let ((board (first play-state))
        (players-colour (second play-state))
        (castling-info (third play-state))
        (en-passant-target (fourth play-state)))         
    (do ((y (- height 1) (- y 1))
         (rv null (append (all-moves-y board players-colour castling-info en-passant-target y) rv)))
      ((< y 0) rv))))
  
(define (all-regular-moves play-state)
  (filter (lambda (moves) (not (null? moves))) (all-moves-loop play-state)))

;; No attack from a castling move...
;; This is called from a castling test - so there is no attack from opponents castling
(define (opponent-attacks play-state)
  (let* ((next-colour (opponent-colour (second play-state)))
         (next-state (list-set play-state 1 next-colour))
         (regular-moves (all-regular-moves next-state)))
    regular-moves))

;;At this moment this list is unsorted and there are duplicates in the list
;; Now what will be the most efficient?
;; - removing the duplicates and therefor a quicker search
;; - leave it as it is and pay a bit of extra time in the searches
;;

;; This one is slow...
(define (unique lst)
  (if (null? lst)
      null
      (let* ((element (first lst))
             (remaining (filter (lambda (e) (not (equal? element e))) lst)))
        (cons element (unique remaining)))))

(define (attacking-positions moves)
  (unique (map second moves)))

(define (all-empty-row? board start-x end-x y)
  (if (> start-x end-x)
      #t
      (if (= (location-value board start-x y) empty)
          (all-empty-row? board (+ start-x 1) end-x y)
          #f)))
  
(define (free-castling-move board King-from-x King-to-x empty-from-x empty-to-x King-at-y)
  (if (all-empty-row? board empty-from-x empty-to-x King-at-y)
        (list (list 4 King-at-y) (list King-to-x King-at-y)) ;; with castling the King always starts from 'e'
        null))

(define (players-castling-move board castling-state)
  (cond ((= castling-state (* white Short-Castling)) (free-castling-move board 5 6 5 6 0 )) ;; e1-g1   O-O
        ((= castling-state (* white Long-Castling))  (free-castling-move board 3 1 1 3 0 )) ;; e1-b1   O-O-O
        ((= castling-state (* black Short-Castling)) (free-castling-move board 5 6 5 6 7 )) ;; e8-g8   O-O
        ((= castling-state (* black Long-Castling))  (free-castling-move board 3 1 1 3 7 )) ;; e8-b8   O-O-O
        (else null)))

(define (all-players-castling-moves board castling-states)
  (let ((castling-moves
         (map (lambda (state) (players-castling-move board state)) castling-states)))
    (remove-nulls castling-moves)))
  

(define (low-x xy)
  (let ((x1 (first (first xy)))
        (x2 (first (second xy)))
        (y (second (first xy))))
    (if (< x1 x2)
        (list x1 y)
        (list x2 y))))

(define (high-x xy)
  (let ((x1 (first (first xy)))
        (x2 (first (second xy)))
        (y (second (first xy))))
    (if (< x1 x2)
        (list x2 y)
        (list x1 y))))
 
(define (non-checked-castling-move attack-positions King-position King-position-max)
  (let ((free (not (member King-position attack-positions))))
  (if (equal? King-position King-position-max)
      free
      (if free
          (let ((next-King-position (list (+ (first King-position) 1) (second King-position))))
            (non-checked-castling-move attack-positions next-King-position King-position-max))
          #f))))

(define (select-free-castling-moves play-state first-selection-moves)
  (let ((opponent-attacks (attacking-positions (opponent-attacks play-state))))
    (filter
     (lambda (King-move)
       (non-checked-castling-move opponent-attacks (low-x King-move) (high-x King-move)))
     first-selection-moves)))

(define (all-castling-moves play-state)
  (let* ((player-colour (second play-state))
         (castling-states (filter (lambda (state) (= player-colour (sgn state))) (third play-state))))
    (if (null? castling-states)
        null
        (let ((first-selection-moves (all-players-castling-moves (first play-state) castling-states)))
          (if (null? first-selection-moves)
              null
              (select-free-castling-moves play-state first-selection-moves))))))

;; These counter moves hav NO check-test (yet)
(define (all-first-moves play-state)
  (let ((regular-moves (all-regular-moves play-state))
        (castling-moves (all-castling-moves play-state)))
        (append regular-moves castling-moves)))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;;
;; Now I need the play-move function
;; ONLY after generating and checking for 'legal' moves, possible moves
;; -------------------------------------------------------------------------------------------------------
;; 'play-move' and helper functions
;;

(define (is-first-double-pawn-move? piece-value from-position to-position)
  (let ((is-Pawn? (= (abs piece-value) Pawn))
        (from-x (first from-position))
        (from-y (second from-position))
        (to-y (second to-position)))
    (and is-Pawn?
         (= (abs (- from-y to-y)) 2))))   ;; a 'double' move

(define (set-en-passant-target from-position to-position)
  (let* ((from-x (first from-position))
        (from-y (second from-position))
        (to-y (second to-position))
        (delta (/ (- to-y from-y) 2)))
    (list from-x (+ from-y delta))))


(define (remove-from-list xs e)
  (filter (lambda (x) (not (= x e))) xs))
  
(define (remove-castling-state from-position piece-value current-castling-state)
  (cond ((and (= piece-value (* white Rook)) (equal? from-position (list 0 0)))
         (remove-from-list current-castling-state (* white Long-Castling)))
        ((and (= piece-value (* white Rook)) (equal? from-position (list (- width 1) 0)))
         (remove-from-list current-castling-state (* white Short-Castling)))
        ((and (= piece-value (* white King)) (equal? from-position (list 4 0)))
         (remove-from-list
          (remove-from-list current-castling-state (* white Short-Castling))
          (* white Long-Castling)))
        ;;
        ((and (= piece-value (* black Rook)) (equal? from-position (list 0 (- height 1))))
         (remove-from-list current-castling-state (* black Long-Castling)))
        ((and (= piece-value (* black Rook)) (equal? from-position (list (- width 1) (- height 1))))
         (remove-from-list current-castling-state (* black Short-Castling)))
        ((and (= piece-value (* black King)) (equal? from-position (list 4 (- height 1))))
         (remove-from-list
          (remove-from-list current-castling-state (* black Short-Castling))
          (* black Long-Castling)))
        ;;
        (else current-castling-state)))
        
(define (manage-castling-states board from-position to-position piece-value current-castling-state)
  (let ((whiteRook (* white Rook))
        (whiteKing (* white King))
        (blackRook (* black Rook))
        (blackKing (* black King)))
    (if (null? current-castling-state)
        null
        (cond ((= piece-value whiteRook) (remove-castling-state from-position whiteRook current-castling-state))
              ((= piece-value whiteKing) (remove-castling-state from-position whiteKing current-castling-state))
              ((= piece-value blackRook) (remove-castling-state from-position blackRook current-castling-state))
              ((= piece-value blackKing) (remove-castling-state from-position blackKing current-castling-state))
              (else                       current-castling-state)))))
 
(define (erase-board-xy board position)
  (let* ((x (first position))
         (y (second position))
         (new-row (list-set (nth board y) x empty)))
    (list-set board y new-row)))

(define (set-board-xy board position piece-value)
  (let* ((x (first position))
         (y (second position))
         (new-row (list-set (nth board y) x piece-value)))
    (list-set board y new-row)))

(define (set-king-position piece-value king-colour new-position current-position)
  (if (= piece-value king-colour)
      new-position
      current-position))


(define (make-one-move board from-position to-position piece-value)
  (erase-board-xy
   (set-board-xy board to-position piece-value)
   from-position))

;; Function for creating all possible legal moves.
;; Legality must be determined beforehand.
;; Castling and en passant are also executed here.
;;
(define (make-move board from-position to-position piece-value)
  (let* ((first-step-board (make-one-move board from-position to-position piece-value))
         (from-x (first from-position))
         (to-x (first to-position))
         (is-castling-move? (and (= (abs piece-value) King)      ;; King moving more than one step
                                 (> (abs (- from-x to-x)) 1)))
         (is-en-passant-take? (and (= (abs piece-value) Pawn)    ;; Pawn moving sideways to an empty field
                                   (> (abs (- from-x to-x)) 0)
                                   (= (location-value board (first to-position) (second to-position)) empty))))
    (cond (is-castling-move?
           (let ((Rook-y (second from-position))
                 (Rook-from-x (if (> to-x 4) 7 0))
                 (Rook-to-x (if (> to-x 4) 5 2))
                 (players-colour (colour piece-value)))
             (make-one-move first-step-board (list Rook-from-x Rook-y) (list Rook-to-x Rook-y) (* players-colour Rook))))
          (is-en-passant-take?
           (let* ((to-y (second to-position))
                  (take-y (if (= (colour piece-value) white)
                             (- to-y 1)
                             (+ to-y 1))))
             (erase-board-xy first-step-board (list to-x take-y))))
          (else first-step-board))))

(define (is-promotion? board piece-value to-position)
  (if (= (abs piece-value) Pawn)
      (let ((row (second to-position))
            (piece-colour (colour piece-value)))
        (or (and (= row 0) (= piece-colour black))
            (and (= row 7) (= piece-colour white))))
      #f))
  
;; The play-move function managing the complete play-state
;;
(define (play-move play-state move)
  (let* ((board (first play-state))
         (from-position (first move))
         (piece-value (location-value board (first from-position) (second from-position)))
         (to-position (second move))
         (promotion-piece (nth play-state 8))
         (next-board (if (is-promotion? board piece-value to-position)
                          (make-one-move board from-position to-position (* (colour piece-value) promotion-piece))
                          (make-move board from-position to-position piece-value)))
         (next-players-colour (opponent-colour (second play-state)))
;; If Rook or King is moved change the Castling state
;; If the Castling state list is empty there is nothing more to be done
         (next-castling-state (manage-castling-states board from-position to-position piece-value (third play-state)))
;; Reset the existing En Passant state         
;; If Pawn first move is a 'double' one set En Passant state
         (en-passant-target
          (if (is-first-double-pawn-move? piece-value from-position to-position)
              (set-en-passant-target from-position to-position)
              No-Position))
;; ONLY for a full game
;; Do a correct Half Moves count!
         (is-full-game? (nth play-state 9))          ;; or puzzle - keep this fixed
         (half-moves (if is-full-game?            ;; this one needs extra rules !!!
                         (+ 1 (nth play-state 4))    ;; 4 as index starting at 0
                         0))
         (full-moves (if is-full-game?
                         (+ 1 (nth play-state 5))
                         0))
;; For ---- a quick 'Check' check.
;; Change the value if one of the Kings has moved 
         (next-white-king-position (set-king-position piece-value (* white King) to-position (nth play-state 6)))
         (next-black-king-position (set-king-position piece-value (* black King) to-position (nth play-state 7))))
    (list next-board next-players-colour next-castling-state en-passant-target
          half-moves full-moves next-white-king-position next-black-king-position
          promotion-piece is-full-game?)))

;; ------ ------ ------ ------ ------ ------ ------ ------ ------ ------ ------ ------ ------ ------ ------
;;
(define (attack-from-move? board Kings-position move attacking-Piece)
  (let ((Kings-column (first Kings-position))
        (Kings-row (second Kings-position))
        (move-column (first move))
        (move-row (second move)))
    (= (safe-location-value board (+ Kings-column move-column) (+ Kings-row move-row)) attacking-Piece)))

(define (attack-from-moves? board start-column start-row move-column move-row attacking-Piece-1 attacking-Piece-2)
  (let* ((test-column (+ start-column move-column))
         (test-row (+ start-row move-row))
         (test-piece (safe-location-value board test-column test-row)))
    (if (= test-piece empty)
        (attack-from-moves?
         board
         test-column test-row
         move-column move-row
         attacking-Piece-1 attacking-Piece-2 )
        (or (= test-piece attacking-Piece-1)
            (= test-piece attacking-Piece-2)))))

(define (Pawn-attacks-on? board Kings-position players-colour opponents-colour)
  (let* ((Pawn-column-left (- (first Kings-position) 1))
         (Pawn-column-right (+ (first Kings-position) 1))
         (King-row (second Kings-position))
         (Pawn-row (+ King-row (if (= players-colour white) 1 -1)))
         (attacking-Pawn (* opponents-colour Pawn))
         (attack-from-left? (= attacking-Pawn (safe-location-value board Pawn-column-left Pawn-row)))
         (attack-from-right? (= attacking-Pawn (safe-location-value board Pawn-column-right Pawn-row))))
    (or attack-from-left? attack-from-right?)))
                
(define (Knight-attacks-on? board Kings-position opponents-colour)
  (let ((all-Knight-moves '( (-1 -2) (-1  2) (-2 -1) (-2 1)
                             ( 1 -2) ( 1  2) ( 2 -1) ( 2 1)))
         (attacking-Knight (* opponents-colour Knight)))
    (ormap (lambda (move)
             (attack-from-move? board Kings-position move attacking-Knight))
           all-Knight-moves)))
  

;; Testing the horizontal and vertical lines
(define (Rooks-and-Queen-attacks-on? board Kings-position opponents-colour)
  (let* ((all-Rook-moves '( (-1  0) ( 1  0)
                            ( 0 -1) ( 0  1)))
         (attacking-Rook (* opponents-colour Rook))
         (attacking-Queen (* opponents-colour Queen)))
    (ormap (lambda (move)
             (attack-from-moves?
              board
              (first Kings-position) (second Kings-position)
              (first move) (second move)
              attacking-Rook attacking-Queen))
           all-Rook-moves)))

;; Testing the diagonal lines
(define (Bishops-and-Queen-attacks-on? board Kings-position opponents-colour)
  (let* ((all-Bishop-moves '( (-1 -1) ( 1 -1)
                              (-1  1) ( 1  1)))
         (attacking-Bishop (* opponents-colour Bishop))
         (attacking-Queen (* opponents-colour Queen)))
    (ormap (lambda (move)
             (attack-from-moves?
              board
              (first Kings-position) (second Kings-position)
              (first move) (second move)
              attacking-Bishop attacking-Queen))
           all-Bishop-moves)))

              
(define (King-blocks-King? board Kings-position opponents-colour)
  (let ((all-King-moves '((-1 -1) (0 -1) (1 -1)
                          (-1  0)        (1  0)
                          (-1  1) (0  1) (1  1)))
        (blocking-King (* opponents-colour King)))
    (ormap (lambda (move)
             (attack-from-move? board Kings-position move blocking-King))
           all-King-moves)))
  
;; The quick version for testing a 'check' position
(define (is-player-checked? board players-colour opponents-colour Kings-position)
  (if (Pawn-attacks-on? board Kings-position players-colour opponents-colour)
      #t
      (if (Knight-attacks-on? board Kings-position opponents-colour)
          #t
          (if (Rooks-and-Queen-attacks-on? board Kings-position opponents-colour)
              #t
              (if (Bishops-and-Queen-attacks-on? board Kings-position opponents-colour)
                  #t
                  (King-blocks-King? board Kings-position opponents-colour))))))
          
(define (is-checked? play-state)
  (let* ((board (first play-state))
         (players-colour (second play-state))
         (opponents-colour (opponent-colour players-colour))
         (players-King-position (if (= players-colour white)
                                    (nth play-state 6)
                                    (nth play-state 7)))
         (opponents-King-position (if (= opponents-colour white)
                                      (nth play-state 6)
                                      (nth play-state 7))))
    (or (is-player-checked? board players-colour opponents-colour players-King-position)
        (is-player-checked? board opponents-colour players-colour opponents-King-position))))
        
(define (filter-non-checked play-state Kings-position players-colour opponents-colour move)
  (let* ((next-state (play-move play-state move))
         (next-board (first next-state))
         (next-Kings-position (if (equal? Kings-position (first move))
                                  (second move)
                                  Kings-position)))
    (if (is-player-checked? next-board players-colour opponents-colour next-Kings-position)
        null
        move)))

(define (all-moves play-state)
  (let ((moves (all-first-moves play-state))
        (Kings-position (King-position play-state))
        (players-colour (second play-state))
        (opponents-colour (opponent-colour (second play-state))))
    (remove-nulls
     (map (lambda (move) (filter-non-checked play-state Kings-position players-colour opponents-colour move)) moves))))

(define (all-opponent-moves play-state)
  (let ((next-colour (opponent-colour (second play-state))))
  (all-moves (list-set play-state 1 next-colour))))




;; End of this code

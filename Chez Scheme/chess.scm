;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; "chess.scm"
;;
;;  Creating a Chess engine in Lisp (using Chez Scheme)
;;
;;  version 0.01a   2025-12-28    A very first draft. Initialising things that could be of good use.
;;  version 0.01b   2025-12-29    Added piece and board data and pretty printing.
;;  version 0.01c   2025-12-29    Start to create lists for all possible moves for any given board setup.
;;  version 0.01d   2025-12-30    More work on creating lists for all possible moves for any given board setup.
;;  version 0.01e   2025-12-30    All moves for individual pieces, except en-passant.
;;  version 0.01f   2025-12-30    All moves for individual pieces. Added the 'list-moves' function.
;;  version 0.01g   2025-12-31    Refactoring the 'all-moves' function, small changes and starting the game loop.
;;  version 0.01h   2026-01-03    Working on the 'make the move' functions. Srious bugs in the moves generator :-(
;;  version 0.01i   2026-01-03    'make the move' functions all look 0K. Added a check on board dimensions.
;;  version 0.01j   2026-01-04    Added 'Check' test.
;;  version 0.01k   2026-01-04    Added 'Checkmate' test, rewrote 'list-moves' for readable output.
;;  version 0.01l   2026-01-06    Issues with the game-loop. The 'all-moves' function is not complete correct.
;;                                The King can move to 'check' position, so check for free positions.
;;  version 0.01m   2026-01-07    Adding a test whether after a player's move the player's King is not checked.
;;                                Refactored a lot on the 'all-moves' and 'all-moves-list' functions.
;;  version 0.01n   2026-01-08    Some minor changes and removing obsolete code.
;;  version 0.01o   2026-01-09    Added the opening library. Started with two moves only.
;;  version 0.01p   2026-01-10    Debuged Castling. My Casting on Rook was TOO strict.
;;                                A first (not standard) list of moves is created, and at least shown at the end.
;;  version 0.01q   2026-01-10    Added promotion, Queen (major) or Knight (minor). Added some helper information.
;;  version 0.01r   2026-01-11    Bug repair for pawn take move. Added some opening moves and Mate-in-2 boards.
;;  version 0.01s   2026-01-12    Started the evaluator for the board position.
;;  version 0.01t   2026-01-12    Added FEN - first conversion from board to FEN.
;;  version 0.01u   2026-01-14    Complete reformatting opening library and refactoring functions.
;;  version 0.02a   2026-01-15    Starting 'best-move' function (now 1 ply only...)
;;  version 0.02b   2026-01-16    Only open e2-e4 for start-game (not for some 'mate in 2'...)
;;                                Working on the 'deep-search' function.
;;  version 0.02c   2026-01-17    Added some chess puzzles
;;  version 0.02d   2026-01-18    Removing the 'First-move' indicator, one function for a standard move of a piece
;;                                Refactoring of the standard move, erase & fill in one go (one nested loop)
;;                                Added a check for a draw position 
;;  version 0.02e   2026-01-20    Some refactoring - struggles with 'tree-search'
;;  version 0.02f   2026-01-21    More work on the 'tree-search' -- almost working 0K. 
;;  version 1.00a   2026-01-21    'tree-search' working 0K. Up to Mate-in-4 working 0K.
;;  version 1.00b   2026-01-22    Added test boards and some minor changes
;;  version 1.00c   2026-01-22    Added 'mate-in-1' check, refactored functions for more speed
;;                                Changes in Knight-position-bonus, now dependent on players colour
;;  version 1.10a   2026-01-26    A working version for Chez Scheme - run: chez chess.scm
;;  version 1.10b   2026-01-26    Small changes. Added + / - search depth
;;  version 1.20a   2026-01-27    Fixed the castling rules - no more illegal castling
;;
;; The code is very inefficient. So refactor...
;;
;;  (cl) 2025-12-31, 2026-01-27 by Arno Jacobs
;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---

;; A small speed increase of ~7%
(optimize-level 3)

(load "boards.scm")
(load "chess-pieces.scm")
(load "opening-library.scm")

;; Chess board dimensions
(define width  8)
(define height 8)

;; 
(define *search-depth* 3)  ;; mate-in-2
;; (define *search-depth* 4)  ;; just for a game 
;; (define *search-depth* 5)  ;; mate-in-3
;; (define *search-depth* 7)  ;; mate-in-4


(define *tracker* 0)

(define (track)
    (set! *tracker* (+ *tracker* 1))
    (display *tracker*)
    (display "\n"))



(define Checkmate 42)
(define Draw      77)
(define Quit      99)
(define QuitGame (list (list Quit Quit) (list Quit Quit))) ;; format as a move

(define AllMoves  64)
(define NoKing    62)

(define NoMoves '())

;; Helper for a promotion piece
(define *promotion-piece* Queen)

(define (minor-promotion)
  (set! *promotion-piece* Knight))

(define (major-promotion)
  (set! *promotion-piece* Queen))

(define (pretty-search-depth)
    (string-append
        "current search depth: "
        (number->string *search-depth*)
        " ply"))

;; Increase to a maximum of 9
(define (increase-search-depth)
    (if (< *search-depth* 9)
        (set! *search-depth* (+ *search-depth* 1)))
    (pretty-search-depth))

;; Decrease to a minimum of 1
(define (decrease-search-depth)
    (if (> *search-depth* 1)
        (set! *search-depth* (- *search-depth* 1)))
    (pretty-search-depth))


(define code-info
  (string-append
   "\n\n* a tiny and simple  (Lisp) (Scheme)  chess engine *\n\n"
   "version 1.10b  ("
   (pretty-search-depth)
   ")\n(cl) 2025-12-31, 2026-01-26  by  Arno Jacobs\n\n"))

(define (pretty-promotion-piece)
  (string-append "\nPromotion piece is set to " (pretty-type-plus *promotion-piece*) "\n" ))

(define (set-minor-promotion)
  (minor-promotion)
  (display (pretty-promotion-piece)))

(define (set-major-promotion)
  (major-promotion)
  (display (pretty-promotion-piece)))

;; Some helper functions for better readability
(define (first lst)  (car    lst))
(define (second lst) (cadr   lst))
(define (third lst)  (caddr  lst))
(define (fourth lst) (cadddr lst))
(define (rest lst)   (cdr    lst))

(define (nth lst n) (list-ref lst n))

(define (safe-nth lst n)
  (if (or (< n 0) (>= n (length lst)))
      '()
      (nth lst n)))

;; The sgn-function is not available in MIT-Scheme
(define (sgn x)
  (cond ((positive? x) 1)
        ((negative? x) -1)
        (else 0)))

(define (colour piece-value) (sgn piece-value))

(define (negate x) (- x))

(define (opponents-colour player-colour) (- player-colour))

(define (absolute-piece-value piece-value)
  (modulo (abs piece-value) 10))

;; All pretty functions convert board & piece data to strings
;; (case) won't work here and on more places, so going for a (cond) list
;; very readable
(define (pretty-type piece-value)
  (let ((apv (absolute-piece-value piece-value)))
        (cond ((= apv King)   "K")
              ((= apv Queen)  "Q")
              ((= apv Bishop) "B")
              ((= apv Knight) "N")
              ((= apv Rook)   "R")
              ((= apv Pawn)   "p")
              (else           " "))))

(define (pretty-state piece-value)
  (let ((pst (get-piece-state piece-value)))
        (cond ((= pst En-Passant) "x")
              ((= pst Castling)   "%")
              (else               " "))))

(define (set-pretty-colour piece-value) 
    (if (< piece-value 0)           ;; \033[1m for bold characters
        "\033[102m\033[30m\033[1m"  ;; 102 bright green background   30 black foreground
        "\033[40m\033[97m\033[1m")) ;; 40 black background           97 bright white forground

(define reset-pretty-colour
    "\033[0m")                      ;; reset code

(define (pretty-piece piece-value)
  (let ((piece-type   (pretty-type   piece-value))
        (piece-state  (pretty-state  piece-value)))
        (string-append (set-pretty-colour piece-value)
                       " " piece-type piece-state
                       reset-pretty-colour)))

(define (pretty-line-of-pieces piece-values)
  (if (null? piece-values)
      "\n\n"
      (string-append (pretty-piece (first piece-values))
                     "  "
                     (pretty-line-of-pieces (rest piece-values)))))

(define (pretty-board board line)
  (if (null? board)
      ""
      (string-append " "
                     (number->string line)
                     "   "
                     (pretty-line-of-pieces (first board))
                     (pretty-board (rest board) (- line 1)))))

(define (display-correct-board board)
  (display "\n\n\n")
  ;; reverse so the first row is at the bottom 
  (display (pretty-board (reverse board) height))
  (display "      a    b    c    d    e    f    g    h\n\n\n"))
      
;; Convert the board data to a string
(define (display-board board)
  (if (and (= (length board) width)
           (= (apply + (map length board)) (* height width)))
      (display-correct-board board)
      (display "Incorrect 'board' data...\n\n")))


;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;;; The readable list of all possible moves
;;

(define (pretty-colour-plus piece-colour)
  (if (= piece-colour white) "white" "black"))

(define (pretty-type-plus piece-type)
  (cond ((= piece-type King)   "King  ")
        ((= piece-type Queen)  "Queen ")
        ((= piece-type Bishop) "Bishop")
        ((= piece-type Knight) "Knight")
        ((= piece-type Rook)   "Rook  ")
        ((= piece-type Pawn)   "Pawn  ")
        (else                  " _    ")))
                          
(define (pretty-piece-plus piece-value)
  (let ((piece-colour (pretty-colour-plus (colour piece-value)))
        (piece-type   (pretty-type-plus (absolute-piece-value piece-value))))
    (string-append piece-colour " " piece-type)))

(define (pretty-move move)
  (let ((move-letter (string (integer->char (+ 96 (first  move)))))
        (move-number (string (integer->char (+ 48 (second move))))))
    (string-append move-letter move-number)))

(define (equal-piece? base-move test-move)
  (equal? (first base-move) (first test-move)))

(define (non-equal-piece? base-move test-move)
  (not (equal-piece? base-move test-move)))

(define (sweep-by-piece moves)
  (if (null? moves)
      NoMoves
      (let ((from-piece (first (first moves)))
            (from-moves (filter (lambda (t) (equal-piece?     (first moves) t)) moves))
            (rest-moves (filter (lambda (t) (non-equal-piece? (first moves) t)) moves)))
        (cons
         (cons from-piece (map second from-moves))
         (sweep-by-piece rest-moves)))))

(define (add-piece-value board moves)
  (let ((x (first  (first moves)))
        (y (second (first moves))))
    (cons
     (cons (location-value board x y)
           (list (first moves)))
     (rest moves))))
          
(define (add-piece-values board sweeped-moves)
  (map (lambda (moves) (add-piece-value board moves)) sweeped-moves))

(define (sweep board moves)
  (add-piece-values board (sweep-by-piece moves)))

(define (pretty-moves-plus from-move to-moves)
  (if (null? to-moves)
      ""
      (string-append from-move "-" (first to-moves) "  "
                     (pretty-moves-plus from-move (rest to-moves)))))

;;; List for one piece
(define (pretty-list-moves moves)
  (let ((piece-description (pretty-piece-plus (first (first moves))))
        (from-move  (pretty-move (second (first moves))))
        (to-moves   (map pretty-move (rest moves))))
    (string-append piece-description " :  " (pretty-moves-plus from-move to-moves) "\n")))

;;; A bit pretty print of all moves
(define (list-moves moves)
  (display (apply string-append (map pretty-list-moves moves))))


;; Reset the Castling state for a piece with a given colour
(define (erase-can-castling-state board x y)
  (fill-location board (get-piece-type (location-value board x y)) x y))
      
;; En-passant is only valid for one move
;;
(define (column-erase-en-passant-states piece-value)
  (if (can-take-En-Passant? piece-value)
      (get-piece-type piece-value)
      piece-value))
      
(define (row-erase-en-passant-states row-pieces)
  (map (lambda (piece) (column-erase-en-passant-states piece)) row-pieces))

(define (erase-en-passant-states board)
  (map (lambda (pieces) (row-erase-en-passant-states pieces)) board))

;; Next step is to generate a list of all possible moves on a board for the player at move
;; First - generate a list of all possible moves for a given location on the board
;; Locations on the chess board are a1..h8
;; In this code for 'a' through 'h' we use 1 through 8, so e2 is entered as (5 2)

(define (get-piece-type piece-value)
  (* (colour piece-value) (absolute-piece-value piece-value)))

(define (get-piece-state piece-value)
  (* 10 (quotient (abs piece-value) 10)))

(define (can-take-En-Passant? piece-value)
  (= En-Passant (get-piece-state piece-value)))

(define (can-do-Castling? piece-value)
  (= Castling (get-piece-state piece-value)))

(define (location-value board x y)
  (nth (nth board (- y 1)) (- x 1)))

(define (safe-location-value board x y)
    (if (or (< x 1) (> x width) (< y 1) (> y height))
      outside
      (nth (nth board (- y 1)) (- x 1))))

(define (is-empty? board x y)
  (= empty (location-value board x y)))

(define (safe-is-empty? board x y)
  (= empty (safe-location-value board x y)))

;; A standard check for a move of King or Knight
;; Either move to an empty spot or move for a take of opponents piece 
(define (check-move-King-Knight board x y piece-colour possible-move)
  (let ((next-piece-value
         (safe-location-value board
                              (+ x (first  possible-move))
                              (+ y (second possible-move)))))
    (and (not (= next-piece-value outside))
         (or (= next-piece-value empty)
             (= piece-colour (- (colour next-piece-value)))))))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; King moves
;;
;; Only check the standard King moves        
;; Move King ONLY to a free space, the king cannot land on a square that is under attack. 
(define (check-king-moves board x y piece-colour possible-moves)
  (filter (lambda (move) (check-move-King-Knight board x y piece-colour move)) possible-moves))

;;  The Official Rules for Castling
;;  Castling is permitted only if all of the following conditions are met: 
;;      Never Moved:                    The king and the involved rook must not have moved previously in the game.
;;      No Obstructions:                All squares between the king and the rook must be vacant.
;;      Not in Check:                   The king cannot currently be in check.
;;      No "Passing Through" Check:     The king cannot pass through a square that is under attack by an opponent's piece.
;;      No "Ending In" Check:           The king cannot land on a square that is under attack. 

(define (not-under-attack? opp-att x y)
    (not (member (list x y) opp-att)))

(define (can-do-a-short-castle? board x y piece-value opp-att)
    (and    (not-under-attack? opp-att    x    y)
            (not-under-attack? opp-att (+ x 1) y)
            (not-under-attack? opp-att (+ x 2) y)
            (is-empty? board (+ x 1) y)
            (is-empty? board (+ x 2) y)
            (=  (piece (colour piece-value) Rook Castling)
                (safe-location-value board (+ x 3) y))))

(define (can-do-a-long-castle? board x y piece-value opp-att) 
    (and    (not-under-attack? opp-att    x    y)
            (not-under-attack? opp-att (- x 1) y)
            (not-under-attack? opp-att (- x 2) y)
            (not-under-attack? opp-att (- x 3) y)
            (is-empty? board (- x 1) y)
            (is-empty? board (- x 2) y)
            (is-empty? board (- x 3) y)
            (=  (piece (colour piece-value) Rook Castling)
                (safe-location-value board (- x 4) y))))

(define (opponent-attacks board piece-colour)
    (let ((opponents-moves (all-moves-no-king board piece-colour)))
    (map second (apply append
        (map (lambda (moves) (strip-piece-value moves)) opponents-moves)))))
             
;; Here check the possible castling moves
(define (check-castling-king-moves board x y piece-value)
    (let ((opp-att (opponent-attacks board (opponents-colour (colour piece-value)))))
        (if (can-do-Castling? piece-value)
            (append (if (can-do-a-long-castle? board x y piece-value opp-att)
                        '((-3 0))
                        NoMoves)
                    (if (can-do-a-short-castle? board x y piece-value opp-att)
                        '((2 0))
                        NoMoves))
            NoMoves)))
                  
(define (king-moves board x y piece-value)
;;    (track)
    (let    ((standard-king-moves '((-1 -1) (0 -1) (1 -1)
                                    (-1  0)         (1  0)
                                    (-1  1) (0  1) (1  1))))
        (if (= (get-piece-state piece-value) Castling)
            (append (check-castling-king-moves board x y piece-value)
                    (check-king-moves board x y (colour piece-value) standard-king-moves))
            (check-king-moves board x y (colour piece-value) standard-king-moves))))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; Knight moves
;;
;; Check for all possible Knight moves  
(define (check-knight-moves board x y piece-colour possible-moves)
  (filter (lambda (move) (check-move-King-Knight board x y piece-colour move)) possible-moves))

(define (knight-moves board x y piece-value)
  (let ((all-knight-moves '( (-1 -2) (-1  2) (-2 -1) (-2 1)
                             ( 1 -2) ( 1  2) ( 2 -1) ( 2 1))))
    (check-knight-moves board x y (colour piece-value) all-knight-moves)))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; Pawn moves
;;
;; Check for all possible Pawn moves 
(define (reverse-direction move)
  (map (lambda (x) (negate x)) move))

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
(define (check-pawn-take board x y piece-colour take)
  (let ((take-piece-value
         (safe-location-value board
                              (+ x (first  take))
                              (+ y (second take)))))
    (and (not (= take-piece-value outside))
         (= piece-colour (- (colour take-piece-value))))))

;; En-passant check
(define (check-pawn-en-passant board x y piece-colour take)
  (let ((white-on-row-5 (and (= piece-colour white) (= y 5)))
        (black-on-row-4 (and (= piece-colour black) (= y 4)))
        (en-passant-piece-value
         (safe-location-value board (+ x (first take)) y))
        (take-piece-value
         (safe-location-value board
                              (+ x (first  take))
                              (+ y (second take)))))
    (if (or white-on-row-5 black-on-row-4)
        (and (not (= take-piece-value outside))
             (= (get-piece-state en-passant-piece-value) En-Passant)
             (= take-piece-value empty))
        #f)))

(define (check-pawn-moves board x y step-moves take-moves piece-colour direction first-move)
  (let ((moves (direct-moves
                (if first-move
                    step-moves
                    (list (first step-moves))) direction))
        (takes (direct-moves take-moves direction)))
    (append (filter (lambda (move) (check-pawn-move board x y move)) moves)
            (filter (lambda (take) (check-pawn-en-passant board x y piece-colour take)) takes)
            (filter (lambda (take) (check-pawn-take board x y piece-colour take)) takes))))

(define (pawn-moves board x y piece-value)
  (let ((step-moves '((0 1) (0 2)))
        (take-moves '((-1 1) (1 1)))
        (piece-colour (colour piece-value))
        (direction  (if (= (colour piece-value) white) 1 -1)))
    (define first-move
      (or (and (= y 2) (= piece-colour white) (is-empty? board x 3))
          (and (= y 7) (= piece-colour black) (is-empty? board x 6))))
    (check-pawn-moves board x y step-moves take-moves piece-colour direction first-move)))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; helper for Queen, Bishop and Rook moves
;;
(define (one-moves-list moves)
  (apply append (map (lambda (step) (safe-nth moves step)) '(0 1 2 3 4 5 6 7))))

(define (walk-directional-step board x y dx dy step piece-colour)
  (let ((px (+ x (* dx step)))
        (py (+ y (* dy step))))
    (append
     (cond ((= (safe-location-value board px py) outside) NoMoves)
           ((safe-is-empty? board px py)
            (cons (list (* dx step) (* dy step))
                  (walk-directional-step board x y dx dy (+ step 1) piece-colour)))
           ((not (= piece-colour (colour (safe-location-value board px py))))
            (list (list (* dx step) (* dy step))))
           (else NoMoves)))))

(define (moves-per-direction board x y piece-colour direction)
  (let ((dx (first  direction))
        (dy (second direction)))
    (walk-directional-step board x y dx dy 1 piece-colour)))

(define (moves-for-all-directions board x y piece-colour directions)
  (one-moves-list
   (map
    (lambda (direction) (moves-per-direction board x y piece-colour direction))
    directions)))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; Queen moves
;;
(define (queen-moves board x y piece-value)
  (let ((directions '((-1 -1) (0 -1) (1 -1)
                      (-1  0)        (1  0)
                      (-1  1) (0  1) (1  1)))
        (piece-colour (colour piece-value)))
    (moves-for-all-directions board x y piece-colour directions)))
    
;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; Bishop moves
;;
(define (bishop-moves board x y piece-value)
  (let ((directions '((-1 -1) (1 -1) (-1  1) (1  1)))
        (piece-colour (colour piece-value)))
    (moves-for-all-directions board x y piece-colour directions )))
    
;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; Rook moves
;;
(define (rook-moves board x y piece-value)
  (let ((directions '((0 -1) (-1  0) (1  0) (0  1)))
        (piece-colour (colour piece-value)))
    (moves-for-all-directions board x y piece-colour directions )))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; All moves for an arbitrary piece on a arbitrary position
;;
(define (moves-for-location board x y)
  (if (or (< x 1) (> x width) (< y 1) (> y height))
      NoMoves
      (let ((piece-value (location-value board x y)))
        (define apv (absolute-piece-value piece-value))
        (cond ((= apv King)   (king-moves   board x y piece-value))
              ((= apv Queen)  (queen-moves  board x y piece-value))
              ((= apv Bishop) (bishop-moves board x y piece-value))
              ((= apv Knight) (knight-moves board x y piece-value))
              ((= apv Rook)   (rook-moves   board x y piece-value))
              ((= apv Pawn)   (pawn-moves   board x y piece-value))
              (else           NoMoves)))))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; All moves for all pieces by colour
;;
;; example: ((3 (2 1)) (1 3) (3 3))
;; Piece value 3 is Knight  -  at (2 1) is b1  -  can move to (1 3) is a3 or (3 3) is c3
;;
(define (translate x y moves)
  (if (null? moves)
      NoMoves
      (let ((move (first moves)))
        (cons (list (+ x (first move)) (+ y (second move))) 
              (translate x y (rest moves))))))

(define (all-moves-x-y board piece-colour selection x y)
    (let*  ((piece-value (safe-location-value board x y))
            (piece-type (abs (get-piece-type piece-value))))
        (if (and    (= selection NoKing)
                    (= piece-type King))
            NoMoves
            (if (or (= piece-value outside)
                    (= piece-value empty)
                    (not (= piece-colour (colour piece-value))))
                NoMoves
                (let ((next-moves (moves-for-location board x y)))          
                (if (null? next-moves)
                    NoMoves
                    (cons   (list piece-value (list x y))
                            (translate x y next-moves))))))))

(define (all-moves-y board piece-colour selection y)
  (do ((x width (- x 1))
       (rv '() (cons (all-moves-x-y board piece-colour selection x y) rv)))
    ((< x 1) rv )))

(define (all-moves-loop board piece-colour selection)
  (do ((y height (- y 1))
       (rv '() (append (all-moves-y board piece-colour selection y) rv)))
    ((< y 1) rv)))

;; This is the function without the King-move - castling helper 
(define (all-moves-no-king board piece-colour)
  (filter (lambda (moves) (not (null? moves))) (all-moves-loop board piece-colour NoKing)))

;; This is the function without the King-check test 
(define (all-moves-nct board piece-colour)
  (filter (lambda (moves) (not (null? moves))) (all-moves-loop board piece-colour AllMoves)))

;; Here the list is first created via 'all-moves-1'
;; Next reshaped and checked for King-check-moves
;; As a last step reshaped the the 'moves-per-piece' list
(define (all-moves board player-colour)
  (sweep board (all-moves-list board player-colour)))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; Create a single list of all moves (from -> to) without piece value
;;
(define (strip-piece-value moves)
  (let ((from (second (first moves)))
        (to   (rest moves)))
    (map (lambda (to-move) (list from to-move)) to)))


(define (player-King-checked? board move piece-colour)
  (let ((test-board (make-move board move)))
    (is-checked-by? test-board (opponents-colour piece-colour))))

(define (remove-King-checked? board move piece-colour)
  (not (player-King-checked? board move piece-colour)))

(define (all-moves-list board piece-colour)
  (let ((all-possible-moves
         (apply append
                (map (lambda (moves) (strip-piece-value moves)) (all-moves-nct board piece-colour)))))
    (filter (lambda (move) (remove-King-checked? board move piece-colour)) all-possible-moves)))

;; Seperate function for 'Check' and 'Checkmate' testing
;; This is to prevent an endless recursive loop
(define (all-moves-list-check-test board piece-colour)
  (apply append (map (lambda (moves) (strip-piece-value moves)) (all-moves-nct board piece-colour))))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; Misc. function for making all the moves on the board
;;

(define (eq-location? x1 y1 x2 y2)
  (and (= x1 x2) (= y1 y2)))

;; Helper for erasing pawn after an en-passant move
(define (empty-location-y board ex ey y)
  (do ((x width (- x 1))
       (rv '() (cons (if (eq-location? ex ey x y)
                         empty
                         (location-value board x y)) rv)))
    ((< x 1) rv )))

(define (empty-location board ex ey)
  (do ((y height (- y 1))
       (rv '() (cons (empty-location-y board ex ey y) rv)))
    ((< y 1) rv)))


;; Helper for changing the state of a piece (like remove the castling option)
(define (fill-location-y board piece-value px py y)
  (do ((x width (- x 1))
       (rv '() (cons (if (eq-location? px py x y)
                         piece-value
                         (location-value board x y)) rv)))
    ((< x 1) rv )))

(define  (fill-location board piece-value px py)
  (do ((y height (- y 1))
       (rv '() (cons (fill-location-y board piece-value px py y) rv)))
    ((< y 1) rv)))

;; Erase the 'from' location and fill the 'to' location
(define (work-move-y board piece-value from-x from-y to-x to-y y)
  (do ((x width (- x 1))
       (rv '() (cons (cond ((eq-location? from-x from-y x y) empty)
                            ((eq-location? to-x   to-y   x y) piece-value)
                            (else                            (location-value board x y)))
                      rv )))
    ((< x 1) rv )))

(define  (work-move board piece-value from-x from-y to-x to-y)
  (do ((y height (- y 1))
       (rv '() (cons (work-move-y board piece-value from-x from-y to-x to-y y) rv)))
    ((< y 1) rv)))

(define (work-next-standard-move board from-x from-y to-x to-y)
  (let ((piece-type (get-piece-type (location-value board from-x from-y))))
    (work-move board piece-type from-x from-y to-x to-y)))

(define (set-En-Passant piece-type)
  (if (< piece-type 0)
      (- piece-type En-Passant)
      (+ piece-type En-Passant)))

(define (work-two-step-pawn-move board from-x from-y to-x to-y)
  (let ((piece-type (get-piece-type (location-value board from-x from-y))))
    (work-move board (set-En-Passant piece-type) from-x from-y to-x to-y)))

;; Work the En-Passant move!
(define (work-pawn-en-passant-take board from-x from-y to-x to-y)
  (let ((next-board (work-next-standard-move board from-x from-y to-x to-y)))
    (empty-location next-board to-x from-y)))

;; Work the Promotion of the pawn
;; Ask for a major or minor promotion
(define (work-promotion-move board from-x from-y to-x to-y)
  (let ((piece-colour (colour (location-value board from-x from-y))))
    (work-move board (* piece-colour *promotion-piece*) from-x from-y to-x to-y)))

(define (work-next-pawn-move board from-x from-y to-x to-y)
  ;; Test for promotion
  (if (or (= 1 to-y) (= height to-y))
      (work-promotion-move board from-x from-y to-x to-y)
      (if (and (= (abs (- from-x to-x)) 1) (is-empty? board to-x to-y))
          (work-pawn-en-passant-take board from-x from-y to-x to-y)
          (if (= (abs (- from-y to-y)) 1)
              (work-next-standard-move board from-x from-y to-x to-y)
              (work-two-step-pawn-move board from-x from-y to-x to-y)))))

(define (work-castling-move board from-x from-y to-x to-y)
  (define board-1 (work-next-standard-move board from-x from-y to-x to-y))
  (if (= to-x 7)
      (work-next-standard-move board-1 height from-y 6 to-y)
      (work-next-standard-move board-1 1      from-y 3 to-y)))
  
(define (work-next-king-move board from-x from-y to-x to-y)
  (if (> (abs (- from-x to-x)) 1)
      (work-castling-move board from-x from-y to-x to-y)
      (work-next-standard-move board from-x from-y to-x to-y)))
      
;; Move to piece
;; Change the state of a Pawn after its first move
;; Change the status of a Rook and King of their colour when any of the three have made a move
;;
(define (work-next-move board piece-type piece-colour from-x from-y to-x to-y)
 (cond ((= piece-type Pawn) (work-next-pawn-move board from-x from-y to-x to-y))
       ((= piece-type King) (work-next-king-move board from-x from-y to-x to-y))
       (else (work-next-standard-move board from-x from-y to-x to-y))))

(define (current-piece-data board x y)
  (let ((piece-value (location-value board x y)))
    (list (get-piece-type piece-value) (get-piece-state piece-value))))

;; Move is checked for legality
;; 
(define (make-move board move)
  (if (null? move)
      board
      (let ((from-x  (first  (first  move)))
            (from-y  (second (first  move)))
            (to-x    (first  (second move)))
            (to-y    (second (second move))))
        (define piece-data   (current-piece-data board from-x from-y))
        (define piece-type   (abs (first piece-data)))
        (define piece-colour (colour (first piece-data)))         
        (define piece-state  (second piece-data))
        (define next-board-1 (erase-en-passant-states board))
        (define next-board-2
          (if (= piece-state Castling)
              (if (or (= piece-type King) (= piece-type Rook))
                  (erase-can-castling-state next-board-1 from-x from-y)
                  next-board-1)
          next-board-1))
        (work-next-move next-board-2 piece-type piece-colour from-x from-y to-x to-y))))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; Check if a board position is 'Check'
;;
(define (get-position-y board player-piece y)
  (do ((x width (- x 1))
       (rv '() (cons (if (= player-piece (get-piece-type (location-value board x y)))
                         (list x y)
                         '()) rv)))
    ((< x 1) rv )))

(define (get-position board player-piece)
  (first
   (filter (lambda (ppos) (not (null? ppos))) 
           (apply append 
                  (do ((y height (- y 1))
                       (rv '() (cons (get-position-y board player-piece y) rv)))
                    ((< y 1) rv))))))

(define (is-checked-by? board player-colour)
  (let ((to-moves (map second (all-moves-list-check-test board player-colour)))
        (opponents-king-position (get-position board (* (opponents-colour player-colour) King))))
    (member opponents-king-position to-moves)))
    
;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; Check if a board position is a 'Checkmate'
;;
;; Assumption that the board position is already a 'check' situation
;; This to 'save' time. But it will always work correct.
;;
(define (single-check-test? board player-colour opponents-rescue-moves)
  (if (null? opponents-rescue-moves)
      #t
      (let ((rescue-board (make-move board (first opponents-rescue-moves))))
        (if (is-checked-by? rescue-board player-colour)
            (single-check-test? board player-colour (rest opponents-rescue-moves))
            #f))))

;; Test all opponents legal moves to see if the 'Check' can be rescued
;;
;; Also test for a draw.
;;
(define (is-checkmate-by? board player-colour)
  (let ((rescue-moves (all-moves-list-check-test board (opponents-colour player-colour))))
    (and (is-checked-by? board player-colour)
         (single-check-test? board player-colour rescue-moves))))

(define (is-stalemate? board player-colour)
  (let ((pieces (length (filter (lambda (piece) (not (= piece empty))) (apply append board)))))
  (or (= pieces 2) ;; only two kings left
      (null? (all-moves board player-colour)))))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; Read commands or moves from the keyboard
;;
;; First the simple parse only for format 'e2e4'
;; (for now 'e2e4' is the working format)
;;
;; Commands:
;;  b     show the current board
;;  c     play a Lisp-code-generated move
;;  e     show the evaluation score of the current board position
;;  f     show the FEN string of the current board position
;;  g     show all the previous moves
;;  h     show this helper information
;;  m     show all possible moves for the current player
;;  M     show all possible moves for the opponent
;;  p     set promotion piece to minor promotion (Knight) before the actual move
;;  P     set promotion piece to major promotion (Queen) before the actual move
;;  s     show the current promotion piece
;;  +/-   increase or decrease the search depth
;;  q     quit the game
;;

(define (display-helper-information)
  (display code-info)
  (display "helper information\n\n")
  (display "  commands:\n")
  (display "    b     show the current board\n")
  (display "    c     play a Lisp-code-generated move\n")
  (display "    e     show the evaluation score of the current board position\n")
  (display "    f     show the FEN string of the current board position\n")
  (display "    g     show all the previous moves\n")
  (display "    h     show this helper information\n")
  (display "    m     show all possible moves for the current player\n")
  (display "    M     show all possible moves for the opponent\n")
  (display "    p     set promotion piece to minor promotion (Knight) before the actual move\n")
  (display "    P     set promotion piece to major promotion (Queen) before the actual move\n")
  (display "    s     show the current promotion piece\n")
  (display "    +/-   increase or decrease the search depth\n")
  (display "    q     quit the game.\n\n")
  (display "the input format for a move: e2e4\n")
  (display (pretty-search-depth))
  (display "\n"))

(define (move-string-to-list move)
  (list (list (- (char->integer (first  move)) 96)
              (- (char->integer (second move)) 48))
        (list (- (char->integer (third  move)) 96)
              (- (char->integer (fourth move)) 48))))
  
(define (parse-move board player-colour entered-move game open-library?)
  (let ((move (string->list entered-move)))
    (if (null? move)
        NoMoves
        (let ((cmd (first move)))
          (if (= 4 (length move))
              (move-string-to-list move)
              (cond ((equal? cmd #\b) (display-board board))
                    ((equal? cmd #\c) (find-best-move game board player-colour open-library?))
                    ((equal? cmd #\e) (display-evaluation-score board player-colour))
                    ((equal? cmd #\f) (display-FEN board player-colour))
                    ((equal? cmd #\g) (display-game game))
                    ((equal? cmd #\h) (display-helper-information))
                    ((equal? cmd #\m) (list-moves (all-moves board player-colour)))
                    ((equal? cmd #\M) (list-moves (all-moves board (negate player-colour))))
                    ((equal? cmd #\p) (set-minor-promotion))
                    ((equal? cmd #\P) (set-major-promotion))
                    ((equal? cmd #\s) (display (pretty-promotion-piece)))
                    ((equal? cmd #\+) (display (increase-search-depth)))
                    ((equal? cmd #\-) (display (decrease-search-depth)))
                    ((equal? cmd #\q)  QuitGame)                    
                    (else              NoMoves)))))))

;; Read, parse and check for legality of the move
;; A legal move will return that move
;; otherwise it will return an empty list if the move is illegal
;;
(define (next-move board player-colour game open-library?)
    (display "\nNext move for player ")
    (display (pretty-colour-plus player-colour))
    (display ": ")
    (flush-output-port (current-output-port))
    (let*   ((input-move (get-line (current-input-port)))
             (move (parse-move board player-colour input-move game open-library?))
             (legal-moves (all-moves-list board player-colour)))
        (if (or (equal? move (void)) (null? move))
            (display "")
            (if (< (first (first move)) 9)
                (display (string-append "\nmove: " (pretty-from-to move)))
                (display "")))
        (if (or (equal? move QuitGame) (member move legal-moves))
            move
            NoMoves)))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; FEN conversions
;; First: open library format to FEN
;;
;; Change the '.' for empty fields to the number of consecutive empty fields
;;
;; First some FEN helpers - old openings library incl. helpers
;;
(define (get-piece-character piece-type)
  (cond ((= piece-type King)   #\K)
        ((= piece-type Queen)  #\Q)
        ((= piece-type Bishop) #\B)
        ((= piece-type Knight) #\N)
        ((= piece-type Rook)   #\R)
        ((= piece-type Pawn)   #\P)
        (else                  #\.)))

(define (convert-piece-for-openings-library piece-value)
  (let ((ptc (get-piece-character (absolute-piece-value piece-value))))
    (string
     (if (= (colour piece-value) black)
         (integer->char (+ 32 (char->integer ptc)))
         ptc))))
         
(define (opening-library-style-row board-row)
  (apply string-append
         (map convert-piece-for-openings-library board-row)))

(define (opening-library-style board)
    (apply string-append 
        (reverse (map opening-library-style-row board))))

(define (count-empties-seq rps)
    (if (null? rps)
        0
        (if (equal? (first rps) #\.)
            (+ 1 (count-empties-seq (rest rps)))
            0)))
    
(define (count-empties rps)
  (integer->char (+ (count-empties-seq rps) 48)))

(define (skip-empties rps)
  (if (null? rps)
      '()
      (if (equal? (first rps) #\.)
          (skip-empties (rest rps))
          rps)))
  
(define (to-FEN-style-row rps)
  (if (null? rps)
      '()
      (let ((f (first rps)))
        (if (equal? f #\.)
            (cons (count-empties rps) (to-FEN-style-row (skip-empties rps)))
            (cons f (to-FEN-style-row (rest rps)))))))

(define (to-FEN-style pls)
  (if (null? pls)
      '()
      (cons
       (cons #\/ (to-FEN-style-row (list-head pls width)))
       (to-FEN-style     (list-tail pls width)))))

;; Drop the first character of a string - safe
(define (string-rest s)
  (if (string=? s "")
      ""
      (list->string (rest (string->list s)))))

(define (ols-to-FEN-style ols player-colour)
  (let ((pls (string->list ols)))
    (string-append
     (string-rest
      (apply string-append
             (map list->string (to-FEN-style pls))))
     " "
     (if (= player-colour white) "w" "b")
     " - -")))

(define (display-FEN board player-colour)
    (let ((ols (opening-library-style board)))
        (display "\n")
        (display (ols-to-FEN-style ols player-colour))
        (display "\n")))

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

(define (get-piece-value-x-y board player-colour x y)
  (let ((piece-type  (location-value board x y))
        (pawn-factor (if (= player-colour white)
                         y
                         (- 9 y))))
    (if (or (= piece-type empty) (= (opponents-colour player-colour) (colour piece-type)))
        0
        (get-piece-value-2 (absolute-piece-value piece-type) pawn-factor))))
        
(define (get-pieces-value-y board player-colour y)
  (do ((x width (- x 1))
       (rv '() (cons (get-piece-value-x-y board player-colour x y) rv)))
    ((< x 1) rv )))

(define (get-pieces-value board player-colour)
  (do ((y height (- y 1))
       (rv '() (append (get-pieces-value-y board player-colour y) rv)))
    ((< y 1) rv)))

;; --- --- Knight bonusses code --- --- --- --- --- --- --- --- ---
;;
;; No range checking...
(define (knight-bonus kx ky player-colour)
  (if (= player-colour white)
      (nth (nth Knight-position-bonus-white (- ky 1)) (- kx 1))
      (nth (nth Knight-position-bonus-black (- ky 1)) (- kx 1))))

(define (knight-bonus-xy board player-colour kx ky)
  (let ((piece-type (location-value board kx ky)))
    (if (= piece-type (* player-colour Knight))
        (knight-bonus kx ky player-colour)
        0)))
    
(define (knight-bonusses-y board player-colour y)
  (do ((x width (- x 1))
       (rv '() (cons (knight-bonus-xy board player-colour x y) rv)))
    ((< x 1) rv )))

(define (knight-bonusses board player-colour)
  (apply + (do ((y height (- y 1))
                (rv '() (append (knight-bonusses-y board player-colour y) rv )))
             ((< y 1) rv))))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; My evaluate function - and yes, I'm not a good chess player
;;
(define (evaluate board player-colour)
  (let ((player-pieces-values    (get-pieces-value board player-colour))
        (opponents-pieces-values (get-pieces-value board (opponents-colour player-colour)))
        (sum-knight-bonus        (-  (knight-bonusses board player-colour)
                                     (knight-bonusses board (opponents-colour player-colour))))
        (checked-by-player       (is-checked-by? board player-colour))
        (checked-by-opponent     (is-checked-by? board (opponents-colour player-colour))))
    (apply + (list 
              (apply + player-pieces-values)
              (negate (apply + opponents-pieces-values))
              sum-knight-bonus
              (if checked-by-player
                  (if (is-checkmate-by? board player-colour)
                      Checkmate-value
                      Check-value)
                  0)
              (if checked-by-opponent
                  (if (is-checkmate-by? board (opponents-colour player-colour))
                      (negate Checkmate-value)
                      (negate Check-value))
                  0)))))

(define (display-evaluation-score board player-colour)
  (display "current board value: ")
  (display (evaluate board player-colour)))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; Here the code for a computer / Lisp-code generated legal move
;; Code will first check the 'opening library' - and be aware - there is NO check for legal moves on that library

;; Pick a random element from a list
(define (random-element ls)
  (nth ls (random (length ls))))

;; Look up board position in opening library
;;
(define (get-opening-move game library-game)
  (let ((gln (length game))
        (lln (length library-game)))
    (if (< gln lln)
        (if (equal? game (list-head library-game gln))
            (first (list-tail library-game gln))
            '())
        '())))

(define (get-opening-library-moves game library)
  (filter (lambda (ls) (not (null? ls)))
          (map (lambda (library-game) (get-opening-move game library-game)) library)))

(define (find-best-move game board player-colour open-library?)
  (if (and (null? game) open-library?)
      (list (list 5 2) (list 5 4))      ;; white will always open with "e2-e4" with a new game
      (if open-library?                 ;; in case of a 'mate-in-2' like puzzle... 
          (if (= player-colour white)
              (let ((open-moves (get-opening-library-moves (reverse game) opening-library-white)))
                (if (null? open-moves)
                    (best-move board player-colour)
                    (random-element open-moves)))
              (let ((open-moves (get-opening-library-moves (reverse game) opening-library-black)))
                (if (null? open-moves)
                    (best-move board player-colour)
                    (random-element open-moves))))
          (best-move board player-colour))))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; (deep) tree search - min-max only   -   no alpha-beta pruning
;;
(define (max-scores scores)
  (let ((max-score (apply max (map first scores))))
    (filter (lambda (score) (= (first score) max-score)) scores)))

(define (search-tree move board player-colour depth)
  (if (null? move)
      Stalemate-value ;; Either this or the negative value ???
      (let ((next-board (make-move board move)))
        (define board-score (evaluate next-board player-colour))
        (if (> board-score Checkmate-range)
            (* depth Checkmate-value)
            (if (= depth 1)
                board-score
                (let ((next-opponents-moves (all-moves-list next-board (opponents-colour player-colour))))
                  (if (null? next-opponents-moves)
                      (negate Stalemate-value)
                      (negate (apply max
                                     (map (lambda (move)
                                            (search-tree move
                                                         next-board
                                                         (opponents-colour player-colour)
                                                         (- depth 1))
                                            ) next-opponents-moves ))))))))))

;; A quick check if there is a mate-in-1 move
;; So there will be no useless deep tree-search for other moves
;;
(define (get-mate-in-1 board moves player-colour)
  (let ((next-boards (map (lambda (move) (make-move board move)) moves)))
    (define scored-boards
      (map (lambda (next-board) (evaluate next-board player-colour)) next-boards))
    (define scored-moves (map list scored-boards moves))
    (define rls (max-scores scored-moves))
    (if (null? rls)
        NoMoves
        (if (> (first (first rls)) Checkmate-range)
            (second (first rls))
            NoMoves))))

(define (best-move board player-colour)
  (let ((next-player-moves (all-moves-list board player-colour)))
    (define mate-in-1 (get-mate-in-1 board next-player-moves player-colour))
    (if (null? mate-in-1)
        (let ((scored-moves
               (map (lambda (move)
                      (list (search-tree move board player-colour *search-depth*) move)) next-player-moves)))
                (second (random-element (max-scores scored-moves))))
        mate-in-1)))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; Display the list of moves made in the game
;;
(define (pretty-from-to move)
  (string-append
   (pretty-move (first move))
   "-"
   (pretty-move (second move))))
  
(define (pretty-game game)
  (if (null? game)
      "\n"
      (if (= (length game) 1)
          (string-append (pretty-from-to (first  game)) "\n")
          (string-append (pretty-from-to (first  game)) "  "
                         (pretty-from-to (second game)) "\n"
                         (pretty-game (rest (rest game)))))))
                     
(define (display-game rgame)
  (let ((game (reverse rgame)))
    (if (null? game)
        (display "")
        (display (string-append "\nThe played game:\n" (pretty-game game))))))
  
;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; Minimal game loop
;;
(define (get-move board player-colour game open-library?)
  (let ((move (next-move board player-colour game open-library?)))
    (if (null? move)
        (get-move board player-colour game open-library?)
        move)))

(define (show-check board player-colour)
  (if (is-checked-by? board player-colour)
      (string-append "\n* Check!  by " (pretty-colour-plus player-colour) "\n\n")
      ""))

;; Function to show to board with the checkmate position
(define (checkmate-board board player-colour game)
  (display-board board)
  (cons (* player-colour Checkmate) game))

;; Function to show to board with the a draw position
(define (draw-board board player-colour game)
  (display-board board)
  (cons (* player-colour Draw) game))

(define (game-loop board player-colour game open-library?)
  (display-board board)
  (display (show-check board (opponents-colour player-colour)))
  (let ((move (get-move board player-colour game open-library?)))
    (if (equal? move QuitGame)
        (cons Quit game)
        (let ((next-board (make-move board move))
              (next-game  (cons move game)))
          (if (is-checkmate-by? next-board player-colour)
              (checkmate-board next-board player-colour next-game)
              (if (is-stalemate? next-board (opponents-colour player-colour))
                  (draw-board next-board player-colour next-game)
                  (game-loop next-board (opponents-colour player-colour) next-game open-library?)))))))

;; Start the game-loop en show end status
;; Incl. Quit or Checkmate test
(define (play-chess board player-colour)
  (define open-library? (and (equal? board initial-board)
                             (= player-colour white)))
  (display code-info)
  (display "\nHave a good game!\n\n")
  (let ((game (game-loop board player-colour NoMoves open-library?)))
    (display-game (rest game))
    (let ((game-state (abs (first game))))
        (cond ((= game-state Checkmate)
            (display (string-append
                        "\n* * *  Checkmate! * * *   by "
                        (pretty-colour-plus (colour (first game)))
                        "\n\nThank you and bye bye.\n\n")))
            ((= game-state Draw)
                (display (string-append
                        "\n* a draw * "
                        "\n\nThank you and bye bye.\n\n")))
            (else             
                (display "\nYou quit. Thank you and bye bye.\n\n")))))
    (exit))

;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---
;; Running, testing, debugging . . .
;;
(define (full) (play-chess initial-board      white))

(define (m2w1) (play-chess Mate-in-2-white-01 white))
(define (m2w2) (play-chess Mate-in-2-white-02 white))
(define (m2w3) (play-chess Mate-in-2-white-03 white))
(define (m2w4) (play-chess Mate-in-2-white-04 white))
(define (m2w5) (play-chess Mate-in-2-white-05 white))
(define (m2w6) (play-chess Mate-in-2-white-06 white))
(define (m2w7) (play-chess Mate-in-2-white-07 white))
(define (m2b1) (play-chess Mate-in-2-black-01 black))   ;; black

(define (m4w1) (play-chess Mate-in-4-white-01 white))
(define (mNw1) (play-chess Mate-in-N-white-01 white))
(define (ct1)  (play-chess castling-tests     white))


;; Mate in 2
;;(m2w1)
;;(m2w2)
;;
(m2w3)
;;(m2w4)
;;(m2w5)
;;(m2w6)
;;(m2w7)
;;(m2b1)

;; misc.
;;(m4w1)
;;(mNw1)
;;(ct1)

;;(full)


;;
;; End of code.
;; ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ---

;; THIS FILE IS FOR OO-EVAL, AND IS LOADED WHEN YOU (run-game 'yourname)
;; DO NOT TRY TO RUN DIRECTLY IN RACKET

;; Import the classes for the world
(run-file "oo-types.scm")

;; Some useful example expressions...
; (setup 'ben-bitdiddle)
; (run-clock 5)
; (screen 'DEITY-MODE #f)
; (screen 'DEITY-MODE #t)
; (me 'LOOK-AROUND)
; (me 'TAKE (thing-named 'engineering-book))
; (me 'GO 'up)
; (me 'GO 'down)
; (me 'GO 'north)


;;;;;;;;;;
;; Utilities for interacting with the system

;; thing-named
;;
;; Grab any kind of thing from avatar's location, given its name.  The
;; thing may be in the possession of the place, or in the possession of
;; a person at the place.  THING-NAMED SHOULD NEVER BE USED IN CLASS
;; DEFINITIONS OR ANY OBJECT YOU CREATE.
(define (thing-named name)
  (define place (me 'LOCATION))
  (define peek-stuff (me 'PEEK-AROUND))
  (define my-stuff (me 'THINGS))
  (define things (place 'THINGS))
  (define all-things (append things (append my-stuff peek-stuff)))
  (define things-named (filter (lambda (x) (eq? name (x 'NAME)))
                               all-things))
  (cond ((null? things-named)
         (error "In here there is nothing named" name))
        ((null? (cdr things-named))   ; just one thing
         (car things-named))
        (else
         (display-message (list "There is more than one thing named"
                                name "here. Picking one of them."))
         (pick-random things-named))))

;; place-named
;;
;; Return a place object, given its name
(define (place-named name)
  (let ((results (filter (lambda (place)
                           (eq? (place 'NAME) name))
                         all-places)))
    (cond ((null? results)
           (error "No place named" name))
          ((null? (cdr results))
           (car results))
          (else
           (display-message (list "There is more than one place named"
                                  name ". Picking one of them."))
           (pick-random results)))))

;; person-named
;;
;; return a person, given its name
(define (person-named name)
  (let ((results (filter (lambda (p) (and ((is-a 'PERSON) p) (eq? (p 'NAME) name)))
                         ((me 'LOCATION) 'THINGS))))
    (cond ((null? results)
           (error "No person named" name))
          ((null? (cdr results))
           (car results))
          (else
           (display-message (list "There is more than one person named"
                                  name "here.  Picking one of them."))
           (pick-random results)))))

;; all-people
;;
;; utility for finding all the people in the world
(define (all-people)
  (append-map (lambda (place) (filter (is-a 'PERSON) (place 'THINGS)))
              all-places))



;;;;;;;;;;
;; World setup

;; create-world
;;
;; This sets up places, connects them, and populates them with objects.
;; It returns a list of the places.

(define (create-world)
  ; Create some places
  (let ((can-go-both-ways
         (lambda (from direction reverse-direction to)
           (new exit from direction to)
           (new exit to reverse-direction from)))

        (great-dome     (new place 'great-dome))
        (little-dome    (new place 'little-dome))
        (lobby-10       (new place 'lobby-10))
        (10-250         (new place '10-250))
        (barker-library (new place 'barker-library))
        (lobby-7        (new place 'lobby-7))
        (infinite       (new place 'infinite-corridor))
        (bldg-26        (new place 'bldg-26))
        (cp32           (new place 'bldg-32-cp-hq))
        (tunnel         (new place 'lab-supply-room))
        (32-123         (new place '32-123))
        (32G            (new place 'gates-tower-roof))
        (32D            (new place 'dreyfoos-tower-roof))
        (student-street (new place 'student-street))
        (great-court    (new place 'great-court))
        (bldg-54        (new place 'green-building-roof))
        (the-dot        (new place 'the-dot))
        (dorm-row       (new place 'dorm-row)))

    ; Connect up places
    (can-go-both-ways lobby-10 'up 'down 10-250)
    (can-go-both-ways 10-250 'up 'down barker-library)
    (can-go-both-ways barker-library 'up 'down great-dome)
    (can-go-both-ways lobby-10 'west 'east lobby-7)
    (can-go-both-ways lobby-7 'west 'east dorm-row)
    (can-go-both-ways lobby-7 'up 'down little-dome)
    (can-go-both-ways lobby-10 'south 'north great-court)
    (can-go-both-ways lobby-10 'east 'west infinite)
    (can-go-both-ways infinite 'north 'south bldg-26)
    (can-go-both-ways infinite 'east 'west bldg-54)
    (can-go-both-ways bldg-26 'east 'west student-street)
    (can-go-both-ways student-street 'down 'up cp32)
    (can-go-both-ways cp32 'south 'north tunnel)
    (can-go-both-ways tunnel 'up 'down bldg-54)
    (can-go-both-ways bldg-54 'south 'north the-dot)
    (can-go-both-ways the-dot 'west 'east great-court)
    (can-go-both-ways student-street 'in 'out 32-123)
    (can-go-both-ways student-street 'up 'down 32G)
    (can-go-both-ways student-street 'skew 'down 32D)

    ; Create some things
    (new thing 'blackboard 10-250)
    (new thing 'lovely-trees great-court)
    (new thing 'flag-pole great-court)

    (list great-dome little-dome lobby-10
          10-250 barker-library lobby-7
          infinite bldg-26 cp32
          tunnel 32-123 32D 32G
          student-street great-court bldg-54 the-dot
          dorm-row)))

;; create-students
;;
;; Each of the students is created in the same place, and later moved.
(define (create-students)
  (let ((dorm (place-named 'dorm-row)))
    (map (lambda (name)
           (new autonomous-person name dorm 4 4))
         '(ben-bitdiddle
           alyssa-p-hacker
           louis-reasoner
           cy-d-fect
           lem-e-tweakit
           dr-v
           gjs))))

;; scatter-people
;;
;; Moves each of the given set of people to a random one of the places
;; specified.
(define (scatter-people people places)
  (for-each
   (lambda (p) (p 'CHANGE-LOCATION (pick-random places)))
   people))

;; create-stuff
;;
;; Creates some things for people to wander around with, in random
;; places
(define (create-stuff places)
  (for-each (lambda (name)
              (new mobile-thing name (pick-random places)))
            '(dollar-bill
              math-book
              coin))
  ;; PROBLEM 8
  (for-each (lambda (name)
              (new hunt-puzzle name (pick-random places)))
            '(hunt-puzzle
              hunt-puzzle
              hunt-puzzle)))

;; weapons
;;
;; Drop in some items to beat back the horde in random places
(define (create-weapons places)
  (for-each (lambda (name)
              (new weapon name (pick-random places) (+ 1 (random 4))))
            '(tons-of-code
              6001-quiz
              inflatable-lambda
              shotgun
              chainsaw)))

(define me 'will-be-set-by-setup)
(define all-places 'will-be-set-by-setup)
(define (setup name)
  (our-clock 'RESET)

  (set! all-places (create-world))

  (scatter-people (create-students) all-places)
  ;; PROBLEM 11
  (create-zombie (pick-random (all-people)))

  (create-stuff all-places)
  (create-weapons all-places)

  (set! me (new avatar name (pick-random all-places)))
  (screen 'SET-ME me)
  (me 'LOOK-AROUND)
  'ready)

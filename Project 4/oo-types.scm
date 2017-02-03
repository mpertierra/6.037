;; THIS FILE IS FOR OO-EVAL, AND IS LOADED WHEN YOU (run-game 'yourname)
;; DO NOT TRY TO RUN DIRECTLY IN RACKET

;; Import the helper functions and singleton objects for managing global
;; state (the clock for event management, and the screen for display).
(run-file "oo-util.scm")

(define named-object
  (make-class
   'NAMED-OBJECT
   root-class
   (:name)
   ((CONSTRUCTOR
     (lambda (name)
       (set! :name name)
       'installed))
    (NAME     (lambda () :name))
    (DESTROY  (lambda () 'destroyed)))))

(define container
  (make-class
   'CONTAINER
   named-object
   (:things)
   ((CONSTRUCTOR
     (lambda (name)
       (super 'CONSTRUCTOR name)
       (set! :things '())))
    (ADD-THING
     (lambda (thing)
       (set! :things
             (cons thing :things))
       'added))
    (DEL-THING
     (lambda (thing)
       (set! :things (delq thing :things))
       'removed))
    (THINGS       (lambda () :things))
    (HAVE-THING?  (lambda (thing) (memq thing :things))))))

(define thing
  (make-class
   'THING
   container
   (:location)
   ((CONSTRUCTOR
     (lambda (name location)
       (super 'CONSTRUCTOR name)
       (set! :location location)
       (:location 'ADD-THING self)
       'installed))
    (LOCATION
     (lambda () :location))
    (EMIT
     (lambda (text)
       (screen 'TELL-ROOM :location
               (append (list "At" (:location 'NAME))
                       text))))
    (DESTROY
     (lambda ()
       (:location 'DEL-THING self)
       (super 'DESTROY))))))

(define mobile-thing
  (make-class
   'MOBILE-THING
   thing
   ()
   ((CHANGE-LOCATION
     (lambda (newloc)
       (:location 'DEL-THING self)
       (set! :location newloc)
       (:location 'ADD-THING self)
       newloc))
    (ENTER-ROOM    (lambda () #t))
    (LEAVE-ROOM    (lambda () #t))
    )))

(define weapon
  (make-class
   'WEAPON
   mobile-thing
   (:damage)
   ((CONSTRUCTOR
     (lambda (name location damage)
       (super 'CONSTRUCTOR name location)
       (set! :damage damage)))
    (DAMAGE
     (lambda () :damage))
    (HIT
     (lambda (perp target)
       (let ((dam (+ 1 (random :damage))))
         (perp 'EMIT (list (perp 'NAME) "hits" (target 'NAME) "with"
                           (self 'NAME) "doing" dam "damage!"))
         (target 'SUFFER dam perp))))
     )))

(define (exit-named exits name)
  (cond ((null? exits)
         #f)
        ((eq? ((car exits) 'NAME) name)
         (car exits))
        (else (exit-named (cdr exits) name))))

(define place
  (make-class
   'PLACE
   container
   (:exits)
   ((CONSTRUCTOR
     (lambda (name)
       (super 'CONSTRUCTOR name)
       (set! :exits '())))
    (EXITS         (lambda () :exits))
    (EXIT-TOWARDS  (lambda (direction) (exit-named :exits direction)))
    (ADD-EXIT
     (lambda (exit)
       (let ((direction (exit 'DIRECTION)))
         (if (self 'EXIT-TOWARDS direction)
             (error (list (self 'NAME) "already has exit" direction))
             (set! :exits (cons exit :exits)))
         'done)))
    )))

(define exit
  (make-class
   'EXIT
   named-object
   (:from :to)
   ((CONSTRUCTOR
     (lambda (from direction to)
       (super 'CONSTRUCTOR direction)
       (set! :from from)
       (set! :to to)
       (if (not (null? :from))
           (:from 'ADD-EXIT self)
           #f)))
    (FROM      (lambda () :from))
    (TO        (lambda () :to))
    (DIRECTION (lambda () :name))
    (USE
     (lambda (whom)
       (whom 'LEAVE-ROOM)
       (screen 'TELL-ROOM (whom 'LOCATION)
               (list (whom 'NAME)
                     "moves from"
                     ((whom 'LOCATION) 'NAME)
                     "to"
                     (:to 'NAME)))
       (whom 'CHANGE-LOCATION :to)
       (whom 'ENTER-ROOM))))))

(define (random-exit place)
  (pick-random (place 'EXITS)))

; Given a list of objects, returns a list of their names.
(define (names-of objects)
  (map (lambda (x) (x 'NAME)) objects))

(define person
  (make-class
   'PERSON
   mobile-thing
   (:health :birthplace)
   ((CONSTRUCTOR
     (lambda (name location)
       (super 'CONSTRUCTOR name location)
       (set! :health 3)
       (set! :birthplace location)))
    (SAY
     (lambda (list-of-stuff)
       (screen 'TELL-ROOM :location
               (append (list "At" (:location 'NAME)
                             (self 'NAME) "says --")
                       list-of-stuff))
       'said-and-heard))
    (PEOPLE-AROUND
     (lambda ()
       (delq self (filter (is-a 'PERSON) (:location 'THINGS)))))
    (STUFF-AROUND
     (lambda ()
       (filter (lambda (x) (not ((is-a 'PERSON) x))) (:location 'THINGS))))
    (PEEK-AROUND
     (lambda ()
       (fold-right append
                   '()
                   (map (lambda (p) (p 'THINGS))
                        (self 'PEOPLE-AROUND)))))
    (TAKE
     (lambda (thing)
       (cond ((self 'HAVE-THING? thing)
              (self 'SAY (list "I am already carrying"
                               (thing 'NAME)))
              #f)
             ((or ((is-a 'PERSON) thing)
                  (not ((is-a 'MOBILE-THING) thing)))
              (self 'SAY (list "I try but cannot take"
                               (thing 'NAME)))
              #f)
             (else
              (let ((owner (thing 'LOCATION)))
                (self 'SAY (list "I take" (thing 'NAME)
                                 "from" (owner 'NAME)))
                (if ((is-a 'PERSON) owner)
                    (owner 'LOSE thing self)
                    (thing 'CHANGE-LOCATION self))
                'taken)))))
    (DROP
     (lambda (thing)
       (cond ((self 'HAVE-THING? thing)
	      (self 'SAY (list "I drop" (thing 'NAME)))
	      (thing 'CHANGE-LOCATION :location)
	      'dropped)
	     (else
	      (self 'SAY (list "I am not carrying" (thing 'NAME)))
	      #f))))
    (HAVE-FIT
     (lambda ()
       (self 'SAY (list "Yaaah! I am upset!"))
       'I-feel-better-now))
    (LOSE
     (lambda (thing to)
       (self 'SAY (list "Hey! You took my" (thing 'NAME)))
       (self 'HAVE-FIT)
       (thing 'CHANGE-LOCATION to)))
    (GO-EXIT
     (lambda (exit)
       (exit 'USE self)))
    (GO
     (lambda (direction)
       (let ((exit ((self 'LOCATION) 'EXIT-TOWARDS direction)))
         (if (and exit ((is-a 'EXIT) exit))
             (begin
               (self 'GO-EXIT exit)
               #t)
             (begin
               (screen 'TELL-ROOM :location
                       (list "No exit in" direction "direction"))
               #f)))))
    (ENTER-ROOM
     (lambda ()
       (let ((others (self 'PEOPLE-AROUND)))
         (if (not (null? others))
             (self 'SAY (cons "Hi" (names-of others)))
             #f))))
    (SUFFER
     (lambda (amt inflictor)
       (self 'SAY (list "Ouch!" amt "hits is more than I want!"))
       (set! :health (- :health amt))
       (if (< :health 1)
           (self 'DIE inflictor)
           'not-dead-yet)))
    (DIE
     (lambda (perp)
       (for-each (lambda (item) (self 'DROP item))
                 :things)
       (self 'DESTROY)))
    )))

(define autonomous-person
  (make-class
   'AUTONOMOUS-PERSON
   person
   (:restlessness :miserly)
   ((CONSTRUCTOR
     (lambda (name location rest miser)
       (super 'CONSTRUCTOR name location)
       (set! :restlessness rest)
       (set! :miserly miser)
       (our-clock 'ADD-CALLBACK
                  (new clock-callback 'time-passes self 'ACT))))
    (ACT
     (lambda ()
       ;; PROBLEM 9
       (for-each
          (lambda (method) (if (symbol-prefix? 'AUTO- method) (self method) 'do-nothing))
          ((self 'GET-CLASS) 'GET-METHODS))))
    (AUTO-MOVE
     (lambda ()
       (if (< (random 10) :restlessness)
           (self 'MOVE-SOMEWHERE)
           'just-chillin)))
    (MOVE-SOMEWHERE
     (lambda ()
       (let ((exit (random-exit (self 'LOCATION))))
         (if exit (self 'GO-EXIT exit) 'nowhere-to-go))))
    (AUTO-TAKE
     (lambda ()
       (if (< (random 10) :miserly)
           (self 'TAKE-SOMETHING)
           'not-a-klepto)))
    (TAKE-SOMETHING
     (lambda ()
       (let ((stuff-in-room (self 'STUFF-AROUND))
             (other-peoples-stuff (self 'PEEK-AROUND)))
         (let ((pick-from (append stuff-in-room other-peoples-stuff)))
           (if (not (null? pick-from))
               (self 'TAKE (pick-random pick-from))
               'nothing-to-take)))))
    (DIE
     (lambda (perp)
       (self 'SAY (list "Lights..growing..dim..."))
       (our-clock 'REMOVE-CALLBACK self 'time-passes)
       (super 'DIE perp)))
    ;; PROBLEM 12
    (AUTO-CURE
      (lambda ()
         (let ((zombie (pick-random (filter (is-a 'ZOMBIE) (self 'PEOPLE-AROUND))))
               (antidote (pick-random (filter (is-a 'WEAPONIZED-CAFFEINE) (self 'THINGS)))))
            (cond
              ((not antidote) 'no-cure)
              ((not zombie) 'no-zombie-to-cure)
              (else (antidote 'HIT self zombie))))))
    )))

;; PROBLEM 11
(define zombie-mixin
  (make-mixin 'ZOMBIE
    (list
      (list 'BECOME-ZOMBIE
        (lambda ()
          (set! :miserly 0) ;; Prevents zombie from taking things
          (self 'SAY (list "uuuuUUUUuuuuh.. brains..."))))
      (list 'NAME (lambda () (symbol-append 'zombie-of- :name)))
      (list 'AUTO-BITE
       (lambda ()
         (if (= (random 2) 0)
             (self 'BITE-SOMEONE)
             'not-a-biting-mood)))
      (list 'BITE-SOMEONE
       (lambda ()
         (let ((victim (pick-random
                        (filter (lambda (x) (and ((is-a 'AUTONOMOUS-PERSON) x)
                                                 (not ((is-a 'ZOMBIE) x))))
                                (self 'PEOPLE-AROUND)))))
           (if victim
               (if (= (random 2) 0)
                   ;; PROBLEM 8
                   (if (null? (filter (is-a 'HUNT-PUZZLE) (victim 'THINGS)))
                     (begin
                       (self 'EMIT (list (self 'NAME) "bites"
                                         (victim 'NAME)))
                       (create-zombie victim))
                     (begin
                       (self 'EMIT (list (self 'NAME) "bites at"
                                     (victim 'NAME) "but is repulsed by hunt-puzzle"))
                       (self 'SAY (list "uuuuUUUUuuuuh.. need brains to solve hunt-puzzle..."))))
                   (self 'EMIT (list (self 'NAME) "bites at"
                                     (victim 'NAME) "but misses")))
               'nobody-to-bite))))
      )))

;; ZOMBIE class no longer needed, using ZOMBIE mixin instead
; (define zombie
;   (make-class
;    'ZOMBIE
;    autonomous-person
;    ()
;    ((CONSTRUCTOR
;      (lambda (name location rest)
;        (super 'CONSTRUCTOR name location rest 0)
;        (self 'SAY (list "uuuuUUUUuuuuh.. brains..."))))
;     (NAME
;      (lambda ()
;        (symbol-append 'zombie-of- :name)))
;     ;; PROBLEM 9
;     ;; this code is no longer necessary, as calling ACT on a zombie object will
;     ;; invoke the ACT method of it's parent class (autonomous-person)
;     ; (ACT
;     ;  (lambda ()
;     ;    (super 'ACT)
;     ;    (self 'AUTO-BITE)))
;     (AUTO-BITE
;      (lambda ()
;        (if (= (random 2) 0)
;            (self 'BITE-SOMEONE)
;            'not-a-biting-mood)))
;     (BITE-SOMEONE
;      (lambda ()
;        (let ((victim (pick-random
;                       (filter (lambda (x) (and ((is-a 'AUTONOMOUS-PERSON) x)
;                                                (not ((is-a 'ZOMBIE) x))))
;                               (self 'PEOPLE-AROUND)))))
;          (if victim
;              (if (= (random 2) 0)
;                  ;; PROBLEM 8
;                  (if (null? (filter (is-a 'HUNT-PUZZLE) (victim 'THINGS)))
;                    (begin
;                      (self 'EMIT (list (self 'NAME) "bites"
;                                        (victim 'NAME)))
;                      (create-zombie self victim
;                                     :restlessness))
;                    (begin
;                      (self 'EMIT (list (self 'NAME) "bites at"
;                                    (victim 'NAME) "but is repulsed by hunt-puzzle"))
;                      (self 'SAY (list "uuuuUUUUuuuuh.. need brains to solve hunt-puzzle..."))))
;                  (self 'EMIT (list (self 'NAME) "bites at"
;                                    (victim 'NAME) "but misses")))
;              'nobody-to-bite))))
;     )))

;; PROBLEM 11
(define (create-zombie victim)
    (instance-add-mixin! victim zombie-mixin)
    (victim 'BECOME-ZOMBIE)
    'converted)

(define avatar
  (make-class
   'AVATAR
   person
   ()
   ((LOOK-AROUND          ; report on world around you
     (lambda ()
       (let ((exits (:location 'EXITS))
             (other-people (self 'PEOPLE-AROUND))
             (my-stuff :things)
             (stuff (self 'STUFF-AROUND)))
         (screen 'TELL-WORLD (list "You are in" (:location 'NAME)))
         (screen 'TELL-WORLD
                 (if (null? my-stuff)
                     '("You are not holding anything.")
                     (append '("You are holding:") (names-of my-stuff))))
         (screen 'TELL-WORLD
                 (if (null? stuff)
                     '("There is no stuff in the room.")
                     (append '("You see stuff in the room:") (names-of stuff))))
         (screen 'TELL-WORLD
                 (if (null? other-people)
                     '("There are no other people around you.")
                     (append '("You see other people:") (names-of other-people))))
         (screen 'TELL-WORLD
                 (if (not (null? exits))
                     (append '("The exits are in directions:") (names-of exits))
                     '("There are no exits...")))
         'OK)))
    (GO
     (lambda (direction)
       (let ((success? (super 'GO direction)))
         (if success? (our-clock 'TICK) #f)
         (self 'LOOK-AROUND))))
    )))

;; PROBLEM 8
(define hunt-puzzle
  (make-class
    'HUNT-PUZZLE
    mobile-thing
    ()
    ()))

;; PROBLEM 8
;; To test that it works, I created a bunch (~10) of hunt puzzles, and 
;; ran the clock, waiting until a zombie attempted to bite a person holding
;; a hunt-puzzle.

;; Here we see, in clock tick 11, louis-reasoner picks up a hunt-puzzle, and
;; in clock tick 15, zombie-of-ben-bitdiddle attempts to bite louis-reasoner
;; but is unable to do so because he is holding a hunt-puzzle.

; --- Clock tick 11 --- 
; louis-reasoner moves from lobby-10 to great-court 
; At great-court louis-reasoner says -- I take hunt-puzzle from great-court 
; cy-d-fect moves from lobby-10 to 10-250 
; lem-e-tweakit moves from lobby-10 to 10-250 
; At 10-250 lem-e-tweakit says -- Hi cy-d-fect 
; gjs moves from lobby-7 to dorm-row 
; zombie-of-ben-bitdiddle moves from green-building-roof to lab-supply-room 

; --- Clock tick 15 --- 
; cy-d-fect moves from lobby-10 to 10-250 
; At 10-250 cy-d-fect says -- Hi lem-e-tweakit 
; dr-v moves from bldg-26 to infinite-corridor 
; At infinite-corridor dr-v says -- I take hunt-puzzle from infinite-corridor 
; zombie-of-george moves from bldg-32-cp-hq to student-street 
; zombie-of-ben-bitdiddle moves from lab-supply-room to green-building-roof 
; At green-building-roof zombie-of-ben-bitdiddle says -- Hi louis-reasoner 
; At green-building-roof zombie-of-ben-bitdiddle bites at louis-reasoner but is repulsed by hunt-puzzle 
; At green-building-roof zombie-of-ben-bitdiddle says -- uuuuUUUUuuuuh.. need brains to solve hunt-puzzle...


;; PROBLEM 12
(define weaponized-caffeine
  (make-class
    'WEAPONIZED-CAFFEINE
    weapon
    ()
   ((CONSTRUCTOR
     (lambda (name location)
       (super 'CONSTRUCTOR name location 0)))
    (HIT
     (lambda (perp target)
      (cond
        (((is-a 'ZOMBIE) target)
          (pop-class! target)
          (self 'DESTROY)
          (perp 'EMIT (list (perp 'NAME) "cured" (target 'NAME) "with" (self 'NAME) "!!!")))
        (else
          (self 'DESTROY))))))))

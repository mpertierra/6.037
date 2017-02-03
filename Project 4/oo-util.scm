;; THIS FILE IS FOR OO-EVAL, AND IS LOADED WHEN YOU (run-game 'yourname)
;; DO NOT TRY TO RUN DIRECTLY IN RACKET

;;;;;;;;;;
;; PROBLEM 10
;; Data abstraction for mixins

(define (make-mixin name methods) (list 'mixin name methods))
(define (mixin? exp) (eq? 'mixin (first exp)))
(define (mixin-name exp) (second exp))
(define (mixin-methods exp) (third exp))

(define (instance-add-mixin! instance mixin)
  (if (not (mixin? mixin))
    (error "Expected mixin but got: " mixin)
    (let ((name (mixin-name mixin))
           (parent-class (instance 'GET-CLASS))
           (slots '())
           (methods (mixin-methods mixin)))
      (instance
            'SET-CLASS!
            (create-class name parent-class slots methods)))))


;;;;;;;;;;
;; PROBLEM 12

(define (pop-class! instance)
  (instance 'SET-CLASS! ((instance 'GET-CLASS) 'GET-PARENT-CLASS)))


;;;;;;;;;;
;; Bootstrapping the object system

(define (is-a type)
  (lambda (obj)
    (if (memq type ((obj 'GET-CLASS) 'GET-TYPES))
        #t
        #f)))

(define root-class
  (make-class
   'ROOT-CLASS
   #f
   ()
   ((GET-CLASS (lambda () :class))
    ;; PROBLEM 10
    (SET-CLASS! (lambda (class) (set! :class class))))))


;;;;;;;;;;
;; The clock object is a singleton which manages clock callbacks.

(define clock
  (make-class
   'CLOCK
   root-class
   (:the-time :callbacks :removed-callbacks)
   ((CONSTRUCTOR (lambda () (self 'RESET)))
    (THE-TIME    (lambda () :the-time))
    (RESET
     (lambda ()
       (set! :the-time 0)
       (set! :callbacks '())
       (set! :removed-callbacks '())
       ;; By default print out clock-ticks -- note how we are adding a
       ;; callback to a method of the clock object itself
       (self 'ADD-CALLBACK
             (new clock-callback 'TICK-PRINTER self 'PRINT-TICK))))
    (TICK
     (lambda ()
       (set! :removed-callbacks '())
       (for-each (lambda (x)
                   (if (not (memq x :removed-callbacks))
                       (x 'ACTIVATE)
                       #f))
                 (reverse :callbacks))
       (set! :the-time (+ :the-time 1))))
    (ADD-CALLBACK
     (lambda (cb)
       (cond ((not ((is-a 'CLOCK-CALLBACK) cb))
              (error "Non callback provided to ADD-CALLBACK"))
             ((null? (filter (lambda (x) (x 'SAME-AS? cb))
                             :callbacks))
              (set! :callbacks (cons cb :callbacks))
              'added)
             (else
              'already-present))))
    (REMOVE-CALLBACK
     (lambda (obj cb-name)
       (set! :callbacks
             (filter (lambda (x)
                       (cond ((and (eq? (x 'NAME) cb-name)
                                   (eq? (x 'OBJECT) obj))
                              (set! :removed-callbacks
                                    (cons x :removed-callbacks))
                              #f)
                             (else #t)))
                     :callbacks))
       'removed))
    (PRINT-TICK
     (lambda ()
       (screen 'TELL-WORLD
               (list "--- Clock tick" (self 'THE-TIME) "---")))))))

;;;;;;;;;;
;; Clock callbacks
;;
;; A callback is an object that stores a target object,
;; message, and arguments.  When activated, it sends the target
;; object the message.  It can be thought of as a button that executes an
;; action at every tick of the clock.
(define clock-callback
  (make-class
   'CLOCK-CALLBACK
   root-class
   (:name :object :msg :data)
   ((CONSTRUCTOR
     (lambda (name object msg . data)
       (set! :name name)
       (set! :object object)
       (set! :msg msg)
       (set! :data data)))
    (NAME     (lambda () :name))
    (OBJECT   (lambda () :object))
    (MESSAGE  (lambda () :msg))
    (ACTIVATE (lambda () (apply :object
                                (cons :msg
                                      :data))))
    (SAME-AS? (lambda (cb)
                (and ((is-a 'CLOCK-CALLBACK) cb)
                     (eq? (self 'NAME)
                          (cb 'NAME))
                     (eq? :object (cb 'OBJECT))))))))

;; Setup global clock object
(define our-clock (new clock))

;; Get the current time
(define (current-time)
  (our-clock 'THE-TIME))

;; Advance the clock some number of ticks
(define (run-clock n)
  (cond ((= n 0) 'DONE)
        (else (our-clock 'TICK)
              ;; remember that this activates each item in callback list
              (run-clock (- n 1)))))

;; Using the clock:
;;
;; When you want the object to start being aware of the clock
;; (during initialization of autonomous-person, for example),
;; add a callback to the clock which activates a method on the
;; object:
;; (our-clock 'ADD-CALLBACK
;;      (new clock-callback 'thingy self 'DO-THINGY))
;; The first argument is a name or descriptor of the callback.
;; The second argument is the object to which to send the message.
;; The third argument is the message (method-name) to send.
;; Additional arguments can be provided and they are sent to
;; the object with the message when the callback is activated.
;; In this case, the method do-thingy should be descriptive of
;; the behavior the object will exhibit when time passes.
;; When the object's lifetime expires (sometimes this is taken
;; literally!), it should remove its callback(s) from the clock.
;; This can be done with
;; (our-clock 'REMOVE-CALLBACK
;;      self 'thingy)
;;
;; An example of using callback names and additional arguments:
;; (our-clock 'ADD-CALLBACK
;;      (new clock-callback 'whoopee me 'SAY '("Whoopee!")))
;; (our-clock 'ADD-CALLBACK
;;      (new clock-callback 'fun me 'SAY '("I am having fun!")))
;; This causes the avatar to say two things every time the clock
;; ticks.



;;;;;;;;;;
;; screen
;;
;; This is a singleton object (only one object of this type in
;; existence at any time), which deals with outputting text to
;; the user.
;;
;; If the screen is in deity-mode, the user will hear every message,
;; regardless of the location of the avatar.  If deity-mode is
;; false, only messages sent to the room which contains the avatar
;; will be heard.

(define screen-object
  (make-class
   'SCREEN
   root-class
   (:deity-mode :me)
   ((CONSTRUCTOR
     (lambda ()
       (set! :deity-mode #t)))
    (SET-ME (lambda (new-me) (set! :me new-me)))
    (TELL-ROOM (lambda (room msg)
                 (if (or :deity-mode
                         (and :me
                              (eq? room (:me 'LOCATION))))
                     (display-message msg)
		     #f)))
    (TELL-WORLD (lambda (msg)
                  (display-message msg)))
    (DEITY-MODE (lambda (value) (set! :deity-mode value)))
    (DEITY-MODE? (lambda () :deity-mode)))))

(define screen (new screen-object))

(define (display-message list-of-stuff)
  (if (not (null? list-of-stuff)) (newline) #f)
  (for-each (lambda (s) (display s) (display " "))
            list-of-stuff)
  'MESSAGE-DISPLAYED)

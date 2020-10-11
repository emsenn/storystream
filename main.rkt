#lang racket

;; Structs
;;   storyserver operator story
;; ---
;; storyserver: LISTENER clock operators stories
(struct storyserver
  (listener
   [clock #:mutable]
   [operators #:mutable]
   [stories #:mutable]))
;; operator: IN OUT parser
(struct operator
  (storyserver in out [parser #:mutable]))
;; story: NAME NARRATIVE
(struct story
  (name narrative))

;; General utility procedures
;;   nth index-of
;; ---
;; nth
;;   list integer -> any
;; Returns the INTEGERth element of LIST
(define (nth l c)
  (cond
    [(null? l) (raise-argument-error 'nth "index OOB")]
    [(= c 0) (first l)]
    [else (nth (rest l) (- c 1))]))
;; index-of
;;   list element -> integer
;; Returns the index of ELEMENT within LIST
(define (index-of l x)
  (for/or ([y l] [i (in-naturals)] #:when (equal? x y)) i))

;; Information presentation procedures (show & render)
;; ---
;; Show procedures (display information to StoryServer operators)
;; ---
;; show-main-menu
;;   operator -> void
;; Shows the StoryServer's main menu to OPERATOR
(define (show-op-menu op)
  (define S
    (storyserver-stories (operator-storyserver op)))
  (message-operator
   op
   (format "~a"
           (string-join
            (map
             (λ (s)
               (format "[~a] : ~a\n"
                       (index-of S s)
                       (story-name s)))
             S)))))
;; Parser procedures
;; ---
;; make-main-menu-parser
;;   operator -> procedure
;; Returns a procedure that parses main menu interactions
(define (make-menu-parser op)
  (λ (op line)
    (define selection (string->number line))
    (message-operator op (format "Selection is ~a" selection))
    (if selection
        (with-handlers
          ([exn:fail:contract?
            (λ (e) (message-operator op "Invalid selection!!")
               (show-op-menu op)
               (printf "parsing error: ~a" e))])
          (define s
            (nth (storyserver-stories (operator-storyserver op))
                 selection))
          (define sn (story-narrative s))
          (set-operator-parser! op (λ (o l)
                                     (printf "BING")
                                     (flush-output
                                      (operator-out o))))
          (thread
           (λ ()
             (let story-loop ()
               (unless (null? sn)
                 (define nl (first sn))
                 (printf "looking at narrative line ~a\n" nl)
                 (cond
                   [(string? nl)
                    (message-operator op nl)]
                   [(number? nl)
                    (sleep nl)])
                 (set! sn (cdr sn))
                 (story-loop)))
             (sleep 1)
             (message-operator op "Story over!")
             (show-op-menu op)
             (set-operator-parser! op (make-menu-parser op)))))
        ((λ ()
           (message-operator op "Invalid selection")
           (show-op-menu op))))))
;; make-login-parser
;;   operator -> procedure
;; Returns a procedure that parses logging into the StoryServer
(define (make-login-parser op)
  (define stage 0)
  (λ (op line)
    (cond
      [(= stage 0)
       (message-operator op "Working so far.")
       (set-operator-parser! op (make-menu-parser op))
       (show-op-menu op)]
      [(= stage 1)
       (message-operator op "Still working.")
       (set! stage 2)]
      [else
       (message-operator op "Not gonna break?")])))

;; StoryServer procedures
;; ---
;; message-operator
;;   operator string -> void
;; Displays STRING to OPERATOR
(define (message-operator op line)
  (define op-out (operator-out op))
  (display
   (format
    (if (eq? #\newline (last (string->list line)))
        "~a"
        "~a\n")
    line)
   op-out)
  (flush-output op-out))
;; remove-operator!
;;   storyserver operator -> void
;; Removes OPERATOR from STORYSERVER
(define (remove-operator! serv op)
  (set-storyserver-operators!
   (remove op (storyserver-operators serv))))
;; handler-operators
;;   storyserver -> void
;; Looks for any input from each of STORYSERVER's operators,
;; sending it to that operator's parser.
(define (handle-operators operators)
  (map
   (λ (op)
     (define op-in (operator-in op))
     (define op-serv (operator-storyserver op))
     (cond
       [(port-closed? op-in)
        (remove-operator! op-serv op)]
       [(byte-ready? op-in)
        (define op-line (read-line op-in))
        (cond
          [(string? op-line)
           ((operator-parser op)
            op
            (list->string
             (remove* (list #\return #\newline)
                      (string->list op-line))))]
          [(eof-object? op-line)
           (remove-operator! op-serv op)])]))
   operators))
;; accept-operator!
;;   storyserver -> void
;; Adds a new connection on STORYSERVER's listener as an operator.
(define (accept-operator! serv)
  (define-values
    (in out)
    (tcp-accept (storyserver-listener serv)))
  (define-values
    (lip lport rip rport)
    (tcp-addresses in #t))
  (define op (operator serv in out (void)))
  (set-operator-parser! op (make-login-parser op))
  (set-storyserver-operators!
   serv
   (append (storyserver-operators serv) (list op)))
  (message-operator
   op
   "Your connection to the storyserver from ~a has been accepted.\nPress ENTER"))

;; Creation procedures
;; ---
;; make-storyserver
;;   [list] [integer] -> storyserver
;; Returns a storyserver whose stories are LIST and port is INTEGER
(define (make-storyserver [stories '()] [port 4748])
  (define serv
    (storyserver (tcp-listen port 5 #t) (void) '() stories))
  (set-storyserver-clock!
   serv
   (thread
    (λ ()
      (let storyserver-loop ()
        (handle-operators (storyserver-operators serv))
        (when (tcp-accept-ready? (storyserver-listener serv))
          (accept-operator! serv))
        (sleep 0.1)
        (storyserver-loop)))))
  serv)
;; make-story
;;   string list -> story
;; Returns a story whose name is STRING and whose narrative is LIST
(define (make-story name narrative)
  (story name narrative))

(module+ test
  (require rackunit)
  (provide storystream-tests)
  (define test-story-name "The Itsy-bitsy Spider")
  (define test-story-narrative
    '("The itsy bitsy spider"
      0.5 "climbed up the water spout" 1
      "Down came the rain" 0.5 "and washed the spider out" 1
      "Out came the sun" 0.5 "and dried up all the rain" 1.5
      "So the itsy-bitsy-spider" 0.5 "went up the spout again"))
  (define storystream-tests
    (test-suite
     "Tests for StoryStream"
     (test-case
         "Making a story with a string and a list of strings and numbers."
       (check-pred story? (make-story test-story-name
                                      test-story-narrative)))
     (test-case
         "Making a storyserver with the test story."
       (check-pred storyserver?
                   (make-storyserver
                    (make-story test-story-name
                                test-story-narrative)))))))

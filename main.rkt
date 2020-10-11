#lang racket

(struct storyserver
  (listener
   [clock #:mutable]
   [operators #:mutable]
   [stories #:mutable]))
(struct operator
  (storyserver in out [parser #:mutable]))
(struct story
  (name narrative))


(define (nth l c)
  (cond
    [(null? l) (raise-argument-error 'nth "index OOB")]
    [(= c 0) (first l)]
    [else (nth (rest l) (- c 1))]))

(define (index-of l x)
  (for/or ([y l] [i (in-naturals)] #:when (equal? x y)) i))

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

(define (remove-operator! serv op)
  (set-storyserver-operators!
   (remove op (storyserver-operators serv))))

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
        (storyserver-loop))))))

(define (make-story name narrative)
  (story name narrative))

(define fight-of-the-good-ship-clarissa
  (make-story
   "The Fight of the Good Ship Clarissa"
   '("The space rocket Clarissa was nine days out from Venus."
     1.5
     "The members of the crew were also out for nine days." )))

(define how-deer-got-eir-antlers
  ;; originally from
  ;; https://www.ccs-nc.org/apps/pages/index.jsp?uREC_ID=373900&type=d&pREC_ID=840882
  ;; http://www.northerncherokeenation.com/how-the-deer-got-his-horns.html
  (make-story
   "How deer got eir antlers"
   '("Long ago the animals were just like people."
     2
     "Some were bashful."
     1
     "Some were full of pride."
     1
     "Some of the animals were humble and some of them tried to best all the other animals at everything."
     3
     "At the particular time of this story, rabbit and deer were having a big argument over who was the fastest runner."
     3
     "They had argued for so long and so loudly that the other animals got tired of listening to them. Bear suggested that they have a race and settle the argument once and for all."
     2
     "\"Whoever wins,\" said Bear, \"is the fastest runner of all.\""
     2
     "Rabbit shook his head. \"Not me,\" he said. \"I must have a prize to look forward to if I win. If I can't win a prize, I will not run.\""
     2
     "One of the animals, beaver, began to carve. He carved a beautiful set of antlers. They were the first set of antlers ever seen in the world and all the animals exclaimed at how wonderful an animal would look with the crown of antlers on his head. When Rabbit saw the antlers he agreed right away to run the race. \"I wish I had that set of antlers,\" he thought to himself."
     2
     "Rabbit said, \"I am new to these woods. Let me have a look around so I will know the lay of the land before the race.\" He ran into a thicket and didn't come out. Soon the other animals began to worry. They waited for a long time. One of the animals went into the thicket to look for the rabbit."
     2
     "The animal soon came out of the thicket. He had an angry look on his face. \"That rabbit is cheating!\" he said. Rabbit soon came out of the thicket. When he was accused of cheating he shouted and said he would never cheat. \"Come on and start the race!\" he yelled to the deer. \"If you don't hurry and start the race the antlers are mine. Hurry up and start the race!\""
     2
     "The deer stood where he was while the other animals went to look in the thicket. They saw where the rabbit had been cutting a short cut out through all the brush and briars. He certainly had been cheating."
     2
     "The animals said to the the rabbit, \"You would lie and cheat to win the prize of the antlers. The deer was going to run the race fairly. He will have the antlers.\" As the rabbit ran away in anger, the animals placed the antlers upon the deer's head. They looked beautiful."
     2
     "Deer still wears his antlers. Once a year they fall off his head to remind him that he did not always have them. He won them through a sense of fair play.")))

(define my-storyserver
  (make-storyserver
   (list how-deer-got-eir-antlers
         fight-of-the-good-ship-clarissa)))

;; As described in The Scheme Programming Language, page 241

;; Basically defines a named function that acts as a class.
(define-syntax define-object
    (syntax-rules ()
        ((_ (name . varlist)
            ((var1 val1) ...)
            ((var2 val2) ...))
         (define name
            (lambda varlist
                (let* ((var1 val1) ...)
                    (letrec ((var2 val2) ...)
                        (lambda (msg . args)
                            (case msg
                                ((var2) (apply var2 args)) ...
                                (else
                                    (error 'name "invalid message ~s"
                                        (cons msg args))))))))))
        ((_ (name . varlist)
            ((var2 val2) ...))
         (define-object (name . varlist)
            ()
            ((var2 val2) ...)))))

(define-syntax send-message
    (syntax-rules ()
        ((_ obj msg arg ...)
         (obj 'msg arg ...))))

(define-object (player name age)
    ((get-age (lambda () age))
     (get-name (lambda () name))
     (set-age! (lambda (x) (set! age x)))
     (set-name! (lambda (x) (set! name x)))))

;; player is now a function that takes two args and the get*/set* member functions
;; 'save' the values passed in as name and age.
(define p (player "David Reynolds" 27))

(print "Age: " (send-message p get-age))
(print "Name: " (send-message p get-name))

(send-message p set-name! "Foo Bar")
(print "Name: " (send-message p get-name))

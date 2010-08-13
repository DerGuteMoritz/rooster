(define-syntax define-class
    (syntax-rules ()
        ((_ (name (var1 val1) ...))
         (define name
            (lambda args
                ;; maps args pairs to var1 val1
                (print args)
                 (let* ((var1 val1) ...)
                    (list (cons 'var1 val1) ...)))))))

(define-class (player (name "") (age 0)))

(define (set-member! obj sym val)
    (if (eq? sym (caar obj))
        (set! (cdar obj) val)
        (set-member! (cdr obj) sym val)))

(define (get-member obj sym)
    (if (eq? sym (caar obj))
        (cdar obj)
        (get-member (cdr obj) sym)))

(define p (player '(name . "david") '(age . 27)))
(print p)

;(set-member! p 'name "David Reynolds")
;(set-member! p 'age 27)

;(print (get-member p 'name))
;(print (get-member p 'age))

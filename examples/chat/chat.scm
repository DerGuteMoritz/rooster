(load "rooster")
(import rooster)

(require 'srfi-13)

(define-syntax define-class
    (syntax-rules ()
        ((_ (name (var1 val1) ...))
         (define name
            (lambda ()
                (let* ((var1 val1) ...)
                    (list (cons 'var1 val1) ...)))))))

(define USER_STATE_NEW 0)
(define USER_STATE_LOGIN 1)
(define USER_STATE_CHATTING 2)

(define-class (user (name "") (state USER_STATE_NEW)))

(define (set-user-var! u sym val)
    (let loop ((li u))
        (if (eq? sym (caar li))
            (set! (cdar li) val)
            (loop (cdr li)))))

(define (get-user-var u sym)
    (let loop ((li u))
        (if (eq? sym (caar li))
            (cdar li)
            (loop (cdr li)))))

(define fd-users-table (make-hash-table))

(define (init-user fd)
    (let ((u (user)))
        (set-user-var! u 'name "")
        (set-user-var! u 'state USER_STATE_NEW)
        (hash-table-set! fd-users-table fd u)))

(define (send-to-room buf sender-fd)
    ;; only send to clients that are chatting
    (let fdloop ((fds fd-list))
        (unless (null? fds)
            (let* ((fd (car fds))
                   (u (hash-table-ref/default fd-users-table fd #f)))
                (unless (eq? u #f)
                    (if (eq? (get-user-var u 'state) USER_STATE_CHATTING)
                        (unless (eq? sender-fd fd)
                            (send-to-client fd (string-append "\n" buf))))))

            (fdloop (cdr fds)))))

(define (handler fd buf)
    (let ((u (hash-table-ref/default fd-users-table fd #f)))
        (if (eq? u #f)
            (begin
                ;; init and then re-handle
                (init-user fd)
                (handler fd buf))
            (let ((state (get-user-var u 'state)))
                (cond ((eq? state USER_STATE_NEW)
                        (begin
                            (send-to-client fd "Enter your name: ")
                            (set-user-var! u 'state USER_STATE_LOGIN)))

                      ((eq? state USER_STATE_LOGIN)
                        (begin
                            (set-user-var! u 'name (string-trim-both buf))
                            (set-user-var! u 'state USER_STATE_CHATTING)
                            (send-to-client fd "Welcome to the chat room.\n\n> ")
                            (send-to-room (format "~A has joined.\n\n> " (get-user-var u 'name)) fd)))

                      ((eq? state USER_STATE_CHATTING)
                        (send-to-client fd (format "You: ~A\n> " buf))
                        (send-to-room (format "~A: ~A\n> " (get-user-var u 'name) buf) fd)))))))

(run-rooster handler)

;;
;;  rooster - epoll server
;;
;;      http://github.com/davidreynolds/rooster
;;
;;  Copyright 2010 David Reynolds
;;  All rights reserved.
;;
;;  Use and distribution licensed under BSD license. See
;;  LICENSE for full text of BSD license.
;;

(module rooster
    (run-rooster
     fd-list
     send-to-client
     send-to-all
     remove-client)

    (import scheme chicken foreign)
    (use tcp srfi-13 srfi-69 epoll)

(foreign-declare "#include <fcntl.h>")

;; define this here because it's not exported by tcp.scm
(define setnonblock (foreign-lambda* bool ((int fd))
    "int val = fcntl(fd, F_GETFL, 0);"
    "if (val == -1) return(0);"
    "return(fcntl(fd, F_SETFL, val | O_NONBLOCK) != -1);"))

(define net-accept (foreign-lambda int "accept" int c-pointer c-pointer))
(define net-write (foreign-lambda int "write" int c-string int))
(define net-read (foreign-lambda int "read" int scheme-pointer int))
(define net-close (foreign-lambda int "close" int))

;; _server_fd and epfd are both initialized in run-rooster
(define _server_fd)
(define epfd)
(define fd-list '())

;; this is set to the request handler passed through run-rooster
(define _RequestHandler)

;; hash tables for doing fd lookups -- these manage i/o buffers
(define fd-write-table (make-hash-table))
(define fd-read-table (make-hash-table))

(define (init-client fd)
    ;; set client's i/o buffers to empty strings
    (hash-table-set! fd-write-table fd (make-string 0))
    (hash-table-set! fd-read-table fd (make-string 0)))

(define (filter-out-fd fd)
    ;; builds a new list after filtering out fd
    (let loop ((li fd-list) (newlist '()))
        (if (null? li)
            newlist
            (if (eq? fd (car li))
                (loop (cdr li) newlist)
                (loop (cdr li) (append newlist (list (car li))))))))

(define (remove-client fd)
    (set! fd-list (filter-out-fd fd))
    (hash-table-remove! fd-write-table fd)
    (hash-table-remove! fd-read-table fd)
    (epoll-delete epfd fd)
    (net-close fd))

(define (send-to-client fd str)
    ;; this function doesn't actually _send_ to the client. it appends
    ;; `str` to the client's write buffer until it's time to really
    ;; write on the socket (epoll tells us when to write)
    (let ((buf (hash-table-ref fd-write-table fd)))
        (hash-table-set! fd-write-table fd (string-append buf str)))

    (epoll-modify epfd fd _WRITE))

(define (send-to-all buf sender-fd)
    (let fdloop ((fds fd-list))
        (unless (null? fds)
            (let ((d (car fds)))
                (if (eq? sender-fd d)
                    (send-to-client d buf)
                    (send-to-client d (string-append "\n" buf))))
            (fdloop (cdr fds)))))

(define (accept-fd sfd)
    (let ((fd (net-accept sfd #f #f)))
        (setnonblock fd)
        (init-client fd)
        (set! fd-list (cons fd fd-list))
        (epoll-add epfd fd _WRITE)

        ;; Let the request handler handle new sockets
        (_RequestHandler fd "")))

(define (write-handler fd)
    ;; epoll tells us to write to socket
    (let ((buf (hash-table-ref fd-write-table fd)))
        (net-write fd buf (string-length buf)))

    ;; clear out write buffer
    (hash-table-set! fd-write-table fd "")

    ;; update epoll to watch for a read event on this fd
    (epoll-modify epfd fd _READ))

(define (read-loop fd maxlen)
    (let ((rbuf (make-string maxlen)))
        (let rloop ((rbytes 0))
            (cond ((string-index rbuf #\newline)
                    (substring rbuf 0 rbytes))

                  ((>= rbytes maxlen)
                    (substring rbuf 0 maxlen))

                  (else
                    (let ((res (net-read fd rbuf (- maxlen rbytes))))
                        (if (= res 0)
                            (remove-client fd)
                            (rloop (+ rbytes res)))))))))

(define (read-handler fd)
    ;; epoll tells us to read from socket
    (let* ((len 4096)
           (buf (read-loop fd len)))
        (_RequestHandler fd buf)
        (epoll-modify epfd fd _WRITE)))

;; this function is passed to epoll-wait as a callback
(define (fd-event-list-handler ls)
    ;; takes a list of (fd . events) pairs
    (unless (null? ls)
        (let* ((pair (car ls))
               (fd (car pair)))
            (if (eq? fd _server_fd)
                (accept-fd _server_fd)
                (cond ((= (bitwise-and (cdr pair) _WRITE) _WRITE)
                        (write-handler fd))

                      ((= (bitwise-and (cdr pair) _READ) _READ)
                        (read-handler fd))))

            ;; loop over rest of (fd . events) list of pairs
            (fd-event-list-handler (cdr ls)))))

;; pass server stuff here (like port number) and a request handler
;; so the server can pass requests to the programmer-defined handler
(define (run-rooster request-handler)
    (let* ((listener (tcp-listen 6666))
           (sfd (tcp-listener-fileno listener)))

        ;; set global server fd
        (set! _server_fd sfd)
        (set! epfd (epoll-create))
        (set! _RequestHandler request-handler)

        (epoll-add epfd sfd _READ)

        (let loop ()
            ;; pass epoll callback to epoll-wait
            (epoll-wait epfd 200 fd-event-list-handler)
            (loop))))
)
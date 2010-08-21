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

;(module rooster-utils
;    (setnonblock net-accept net-write net-read net-close init-client
;     fd-read-buffer fd-write-buffer remove-client-buffers! set-fd-write-buffer!)
;
;    (import scheme chicken foreign)
;    (use tcp srfi-69 epoll)

(declare
    (unit rooster-utils)
    (uses tcp srfi-69)
    (export setnonblock net-accept net-write net-read net-close init-client
            fd-read-buffer fd-write-buffer remove-client-buffers! set-fd-write-buffer!))

(foreign-declare "#include <fcntl.h>")

(define setnonblock (foreign-lambda* bool ((int fd))
#<<EOF
int val = fcntl(fd, F_GETFL, 0);
if (val == -1) return 0;
return (fcntl(fd, F_SETFL, val | O_NONBLOCK) != 1);
EOF
))

(define net-accept (foreign-lambda int "accept" int c-pointer c-pointer))
(define net-write (foreign-lambda int "write" int c-string int))
(define net-read (foreign-lambda int "read" int scheme-pointer int))
(define net-close (foreign-lambda int "close" int))

;; hash tables for doing fd lookups -- these manage i/o buffers
(define _fd_write_table (make-hash-table))
(define _fd_read_table (make-hash-table))

(define (init-client fd)
    ;; set client's i/o buffers to empty strings
    (hash-table-set! _fd_write_table fd (make-string 0))
    (hash-table-set! _fd_read_table fd (make-string 0)))

(define (fd-read-buffer fd)
    ;; returns rbuf for fd or #f
    (hash-table-ref/default _fd_read_table fd #f))

(define (fd-write-buffer fd)
    ;; returns wbuf for fd or #f
    (hash-table-ref/default _fd_write_table fd #f))

(define (set-fd-write-buffer! fd str)
    (hash-table-set! _fd_write_table fd str))

(define (remove-client-buffers! fd)
    (hash-table-remove! _fd_write_table fd)
    (hash-table-remove! _fd_read_table fd))

;)

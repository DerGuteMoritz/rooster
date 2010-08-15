(define (filter-out li n)
    (let loop ((l li) (newlist '()))
        (if (null? l)
            newlist
            (if (eq? n (car l))
                (loop (cdr l) newlist)
                (loop (cdr l) (append newlist (list (car l))))))))

(define L '(1 2 3 4))

(print (filter-out L 1))

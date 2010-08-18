(load "rooster")
(import rooster)

(define (handler fd buf)
    (send-to-client fd buf))

(run-rooster handler)

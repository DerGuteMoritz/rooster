(require 'rooster)

(define headers "HTTP/1.1 200 OK\r\nContent-Length: 12\r\n\r\n")

(define (handler fd buf)
    (send-to-client fd (string-append headers "Hello, world")))

(run-rooster handler)

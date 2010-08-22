(require 'rooster)

(define (handler fd buf)
    (let* ((msg "Hello, world")
           (headers (format
                        "HTTP/1.1 200 OK\r\nContent-Length:~A\r\n\r\n"
                        (string-length msg))))
        (send-to-client fd (string-append headers msg))))

(run-rooster handler)

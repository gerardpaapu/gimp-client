#lang scheme
(define *the-magic-number* 71)

(define (message-get-body port)
  (let* ([header (read-bytes 4 port)]
         [magic  (bytes-ref header 0)]
         [error-code (bytes-ref header 1)]
         [length-bytes (subbytes header 2 4)]
         [length (integer-bytes->integer length-bytes
                                         #f
                                         #t)])

    (when (not (= *the-magic-number* magic))
          (error "magic number wrong"))
    (when (= error-code 1)
          (error "server threw an error"))
    (read-bytes length port)))

(define (sexp->bytes sexp)
  (let ([out (open-output-string)])

    (write sexp out)
    (get-output-bytes out)))

(define (make-message sexp)
  (let* ([str (sexp->bytes sexp)]
         [length (bytes-length str)]
         [lbytes (integer->integer-bytes length 2 #f #t)])
    
    (bytes-append (bytes *the-magic-number*) lbytes str)))

(define (send-sexp host port sexp)
  (let-values ([(in out) (tcp-connect host port)])

   (write-bytes-avail (make-message sexp) out)
   (message-get-body in)))

(provide send-sexp)

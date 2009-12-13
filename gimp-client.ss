#lang scheme

(define current-connection (make-parameter #f))
(define-struct connection (host port input output))

(define (connect host port)
  (let-values ([(in out) (tcp-connect host port)])
    (current-connection
     (make-connection host port in out))))

(define (disconnect)
  (current-connection #f))

(define-syntax with-connection
  (syntax-rules ()
    [(with-connection (host port)
                      exp ...)
     (begin (connect host port)
            (let ([val (begin exp ...)])
                 (disconnect)
                 val))]))

(define *the-magic-number* 71)

(define (g-int->integer i)
  (integer-bytes->integer i #f #t))

(define (integer->g-int i)
  (integer->integer-bytes i 2 #f #t))

(define *max-message-length* (g-int->integer (bytes 255 255)))

(define (read-message port)
  (let* ([header (read-bytes 4 port)]
         [magic  (bytes-ref header 0)]
         [error-code (bytes-ref header 1)]
         [length-bytes (subbytes header 2 4)]
         [length (g-int->integer length-bytes)])
    
    (when (not (= *the-magic-number* magic))
          (error "magic number wrong"))
    (when (= error-code 1)
          (error "server threw an error"))
    (read-bytes length port)))

(define (sexp->bytes sexp)
  (let ([out (open-output-string)])
    (write sexp out)
    (get-output-bytes out)))

(define (make-message str)
  (let* ([length (bytes-length str)]
         [lbytes (integer->g-int length)])
    (bytes-append (bytes *the-magic-number*) lbytes str)))

(define (send-bytes str)
  (if (not (current-connection))
      (error "not connected")
      (let ([in (connection-input (current-connection))]
            [out (connection-output (current-connection))])
        (map (lambda (chunk)
               (write-bytes-avail (make-message chunk) out)
               (read-message in))
             (split-every/bytes str *max-message-length*)))))

(define (send-expression exp)
  (send-bytes (sexp->bytes exp)))

(define (send-file file)
  (if (not (current-connection))
      (error "not connected")
      (let ([in (connection-input (current-connection))]
            [out (connection-output (current-connection))]
            [file-in (open-input-file file)])
        (do ([str (read-bytes *max-message-length* file-in)])
            ((eof-object? str))
          (write-bytes-avail str out)
          (read-message in)))))

(define (split-every ls n)
  (if (< (length ls) n)
      (list ls)
      (let-values ([(head tail) (split-at ls n)])
        (cons head
              (if (< (length tail) n)
                  (list tail)
                  (split-every tail n))))))

(define (split-every/bytes str n)
  (let* ([ls  (bytes->list str)]
         [ls* (split-every ls n)])
    (map list->bytes ls*)))

(provide with-connection send-expression send-file)
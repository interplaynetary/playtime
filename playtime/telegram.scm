(define-module (playtime telegram)
  ; #:use-module (ice-9 socket)
  #:use-module (ice-9 textual-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (json)
  #:use-module (goblins)
  #:use-module (goblins actor-lib methods)
  #:export (^telegram-player))

(define SOCKET-PATH "/tmp/guile_js_socket")

(define (send-to-socket message)
  (let ((sock (socket PF_UNIX SOCK_STREAM 0)))
    (catch 'system-error
      (lambda ()
        (connect sock AF_UNIX SOCKET-PATH)
        (let ((out (open-output-string)))
          (display (scm->json-string message) out)
          (let ((data (string->utf8 (get-output-string out))))
            (send sock data 0)))
        (close-port sock))
      (lambda (key . args)
        (close-port sock)
        (format #t "Error: Unable to connect to socket at ~a. Error details: ~a\n" 
                SOCKET-PATH (strerror (car (last-pair args))))))))

(define (receive-from-socket)
  (let ((sock (socket PF_UNIX SOCK_STREAM 0)))
    (catch 'system-error
      (lambda ()
        (connect sock AF_UNIX SOCKET-PATH)
        (let* ((message (json-string->scm (get-string-all sock))))
          (close-port sock)
          message))
      (lambda (key . args)
        (close-port sock)
        (format #t "Error: Unable to connect to socket at ~a. Error details: ~a\n" 
                SOCKET-PATH (strerror (car (last-pair args))))
        #f))))

(define (^telegram-player bcom username)
  (methods
    ((cue msg) ;; cue the player to do something
     (send-to-socket `((type . "cue") (content . ,(format #f "Hey ~a! ~a" username msg)))))
    ((request msg) ;; request input from the player
     (send-to-socket `((type . "request") (content . ,(format #f "~a, ~a" username msg))))
     (let ((response (receive-from-socket)))
       (assoc-ref response 'content)))
    ((who) username)))

(define context (spawn-vat))

(call-with-vat context
  (lambda ()
    (let ((player (spawn ^telegram-player 'fronx84)))
      ; ($ player 'cue "it's playtime!")
      (display ($ player 'request "how's things?"))
    )))

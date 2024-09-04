(define-module (playtime telegram)
  #:use-module (ice-9 socket)
  #:use-module (ice-9 textual-ports)
  #:use-module (json)
  #:export (^telegram-player))

(define SOCKET-PATH "/tmp/guile_js_socket")

(define (send-to-telegram message)
  (let ((sock (socket PF_UNIX SOCK_STREAM 0)))
    (connect sock AF_UNIX SOCKET-PATH)
    (display (scm->json-string message) (socket-output-port sock))
    (close-port sock)))

(define (request-from-telegram)
  (let ((sock (socket PF_UNIX SOCK_STREAM 0)))
    (bind sock AF_UNIX SOCKET-PATH)
    (listen sock 1)
    (let* ((conn (accept sock))
           (input-port (car conn))
           (message (json-string->scm (get-string-all input-port))))
      (close-port input-port)
      message)))

(define (^telegram-player bcom username)
  (methods
    ((cue msg) ;; cue the player to do something
     (send-to-telegram `((type . "cue") (content . ,(format #f "Hey ~a! ~a" name msg)))))
    ((request msg) ;; request input from the player
     (send-to-telegram `((type . "request") (content . ,(format #f "~a, ~a: " name msg))))
     (let ((response (receive-from-telegram)))
       (assoc-ref response 'content)))
    ((who) username)))

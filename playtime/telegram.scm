(define-module (playtime telegram)
  #:use-module (web client)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (json)
  #:use-module (goblins)
  #:use-module (goblins actor-lib methods)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 receive)
  #:export (^telegram-player))

(use-modules (ice-9 format))

(define SERVER-URL "http://localhost:3000")

(define (send-http-request method endpoint params)
  (let* ((base-uri (string->uri SERVER-URL))
         (uri (build-uri (uri-scheme base-uri)
                         #:host (uri-host base-uri)
                         #:port (uri-port base-uri)
                         #:path endpoint
                         #:query (string-join (map (lambda (pair)
                                                     (string-append (car pair) "=" (cdr pair)))
                                                   params)
                                              "&")))
         (uri-string (uri->string uri)))
    (display (format #f "Sending request to: ~a\n" uri-string))
    (catch #t
      (lambda ()
        (receive (response response-body)
            (http-request uri-string #:method method)
            (if (= (response-code response) 200)
                response-body
                (begin
                  (display (format #f "HTTP request failed with code ~a\n" (response-code response)))
                  #f))))
      (lambda (key . args)
        (display (format #f "Exception caught: ~a\n" key))
        (display (format #f "Arguments: ~s\n" args))
        #f))))

(define (^telegram-player bcom name telegram-user-id)
  (methods
    ((cue msg) ;; cue the player to do something
     (send-http-request 'POST "/send-message"
                        `(("userId" . ,telegram-user-id)
                          ("content" . ,(format #f "Hey ~a! ~a" name msg)))))
    ((request msg) ;; request input from the player
     (let ((response (send-http-request 'GET "/request-input"
                                        `(("userId" . ,telegram-user-id)
                                          ("content" . ,(format #f "~a, ~a" name msg))))))
       (and response (json-string->scm response))))
    ((who) name)
    ((hello) ;; hello method
     (let ((response (send-http-request 'GET "/hello" '())))
       (if response
           (format #f "Server says: ~a" response)
           "Failed to get response from server")))))

(define context (spawn-vat))

(call-with-vat context
  (lambda ()
    (display "Starting telegram player\n")
    (let ((player (spawn ^telegram-player "Fronx" "725085107")))
      (display ($ player 'cue "what's up"))
      (newline))))

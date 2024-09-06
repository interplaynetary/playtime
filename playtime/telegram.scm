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
  #:use-module (ice-9 format)
  #:use-module (ice-9 pretty-print)
  #:export (^telegram-player))


(define SERVER-URL "http://localhost:3000")

(define (params->query-string params)
  (string-join (map (lambda (pair)
                      (string-append (uri-encode (car pair))
                                     "="
                                     (uri-encode (cdr pair))))
                    params)
               "&"))

(define (send-http-request-base http-method endpoint params)
  (let* ((base-uri (string->uri SERVER-URL))
         (uri (build-uri (uri-scheme base-uri)
                         #:host (uri-host base-uri)
                         #:port (uri-port base-uri)
                         #:path endpoint))
         (uri-string (uri->string uri))
         (json-body (and (eq? http-method http-post)
                         (scm->json-string params)))
         (headers (if (eq? http-method http-post)
                      '((Content-Type . "application/json"))
                      '()))
         (request-args `(,uri-string
                         #:headers ,headers
                         ,@(if (eq? http-method http-post)
                               `(#:body ,json-body)
                               `(#:query ,(params->query-string params))))))
    (catch #t
      (lambda ()
        (receive (response body)
            (apply http-method request-args)
          (if (= (response-code response) 200)
              (if (eq? http-method http-get)
                  (utf8->string body)
                  (json-string->scm (utf8->string body)))  ; Parse JSON response for POST requests
              (begin
                (display (format #f "HTTP request failed with code ~a\n" (response-code response)))
                #f))))
      (lambda (key . args)
        (display (format #f "Exception caught: ~a\n" key))
        (display (format #f "Arguments: ~s\n" args))
        #f))))

(define (send-http-get endpoint params)
  (send-http-request-base http-get endpoint params))

(define (send-http-post endpoint params)
  (send-http-request-base http-post endpoint params))

(define (^telegram-player bcom name telegram-user-id)
  (methods
    ((cue msg) ;; cue the player to do something
     (send-http-post "/send-message"
                     `((userId . ,telegram-user-id)
                       (content . ,(format #f "Hey ~a! ~a" name msg)))))
    ((request msg) ;; request input from the player
     (send-http-post "/request-input"
                    `((userId . ,telegram-user-id)
                      (content . ,(format #f "~a, ~a" name msg)))))
    ((who) name)
    ((hello) ;; hello method
     (let ((response (send-http-get "/hello" '())))
       (if response
           (format #f "Server says: ~a" response)
           "Failed to get response from server")))))

(define context (spawn-vat))

(call-with-vat context
  (lambda ()
    (display "Starting telegram player\n")
    (let ((player (spawn ^telegram-player "Fronx" "725085107")))
      (display ($ player 'request "what's up"))
      (newline))))

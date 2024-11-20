(define-module (playtime player)
  #:use-module (playtime registry)
  #:use-module (playtime telegram)
  #:use-module (playtime utils)
  #:use-module (ice-9 readline)
  #:use-module (goblins)
  #:use-module (goblins actor-lib cell)
  #:use-module (goblins actor-lib methods)
  #:export (^player)
  #:export (get-player))


(define* (^player bcom name #:optional [telegram-user-id #f] [capabilities (make-hash-table)])
  (define telegram-player
    (if telegram-user-id
        (spawn ^telegram-player name telegram-user-id)
        #f))
  (methods
    ((add-capability key value)
      (hash-set! capabilities key value))
    ((has-capability? key)
      (hash-ref capabilities key #f))
    ((add-telegram _telegram-user-id _telegram-username)
      (hash-set! capabilities 'telegram #t)
      (registry 'register-telegram-lookup _telegram-username (normalize-name name))
      (bcom (^player bcom name _telegram-user-id capabilities)))
    ((cue msg) ;; cue the player to do something
      (if telegram-player
          (<- telegram-player 'cue msg)
          (display-flush (format #f "Hey ~a! ~a\n" name msg))))
    ((request msg) ;; request input from the player
      (if telegram-player
          (<- telegram-player 'request msg)
          (let ((response (readline (format #f "~a, ~a: " name msg))))
            `(("text" . ,response)))))
    ((who) name)
  ))

(define* (get-player name)
  (begin
    (let ((player-symbol (normalize-name name)))
      (let ((existing-player (registry 'get-player player-symbol)))
      (if existing-player
          existing-player
          (let ((new-player (spawn ^player name)))
            (registry 'register-player player-symbol new-player)
            new-player))))))
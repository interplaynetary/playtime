(define-module (playtime interpreter2)
  #:use-module (playtime registry)
  #:use-module (playtime telegram)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-19)
  #:use-module (ice-9 readline)
  #:use-module (ice-9 pretty-print)
  #:use-module (language tree-il)
  #:use-module (web uri)
  #:use-module (goblins)
  #:use-module (goblins actor-lib methods)
  #:use-module (fibers conditions)
  #:export (print-and-run)
  #:export (request-text)
  #:export (play)
  #:export (cast)
  #:export (player)
  #:export (this)
  #:export (any)
  #:export (every)
  #:export (cue)
  #:export (request)
  #:export (context)
  #:export (enactment)
  #:export (roles)
  #:export (scripts))

(activate-readline)

(define (print-and-run x)
  (let ((expanded (macroexpand x)))
    (pretty-print (tree-il->scheme expanded))
    (eval x (interaction-environment))))

(define* (^player bcom name #:optional [telegram-user-id #f])
  (define telegram-player
    (if telegram-user-id
        (spawn ^telegram-player name telegram-user-id)
        #f))
  (methods
    ((cue msg) ;; cue the player to do something
      (if telegram-player
          (<- telegram-player 'cue msg)
          (display (format #f "Hey ~a! ~a\n" name msg))))
    ((request msg) ;; request input from the player
      (if telegram-player
          (<- telegram-player 'request msg)
          (let ((response (readline (format #f "~a, ~a: " name msg))))
            `(("text" . ,response)))))
    ((who) name)
  ))

(define (normalize-name name)
  (string->symbol
    (string-map (lambda (c)
                  (cond
                    ((char-alphabetic? c) (char-downcase c))
                    ((char-numeric? c) c)
                    (else #\-)))
                (string-trim name))))

(define* (get-player name #:optional telegram-user-id)
  (begin
    ; (display (format #f "Getting player: ~a, ~a\n" name telegram-user-id))
    (let ((player-symbol (normalize-name name)))
      (let ((existing-player (registry 'get-player player-symbol)))
      (if existing-player
          existing-player
          (let ((new-player (spawn ^player name telegram-user-id)))
            (registry 'register-player player-symbol new-player)
            new-player))))))

(define-syntax define-player-call
  (syntax-rules ()
    [(define-request-call name transformer)
     (define-syntax name
       (lambda (stx)
        (with-ellipsis :::
          (syntax-case stx ()
            [(_ args :::)
              (with-syntax ([__player (datum->syntax stx '__player)])
                  #'(transformer __player args :::))
            ]))))
    ]))

;; This is called when a role is defined, not when it is
;; instantiated. The function returns a lambda that can be
;; spawned as a Goblins actor via `spawn <role-class> <player>`.
(define (init-role _name _scripts)
  (lambda (bcom _player) ;; to be spawned as Goblins actor
    (extend-methods _scripts
      ((role-name) _name)
      ((who)         ($ _player 'who))
      ((cue msg)     (<- _player 'cue msg))
      ((request msg) (<- _player 'request msg))
    )))

(define-player-call cue
  (lambda (p msg)
    (<- p 'cue msg)))

(define-player-call request
  (lambda (p msg handler)
    (on (<- p 'request msg) handler)))

(define-player-call request-text
  (lambda (p msg handler)
    (display (format #f "Requesting text from ~a\n" ($ p 'who)))
    (on (<- p 'request msg)
      (lambda (response) (handler (assoc-ref response "text")))
    )))

(define-syntax player
  (lambda (stx)
    (syntax-case stx ()
      [(_ script-name args ...)
        (with-syntax ([__player (datum->syntax stx '__player)])
          #'(<- __player 'script-name __player args ...))
      ])))

;; Alternative to 'player', which should be preferred.
;; 'this' is not recommended, because we can't check the (redundant)
;; role-name parameter.
(define-syntax this
  (lambda (stx)
    (syntax-case stx ()
      [(_ role-name script-name args ...)
        (with-syntax ([__player (datum->syntax stx '__player)])
          #'(<- __player 'script-name __player args ...))
      ])))

(define-syntax scripts
  (lambda (stx)
    (syntax-case stx (__player)
      [(_ ((script-name args ...) body ...) ...)
       (with-syntax ([__player (datum->syntax stx '__player)])
          #'(begin
              (methods
                ;; this is where __player is bound to the player
                ((script-name __player args ...)
                  (begin body ...))
                ...))
        )]
      [_ (begin
           (display "Unexpected syntax in scripts: ")
           (display (syntax->datum stx))
           (newline))])))

(define (find-user-by-username username)
  (let ((response (send-http-get
                    (string-append "/find-user-by-username/" (uri-encode username))
                    `())))
    (if (assoc-ref response 'error)
        (throw 'user-not-found-error (assoc-ref response 'error))
        response)))

;; Example: (cast cleaner "Alice")
(define-syntax cast
  (lambda (stx)
    (syntax-case stx (telegram)
      [(_ role-name player-name)
       #'(let* ((player-to-cast (get-player player-name))
                (role-instance (spawn (registry 'get-role 'role-name) player-to-cast)))
           (registry 'register-role-player 'role-name role-instance))]

      [(_ role-name telegram telegram-username)
       #'(let ((user (find-user-by-username telegram-username)))
           (if user
              (let* ((player-to-cast (get-player (assoc-ref user "name") (assoc-ref user "id")))
                  (role-instance (spawn (registry 'get-role 'role-name) player-to-cast)))
                (begin
                  (registry 'register-role-player 'role-name role-instance)
                  (display (format #f "\nTelegram player @~a registered.\n" telegram-username)))
                )
              (begin
                (display (format #f "\n===> Telegram player @~a not found.\n     Open a chat with ~a and say hi to register.\n\n" telegram-username "https://t.me/the_playtime_bot"))
              )))])))

;; TODO recast
; (define-syntax recast
;   (lambda (stx)
;     (syntax-case stx ()
;       [(_ role-name)
;         #'(begin
;             (let* ((__player (get-player player-name))
;                    (role-instance (spawn (registry 'get-role role-name) __player)))
;               (registry 'register-role-player role-name role-instance))
;         )])))

(define (run-role-script selector role-name script . args)
  (let ((role-players (case selector
                        ((any) (list (registry 'get-role-player role-name 'any)))
                        ((every) (registry 'get-role-players role-name))
                        (else (error "Invalid selector for run-role-script")))))
    (if (null? role-players)
        (begin
          (display (format #f "No role players found for role: ~a\n" role-name)))
        (for-each
         (lambda (role-player)
           (display (format #f "Player \"~a\" -> ~a ~a\n" ($ role-player 'who) script args))
           (apply <- (append (list role-player script role-player) (or args '()))))
         role-players))))

(define-syntax any
  (syntax-rules ()
    [(any role-name script-name . args)
     (run-role-script 'any 'role-name 'script-name . args)]))

(define-syntax every
  (syntax-rules ()
    [(every role-name script-name . args)
     (run-role-script 'every 'role-name 'script-name . args)]))

(define-syntax define-role
  (lambda (stx)
    (syntax-case stx ()
      [(_ role-name _scripts)
          #'(begin
              (let ((role-class (init-role 'role-name _scripts)))
                (registry 'register-role 'role-name role-class))
            )])))

(define-syntax roles
  (lambda (stx)
    (syntax-case stx ()
      ;; item = scripts|requires
      [(_ (role-name a_scripts) ...)
         #'(begin ;; roles
             (newline)
             (display "Roles: ")
             (newline)
             (begin ;; each role
                (display (format #f "  ~a\n" 'role-name))
                (define-role role-name a_scripts)) ...
         )]
      [_ (begin
           (display "Unexpected syntax in roles: ")
           (display (syntax->datum stx))
           (newline)
          )])))

(define-syntax enactment
  (lambda (stx)
    (syntax-case stx ()
      ((_ stmt ...)
        (with-syntax ([the-enactment (datum->syntax stx 'the-enactment)])
          #'(define (the-enactment)
              stmt
              ...
            ))))))

(define-syntax context
  (lambda (stx)
    (syntax-case stx ()
      ;; context-item = enactment|roles
      [(_ name context-item ...)
          #'(begin
              (define name (spawn-vat))
              context-item ...
            )])))

(define-syntax play
  (lambda (stx)
    (syntax-case stx ()
      [(_ context body ...)
       (with-syntax ([the-enactment (datum->syntax stx 'the-enactment)])
         #'(call-with-vat context
             (lambda ()
                (begin body ...)
                ; (print-role-players)
                ; (newline)
                (display "\n~~~ Enactment ~~~\n\n")
                (the-enactment)
                ; (define forever (make-condition))
                ;   (wait forever)
                (display "~~~ The End ~~~\n")
              )))])))

(define (print-role-players)
  (display "Current script table:\n")
  (hash-for-each
   (lambda (key value)
     (format #t "  ~a: ~a\n" key value))
     (registry 'role-players)))

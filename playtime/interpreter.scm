(define-module (playtime interpreter)
  #:use-module (playtime registry)
  #:use-module (playtime telegram)
  #:use-module (playtime utils)
  #:use-module (playtime player)
  #:use-module (playtime transparency)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-19)
  #:use-module (ice-9 readline)
  #:use-module (ice-9 textual-ports)
  #:use-module (web uri)
  #:use-module (goblins)
  #:use-module (goblins actor-lib cell)
  #:use-module (goblins actor-lib methods)
  #:use-module (fibers conditions)
  #:export (print-and-run)
  #:export (request-get)
  #:export (get)
  #:export (set)
  #:export (inc)
  #:export (dec)
  #:export (start)
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
  #:export (state)
  #:export (scripts))

(activate-readline)

;; --- INTERNAL SYNTAX -----

(define-syntax define-player-call
  (syntax-rules ()
    [(define-request-call name transformer)
     (define-syntax name
       (lambda (stx)
        (with-ellipsis :::
          (syntax-case stx (script-player)
            [(_ args :::)
              (with-syntax ([script-player (datum->syntax stx 'script-player)])
                  #'(transformer script-player args :::))
            ]))))
    ]))

(define (run-role-script selector role-name script . args)
  (let ((role-players (case selector
                        ((any) (let ((p (registry 'get-role-player role-name 'any)))
                                 (if p (list p) '())))
                        ((every) (registry 'get-role-players role-name))
                        (else (error "Invalid selector for run-role-script")))))
    (if (null? role-players)
      '()
      (map
        (lambda (role-player)
          ; (display (format #f "Player \"~a\" -> ~a ~a\n" ($ role-player 'who) script args))
          (apply <- (append (list role-player script role-player) (or args '()))))
        role-players))))

;; This is called when a role is defined, not when it is
;; instantiated. The function returns a lambda that can be
;; spawned as a Goblins actor via `spawn <^role> <player>`.
(define (init-role _name _requires _scripts)
  (let ((^role
          (lambda (bcom _player) ;; to be spawned as Goblins actor
            (extend-methods _scripts
              ((role-name) _name)
              ((who)         ($ _player 'who))
              ((symbol)      (normalize-name ($ _player 'who)))
              ((cue msg)     (<- _player 'cue msg))
              ((request msg) (<- _player 'request msg))
              ((has-capability? key) ($ _player 'has-capability? key))
            ))))
    (display (format #f "Init role ~a with requires: ~a\n" _name _requires))
    (set-procedure-property! ^role 'requires _requires)
    ^role))

(define-syntax define-role
  (lambda (stx)
    (syntax-case stx (requires scripts)
      [(_ script-player role-name (requires _req ...) (scripts _script ...))
          #`(begin (let ((^role (init-role 'role-name `(,_req ...) (scripts script-player _script ...))))
                (registry 'register-role 'role-name ^role)))]
      [(_ script-player role-name (scripts _script ...) (requires _req ...))
          #`(begin (let ((^role (init-role 'role-name `(,_req ...) (scripts script-player _script ...))))
                (registry 'register-role 'role-name ^role)))]
      [(_ script-player role-name (scripts _script ...))
          #`(begin (let ((^role (init-role 'role-name '() (scripts script-player _script ...))))
                (registry 'register-role 'role-name ^role)))]
      [(_ script-player role-name)
          #`(begin (let ((^role (init-role 'role-name '() '())))
                (registry 'register-role 'role-name ^role)))]
    )))

(define-syntax add-capability
  (lambda (stx)
    (syntax-case stx (telegram)
      [(_ player telegram telegram-username)
       #'(let* ((user (find-user-by-telegram-username telegram-username))
                (telegram-user-id (and user (assoc-ref user "id"))))
          (if telegram-user-id
            (begin
              ($ player 'add-telegram telegram-user-id telegram-username)
              (display-flush (format #f "\n===> Telegram player @~a registered (user-id ~a).\n\n" telegram-username telegram-user-id)))
            (display-flush (format #f "\n===> Telegram player @~a not found.\n     Open a chat with ~a and say hi to register.\n\n"
              telegram-username "https://t.me/the_playtime_bot")))
         )]
      [(_ player key value)
        #'($ player 'add-capability 'key value)]
    )))

(define (casting-loop start organizer)
  (if ($ organizer 'has-capability? 'telegram)
    (let loop ()
      (begin
        (display-role-summary-telegram organizer)
        (on ($ organizer 'request
              (if (registry 'all-roles-cast?)
                  "All roles are cast. Type (start) to begin, or continue casting with (cast ...)"
                  "Use (cast <role> <player>) to assign roles"))
          (lambda (response)
            (let* ((input (assoc-ref response "text"))
                   (result (try-eval-command input
                                           (if (registry 'all-roles-cast?)
                                               '(start cast)
                                               '(cast))
                                           organizer)))
              (if result
                (cond
                  [(eq? result 'cast) (loop)]
                  [(eq? result 'start) (display-flush "The enactment has started...")])
                (loop)))))))
    (let loop ()
      (begin
        (newline)
        (display-role-summary)
        (display-flush
          (if (registry 'all-roles-cast?)
              "All roles are cast. Type 'start' to begin, or continue casting.\n> "
              "> "))
        (let* ((input (readline))
               (result (try-eval-command input
                                       (if (registry 'all-roles-cast?)
                                           '(start cast)
                                           '(cast))
                                       organizer)))
          (if (string=? "exit" (string-trim input))
              (display-flush "Exiting...\n")
              (if result
                  (when (eq? result 'cast)
                    (loop))
                  (begin
                    (display-flush
                      (if (registry 'all-roles-cast?)
                          "Unknown command. Available commands: cast, start, exit\n"
                          "Unknown command. Available commands: cast, exit\n"))
                    (loop)))))))))

(define terminal "__terminal__")
(define organizer-username
  (if (> (length (command-line)) 2)
      (caddr (command-line))  ;; Use provided telegram username
      terminal))            ;; Default to "terminal"

(define (init-states)
  (hash-for-each
    (lambda (key value)
      (registry 'set-state key (spawn ^cell value)))
    (registry 'states)))


;; ----- PUBLIC SYNTAX ------------------

(define-player-call cue
  (lambda (p msg)
    (<- p 'cue msg)))

(define-player-call request
  (lambda (p msg)
    (<- p 'request msg)))

(define-player-call request-get
  (lambda (p key msg)
    (display (format #f "Requesting text from ~a\n" ($ p 'who)))
    (on (<- p 'request msg)
      (lambda (response) (assoc-ref response key))
      #:promise? #t)
    ))

(define-syntax player
  (lambda (stx)
    (syntax-case stx ()
      [(_ script-name args ...)
        (with-syntax ([script-player (datum->syntax stx 'script-player)])
          #'(<- script-player 'script-name script-player args ...))
      ])))

;; Alternative to 'player' so we can write `this <role-name> <script>`
;; Note: 'this' is not recommended, because we don't actually check the
;; role-name parameter against the player's current role.
(define-syntax this
  (lambda (stx)
    (syntax-case stx ()
      [(_ role-name script-name args ...)
        (with-syntax ([script-player (datum->syntax stx 'script-player)])
          #'(<- script-player 'script-name script-player args ...))
      ])))

; (define-syntax recast
;   (lambda (stx)
;     (syntax-case stx ()
;       [(_ role-name)
;         #'(begin
;             (let* ((__player (get-player player-name))
;                    (role-instance (spawn (registry 'get-role role-name) __player)))
;               (registry 'register-role-player role-name role-instance))
;         )])))

(define-syntax any
  (syntax-rules ()
    [(any role-name script-name . args)
     (run-role-script 'any 'role-name 'script-name . args)]))

(define-syntax every
  (syntax-rules ()
    [(every role-name script-name . args)
     (run-role-script 'every 'role-name 'script-name . args)]))

(define-syntax scripts
  (lambda (stx)
    (syntax-case stx ()
      [(_ script-player ((script-name args ...) body ...) ...)
          #'(begin
              (methods
                ((script-name script-player args ...)
                  (with-frame
                    (make-execution-frame
                      (symbol->string 'script-name)
                      ($ script-player 'who)
                      ($ script-player 'role-name))
                    (begin body ...)))
                ...)
        )])))

(define-syntax roles
  (lambda (stx)
    (syntax-case stx ()
      [(_ (role-name role-body ...) ...)
        (with-syntax ([script-player (datum->syntax stx 'script-player)])
          #'(begin ;; roles
              (newline)
              (display "Roles: ")
              (newline)
              (begin ;; each role
                  (display (format #f "  ~a\n" 'role-name))
                  (define-role script-player role-name role-body ...)) ...)
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
        (with-syntax ([start (datum->syntax stx 'start)])
          #'(define (start)
              (with-frame
                (make-execution-frame "enactment" #f #f)
                stmt
                ...)))))))

(define-syntax state
  (lambda (stx)
    (syntax-case stx ()
      [(_ (name initial-value) ...)
       (with-syntax ([state-map (datum->syntax stx 'state-map)])
         #'(begin
              (registry 'set-state 'name initial-value) ...
              (display "[state] ")(print-hash (registry 'states))
           ))])))

(define-syntax context
  (lambda (stx)
    (syntax-case stx ()
      ;; context-item = enactment|roles|state
      [(_ name context-item ...)
          #'(begin
              (define name (spawn-vat))
              context-item ...
            )])))

(define-syntax get
  (lambda (stx)
    (syntax-case stx ()
      [(_ key)
        #'($ (registry 'get-state 'key))])))

(define-syntax set
  (lambda (stx)
    (syntax-case stx ()
      [(_ key value)
        #'($ (registry 'get-state 'key) value)])))

(define-syntax inc
  (lambda (stx)
    (syntax-case stx ()
      [(_ key)
       #'(set key (+ (get key) 1))])))

(define-syntax dec
  (lambda (stx)
    (syntax-case stx ()
      [(_ key)
       #'(set key (- (get key) 1))])))

;; Example: (cast cleaner "Alice")
(define-syntax cast
  (lambda (stx)
    (syntax-case stx ()
      [(_ role-name player-name)
       #'(let* ((player (get-player player-name))
                (role-instance (and player (spawn (registry 'get-role 'role-name) player))))
           (registry 'register-role-player 'role-name role-instance))]
      [(_ role-name player-name (key value) ...)
       #'(let* ((player (get-player player-name))
                (role-instance (and player (spawn (registry 'get-role 'role-name) player))))
           (if role-instance
             (begin
               (registry 'register-role-player 'role-name role-instance)
               (begin
                 (add-capability player key 'value) ;; key can be e.g. telegram
                 ...))
             (error "Player not found:" player-name player)
          ))]
    )))

(define-syntax play
  (lambda (stx)
    (syntax-case stx ()
      [(_ context body ...)
        (with-syntax ([start (datum->syntax stx 'start)])
          #'(call-with-vat context
              (lambda ()
                (let ((organizer (get-player organizer-username)))
                  (when (not (string=? organizer-username terminal))
                    (add-capability organizer telegram organizer-username))
                  (init-states)
                  (begin body ...)
                  (casting-loop start organizer))
              )))])))

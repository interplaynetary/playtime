(define-module (playtime interpreter2)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 readline)
  #:use-module (goblins)
  #:use-module (goblins actor-lib methods)
  #:export (enact)
  #:export (cast)
  #:export (cue)
  #:export (request)
  #:export (context)
  #:export (enactment)
  #:export (roles)
  #:export (scripts))

(activate-readline)

(define (capitalize-symbol sym)
  (string->symbol
   (string-capitalize
    (symbol->string sym))))

(define (symbol-to-uppercase sym)
  (string->symbol
   (string-upcase
    (symbol->string sym))))

(define (hash-keys table)
  (hash-map->list (lambda (key value) key) table))

(define registry
  (let ((players (make-hash-table))
        (roles (make-hash-table))
        (role-players (make-hash-table)))
    (methods
      ((register-player symbol actor)
        (hash-set! players symbol actor))
      ((get-player symbol)
        (hash-ref players symbol #f))
      ((register-role symbol role)
        (hash-set! roles symbol role))
      ((get-role symbol)
        (hash-ref roles symbol #f))
      ((register-role-player role-symbol player-symbol)
        (hash-set! role-players role-symbol player-symbol))
      ((get-role-player role-symbol)
        (let ((existing-role-player (hash-ref role-players role-symbol #f)))
          (if existing-role-player
              existing-role-player
              (let* ((role-class (hash-ref roles role-symbol #f))
                     (player-symbol (hash-ref players (car (hash-keys players)) #f))
                     (player (hash-ref players player-symbol #f)))
                (if (and role-class player)
                    (let ((new-role-player (spawn role-class player)))
                      (hash-set! role-players role-symbol new-role-player)
                      new-role-player)
                    #f)))))
      ((roles) roles)
      ((players) players)
      ((role-players) role-players))))

(define (^player bcom name)
  (methods
    ((cue msg) ; cue the player to do something
      (display (format #f "Hey ~a! ~a\n" name msg)))
    ((request msg)
      (readline (format #f "~a, ~a: " name msg)))
    ((who) name)
  ))

(define (run-role-script role-name script)
  ; (display "Running script: ")
  ; (display script)
  ; (display " of role: ")
  ; (display role-name)
  ; (newline)
  (let ((role-player (registry 'get-role-player role-name)))
    (if role-player
        (begin
          ; (display (format #f "Player \"~a\" -> ~a\n" ($ role-player 'player-name) script))
          ($ role-player script role-player))
        (begin
          (display "No role player found for role: ")
          (display role-name)
          (newline)))))

(define (normalize-name name)
  (string->symbol
    (string-map (lambda (c)
                  (if (char-alphabetic? c)
                      (char-downcase c)
                      #\-))
                (string-trim name))))

(define (get-player name)
  (let ((player-symbol (normalize-name name)))
    (let ((existing-player (registry 'get-player player-symbol)))
      (if existing-player
          existing-player
          (let ((new-player (spawn ^player name)))
            (registry 'register-player player-symbol new-player)
            new-player)))))

(define-syntax cue
  (lambda (stx)
    (syntax-case stx ()
      [(_ msg)
        (with-syntax ([player (datum->syntax stx 'player)])
          #'($ player 'cue msg))
      ])))

(define-syntax request
  (lambda (stx)
    (syntax-case stx ()
      [(_ msg)
        (with-syntax ([player (datum->syntax stx 'player)])
          #'($ player 'request msg))
      ])))

(define-syntax scripts
  (lambda (stx)
    (syntax-case stx (player)
      [(_ (script-name script-body ...) ...)
       (with-syntax ([player (datum->syntax stx 'player)])
          #'(begin
              (methods
                ((script-name player)
                  (begin script-body ...))
                ...))
        )]
      [_ (begin
           (display "Unexpected syntax in scripts: ")
           (display (syntax->datum stx))
           (newline))])))

;; This is called when a role is defined, not when it is
;; instantiated. The function returns a lambda that can be
;; spawned as a Goblins actor via `spawn <role-class> <player>`.
(define (init-role _name _scripts)
  (lambda (bcom _player) ;; to be spawned as Goblins actor
    (extend-methods _scripts
      ((role-name) _name)
      ((player-name) ($ _player 'who))
      ((cue msg) ($ _player 'cue msg))
      ((request msg) ($ _player 'request msg)))))

(define-syntax cast
  (lambda (stx)
    (syntax-case stx ()
      [(_ role-name player-name)
        #'(begin
            (let* ((player (get-player player-name))
                   (role-instance (spawn (registry 'get-role role-name) player)))
              (registry 'register-role-player role-name role-instance))
        )])))

(define-syntax define-role
  (lambda (stx)
    (syntax-case stx ()
      [(_ role-name _scripts)
          #'(begin
              (let ((role-class (init-role 'role-name _scripts)))
                (registry 'register-role 'role-name role-class))

              (define (role-name script-name)
                (run-role-script (syntax->datum 'role-name) script-name))
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
      ((_ (role-name script) ...)
        (with-syntax ([the-enactment (datum->syntax stx 'the-enactment)])
          #'(define (the-enactment)
              (run-role-script (syntax->datum 'role-name) script)
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

(define-syntax player
  (lambda (stx)
    (syntax-case stx ()
      [(_ name)
        #'(get-player name)])))

(define-syntax enact
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
                (display "~~~ The End ~~~\n")
              )))])))

(define (print-role-players)
  (display "Current script table:\n")
  (hash-for-each
   (lambda (key value)
     (format #t "  ~a: ~a\n" key value))
     (registry 'role-players)))

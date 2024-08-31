(define-module (playtime interpreter2)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 readline)
  #:use-module (goblins)
  #:use-module (goblins actor-lib methods)
  #:export (enact)
  #:export (player)
  #:export (cue)
  #:export (request)
  #:export (confirmation)
  #:export (context)
  #:export (context-item)
  #:export (enactment)
  #:export (roles)
  #:export (role)
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

(define (^registry bcom)
  (define players (make-hash-table))
  (methods
    ((register symbol actor)
      (hash-set! players symbol actor))
    ((get-player symbol)
      (hash-ref players symbol #f))
    ((list-players)
      (hash-map->list (lambda (k v) v) players))))

(define (^player bcom name)
  (methods
    ((cue msg) ; cue the player to do something
      (format #f "Hey ~a! ~a" name msg)
    )))

;; TODO: cue and request should implicitly select a
;; player to send the message to

(define cue display)

(define request readline)

(define confirmation "Confirm here when you're ready: ")

(define-syntax scripts
  (lambda (stx)
    (syntax-case stx ()
      [(_ (script-name script-body ...) ...)
       #`(begin
           (display "    Scripts:")
           (newline)
           (methods 
              ((script-name)
                (begin script-body ...))
             ...)
          )]
      [_ (begin
           (display "Unexpected syntax in scripts: ")
           (display (syntax->datum stx))
           (newline)
          )])))

(define-syntax role-item
  (lambda (stx)
    (syntax-case stx (requires scripts)
      [(_ (requires req ...))
        #'(begin
            (display "    Requires:")
            (newline)
            (display "      ")
            (begin
              (display req)
              (display ", ")
              (display req)) ...
            (newline)
          )]
      [(_ (scripts script-def ...))
        #'(scripts script-def ...)]
      [_
        (begin
          (display "Unexpected syntax in role-item: ")
          (display (syntax->datum stx))
          (newline)
        )])))

(define-syntax roles
  (lambda (stx)
    (syntax-case stx ()
      ;; item = scripts|requires
      [(_ (role-name item ...) ...)
       #'(begin
           (newline)
           (display "Roles: ")
           (newline)
           (begin
            (newline)
            (display "  ")
            (display (symbol-to-uppercase 'role-name))
            (newline)
            (define role-name
              (role-item item) ...))
           ...
          )]
      [_ (begin
           (display "Unexpected syntax in roles: ")
           (display (syntax->datum stx))
           (newline)
          )])))

(define-syntax enactment
  (lambda (x)
    (syntax-case x ()
      ((_ (role action) ...)
       #'(begin
           (newline)
           (display "Enactment:") (newline)
           (begin
             (display "  ") (display 'role) (display " ") (display 'action) (newline))
           ...
          )))))

(define-syntax context
  (lambda (stx)
    (syntax-case stx ()
      ;; context-item = enactment|roles
      [(_ name context-item ...)
       #`(begin
           (display "Context: ") (display 'name) (newline)
           (define name (spawn-vat))
           context-item ...
          )])))

(define (normalize-name name)
  (string->symbol
    (string-map (lambda (c)
                  (if (char-alphabetic? c)
                      (char-downcase c)
                      #\-))
                (string-trim name))))

(define (get-player registry name)
  (let ((player-symbol (normalize-name name)))
    (let ((existing-player ($ registry 'get-player player-symbol)))
      (if existing-player
          existing-player
          (let ((new-player (spawn ^player name)))
            ($ registry 'register player-symbol new-player)
            new-player)))))

(define-syntax player
  (lambda (stx)
    (syntax-case stx ()
      [(_ name)
        (with-syntax ([registry (datum->syntax stx 'registry)])
          #'(get-player registry name))])))

(define-syntax enact
  (lambda (stx)
    (syntax-case stx ()
      [(_ context body ...)
       (with-syntax ([registry (datum->syntax stx 'registry)])
         #'(call-with-vat context
             (lambda ()
                (let ([registry (spawn ^registry)])
                  (begin body ...)))))])))

(define *script-table* (make-hash-table))
(define *role-requirements* '())

(define (print-script-table)
  (display "Current script table:\n")
  (hash-for-each
   (lambda (key value)
     (format #t "~a: ~a\n" key value))
   *script-table*))

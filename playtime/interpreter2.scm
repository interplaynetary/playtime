(define-module (playtime interpreter2)
  #:use-module (srfi srfi-13)
  #:use-module (goblins)
  #:use-module (goblins actor-lib methods)
  #:export (enact)
  #:export (player)
  #:export (^player)
  #:export (context)
  #:export (context-item)
  #:export (enactment)
  #:export (roles)
  #:export (role)
  #:export (scripts))


(define (capitalize-symbol sym)
  (string->symbol
   (string-capitalize
    (symbol->string sym))))

(define (symbol-to-uppercase sym)
  (string->symbol
   (string-upcase
    (symbol->string sym))))

(define-syntax scripts
  (lambda (stx)
    (syntax-case stx ()
      [(_ (script-name script-body) ...)
       #`(begin
           (display "    Scripts:")
           (newline)
           (begin
            (display "      ")
            (display 'script-name)
            (newline)
            (display "        ")
            (display 'script-body)
            (newline)) ...
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
            ;; define class with name role-name

            (role-item item) ...)
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

(define (^player bcom name)
  (methods
    ((cue msg) ; cue the player to do something
      (format #f "Hey ~a! ~a" name msg)
    )))

(define (player name)
  (let ((player-symbol (normalize-name name)))
    (if (defined? player-symbol)
        (eval player-symbol (current-module))
        (let ((new-player (spawn ^player name)))
          (eval `(define ,player-symbol ,new-player)
                (current-module))
          new-player))))

(define-syntax enact
  (lambda (stx)
    (syntax-case stx ()
      [(_ context body ...)
       #'(call-with-vat context
           (lambda ()
             (begin
               (let ([result body])
                 (display result)
                 (newline)) ...)))])))

(define *script-table* (make-hash-table))
(define *role-requirements* '())

(define (print-script-table)
  (display "Current script table:\n")
  (hash-for-each
   (lambda (key value)
     (format #t "~a: ~a\n" key value))
   *script-table*))

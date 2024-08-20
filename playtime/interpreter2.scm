(define-module (playtime interpreter2)
  #:export (context)
  #:export (context-item)
  #:export (enactment)
  #:export (roles)
  #:export (role)
  #:export (scripts))

(define-syntax scripts
  (lambda (stx)
    (syntax-case stx ()
      [(_ (script-name script-body) ...)
       #`(begin
           (display "Scripts: ")
           (newline)
           (begin
            (display "  ")
            (display 'script-name)
            (newline)
            (display "    ")
            (display 'script-body)
            (newline)) ...
          )]
      [_ (begin
           (display "Unexpected syntax in scripts: ")
           (display (syntax->datum stx))
           (newline)
          )])))

(define-syntax requires
  (lambda (stx)
    (syntax-case stx ()
      [(_ (body))
        #'(begin
            (display "Requires: ")
            (display (syntax->datum #'body))
            (newline)
          )])))

(define-syntax roles
  (lambda (stx)
    (syntax-case stx ()
      [(_ (role-name role-item ...) ...)
       #`(begin
           (display "Roles: ")
           (newline)
           (begin
            (display "  ")
            (display 'role-name)
            (newline)) ...
           (begin role-item ...) ...
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
           (display "Enactment:") (newline)
           (begin
             (display "  ") (display 'role) (display " ") (display 'action) (newline))
           ...
          )))))

(define-syntax context
  (lambda (stx)
    (syntax-case stx ()
      [(_ name context-item ...)
       #`(begin
           (display "Context: ") (display 'name) (newline)
           context-item ...
          )])))

(define *script-table* (make-hash-table))
(define *role-requirements* '())

(define (print-script-table)
  (display "Current script table:\n")
  (hash-for-each
   (lambda (key value)
     (format #t "~a: ~a\n" key value))
   *script-table*))

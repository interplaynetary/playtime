(define-module (playtime interpreter2)
  #:export (context)
  #:export (enactment)
  #:export (roles)
  #:export (scripts))

;; Top-level context parser
(define-syntax context
  (lambda (stx)
    (syntax-case stx ()
      [(_ name (enactment enactment-content ...) (roles roles-content ...))
       #`(begin
           ;; Ensure `enactment` and `roles` are parsed within the context
           (enactment enactment-content ...)
           (roles roles-content ...))]
      [(_ name (roles roles-content ...) (enactment enactment-content ...))
       #`(begin
           (roles roles-content ...)
           (enactment enactment-content ...))]
      ;; Catch-all for unexpected patterns
      [_ (begin
           (display "Unexpected syntax in context: ")
           (display (syntax->datum stx))
           (newline))])))

;; Define the enactment block keyword
(define-syntax enactment
  (lambda (stx)
    (syntax-case stx ()
      ;; Match the enactment block and handle its contents
      [(_ (role-name script ...))
       #`(begin
           ;; Handle the role and scripts
           (display "Enactment: Role ")
           (display (syntax->datum #'role-name))
           (display " with scripts: ")
           (display (syntax->datum #'(script ...)))
           (newline))]
      ;; Catch-all for unexpected patterns
      [_ (begin
           (display "Unexpected syntax in enactment: ")
           (display (syntax->datum stx))
           (newline))])))

;; Define the roles block keyword
(define-syntax roles
  (lambda (stx)
    (syntax-case stx ()
      ;; Match the roles block
      [(_ role-content ...)
       #`(begin
           ;; Handle each role directly using ellipses
           (role role-content) ...)]
      ;; Catch-all for unexpected patterns
      [_ (begin
           (display "Unexpected syntax in roles: ")
           (display (syntax->datum stx))
           (newline))])))

;; Define the role keyword
(define-syntax role
  (lambda (stx)
    (syntax-case stx ()
      ;; Match an individual role and its scripts
      [(_ role-name (scripts script ...))
       #`(begin
           ;; Handle the role and scripts
           (display "Role: ")
           (display (syntax->datum #'role-name))
           (display " with scripts: ")
           (display (syntax->datum #'(script ...)))
           (newline))]
      ;; Catch-all for unexpected patterns
      [_ (begin
           (display "Unexpected syntax in role: ")
           (display (syntax->datum stx))
           (newline))])))

;; Define the scripts block keyword (optional if you need further behavior)
(define-syntax scripts
  (lambda (stx)
    (syntax-case stx ()
      ;; Match the scripts block
      [(_ script ...)
       #`(begin
           ;; Handle each script
           (display "Scripts: ")
           (display (syntax->datum #'(script ...)))
           (newline))]
      ;; Catch-all for unexpected patterns
      [_ (begin
           (display "Unexpected syntax in scripts: ")
           (display (syntax->datum stx))
           (newline))])))

(define *script-table* (make-hash-table))
(define *role-requirements* '())

(define (print-script-table)
  (display "Current script table:\n")
  (hash-for-each
   (lambda (key value)
     (format #t "~a: ~a\n" key value))
   *script-table*))

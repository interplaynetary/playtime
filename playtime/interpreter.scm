(define-module (playtime interpreter)
  #:export (execute-program))

(define (parse-program program)
  (let ((context (cadr program)))
    (parse-context context (cddr program))))

(define (parse-context context body)
  (let ((enactment (find-enactment body))
        (roles (find-roles body)))
    (parse-roles roles)))

(define (find-enactment body)
  (car (filter (lambda (x) (eq? (car x) 'enactment)) body)))

(define (find-roles body)
  (car (filter (lambda (x) (eq? (car x) 'roles)) body)))

(define (parse-roles roles)
  (map parse-role (cdr roles)))

(define (parse-role role)
  (let* ((role-name (car role))
         (body (cdr role))
         (scripts (find-scripts body))
         (requires (find-requires body)))
    (define-scripts role-name scripts)
    (define-requirements role-name requires)))

(define (find-scripts body)
  (car (filter (lambda (x) (eq? (car x) 'scripts)) body)))

(define (find-requires body)
  (car (filter (lambda (x) (eq? (car x) 'requires)) body)))

(define (define-scripts role-name scripts)
  (map (lambda (script)
         (let ((script-name (car script))
               (script-body (cdr script)))
           (define-script role-name script-name script-body)))
       (cdr scripts)))

(define *script-table* (make-hash-table))

(define (define-script role-name script-name script-body)
  (define (script-function . args)
    (display (list role-name script-name args)))
  (hash-set! *script-table* script-name script-function))

(define (define-requirements role-name requires)
  ;; Simply store the requirements for later use
  (set! *role-requirements* (cons (cons role-name (cdr requires)) *role-requirements*)))

(define *role-requirements* '())

(define (get-role-requirements role)
  (cdr (assoc role *role-requirements*)))

(define (print-script-table)
  (display "Current script table:\n")
  (hash-for-each
   (lambda (key value)
     (format #t "~a: ~a\n" key value))
   *script-table*))

(define (execute-program program)
  (parse-program program)
  (display "Program parsed successfully")
  (newline)

  (print-script-table)
)

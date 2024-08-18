(add-to-load-path (dirname (current-filename)))

(define-module (playtime main)
  #:use-module (playtime interpreter)
  #:use-module (ice-9 rdelim)  ;; For reading entire files
  #:use-module (ice-9 readline)
  #:use-module (srfi srfi-1)  ;; For list functions
  #:use-module (goblins)
  #:use-module (goblins actor-lib methods)
  #:export (main))

(display "Goblins module loaded successfully")
(newline)

(define (load-program-from-file filename)
  (with-input-from-file filename
    (lambda ()
      (let ((contents (read)))
        contents))))

(define (main args)
  (if (> (length args) 1)
      (let ((filename (second args)))
        (catch #t
          (lambda ()
            (let ((program (load-program-from-file filename)))
              (execute-program program)))
          (lambda (key . args)
            (format (current-error-port) "Error: ~A~%" (car args)))))
      (begin
        (format (current-output-port) "Usage: guile -s main.scm <program-file>\n")
        (exit 1))))

(main (command-line))

(add-to-load-path (dirname (current-filename)))

(define-module (playtime main)
  #:use-module (playtime interpreter2)
  #:use-module (ice-9 rdelim)  ;; For reading entire files
  #:use-module (ice-9 readline)
  #:use-module (srfi srfi-1)  ;; For list functions
  #:use-module (goblins)
  #:use-module (goblins actor-lib methods)
  ; #:use-modules (system base macros)
  #:export (main))

(display "Goblins module loaded successfully")
(newline)

(context team-abc
  (enactment
    (manager plan)
    (designer design))
  (roles
    (designer
      (scripts
        (design ('abc))
        (present ('bcd))))
    (manager
      (scripts
        (plan ('abc))
        (assess ('bcd))))
    (engineer
      (scripts
        (code ('abc))
        (test ('bcd))))
  )
)

(define file-to-load (cadr (command-line)))
(display "Loading play: ") (display file-to-load) (newline)

(define (load-with-macros file)
  (let ((env (interaction-environment)))
    ;; Use `eval` to load the file within the custom environment
    (with-input-from-file file
      (lambda ()
        (let loop ((expr (read)))
          (unless (eof-object? expr)
            (eval expr env)
            (loop (read))))))))

; (load-with-macros file-to-load)
(load file-to-load)

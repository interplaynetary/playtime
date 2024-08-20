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

(define file-to-load (cadr (command-line)))
(display "Loading play: ") (display file-to-load) (newline)

(load file-to-load)

(add-to-load-path (dirname (current-filename)))

(define-module (playtime main)
  #:use-module (playtime interpreter2)
  #:use-module (ice-9 rdelim)  ;; For reading entire files
  #:export (main))

(display "Goblins module loaded successfully")
(newline)

(define file-to-load (cadr (command-line)))
(display "Loading play: ") (display file-to-load) (newline)

(load file-to-load)


(add-to-load-path (dirname (current-filename)))

(define-module (playtime main)
  #:use-module (playtime interpreter2)
  #:use-module (ice-9 rdelim)  ;; For reading entire files
  #:use-module (ice-9 readline)
  #:use-module (srfi srfi-1)  ;; For list functions
  #:use-module (language tree-il)
  #:use-module (goblins)
  #:use-module (goblins actor-lib methods)
  #:export (main))

(display "Goblins module loaded successfully")
(newline)

; (newline)
; (display (syntax->datum (macroexpand
;   '(context team-abc
;    )
; )))
; (newline)
; (newline)

(define file-to-load (cadr (command-line)))
(display "Loading play: ") (display file-to-load) (newline)

(load file-to-load)

;; todo: write an actor for representing players
(define (^greeter bcom our-name)   ; constructor (outer procedure)
  (lambda (your-name)              ; behavior    (inner procedure)
    (format #f "Hello ~a, my name is ~a!"
            your-name our-name)))

;; todo: write a macro that removes the boilerplate
(newline)
(display (call-with-vat team-abc
   (lambda ()
     (define alice (spawn ^greeter "Alice"))
     ($ alice "Bob"))))
(newline)

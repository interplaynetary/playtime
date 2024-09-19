(define-module (playtime registry)
  #:use-module (srfi srfi-13)
  #:use-module (goblins)
  #:use-module (goblins actor-lib methods)
  #:export (registry))

(define (hash-keys table)
  (hash-map->list (lambda (key value) key) table))

(define (player-names player-symbols)
  (map (lambda (p) ($ p 'who)) player-symbols))

(define registry
  (let ((players (make-hash-table))
        (roles (make-hash-table))
        (role-players (make-hash-table)) ;; role-name -> list of spawned role players
        (last-selected (make-hash-table)))
    (methods
      ((register-player symbol actor)
        (hash-set! players symbol actor))
      ((get-player symbol)
        (hash-ref players symbol #f))
      ((register-role symbol role)
        (hash-set! roles symbol role))
      ((get-role symbol)
        (hash-ref roles symbol #f))
      ((register-role-player role-symbol role-instance)
        (let ((current-players (hash-ref role-players role-symbol '())))
          (hash-set! role-players role-symbol 
            (cons role-instance current-players))))
      ((get-role-player role-symbol selector)
        (let ((existing-players (hash-ref role-players role-symbol '())))
          (newline)
          (if (null? existing-players)
              (begin
                (display (format #f "No players registered for role ~a\n" role-symbol))
                #f)  ; Return #f if no players are registered for this role
              (case selector
                ((any)
                 (let* ((selected (car existing-players))
                        (rotated-list (append (cdr existing-players) (list selected))))
                   (display (format #f "[any ~a] Selected player: ~a\n" role-symbol ($ selected 'who)))
                   (display (format #f "[any ~a] Rotated list: ~a\n" role-symbol (player-names rotated-list)))
                   (hash-set! role-players role-symbol rotated-list)
                   (hash-set! last-selected role-symbol selected)
                   selected))
                ((this)
                 (let ((last (hash-ref last-selected role-symbol #f)))
                   (display (format #f "[this ~a] Select last selected player: ~a\n" 
                                    role-symbol
                                    (and last ($ last 'who))))
                   (or last
                       (let ((selected (car existing-players)))
                         (hash-set! last-selected role-symbol selected)
                         selected))))
                (else (error "Invalid selector"))))))
      ((roles) roles)
      ((players) players)
      ((role-players) role-players)
      ((last-selected) last-selected))))

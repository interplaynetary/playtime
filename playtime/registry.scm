(define-module (playtime registry)
  #:use-module (srfi srfi-1) ;; for `every`
  #:use-module (srfi srfi-13)
  #:use-module (goblins)
  #:use-module (goblins actor-lib methods)
  #:export (registry))

(define (hash-keys table)
  (hash-map->list (lambda (key value) key) table))

(define (player-names player-symbols)
  (map (lambda (p) ($ p 'who)) player-symbols))

(define (requirements role-symbol)
  (let* ((role (registry 'get-role role-symbol))
         (requires (and role (procedure-property role 'requires))))
    (or requires '())))

(define (is-suitable requirements)
  (lambda (player)
    (let ((missing-requirements
           (filter (lambda (req) (not ($ player 'has-capability? req)))
                   requirements)))
      (if (null? missing-requirements)
          #t
          (begin
            ($ player 'cue
               (format #f "Oops, you don't currently meet the following requirements for this role: ~a"
                  missing-requirements))
            #f)))))

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
      ((get-role-players role-symbol)
        (let* ((all-players (hash-ref role-players role-symbol '()))
               (role-requirements (requirements role-symbol)))
          (display (format #f "Checking role requirements for ~a: ~a\n" role-symbol role-requirements))
          (filter (is-suitable role-requirements) all-players)))
      ((get-role-player role-symbol selector)
        (let* ((role-requirements (requirements role-symbol))
               (a (display (format #f "Checking role requirements for ~a: ~a\n" role-symbol role-requirements)))
               (all-players (hash-ref role-players role-symbol '()))
               (suitable-players (filter (is-suitable role-requirements) all-players)))
          (if (null? suitable-players)
              (begin
                (display (format #f "No players registered for role ~a who meet the requirements ~a\n" role-symbol role-requirements))
                #f)  ; Return #f if no players meet the requirements
              (case selector
                ((any)
                 (let* ((selected (car suitable-players))
                        (rotated-list (append (cdr suitable-players) (list selected))))
                   (display (format #f "[any ~a] Selected player: ~a\n" role-symbol ($ selected 'who)))
                   (display (format #f "[any ~a] Rotated list: ~a\n" role-symbol (player-names rotated-list)))
                   (hash-set! role-players role-symbol rotated-list)
                   (hash-set! last-selected role-symbol selected)
                   selected))
                (else (error "Invalid selector"))))))
      ((roles) roles)
      ((players) players)
      ((role-players) role-players)
      ((last-selected) last-selected))))

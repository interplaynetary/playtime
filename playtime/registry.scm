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

(define (is-suitable role-symbol)
  (let ((reqs (requirements role-symbol)))
    (lambda (player)
      ; (display (format #f "[~a] Checking requirements ~a for ~a\n" role-symbol reqs ($ player 'who)))
      (let ((missing-reqs
           (filter (lambda (req) (not ($ player 'has-capability? req)))
                   reqs)))
      (if (null? missing-reqs)
          #t
          (begin
            ($ player 'cue
               (format #f "Oops, you don't currently meet the requirements ~a for the ~a role"
                  missing-reqs role-symbol))
            #f))))))

(define (add-to-hash-list! hash key value)
  (let ((current-list (hash-ref hash key '())))
    (hash-set! hash key (cons value current-list))))

(define registry
  (let ((players (make-hash-table))
        (roles (make-hash-table))
        (role-players (make-hash-table)) ;; role-name -> list of spawned role players
        (player-roles (make-hash-table)) ;; player-symbol -> list of role-symbols
        (last-selected (make-hash-table))
        (states (make-hash-table))
        (telegram-to-player (make-hash-table))) ;; telegram username -> player symbol
    (methods
      ((set-state key value)
        (hash-set! states key value))
      ((get-state key)
        (hash-ref states key #f))
      ((states) states)
      ((register-player symbol actor)
        (hash-set! players symbol actor))
      ((register-telegram-lookup telegram-username player-symbol)
        (hash-set! telegram-to-player telegram-username player-symbol))
      ((get-player-symbol-by-telegram username)
        (hash-ref telegram-to-player username #f))
      ((get-player symbol)
        (hash-ref players symbol #f))
      ((register-role symbol role)
        (hash-set! roles symbol role))
      ((get-role symbol)
        (hash-ref roles symbol #f))
      ((register-role-player role-symbol role-instance)
        (let ((player-symbol ($ role-instance 'symbol)))
          (add-to-hash-list! role-players role-symbol role-instance)
          (add-to-hash-list! player-roles player-symbol role-symbol)))
      ((get-role-players role-symbol)
        (let ((all-players (hash-ref role-players role-symbol '())))
          (filter (is-suitable role-symbol) all-players)))
      ((get-role-player role-symbol selector)
        (let* ((all-players (hash-ref role-players role-symbol '()))
               (suitable-players (filter (is-suitable role-symbol) all-players)))
          (if (null? suitable-players)
              (begin
                ; (display (format #f "[Registry] No players registered for role ~a who meet the requirements ~a\n" role-symbol (requirements role-symbol)))
                #f)  ; Return #f if no players meet the requirements
              (case selector
                ((any)
                 (let* ((selected (car suitable-players))
                        (rotated-list (append (cdr suitable-players) (list selected))))
                  ;  (display (format #f "[any ~a] Selected player: ~a\n" role-symbol ($ selected 'who)))
                  ;  (display (format #f "[any ~a] Rotated list: ~a\n" role-symbol (player-names rotated-list)))
                   (hash-set! role-players role-symbol rotated-list)
                   (hash-set! last-selected role-symbol selected)
                   selected))
                (else (error "Invalid selector"))))))
      ((roles) roles)
      ((players) players)
      ((role-players) role-players)
      ((all-roles-cast?)
        (every (lambda (role-symbol)
                 (not (null? (registry 'get-role-players role-symbol))))
               (hash-keys roles)))
      ((last-selected) last-selected)
      ((player-roles) player-roles)
      ((get-player-roles player-symbol)
       (hash-ref player-roles player-symbol '()))
      ((has-role? player-symbol role-symbol)
       (let ((player-role-list (hash-ref player-roles player-symbol '())))
         (member role-symbol player-role-list)))
)))

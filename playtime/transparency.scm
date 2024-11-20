(define-module (playtime transparency)
  #:use-module (playtime utils)
  #:export (current-execution-stack)
  #:export (make-execution-frame)
  #:export (format-execution-frame)
  #:export (with-execution-tracking)
  #:export (with-frame))

(define current-execution-stack (make-parameter '()))

(define (make-execution-frame label player role)
  (let ((frame (make-hash-table)))
    (when label (hash-set! frame 'label label))
    (when player (hash-set! frame 'player player))
    (when role (hash-set! frame 'role role))
    frame))

(define (format-execution-frame frame)
  (let ((label (hash-ref frame 'label ""))
        (player (hash-ref frame 'player #f))
        (role (hash-ref frame 'role #f)))
    (string-append
      label
      (if player (format #f "[~a" player) "")
      (if role (format #f ":~a" role) "")
      (if (or player role) "]" ""))))

(define (with-execution-tracking frame thunk)
  (parameterize ((current-execution-stack
                  (cons frame (current-execution-stack))))
    (let ((result (thunk)))
      (display-flush
       (format #f "~a: ~a\n"
               (string-join
                (map format-execution-frame
                     (reverse (current-execution-stack)))
                " → ")
               (if (promise? result) "<promise>" result)))
      result)))

(define-syntax with-frame
  (syntax-rules ()
    [(_ frame-info expr ...)
     (with-execution-tracking frame-info
       (lambda () (begin expr ...)))]))

(define-module (playtime utils)
  #:use-module (playtime registry)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 readline)
  #:use-module (language tree-il)
  #:use-module (goblins)
  #:export (display-flush)
  #:export (print-and-run)
  #:export (print-hash)
  #:export (hash-map->string)
  #:export (normalize-name)
  #:export (display-role-summary)
  #:export (display-role-summary-telegram)
  #:export (try-eval-command))

(define (display-flush . args)
  (for-each (lambda (arg)
              (display arg)
              (force-output (current-output-port)))
            args))

(define (print-and-run x)
  (let ((expanded (macroexpand x)))
    (pretty-print (tree-il->scheme expanded))
    (eval x (interaction-environment))))

(define (print-hash hash)
  (hash-for-each (lambda (key value)
                  (format #t "  ~a => ~a~%" key value))
               hash))

(define (hash-map->string hash)
  (let ((result ""))
    (hash-for-each
     (lambda (key value)
       (set! result (string-append result (format #f "  ~a => ~a\n" key value))))
     hash)
    result))

(define (normalize-name name)
  (string->symbol
    (string-map (lambda (c)
                  (cond
                    ((char-alphabetic? c) (char-downcase c))
                    ((char-numeric? c) c)
                    (else #\-)))
                (string-trim name))))

(define (colorize text color)
  (let ((code (case color
                ((black)        "30")
                ((red)          "31")
                ((green)        "32")
                ((yellow)       "33")
                ((blue)         "34")
                ((magenta)      "35")
                ((cyan)         "36")
                ((white)        "37")
                ((bright-black) "90")
                ((bright-red)   "91")
                ((bright-green) "92")
                ((bright-yellow)"93")
                ((bright-blue)  "94")
                ((bright-magenta)"95")
                ((bright-cyan)  "96")
                ((bright-white) "97")
                ((bold)         "1")
                (else           "0"))))
    (format #f "\x1b[~am~a\x1b[0m" code text)))

(define (bold text)
  (colorize text 'bold))

(define (strip-ansi-sequences str)
  (string-replace-substring
    (string-replace-substring
      (string-replace-substring str "\x1b[0m" "")
      "\x1b[1m" "")
    "\x1b[92m" ""))

(define (pad-string str length)
  (let ((padding-length (max 0 (- length (string-length (strip-ansi-sequences str))))))
    (if (> padding-length 0)
      (string-append str (make-string padding-length #\space))
      str)))

(define (display-framed content width)
  (display-flush (format #f "⎥ ~a ⎪\n" (pad-string content width))))

(define (display-role-summary)
  (let ((width 71))
    (display-flush "┌~~~~~~~~~~~~ 𝗥𝗼𝗹𝗲 𝗖𝗮𝘀𝘁𝗶𝗻𝗴𝘀 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~┐\n")
    (hash-for-each
      (lambda (role-name _)
        (let ((players (registry 'get-role-players role-name)))
          (if (not (null? players))
            (for-each (lambda (player)
              (display-framed (format #f "~a (cast ~a ~a)"
                  (colorize "[✓]" 'bright-green)
                  (colorize role-name 'bright-white)
                  (colorize (format #f "\"~a\"" ($ player 'who)) 'green))
                width))
              players)
            (display-framed (format #f "~a (cast ~a \"<player-name>\" (telegram \"<username>\"))"
                  (colorize "[ ]" 'bright-yellow)
                  (colorize role-name 'bright-white))
                width))
          ))
      (registry 'roles))
    (display-flush "└~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~┘\n")
    (newline)
  ))

(define (display-role-summary-telegram organizer)
  (let ((output (call-with-output-string
                  (lambda (port)
                    (format port "Role Castings:~%~%")
                    (hash-for-each
                      (lambda (role-name _)
                        (let ((players (registry 'get-role-players role-name)))
                          (if (not (null? players))
                            (for-each (lambda (player)
                              (format port "[✓] (cast ~a ~a)~%"
                                     role-name ($ player 'who)))
                              players)
                            (format port "[ ] (cast ~a \"<player-name>\" (telegram \"<username>\"))~%"
                                   role-name))))
                      (registry 'roles))))))
    ($ organizer 'cue output)))

(define (add-parentheses-if-needed input)
  (if (and (> (string-length input) 0)
           (char=? (string-ref input 0) #\x28))  ; #\x28 is equivalent to #\(
      input                           ; Input already has parentheses
      (string-append "(" input ")"))) ; Add parentheses if missing

(define (parse-command-string input organizer)
  (catch #t
    (lambda ()
      (with-exception-handler
        (lambda (exn)
          ($ organizer 'cue (format #f "Parse error: ~a" exn))
          #f)
        (lambda ()
          (read (open-input-string (add-parentheses-if-needed input))))))
    (lambda (key . args)
      ($ organizer 'cue (format #f "Parse error: ~a ~a" key (if (pair? args) (car args) "")))
      #f)))

(define (valid-command? expr allowed-commands)
  (and (pair? expr)              ; Is it a list?
       (memq (car expr) allowed-commands))) ; Is the command allowed?

(define (try-eval-command input allowed-commands organizer)
  (catch #t  ; catch all exceptions
    (lambda ()
      (let ((expr (parse-command-string input organizer)))
        (and expr
             (valid-command? expr allowed-commands)
             (begin
               (eval expr (interaction-environment))
               (car expr)))))  ; Return the command that was executed
    (lambda (key . args)
      (let ((error-msg
              (string-append
                "Error executing command:\n"
                "  Type: " (symbol->string key) "\n"
                (string-append "  Details: " (format #f "~a" args) "\n"))))
        ($ organizer 'cue error-msg))
      #f)))  ; Return #f to indicate failure
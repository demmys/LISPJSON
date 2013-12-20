#|
 |
 | Lexical Analyzer
 |
 |#
(defun json-lex-r (in state stack)
 (let (
       (c (read-char in nil 'EOF))
      )
  (case state

   (INITIAL
    (if (equal c 'EOF)
     (list 'EOF)
     (if (digit-char-p c)
      (json-lex-r in 'NUMBER (digit-char-p c))
      (case c
       (#\{ (list 'LBRACE))
       (#\} (list 'RBRACE))
       (#\, (list 'COMMA))
       (#\: (list 'COLON))
       (#\[ (list 'LBRACKET))
       (#\] (list 'RBRACKET))
       (#\" (json-lex-r in 'STRING nil))
       (#\- (json-lex-r in 'MINUS 0))
       ((#\t #\f #\n)
        (let (
              (analyzed (json-lex-r in 'WORD (list c)))
              (token
               (case c
                (#\t (list "true" 'TRUE))
                (#\f (list "false" 'FALSE))
                (#\n (list "null" 'NULL))
               )
             )
            )
         (if (equal (car analyzed) (car token))
          (append (cdr token) (cdr analyzed))
          (error "Lexical analyze error: illegal word ~S" analyzed)
         )
        )
       )
       ((#\Space #\Linefeed) (json-lex-r in state stack))
       (t (error "Lexical analyze error: illegal character ~C" c))
      )
     )
    )
   )

   (WORD
    (case c
     ((#\Space #\Linefeed) (list (concatenate 'STRING (reverse stack))))
     ((EOF #\} #\] #\,)
      (list
       (concatenate 'STRING (reverse stack))
       (case c
        (EOF 'EOF)
        (#\} 'RBRACE)
        (#\] 'RBRACKET)
        (#\, 'COMMA)
       )
      )
     )
     (t (json-lex-r in 'WORD (cons c stack)))
    )
   )

   (STRING
    (case c
     (#\" (list (concatenate 'STRING (reverse stack))))
     (#\\ (json-lex-r in 'ESCCHAR stack))
     (EOF (error "Ends within string."))
     (t (json-lex-r in state (cons c stack)))
    )
   )

   (ESCCHAR
    (case c
     ((#\" #\\ #\/) (json-lex-r in 'STRING (cons c stack)))
     ;TODO should analyze \b \f \n \r \t \uXXXX
     (t (error "Lexical analyze error: illegal escape character \\~C" c))
    )
   )

   (NUMBER
    (if (equal c 'EOF)
     (list stack 'EOF)
     (if (digit-char-p c)
      (json-lex-r in state (+ (* stack 10) (digit-char-p c)))
      (case c
       ((#\Space #\Linefeed) (list stack))
       ((#\} #\] #\,)
        (list
         stack
         (case c
          (#\} 'RBRACE)
          (#\] 'RBRACKET)
          (#\, 'COMMA)
         )
        )
       )
       ;TODO should analyze float and exp
       (t (error "Lexical analyze error: illegal number ~C" c))
      )
     )
    )
   )

   (MINUS
    (if (digit-char-p c)
     (let (
           (analyzed (json-lex-r in 'NUMBER (digit-char-p c)))
          )
      (cons (- (car analyzed)) (cdr analyzed))
     )
     (error "Lexical analyze error: illegal number ~C" c)
    )
   )

  )
 )
)

(defun json-lex (in)
 (json-lex-r in 'INITIAL nil)
)



#|
 |
 | Parser
 |
 |#
(defun json-parse-r (in state queue)
 (let (
       (next-token #'(lambda (queue) (if (null queue) (json-lex in) queue)))
      )
  (case state

   (JSON
    (let (
          (value (json-parse-r in 'VALUE nil))
         )
     (let (
           (end (car (funcall next-token (cdr value))))
          )
      (if (equal end 'EOF)
       (car value)
       (error "Parse error: illegal sequence of value in top level.")
      )
     )
    )
   )

   (VALUE
    (let (
          (token (funcall next-token queue))
         )
     (let (
           (w (car token))
           (r (cdr token))
          )
      (cond
       ((numberp w) token)
       ((stringp w) token)
       (t
        (case w
         ((TRUE FALSE NULL) token)
         (LBRACKET
          (let (
                (elements (json-parse-r in 'ELEMENTS r))
               )
           (let (
                 (rbracket (funcall next-token (cdr elements)))
                )
            (if (equal (car rbracket) 'RBRACKET)
             (cons (car elements) (cdr rbracket))
             (error "Parse error: JSON Array does not end with \"]\"")
            )
           )
          )
         )
        )
       )
      )
     )
    )
   )

   (ELEMENTS
    (let (
          (value (json-parse-r in 'VALUE queue))
         )
     (let (
           (comma (funcall next-token (cdr value)))
          )
      (if (equal (car comma) 'COMMA)
       (let (
             (elements (json-parse-r in 'ELEMENTS (cdr comma)))
            )
        (cons (cons (car value) (car elements)) (cdr elements))
       )
       (cons (list (car value)) comma)
      )
     )
    )
   )

  )
 )
)

(defun json-parse (in)
 (json-parse-r in 'JSON nil)
)



#|
 |
 | Stream selector
 |
 |#
(defun get-stream (file callback)
 (if (null file)
  (funcall callback *standard-input*)
  (with-open-file (in file :direction :input)
   (funcall callback in)
  )
 )
)



#|
 |
 | Main
 |
 |#
(defun main ()
 (print
  (get-stream
   (car *args*)
   #'json-parse
  )
 )
)

(main)

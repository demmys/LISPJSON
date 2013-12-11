(defun lex-r (in state stack)
 (let (
       (c (read-char in nil 'EOF))
      )
  (if (equal c 'EOF)
   (list 'EOF)
   (case state

    ('NOMAL
     (if (digit-char-p c)
      (lex-r in 'NUMBER (digit-char-p c))
      (case c
       (#\{ (list 'LBRACE))
       (#\} (list 'RBRACE))
       (#\, (list 'COMMA))
       (#\: (list 'COLON))
       (#\[ (list 'LBRACKET))
       (#\] (list 'RBRACKET))
       (#\" (lex-r in 'STRING nil))
       (#\- (lex-r in 'MINUS 0))
       ((#\t #\f #\n)
        (let (
              (analyzed (lex-r in 'WORD (list c)))
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
          (error "Lexical analyze error: illigal word ~S" analyzed)
         )
        )
       )
       ((#\Space #\Linefeed) (lex-r in state stack))
       (t (error "Lexical analyze error: illigal character ~C" c))
     )
     )
    )

    ('WORD
     (case c
      ((#\Space #\Linefeed) (list (concatenate 'STRING (reverse stack))))
      ((#\} #\] #\,)
       (list
        (concatenate 'STRING (reverse stack))
        (case c
         (#\} 'RBRACE)
         (#\] 'RBRACKET)
         (#\, 'COMMA)
        )
       )
      )
      (t (lex-r in 'WORD (cons c stack)))
     )
    )

    ('STRING
     (case c
      (#\" (list (concatenate 'STRING (reverse stack))))
      (#\\ (lex-r in 'ESCCHAR stack))
      (t (lex-r in state (cons c stack)))
     )
    )

    ('ESCCHAR
     (case c
      ((#\" #\\ #\/) (lex-r in 'STRING (cons c stack)))
      ;TODO should analyze \b \f \n \r \t \uXXXX
      (t (error "Lexical analyze error: illigal escape character \\~C" c))
     )
    )

    ('NUMBER
     (if (digit-char-p c)
      (lex-r in state (+ (* stack 10) (digit-char-p c)))
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
       (t (error "Lexical analyze error: illigal number ~C" c))
      )
     )
    )

    ('MINUS
     (if (digit-char-p c)
      (let (
            (analyzed (lex-r in 'NUMBER (digit-char-p c)))
           )
       (cons (- (car analyzed)) (cdr analyzed))
      )
      (error "Lexical analyze error: illigal number ~C" c)
     )
    )

   )
  )
 )
)

(defun lex (in)
 (lex-r in 'NOMAL nil)
)

(defun parse-r (in obj)
 (let (
       (w (lex in))
      )
  (case (car w)
   ('EOF (print "Parse finished."))
   (t (print w) (parse-r in nil))
  )
 )
)

(defun parse (in)
 (parse-r in nil)
)

(defun get-stream (file callback)
 (if (null file)
  (funcall callback *standard-input*)
  (with-open-file (in file :direction :input)
   (funcall callback in)
  )
 )
)

(defun main ()
 (get-stream
  (car *args*)
  #'parse
 )
)

(main)

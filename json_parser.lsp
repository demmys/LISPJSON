(defun get-stream (file callback)
 (if (null file)
  (funcall callback *standard-input*)
  (with-open-file (in file :direction :input)
   (funcall callback in)
  )
 )
)

(defun lex-r (in state stack)
 (let (
       (c (read-char in nil 'EOF))
      )
  (if (equal c 'EOF)
   'EOF
   (case state

    ('NOMAL
     (cond
      ((equal c #\{) 'LBRACE)
      ((equal c #\}) 'RBRACE)
      ((equal c #\,) 'COMMA)
      ((equal c #\:) 'COLON)
      ((equal c #\[) 'LBRACKET)
      ((equal c #\]) 'RBRACKET)
      ((equal c #\") (lex-r in 'STRING nil))
      ((equal c #\-) (lex-r in 'MINUS 0))
      ((digit-char-p c) (lex-r in 'NUMBER (digit-char-p c)))
      ((equal c #\t)
       (if (equal (lex-r in 'WORD (list #\t)) "true")
        'TRUE
        (error "Lexical analyze error: illigal word" )
       )
      )
      ((equal c #\f)
       (if (equal (lex-r in 'WORD (list #\f)) "false")
        'FALSE
        (error "Lexical analyze error: illigal word" )
       )
      )
      ((equal c #\n)
       (if (equal (lex-r in 'WORD (list #\n)) "null")
        'NULL
        (error "Lexical analyze error: illigal word" )
       )
      )
      (t
       (case c
        ((#\Space #\Linefeed) (lex-r in state stack))
        (t (error "Lexical analyze error: illigal character ~C" c))
       )
      )
     )
    )

    ('WORD
     (case c
      ;TODO cannot end with } ] ,
      ((#\Space #\Linefeed) (concatenate 'STRING (reverse stack)))
      (t (lex-r in 'WORD (cons c stack)))
     )
    )

    ('STRING
     (case c
      (#\" (concatenate 'STRING (reverse stack)))
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
       ;TODO cannot end with } ] ,
       ((#\Space #\Linefeed) stack)
       ;TODO should analyze float and exp
       (t (error "Lexical analyze error: illigal number ~C" c))
      )
     )
    )

    ('MINUS
     (if (digit-char-p c)
      (lex-r in state (+ (* stack 10) (digit-char-p c)))
      (case c
       ;TODO cannot end with } ] ,
       ((#\Space #\Linefeed) (- stack))
       ;TODO should analyze float and exp
       (t (error "Lexical analyze error: illigal number ~C" c))
      )
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
  (case w
   ('EOF (print "Parse finished."))
   (t (print w) (parse-r in nil))
  )
 )
)

(defun parse (in)
 (parse-r in nil)
)

(defun main ()
 (get-stream
  (car *args*)
  #'parse
 )
)

(main)

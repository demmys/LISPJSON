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
 | JSON
 |
 |#
(defun json-object ()
 (list 'OBJECT)
)
(defun json-object-append (jobj key val)
 (cons 'OBJECT (acons key val (cdr jobj)))
)

(defun json-array ()
 (list 'ARRAY)
)
(defun json-array-append (jary val)
 (cons 'ARRAY (cons val (cdr jobj)))
)

(defun json-number (n)
 (list 'NUMBER n)
)

(defun json-string (s)
 (list 'STRING s)
)

(defun json-const (kind)
 (list kind)
)



#|
 |
 | Parser
 |
 |#
(defun json-append (json key_val val_nil)
 (case (car json)
  (OBJECT (json-object-append json key_val val_nil))
  (ARRAY (json-array-append json key_val))
  ; TODO statement of error
  (t (error "Parse error: "))
 )
)

(defun json-parse-r (in state init res)
 (let (
       (token (if (null init) (json-lex in) init))
      )
  (let (
        (w (car token))
        (r (cdr token))
       )
   (case state

    (INITIAL
     (cond
      ((numberp w) (json-parse-r in 'END r w))
      ((stringp w) (json-parse-r in 'END r w))
      (t (case w
          (TRUE (json-parse-r in 'END r w))
          (FALSE (json-parse-r in 'END r w))
          (NULL (json-parse-r in 'END r w))
         )
      )
     )
    )

    (END
     (if (equal w 'EOF)
      res
      (error "Parse error: illegal sequence of value in top level.")
     )
    )

   )
  )
 )
)

(defun json-parse (in)
 (json-parse-r in 'INITIAL nil nil)
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

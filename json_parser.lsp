#|
 |
 | Lexical Analyzer
 |
 |#
(defun json-lex-r (in state stack)
 (let (
       (c (read-char in nil 'EOF))
      )
  (if (equal c 'EOF)
   (list 'EOF)
   (case state

    ('NOMAL
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
          (error "Lexical analyze error: illigal word ~S" analyzed)
         )
        )
       )
       ((#\Space #\Linefeed) (json-lex-r in state stack))
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
      (t (json-lex-r in 'WORD (cons c stack)))
     )
    )

    ('STRING
     (case c
      (#\" (list (concatenate 'STRING (reverse stack))))
      (#\\ (json-lex-r in 'ESCCHAR stack))
      (t (json-lex-r in state (cons c stack)))
     )
    )

    ('ESCCHAR
     (case c
      ((#\" #\\ #\/) (json-lex-r in 'STRING (cons c stack)))
      ;TODO should analyze \b \f \n \r \t \uXXXX
      (t (error "Lexical analyze error: illigal escape character \\~C" c))
     )
    )

    ('NUMBER
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
       (t (error "Lexical analyze error: illigal number ~C" c))
      )
     )
    )

    ('MINUS
     (if (digit-char-p c)
      (let (
            (analyzed (json-lex-r in 'NUMBER (digit-char-p c)))
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

(defun json-lex (in)
 (json-lex-r in 'NOMAL nil)
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
  ('OBJECT (json-object-append json key_val val_nil))
  ('ARRAY (json-array-append json key_val))
  ; TODO statement of error
  (t (error "Parse error: "))
 )
)

(defun json-parse-r (in init res)
 (let (
       (token (if (null init) (json-lex in) init))
      )
  (let (
        (w (car token))
        (r (cdr token))
       )
   (cond
    ((numberp w) (json-parse-r in r (print w)))
    ((stringp w) (json-parse-r in r (print w)))
    (t
     (json-parse-r
      in
      r
      (case w
       ('EOF (exit))
       ('LBRACE (print w))
       ('RBRACE (print w))
       ('COMMA (print w))
       ('COLON (print w))
       ('LBRACKET (print w))
       ('RBRACKET (print w))
       ('TRUE (print w))
       ('FALSE (print w))
       ('NULL (print w))
      )
     )
    )
   )
  )
 )
)

(defun json-parse (in)
 (json-parse-r in nil nil)
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
 (get-stream
  (car *args*)
  #'json-parse
 )
)

(main)

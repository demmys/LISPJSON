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
         (LBRACE
          (let (
                (members (json-parse-r in 'MEMBERS r))
               )
           (let (
                 (rbrace (funcall next-token (cdr members)))
                )
            (if (equal (car rbrace) 'RBRACE)
             (cons (car members) (cdr rbrace))
             (error "Parse error: JSON Object does not end with \"}\"")
            )
           )
          )
         )
         (t (error "Parse error: ~S is not a value" w))
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

   (PAIR
    (let (
          (string (funcall next-token queue))
         )
     (let (
           (colon (funcall next-token (cdr string)))
          )
      (let (
            (value (json-parse-r in 'VALUE (cdr colon)))
           )
       (if (and (stringp (car string)) (equal (car colon) 'COLON))
        (cons (cons (car string) (car value)) (cdr value))
        (error "Parse error: illegal pair in object.")
       )
      )
     )
    )
   )

   (MEMBERS
    (let (
          (pair (json-parse-r in 'PAIR queue))
         )
     (let (
           (comma (funcall next-token (cdr pair)))
          )
      (if (equal (car comma) 'COMMA)
       (let (
             (members (json-parse-r in 'MEMBERS (cdr comma)))
            )
        (cons (cons (car pair) (car members)) (cdr members))
       )
       (cons (list (car pair)) comma)
      )
     )
    )
   )

  )
 )
)

(defun json-parse (in)
 (if (stringp in)
  (json-parse-r (make-string-input-stream in) 'JSON nil)
  (json-parse-r in 'JSON nil)
 )
)



#|
 |
 | JSON
 |
 |#
(defun json-object-get (object key counter)
 (if (equal counter (length object))
  nil
  (let (
        (current (nth counter object))
       )
   (if (equal (car current) key)
    (cdr current)
    (json-object-get object key (1+ counter))
   )
  )
 )
)

(defun json-get (array_object key)
 (if (atom array_object)
  (error "Can not get from value.")
  (cond
   ((numberp key) (nth key array_object))
   ((stringp key) (json-object-get array_object key 0))
  )
 )
)



#|
 |
 | JSON printer
 |
 |#
(defun json-pairs-print (pairs res)
 (if (null pairs)
  res
  (let (
        (pair (car pairs))
       )
   (json-pairs-print
    (cdr pairs)
    (format nil "~A~S: ~A~A"
     res
     (car pair)
     (json-print-r (cdr pair))
     (if (> (length pairs) 1) ", " " ")
    )
   )
  )
 )
)

(defun json-elements-print (elements res)
 (if (null elements)
  res
  (json-elements-print
   (cdr elements)
   (format nil "~A~A~A"
    res
    (json-print-r (car elements))
    (if (> (length elements) 1) ", " " ")
   )
  )
 )
)

(defun json-is-object (json counter)
 (if (null json)
  t
  (let (
        (pair (car json))
       )
   (if (listp pair)
    (if (stringp (car pair))
     (json-is-object (cdr json) (1+ counter))
     nil
    )
    nil
   )
  )
 )
)

(defun json-print-r (json)
 (cond
  ((numberp json) (format nil "~D" json))
  ((stringp json) (format nil "~S" json))
  ((listp json)
   (if (json-is-object json 0)
    (format nil "{ ~A}" (json-pairs-print json ""))
    (format nil "[ ~A]" (json-elements-print json ""))
   )
  )
  (t
   (case json
    (TRUE (format nil "~A" "true"))
    (fALSE (format nil "~A" "false"))
    (NULL (format nil "~A" "false"))
    (t (error "This object is not a part of JSON format."))
   )
  )
 )
)

(defun json-print (json)
 (format t "~A~%" (json-print-r json))
 json
)

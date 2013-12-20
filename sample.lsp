(load "json.lsp")

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
 (let (
       (json
        (get-stream
         (car *args*)
         #'json-parse
        )
       )
      )
  (print
   (json-print
    (acons "readed" 'TRUE json)
   )
  )
 )
)

(main)

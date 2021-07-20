(load "loop-defmacro.scm")

(call-with-output-file "../loop.scm"
  (lambda (op)
    (write-string "(define loop (let ()" op)
    (map (lambda (f)
           (call-with-input-file f
             (lambda (ip)
               (loop for t = (read-string 1024 ip)
                     until (eq? #<eof> t)
                     do (write-string t op)))))
         files)
    (write-string "(macro forms (expand-body forms))))" op)))

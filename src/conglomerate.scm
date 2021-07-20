(load "loop-defmacro.scm")

(call-with-output-file "../loop.scm"
  (lambda (op)
    (write-string "; automatically generated; do not edit\n\n(define loop (let ()\n" op)
    (loop for f in files
          do (call-with-input-file f
               (lambda (ip)
                 (loop for t = (read-string 1024 ip)
                       until (eq? #<eof> t)
                       do (write-string t op)))))
    (write-string "(macro forms (expand-body forms))))" op)))
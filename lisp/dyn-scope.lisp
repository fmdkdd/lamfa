(defun lookup (item list)
  (cdr (assoc item list :test #'equalp)))

(defparameter eval-rules
  `((num . ,(lambda (num)
              (nth 1 num)))
    (plus . ,(lambda (plus)
               (+ (interp (nth 1 plus))
                  (interp (nth 2 plus)))))))

(defun interp (node)
  (let ((f (lookup (nth 0 node) eval-rules)))
    (when (functionp f)
      (funcall f node))))

(print (interp '(num 1))) ;: 1
(print (interp '(plus (num 1) (num 2)))) ;: 3

(let ((eval-rules
       (append
        `(num . ,(lambda (num)
                   (* 2 (nth 1 num))))
        eval-rules)))
  (print (interp '(plus (num 1) (num 2)))))

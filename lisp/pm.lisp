(defun lookup (item list)
  (cdr (assoc item list :test #'equalp)))

(defun address (a)
  (list 'address a))

(defun closure (x e env)
  (list 'closure x e env))

(defparameter *bottom* '(bottom))

(defparameter *application-rules*
  `((closure . ,(lambda (s c v)
                  (destructuring-bind (_ x e env) c
                    (let ((env2 (cons (cons x v) env)))
                      (eval-node s env2 e)))))

    ))

(defun eval-apply (s v1 v2)
  (let ((f (lookup (car v1) *application-rules*)))
    (funcall f s v1 v2)))

(defparameter *deref-rules*
  `((address . ,(lambda (s l)
                  (destructuring-bind (_ a) l
                    (lookup a s))))

    ))

(defun eval-deref (s v)
  (let ((f (lookup (car v) *deref-rules*)))
    (funcall f s v)))

(defparameter *rules*
  `((c . ,(lambda (s env node)
            (destructuring-bind (_ e) node
              (list s e))))

    (v . ,(lambda (s env node)
            (destructuring-bind (_ e) node
              (list s (lookup e env)))))

    (fun . ,(lambda (s env node)
              (destructuring-bind (_ x e) node
                (list s (closure x e env)))))

    (app . ,(lambda (s env node)
              (destructuring-bind (_ e1 e2) node
                (destructuring-bind (s1 v1) (eval-node s env e1)
                  (destructuring-bind (s2 v2) (eval-node s1 env e2)
                    (eval-apply s2 v1 v2))))))

    (ref . ,(lambda (s env node)
              (destructuring-bind (_ e) node
                (destructuring-bind (s1 v1) (eval-node s env e)
                  (let* ((a (length s1))
                         (s2 (cons (cons a v1) s1)))
                    (list s2 (address a)))))))

    (deref . ,(lambda (s env node)
                (destructuring-bind (_ e) node
                  (destructuring-bind (s1 v1) (eval-node s env e)
                    (list s1 (eval-deref s1 v1))))))

    ))

(defun eval-node (store env node)
  (let ((f (lookup (car node) *rules*)))
    (funcall f store env node)))

(defun eval-program (AST env store)
  (eval-node store env AST))

(eval-program
 '(app (fun "x" (deref (v "x")))
       (ref (c 42)))
 '() '())

;; Facets

(defun facet (k vh vl)
  (list 'facet k vh vl))

(defun mk-facet (pc v1 v2)
  (if (eq (length pc) 0)
      v1
    (let ((k (car pc))
          (rest (cdr pc)))
      (if (> k 0)
          (facet k (mk-facet rest v1 v2) v2)
        (facet k v2 (mk-facet rest v1 v2))))))

(defparameter *facets/rules*
  (append
   `((ref . ,(lambda (s env node)
               (destructuring-bind (_ e) node
                 (destructuring-bind (s1 v1) (eval-node s env e)
                   (let* ((a (length s1))
                          (s2 (cons
                               (cons a (mk-facet *pc* v1 *bottom*))
                               s1)))
                     (list s2 (address a))))))))
   *rules*
   ))

(defparameter *facets/application-rules*
  (append
   `((facet . ,(lambda (s f)
                 (destructuring-bind (_ k vh vl) f
                   (cond ((member k *pc*) (eval-apply s vh v2))
                         ((member (- k) *pc*) (eval-apply s vl v2))
                         (t
                          (destructuring-bind (s1 vh1)
                              (let ((*pc* (adjoin k *pc*)))
                                (eval-apply s vh v2))
                            (destructuring-bind (s2 vl1)
                                (let ((*pc* (adjoin (- k) *pc*)))
                                  (eval-apply s1 vl v2))
                              (list s2 (mk-facet k vh1 vl1))))))))))
  *application-rules*
  ))

(defparameter *facets/deref-rules*
  (append
   `((facet . ,(lambda (s f)
                 (destructuring-bind (_ k vh vl) f
                   (cond ((member k *pc*) (eval-deref s vh))
                         ((member (- k) *pc*) (eval-deref s vl))
                         (t (mk-facet k
                                      (eval-deref s vh)
                                      (eval-deref s vl))))))))
  *deref-rules*
  ))

(defun facets/eval-program (AST env store *pc*)
  (declare (special *pc*))
  (let ((*rules* *facets/rules*)
        (*application-rules* *facets/application-rules*)
        (*deref-rules* *facets/deref-rules*))
    (eval-program AST env store)))

(facets/eval-program
 '(app (fun "x" (deref (v "x")))
       (ref (c 42)))
 '() '() '(1))

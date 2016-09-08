#lang racket

  (define eval-num (make-parameter
                    (lambda (num)
                      (cadr num))))

  (define eval-plus (make-parameter
                     (lambda (plus)
                       (+ ((interp) (cadr plus)
                          ((interp) (caddr plus)))))))

  (define interp
    (make-parameter
     (lambda (node)
       (match (car node)
         ['num ((eval-num) node)]
         ['plus ((eval-plus) node)]))))

(print ((interp) '(num 1)))

(parameterize ([eval-num (lambda (num)
                           (* 2 (cadr num)))])
  (print ((interp) '(num 1))))

(print ((interp) '(num 1)))
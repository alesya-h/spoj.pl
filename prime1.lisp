;; -*- mode: lisp; coding: utf-8 -*-

(deftype myint () '(integer 1 1000000000))

(set-dispatch-macro-character #\# #\%
           #'(lambda (stream char1 char2)
               (let ((str (gensym)))
                 `(let ((,str ,(read stream t nil t)))
                    (defparameter _ ,str)
                    (print ,str)))))


(proclaim '((optimize speed) (type *first-primes* (cons myint *))))

(defparameter *first-primes* (list 2 3))
(defparameter *last-checked-prime* 3)

(defun genprimes (upto)
  (loop for i from (1+ *last-checked-prime*) to upto
     do (loop with last-to-check = (floor (sqrt i))
           for current on *first-primes*
           while (<= (car current) last-to-check) 
           do (if (eq 0 (mod i (car current))) (return))
           finally (nconc current (list i)))
     do (setf *last-checked-prime* i)))

#.(genprimes (ceiling (sqrt 1000000000)))

(defparameter *dst* (vector (length *first-primes*)))


(defun primes-in-range (m n)
  (let ((first-primes (delete-if (lambda (x) (> x n)) *first-primes*)))
    (unless (> m n)
      (let ((mods (mapcar (lambda (p) (mod (1- m) p)) first-primes)))
        (loop for i from m to n
           for prime = t
           do (let ((prime t))
                (loop for cur in first-primes
                   for mod on mods
                   do (if (and (eq (setf (car mod) (mod (1+ (car mod)) cur)) 0)
                               (< cur i))
                          (setf prime nil)))
                (if prime (print i)))))))
        
  (unless (> m n)
    (genprimes n)
    (loop for current in *first-primes*
       while (<= current n)
       do (if (>= current m) (format t "~A~%" current)))))

(defun main (tt)
  (dotimes (x tt)
    (let* ((mn (read-from-string (format nil "(~A)" (read-line))))
           (m (car mn))
           (n (cadr mn)))
      (unless (eql x 0) (format t "~%"))
      (primes-in-range m n))))
(main (read))


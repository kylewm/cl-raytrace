(in-package :raytrace)

(defun curry (function &rest args)
    (lambda (&rest more-args)
      (apply function (append args more-args))))

(defun flatmap (f-returning-list list)
  (reduce #'append 
	  (mapcar f-returning-list list)))

(defun make-color (red green blue)
  (list red green blue))

(defun color-red (color)
  (car color))

(defun color-green (color)
  (cadr color))

(defun color-blue (color)
  (caddr color))

(defun color-to-256 (comp)
  (round (* 255 (max 0 (min 1 comp)))))

(defun make-point (x y &optional z)
  (if (null z)
      (list x y)
      (list x y z)))

(defun point-x (pt) (car pt))
(defun point-y (pt) (cadr pt))
(defun point-z (pt) (caddr pt))

(defun square (x) (* x x))

(defun sum-of-squares (pt)
  (reduce #'+ (mapcar #'square pt)))

(defun magnitude (pt)
  (sqrt (sum-of-squares pt)))

(defun normalize (pt)
  (let ((mag (magnitude pt)))
    (mapcar (lambda (x) (/ x mag)) pt)))

(defun dot-product (vec1 vec2)
  (reduce #'+ (mapcar #'* vec1 vec2)))

(defun vector-add (&rest vectors)
  (apply #'mapcar #'+ vectors))

(defun vector-sub (&rest vectors)
  (apply #'mapcar #'- vectors))

(defun vector-mult-scalar (vec &rest scalars)
  (mapcar (lambda (x) (apply #'* x scalars)) vec))

(defun vector-div-scalar (vec scalar)
  (mapcar (lambda (x) (/ x scalar)) vec) )

(defun calc-direction (start end)
  (normalize (vector-sub end start)))

(defun vector-avg (vecs)
  (vector-div-scalar 
   (apply #'vector-add vecs) (length vecs)))

(defun vector-negate (vec)
  (vector-mult-scalar vec -1))

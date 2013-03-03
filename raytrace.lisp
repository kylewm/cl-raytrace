(defpackage :raytrace
  (:use :common-lisp :png))

(in-package :raytrace)

(defparameter *image-width* 400)
(defparameter *image-height* 400)
(defparameter *viewplane-distance* 10)
(defparameter *viewplane-width* 10)
(defparameter *viewplane-height* 10)


(defun curry (function &rest args)
    (lambda (&rest more-args)
      (apply function (append args more-args))))

(defun make-color (red green blue)
  (list red green blue))

(defun color-red (color)
  (car color))

(defun color-green (color)
  (cadr color))

(defun color-blue (color)
  (caddr color))

(defun make-point (x y &optional z)
  (if (null z)
      (list x y)
      (list x y z)))

(defun point-x (pt)
  (car pt))

(defun point-y (pt)
  (cadr pt))

(defun point-z (pt)
  (caddr pt))

(defun square (x)
  (* x x))

(defun sum-of-squares (pt)
  (reduce #'+ (mapcar #'square pt)))

(defun magnitude (pt)
  (sqrt (sum-of-squares pt)))

(defun normalize (pt)
  (let ((mag (magnitude pt)))
    (mapcar (lambda (x) (/ x mag)) pt)))

(defun make-ray (origin dir)
  (list origin (normalize dir)))

(defun make-ray-from-points (start end)
  (make-ray start (mapcar #'- end start)))

(defun ray-origin (ray)
  (car ray))

(defun ray-x (ray)
  (point-x (ray-origin ray)))

(defun ray-y (ray)
  (point-y (ray-origin ray)))

(defun ray-z (ray)
  (point-z (ray-origin ray)))

(defun ray-dir (ray)
  (cadr ray))

(defun ray-i (ray)
  (point-x (ray-dir ray)))

(defun ray-j (ray)
  (point-y (ray-dir ray)))

(defun ray-k (ray)
  (point-z (ray-dir ray)))

(defun object-type (obj)
  (car obj))

(defun object-color (obj)
  (cadr obj))

(defun make-sphere (color center radius)
  (list 'sphere color center radius))

(defun sphere-center (sphere)
  (caddr sphere))

(defun sphere-radius (sphere)
  (cadddr sphere))

(defun sphere-l (sphere)
  (point-x (sphere-center sphere)))

(defun sphere-m (sphere)
  (point-y (sphere-center sphere)))

(defun sphere-n (sphere)
  (point-z (sphere-center sphere)))

(defun sphere-p (obj)
  (eq (car obj) 'sphere))

(defun make-intersection (time normal obj)
  (list time (normalize normal) obj))

(defun intersection-t (inters)
  (car inters))

(defun intersection-normal (inters)
  (cadr inters))

(defun intersection-object (inters)
  (caddr inters))

(defun find-intersection-sphere (ray sphere)
  (let* ((a (sum-of-squares (ray-dir ray)))
	 (b (+ (* 2 (ray-i ray) (- (ray-x ray) (sphere-l sphere)))
	       (* 2 (ray-j ray) (- (ray-y ray) (sphere-m sphere)))
	       (* 2 (ray-k ray) (- (ray-z ray) (sphere-n sphere)))))
	 (c (+ (sum-of-squares (sphere-center sphere))
	       (sum-of-squares (ray-origin ray))
	       (* -2 (sphere-l sphere) (ray-x ray))
	       (* -2 (sphere-m sphere) (ray-y ray))
	       (* -2 (sphere-n sphere) (ray-z ray))
	       (- (square (sphere-radius sphere)))))
	 (det (- (square b) (* 4 a c))))
    
    (if (< det 0)
	nil
	(let* ((t1 (/ (+ (- b) (sqrt det)) (* 2 a)))
	       (t2 (/ (- (- b) (sqrt det)) (* 2 a)))
	       (time (min t1 t2))
	       (normal (make-point (/ (- (ray-x ray) (sphere-l sphere)) (sphere-radius sphere))
				   (/ (- (ray-y ray) (sphere-m sphere)) (sphere-radius sphere))
				   (/ (- (ray-z ray) (sphere-n sphere)) (sphere-radius sphere)))))
	  (make-intersection time normal sphere)))))

(defun find-intersection-object (ray obj)
  (cond ((sphere-p obj) (find-intersection-sphere ray obj))
	(t nil)))

(defun find-intersection (ray)
  (reduce 
   (lambda (best-int obj) 
     (let ((obj-int (find-intersection-object ray obj)))
       (if (or (null best-int)
	       (and obj-int (< (intersection-t obj-int) (intersection-t best-int))))
	   obj-int
	   best-int))) 
   *scene* :initial-value nil))

(defparameter *scene*
  (list
   (make-sphere (make-color 0 0 255) (make-point 20 5 -150) 30)
   (make-sphere (make-color 255 0 0) (make-point -10 -5 -100) 30)))

(defun set-pixel (img pt color)
  (setf (aref img (point-y pt) (point-x pt) 0) (color-red color))
  (setf (aref img (point-y pt) (point-x pt) 1) (color-green color))
  (setf (aref img (point-y pt) (point-x pt) 2) (color-blue color)))

(defun shoot-ray (ray)
  (let ((inters (find-intersection ray)))
    (if inters
	(object-color (intersection-object inters))
	(make-color 0 0 0))))

(defun raytrace (output-pathname)
  (let
      ((img (png:make-image *image-height* *image-width* 3 8)))
    (dotimes (x *image-width*)
      (dotimes (y *image-height*)	
	(let* ((viewplane-x (- (/ (* x *viewplane-width*) *image-width*) (/ *viewplane-width* 2)))
	       (viewplane-y (- (/ (* y *viewplane-height*) *image-height*) (/ *viewplane-height* 2)))
	       (ray (make-ray-from-points 
		     (make-point 0 0 0) 
		     (make-point viewplane-x viewplane-y (- *viewplane-distance*))))
	       (color (shoot-ray ray)))
	  (set-pixel img
		     (make-point x y)
		     color))))        
    (with-open-file (output output-pathname :element-type '(unsigned-byte 8)
     			    :direction :output :if-exists :supersede)
      (png:encode img output))))

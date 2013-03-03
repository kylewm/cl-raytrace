(ql:quickload 'png)

(defpackage :raytrace
  (:use :common-lisp :png))

(in-package :raytrace)

(defparameter *image-width* 400)
(defparameter *image-height* 400)
(defparameter *viewplane-distance* 10)
(defparameter *viewplane-width* 10)
(defparameter *viewplane-height* 10)

(defun make-point (x y &optional z)
  (if (null z)
      (list x y)
      (list x y z)))


(defun make-color (red green blue)
  (list red green blue))

(defclass ray ()
  ((origin :initarg :origin
	   :reader origin)
   (direction :initarg :direction
	      :reader direction)))

(defclass scene-object () 
  ((material
    :initarg :material
    :reader material)))

(defclass sphere (scene-object)
  ((center
    :initarg :center
    :reader center)
   (radius
    :initarg :radius
    :reader radius)))

(defclass ray-intersection ()
  ((time
    :initarg :intersect-time
    :reader intersect-time)
   (point
    :initarg :point
    :reader point)
   (normal
    :initarg :normal
    :reader normal)
   (intersect-object
    :initarg :intersect-object
    :reader intersect-object)))

(defclass light ()
  ((intensity
    :initarg :intensity
    :reader intensity)
   (color
    :initarg :color
    :reader color)))

(defclass point-light (light)
  ((point
    :initarg :point
    :reader point)))

(defclass material ()
    ((color
      :initarg :color
      :reader color)))

(defparameter *ambient-light*
  (make-instance 'light :intensity 0.2 :color (make-color 1.0 1.0 1.0)))

(defparameter *red-material*
  (make-instance 'material :color (make-color 1.0 0.0 0.0)))

(defparameter *blue-material*
  (make-instance 'material :color (make-color 0.0 0.0 1.0)))

(defparameter *scene*
  (list
   (make-instance 'sphere :material *blue-material* :center (make-point -15 -10 -105) :radius 30)
   (make-instance 'sphere :material *red-material* :center (make-point 15 10 -95) :radius 30)))

(defparameter *point-lights*
  (list
   (make-instance 'point-light :point (make-point 0 -20 -50) :intensity 0.8 :color (make-color 1.0 1.0 1.0))))

(defun curry (function &rest args)
    (lambda (&rest more-args)
      (apply function (append args more-args))))

(defun color-red (color)
  (car color))

(defun color-green (color)
  (cadr color))

(defun color-blue (color)
  (caddr color))

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

(defun vector-mult-scalar (vec scalar)
  (mapcar (lambda (x) (* x scalar)) vec))

(defun vector-div-scalar (vec scalar)
  (mapcar (lambda (x) (/ x scalar)) vec) )

(defun ray-x (ray) (point-x (origin ray)))
(defun ray-y (ray) (point-y (origin ray)))
(defun ray-z (ray) (point-z (origin ray)))

(defun ray-i (ray) (point-x (direction ray)))
(defun ray-j (ray) (point-y (direction ray)))
(defun ray-k (ray) (point-z (direction ray)))

(defun calc-direction (start end)
  (normalize (vector-sub end start)))

(defun make-ray-from-points (start end)
  (make-instance 'ray :origin start :direction (calc-direction start end)))

(defgeneric find-intersection-object (ray object))

(defmethod find-intersection-object (ray (object sphere))
    (let* ((radius (radius object))
	   (sphere-l (point-x (center object)))
	   (sphere-m (point-y (center object)))
	   (sphere-n (point-z (center object)))
	   (a (sum-of-squares (direction ray)))
	   (b (+ (* 2 (ray-i ray) (- (ray-x ray) sphere-l))
		 (* 2 (ray-j ray) (- (ray-y ray) sphere-m))
		 (* 2 (ray-k ray) (- (ray-z ray) sphere-n))))
	   (c (+ (sum-of-squares (center object))
		 (sum-of-squares (origin ray))
		 (* -2 sphere-l (ray-x ray))
		 (* -2 sphere-m (ray-y ray))
		 (* -2 sphere-n (ray-z ray))
		 (- (square radius))))
	   (det (- (square b) (* 4 a c))))
      
    (if (< det 0)
	nil
	(let* ((t1 (/ (+ (- b) (sqrt det)) (* 2 a)))
	       (t2 (/ (- (- b) (sqrt det)) (* 2 a)))
	       (time (min t1 t2))
	       (point (vector-add (origin ray)
				  (vector-mult-scalar (direction ray) time)))
	       (normal (vector-div-scalar (vector-sub (origin ray) (center object))
					  radius)))
	  (make-instance 'ray-intersection
			 :intersect-time time
			 :point point
			 :normal normal
			 :intersect-object object)))))

(defun find-intersection (ray)
  (reduce 
   (lambda (best-int obj) 
     (let ((obj-int (find-intersection-object ray obj)))
       (if (or (null best-int)
	       (and obj-int (< (intersect-time obj-int) (intersect-time best-int))))
	   obj-int
	   best-int))) 
   *scene* :initial-value nil))

(defgeneric calculate-color (material inters))

(defgeneric get-ambient-component (material inters))

(defgeneric get-point-light-component (material inters light))

(defmethod get-ambient-component ((mat material) inters)
  (vector-mult-scalar (color mat) (intensity *ambient-light*)))

(defmethod get-point-light-component ((mat material) inters (light point-light))
  (let* ((light-dir (calc-direction (point inters) (point light)))
	 (normal (normal inters))
	 (intensity (* (dot-product light-dir normal)
		       (intensity light))))
    (mapcar (lambda (comp) (* comp intensity)) (color mat))))

(defmethod calculate-color ((mat material) inters)
  ;; for each light source
  (vector-add (get-ambient-component mat inters)
	      (apply #'vector-add
		     (mapcar (lambda (light) (get-point-light-component mat inters light))
			     *point-lights*)))) 

(defun color-to-256 (comp)
  (round (* 255 (max 0 (min 1 comp)))))

(defun set-pixel (img pt color)
  (setf (aref img (point-y pt) (point-x pt) 0) (color-to-256 (color-red color)))
  (setf (aref img (point-y pt) (point-x pt) 1) (color-to-256 (color-green color)))
  (setf (aref img (point-y pt) (point-x pt) 2) (color-to-256 (color-blue color))))

(defun shoot-ray (ray)
  (let ((inters (find-intersection ray)))
    (if inters
	(calculate-color (material (intersect-object inters)) inters)
	(make-color 0 0 0))))

(defun raytrace (output-pathname)
  (let ((img (png:make-image *image-height* *image-width* 3 8)))
    (dotimes (x *image-width*)
      (dotimes (y *image-height*)	
	(let* ((viewplane-x (- (/ (* x *viewplane-width*) *image-width*) (/ *viewplane-width* 2)))
	       (viewplane-y (- (/ (* y *viewplane-height*) *image-height*) (/ *viewplane-height* 2)))
	       (ray (make-ray-from-points 
		     (make-point 0 0 0) 
		     (make-point viewplane-x viewplane-y (- *viewplane-distance*))))
	       (point-color (shoot-ray ray)))
	  (set-pixel img
		     (make-point x y)
		     point-color))))        
    (with-open-file (output output-pathname :element-type '(unsigned-byte 8)
     			    :direction :output :if-exists :supersede)
      (png:encode img output))))

(raytrace "test.png")

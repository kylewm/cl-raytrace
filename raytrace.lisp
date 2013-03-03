(defpackage :raytrace
  (:use :common-lisp :png))

(in-package :raytrace)

(defparameter *image-width* 400)
(defparameter *image-height* 400)
(defparameter *viewplane-distance* 10)
(defparameter *viewplane-width* 10)
(defparameter *viewplane-height* 10)
(defparameter *point-light-intensity* 0.3)

(defparameter *scene*
  (list
   (make-instance 'sphere :color (make-color 0 0 255) :center (make-point -15 -10 -105) :radius 30)
   (make-instance 'sphere :color (make-color 255 0 0) :center (make-point 15 10 -95) :radius 30)
   ;;(make-instance 'sphere :color (make-color 255 0 0) :center (make-point 10 -5 -75) :radius 10)
   ))

(defparameter *lights*
  (list
   (make-point 0 -20 -50)))

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

(defclass ray ()
  ((origin :initarg :origin
	   :reader origin)
   (direction :initarg :direction
	      :reader direction)))

(defmethod initialize-instance :after ((ray ray) &key)
  (setf (slot-value ray 'direction)
	(normalize (slot-value ray 'direction))))

(defun ray-x (ray) (point-x (origin ray)))
(defun ray-y (ray) (point-y (origin ray)))
(defun ray-z (ray) (point-z (origin ray)))

(defun ray-i (ray) (point-x (direction ray)))
(defun ray-j (ray) (point-y (direction ray)))
(defun ray-k (ray) (point-z (direction ray)))

(defun make-ray-from-points (start end)
  (make-instance 'ray :origin start :direction (mapcar #'- end start)))

(defclass scene-object () 
  ((color
    :initarg :color
    :reader color)))

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
   (intersect-point
    :initarg :intersect-point
    :reader intersect-point)
   (normal
    :initarg :normal
    :reader normal)
   (intersect-object
    :initarg :intersect-object
    :reader intersect-object)))

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
	       (intersect-point (vector-add (origin ray)
					    (vector-mult-scalar (direction ray) time)))
	       (normal (vector-div-scalar (vector-sub (origin ray) (center object))
					  radius)))
	  (make-instance 'ray-intersection
			 :intersect-time time
			 :intersect-point intersect-point
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

(defun set-pixel (img pt color)
  (setf (aref img (point-y pt) (point-x pt) 0) (max 0 (min 255 (round (color-red color)))))
  (setf (aref img (point-y pt) (point-x pt) 1) (max 0 (min 255 (round (color-green color)))))
  (setf (aref img (point-y pt) (point-x pt) 2) (max 0 (min 255 (round (color-blue color))))))

(defun calculate-light-intensity (inters light)
  (let* ((to-light (make-ray-from-points (intersect-point inters)
					 light))
	 (normal (normal inters))
	 (light-dir (direction to-light))
	 (intensity  (* (dot-product light-dir normal)
			*point-light-intensity*))
	 (color (color (intersect-object inters))))
    (mapcar (lambda (comp) (* comp intensity)) color)))

(defun calculate-intensity (inters)
  ;; for each light source
  (apply #'vector-add
	 (mapcar (curry #'calculate-light-intensity inters) *lights*)))

(defun shoot-ray (ray)
  (let ((inters (find-intersection ray)))
    (if inters
	(calculate-intensity inters)
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

(raytrace "test.png")

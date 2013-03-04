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
      :reader color)
     (diffuse-factor
      :initarg :diffuse-factor
      :reader diffuse-factor)
     (specular-factor
      :initarg :specular-factor
      :reader specular-factor)
     (specular-n
      :initarg :specular-n
      :reader specular-n)))

(defparameter *black* (make-color 0 0 0))

(defparameter *white* (make-color 1 1 1))

(defparameter *eye* (make-point 0 0 0))

(defparameter *ambient-light*
  (make-instance 'light :intensity 0.2 :color (make-color 1.0 1.0 1.0)))

(defparameter *point-lights*
  (list
   (make-instance 'point-light :point (make-point -100 -20 0) :intensity 0.8 :color (make-color 1 1 1))
   (make-instance 'point-light :point (make-point 30 -100 0) :intensity 0.5 :color (make-color 1 1 1))))

(defparameter *red-material*
  (make-instance 'material :color (make-color 1 0 0) :diffuse-factor 0.4 :specular-factor 0.6 :specular-n 8))

(defparameter *blue-material*
  (make-instance 'material :color (make-color 0 0 1) :diffuse-factor 0.4 :specular-factor 0.6 :specular-n 8))

(defparameter *green-material*
  (make-instance 'material :color (make-color 0 1 0) :diffuse-factor 0.6 :specular-factor 0.4 :specular-n 8))

(defparameter *scene*
  (list
   (make-instance 'sphere :material *blue-material* :center (make-point -20 20 -100) :radius 20)
   (make-instance 'sphere :material *green-material* :center (make-point 0 -5 -125) :radius 20)
   (make-instance 'sphere :material *red-material* :center (make-point 20 -30 -150) :radius 20)))


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

(defun vector-mult-scalar (vec &rest scalars)
  (mapcar (lambda (x) (apply #'* x scalars)) vec))

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
	       (normal (vector-div-scalar (vector-sub point (center object))
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

       (cond ((null obj-int) best-int)
	     ((< (intersect-time obj-int) 0.01) best-int)
	     ((null best-int) obj-int)
	     ((< (intersect-time obj-int)
		 (intersect-time best-int)) obj-int)
	     (t best-int)))) 
   *scene* :initial-value nil))

(defun in-shadow (light-ray)
  (find-intersection light-ray))

(defgeneric calculate-color (ray material inters))

(defgeneric get-ambient-component (ray material inters))

(defgeneric get-point-light-component (ray material inters light))

(defmethod get-ambient-component (ray (mat material) inters)
  (vector-mult-scalar (color mat) (intensity *ambient-light*)))

(defmethod get-point-light-component (ray (mat material) inters (light point-light))
  (let* ((light-ray (make-ray-from-points (point inters) (point light)))
	 (light-dir (direction light-ray))
	 (view-dir (vector-mult-scalar (direction ray) -1))
	 (half-dir (vector-div-scalar (vector-add light-dir view-dir) 2))
	 (ln-dot-prod (dot-product light-dir (normal inters)))
	 (nh-dot-prod (dot-product (normal inters) half-dir))
	 (diffuse-component (* (diffuse-factor mat) ln-dot-prod))
	 (specular-component (* (specular-factor mat) (expt nh-dot-prod (specular-n mat)))))
    (if (not (in-shadow light-ray))
	(vector-add 
	 (vector-mult-scalar (color mat) (intensity light) diffuse-component)
	 (vector-mult-scalar *white* (intensity light) specular-component))

	*black*)))

(defmethod calculate-color (ray (mat material) inters)
  ;; for each light source
  (vector-add (get-ambient-component ray mat inters)
	      (apply #'vector-add
		     (mapcar (lambda (light) (get-point-light-component ray mat inters light))
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
	(calculate-color ray (material (intersect-object inters)) inters)
	(make-color 0 0 0))))

(defun raytrace (output-pathname)
  (let ((img (png:make-image *image-height* *image-width* 3 8)))
    (dotimes (x *image-width*)
      (dotimes (y *image-height*)	
	(let* ((viewplane-x (- (/ (* x *viewplane-width*) *image-width*) (/ *viewplane-width* 2)))
	       (viewplane-y (- (/ (* y *viewplane-height*) *image-height*) (/ *viewplane-height* 2)))
	       (ray (make-ray-from-points
		     *eye*
		     (make-point viewplane-x viewplane-y (- *viewplane-distance*))))
	       (point-color (shoot-ray ray)))
	  (set-pixel img
		     (make-point x y)
		     point-color))))        
    (with-open-file (output output-pathname :element-type '(unsigned-byte 8)
     			    :direction :output :if-exists :supersede)
      (png:encode img output))))

(raytrace "test.png")

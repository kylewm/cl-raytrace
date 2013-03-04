(in-package :raytrace)

(defparameter *image-width* 400)
(defparameter *image-height* 400)
(defparameter *viewplane-distance* 10)
(defparameter *viewplane-width* 10)
(defparameter *viewplane-height* 10)
(defparameter *black* (make-color 0 0 0))
(defparameter *white* (make-color 1 1 1))
(defparameter *eye* (make-point 0 0 0))

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

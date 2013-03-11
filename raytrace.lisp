(in-package :raytrace)

(defgeneric find-intersections-object (ray object))

(defun make-ray-intersection (ray time point normal object)
  (make-instance 'ray-intersection
		 :intersect-time time
		 :point point
		 :normal normal
		 :intersect-object object
		 :direction (if (< (dot-product (direction ray) normal) 0)
				'entering 
				'leaving)))

(defun make-ray-sphere-intersection (ray time sphere)
  (let* ((point (vector-add (origin ray) (vector-mult-scalar (direction ray) time)))
	 (normal (vector-div-scalar (vector-sub point (center sphere))
				    (radius sphere)))
	 (normal-corrected
	  (if (< (dot-product (direction ray) normal) 0)
	      normal
	      (vector-negate normal))))
    (make-ray-intersection ray time point normal-corrected sphere)))

(defmethod find-intersections-object (ray (object sphere))
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
	(let ((t1 (/ (+ (- b) (sqrt det)) (* 2 a)))
	      (t2 (/ (- (- b) (sqrt det)) (* 2 a))))
	  (list (make-ray-sphere-intersection ray t1 object)
		(make-ray-sphere-intersection ray t2 object))))))

(defun find-intersection (ray)
  (let* ((intersections (flatmap 
			(curry #'find-intersections-object ray)
			*scene*))
	(best-intersection 
	 (reduce 
	  (lambda (best-int obj-int)
	    (cond ((null obj-int) best-int)
		  ;; small epsilon allows us to cull intersections in the
		  ;; opposite direction of the vector
		  ((< (intersect-time obj-int) 0.01) best-int)
		  ((null best-int) obj-int)
		  ((< (intersect-time obj-int)
		      (intersect-time best-int)) obj-int)
		  (t best-int))) 
	  intersections :initial-value nil)))
    best-intersection))

(defun calc-shadow-factor (light-ray)
  (let ((inters (find-intersection light-ray)))
    (if inters
	(- 1 (transparency (material (intersect-object inters))))
	1)))

(defun get-ambient-component (ray mat inters)
  (vector-mult-scalar (color mat) (intensity *ambient-light*)))

(defun get-point-light-component (ray mat inters light)
  (let* ((light-ray (make-ray-from-points (point inters) (point light)))
	 (light-dir (direction light-ray))
	 (view-dir (vector-mult-scalar (direction ray) -1))
	 (half-dir (normalize (vector-add light-dir view-dir)))
	 (ln-dot-prod (dot-product light-dir (normal inters)))
	 (nh-dot-prod (dot-product (normal inters) half-dir))
	 (diffuse-component (* (diffuse-factor mat) ln-dot-prod))
	 (specular-component (* (specular-factor mat) (expt nh-dot-prod (specular-n mat))))
	 (shadow-factor (calc-shadow-factor light-ray)))
    (vector-mult-scalar 
     (vector-add 
      (vector-mult-scalar (color mat) (intensity light) diffuse-component)
      (vector-mult-scalar *white* (intensity light) specular-component))
     shadow-factor)))

(defun calc-color (ray mat inters)
  (let ((ambient-component (get-ambient-component ray mat inters))
	;; for each light source
	(local-components 
	 (mapcar (curry #'get-point-light-component ray mat inters) *point-lights*)))
    (vector-mult-scalar 
     (reduce #'vector-add (cons ambient-component local-components))
     (attenuation ray)))) 

(defun set-pixel (img pt color)
  (setf (aref img (point-y pt) (point-x pt) 0) (color-to-256 (color-red color)))
  (setf (aref img (point-y pt) (point-x pt) 1) (color-to-256 (color-green color)))
  (setf (aref img (point-y pt) (point-x pt) 2) (color-to-256 (color-blue color))))

(defun reflect-ray (ray mat inters)
  (let* ((view-dir (look-direction ray))
	 (normal-dir (normal inters))
	 (nl-dot-prod (dot-product normal-dir view-dir))
	 (reflect-dir 
	  (vector-sub (vector-mult-scalar normal-dir (* 2 nl-dot-prod))
		      view-dir)))
    (make-instance
     'ray
     :origin (point inters)
     :direction reflect-dir
     :attenuation (* (attenuation ray) (reflectivity mat)))))

(defun refract-ray (ray mat inters)
  (let* ((mu1 (refraction-index ray))
	 (mu2 (if (eq (direction inters) 'entering) (refraction-index mat) 1.0))
	 (mu (/ mu1 mu2))
	 (c1 (- (dot-product (normal inters) (direction ray))))
	 (c2 (sqrt (- 1 (* mu mu (- 1 (* c1 c1))))))
	 (refract-dir (vector-add
		       (vector-mult-scalar (direction ray) mu)
		       (vector-mult-scalar (normal inters) (- (* mu c1) c2)))))
    (make-instance
     'ray
     :origin (point inters)
     :direction refract-dir
     :attenuation (* (attenuation ray) (transparency mat)))))

(defun shoot-ray (ray depth)
  (if (>= depth 0)
      (let ((inters (find-intersection ray)))
	(if inters
	    (let* ((mat (material (intersect-object inters)))
		   (local (calc-color ray mat inters))
		   (global-reflected
		    (if (> (reflectivity mat) 0)
			(let ((reflected-ray (reflect-ray ray mat inters)))
			  (if reflected-ray 
			      (shoot-ray reflected-ray (- depth 1))
			      *black*))
			*black*))
		   (global-refracted
		    (if (> (transparency mat) 0)
		   	(let ((refracted-ray (refract-ray ray mat inters)))
		   	  (if refracted-ray
		   	      (shoot-ray refracted-ray (- depth 1))
		   	      *black*))
		   	*black*)))
	      (vector-add local global-reflected global-refracted))
	    *black*))
      *black*))


(defun sample (view-point)
  (let* ((dir (calc-direction *eye* view-point) )
	 (ray (make-instance 'ray :origin *eye* :direction dir :attenuation 1.0)))
    (shoot-ray ray 3)))

(defun supersample (view-points)
  (vector-avg
   (mapcar #'sample view-points)))

(defun supersample-points (x y z px-width px-height)
  (let ((w (/ px-width 2))
	(h (/ px-height 2)))
    (list (make-point (- x w) (- y h) z)
	  (make-point (- x w) y       z)
	  (make-point (- x w) (+ y h) z)
	  (make-point x       (- y h) z)
	  (make-point x       y       z)
	  (make-point x       (+ y h) z)
	  (make-point (+ x w) (- y h) z)
	  (make-point (+ x w) y       z)
	  (make-point (+ x w) (+ y h) z))))

(defun raytrace (output-pathname)
  (let ((img (png:make-image *image-height* *image-width* 3 8))
	(pixel-width (/ *viewplane-width* *image-width*))
	(pixel-height (/ *viewplane-height* *image-height*)))
    (dotimes (x *image-width*)
      (dotimes (y *image-height*)
	(let* ((viewplane-x (- (/ (* x *viewplane-width*) *image-width*) (/ *viewplane-width* 2)))
	       (viewplane-y (- (/ (* y *viewplane-height*) *image-height*) (/ *viewplane-height* 2)))
	       (point-color (if *use-supersample*
				(supersample (supersample-points viewplane-x viewplane-y
								 (- *viewplane-distance*)
								 pixel-width pixel-height))
				(sample (make-point viewplane-x viewplane-y
						    (- *viewplane-distance*))))))
	  (set-pixel img
		     (make-point x y)
		     point-color))))        
    (with-open-file (output output-pathname :element-type '(unsigned-byte 8)
     			    :direction :output :if-exists :supersede)
      (png:encode img output))))

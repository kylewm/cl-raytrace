(in-package :raytrace)

(defparameter *image-width* 400)
(defparameter *image-height* 400)
(defparameter *viewplane-distance* 10)
(defparameter *viewplane-width* 10)
(defparameter *viewplane-height* 10)
(defparameter *black* (make-color 0 0 0))
(defparameter *white* (make-color 1 1 1))
(defparameter *eye* (make-point 0 0 0))
(defparameter *use-supersample* t)

(defparameter *ambient-light*
  (make-instance 'light
		 :intensity 0.2
		 :color (make-color 1.0 1.0 1.0)))

(defparameter *point-lights*
  (list
   (make-instance 'point-light 
		  :point (make-point -200 -50 0) 
		  :intensity 0.8
		  :color (make-color 1 1 1))
   (make-instance 'point-light 
		  :point (make-point 0 -100 0) 
		  :intensity 0.5
		  :color (make-color 1 1 1))))

(defparameter *red-material*
  (make-instance 'material 
		 :color (make-color 0.8 0.2 0.1)
		 :diffuse-factor 0.8
		 :specular-factor 0.5
		 :specular-n 32
		 :reflectivity 0.3))

(defparameter *purple-material*
  (make-instance 'material 
		 :color (make-color 0.8 0.1 0.8)
		 :diffuse-factor 0.8
		 :specular-factor 0.5
		 :specular-n 32
		 :reflectivity 0.3))

(defparameter *blue-material*
  (make-instance 'material
		 :color (make-color 0.1 0.3 0.4)
		 :diffuse-factor 0.1
		 :specular-factor 0.8
		 :specular-n 32
		 :reflectivity 0.2
		 :transparency 0.8
		 :refraction-index 1.2))

(defparameter *green-material*
  (make-instance 'material
		 :color (make-color 0.2 0.8 0.1)
		 :diffuse-factor 0.6
		 :specular-factor 0.8
		 :specular-n 32
		 :reflectivity 0.2))

(defparameter *teal-material*
  (make-instance 'material
		 :color (make-color 0.1 0.8 0.8)
		 :diffuse-factor 0.6
		 :specular-factor 0.8
		 :specular-n 32
		 :reflectivity 0.2))

(defparameter *orange-material*
  (make-instance 'material
		 :color (make-color 0.9 0.4 0.1)
		 :diffuse-factor 0.8
		 :specular-factor 0.8
		 :specular-n 32
		 :reflectivity 0.2))

(defparameter *scene*
  (list
   (make-instance 'sphere
		  :material *blue-material*
		  :center (make-point 0 0 -80)
		  :radius 15)
   (make-instance 'sphere
		  :material *red-material*
		  :center (make-point -25 -8 -125)
		  :radius 15)
   (make-instance 'sphere
		  :material *orange-material*
		  :center (make-point 0 -18 -100)
		  :radius 15)
   (make-instance 'sphere
		  :material *green-material*
		  :center (make-point -15 20 -110)
		  :radius 15)
   (make-instance 'sphere
		  :material *teal-material*
		  :center (make-point 10 20 -100)
		  :radius 15)
   (make-instance 'sphere
		  :material *purple-material*
		  :center (make-point 25 -5 -125)
		  :radius 15)))

(defun main ()
  (raytrace "test.png"))
;;(sb-ext:run-program "/usr/bin/ristretto" (list "test.png"))

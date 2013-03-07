(in-package :raytrace)

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

(defparameter *blue-material*
  (make-instance 'material
		 :color (make-color 0.1 0.2 0.8)
		 :diffuse-factor 0
		 :specular-factor 0
		 :specular-n 32
		 :reflectivity 0
		 :transparency 1
		 :refraction-index 1.0))

(defparameter *green-material*
  (make-instance 'material
		 :color (make-color 0.2 0.8 0.1)
		 :diffuse-factor 0.6
		 :specular-factor 0.8
		 :specular-n 32
		 :reflectivity 0.2))

(defparameter *scene*
  (list
   (make-instance 'sphere
		  :material *blue-material*
		  :center (make-point -20 0 -100)
		  :radius 20)
   (make-instance 'sphere
		  :material *green-material*
		  :center (make-point 0 -20 -125)
		  :radius 20)
   (make-instance 'sphere
		  :material *red-material*
		  :center (make-point 20 10 -100)
		  :radius 20)))

(defun main ()
  (raytrace "test.png"))
;;(sb-ext:run-program "/usr/bin/ristretto" (list "test.png"))


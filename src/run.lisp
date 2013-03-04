(in-package :raytrace)

(defparameter *ambient-light*
  (make-instance 'light
		 :intensity 0.2
		 :color (make-color 1.0 1.0 1.0)))

(defparameter *point-lights*
  (list
   (make-instance 'point-light 
		  :point (make-point -100 -20 0) 
		  :intensity 0.8 
		  :color (make-color 1 1 1))
   (make-instance 'point-light 
		  :point (make-point 30 -100 0) 
		  :intensity 0.5 
		  :color (make-color 1 1 1))))

(defparameter *red-material*
  (make-instance 'material 
		 :color (make-color 1 0 0)
		 :diffuse-factor 0.4
		 :specular-factor 0.6
		 :specular-n 8))

(defparameter *blue-material*
  (make-instance 'material
		 :color (make-color 0 0 1)
		 :diffuse-factor 0.4
		 :specular-factor 0.6
		 :specular-n 8))

(defparameter *green-material*
  (make-instance 'material
		 :color (make-color 0 1 0)
		 :diffuse-factor 0.6
		 :specular-factor 0.4
		 :specular-n 8))

(defparameter *scene*
  (list
   (make-instance 'sphere
		  :material *blue-material*
		  :center (make-point -20 20 -100)
		  :radius 20)
   (make-instance 'sphere
		  :material *green-material*
		  :center (make-point 0 -5 -125)
		  :radius 20)
   (make-instance 'sphere
		  :material *red-material*
		  :center (make-point 20 -30 -150)
		  :radius 20)))

(raytrace "test.png")

(in-package :raytrace)

(defclass ray ()
  ((origin
    :initarg :origin
    :reader origin)
   (direction
    :initarg :direction
    :reader direction)
   (attenuation
    :initarg :attenuation
    :reader attenuation)
   (refraction-index
    :initarg :refraction-index
    :initform 1.0
    :reader refraction-index)))

(defun make-ray-from-points (start end)
  (make-instance 'ray :origin start :direction (calc-direction start end)))

(defun ray-x (ray) (point-x (origin ray)))
(defun ray-y (ray) (point-y (origin ray)))
(defun ray-z (ray) (point-z (origin ray)))

(defun ray-i (ray) (point-x (direction ray)))
(defun ray-j (ray) (point-y (direction ray)))
(defun ray-k (ray) (point-z (direction ray)))

(defmethod look-direction ((ray ray))
  (vector-negate (direction ray)))

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
    :reader intersect-object)
   (direction
    :initarg :direction
    :reader direction
    :initform 'entering)))

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
    :reader specular-n)
   (reflectivity
    :initarg :reflectivity
    :reader reflectivity)
   (transparency
    :initarg :transparency
    :initform 0.0
    :reader transparency)
   (refraction-index
    :initarg :refraction-index
    :initform 1.0
    :reader refraction-index)))

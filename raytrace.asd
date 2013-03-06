
(asdf:defsystem :raytrace
  :name "Raytracing Experiment"
  :author "Kyle Mahan"
  :description "Simple raytracer for learning Common Lisp"
  :depends-on (:png)
  :components ((:file "packages")
	       (:file "util" :depends-on ("packages"))
	       (:file "classes" :depends-on ("packages"))
	       (:file "raytrace" :depends-on ("util" "classes"))
	       (:file "run" :depends-on ("raytrace"))))

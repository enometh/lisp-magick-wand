;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Thu Feb 12 16:25:44 2015 +0530 <enometh@meer.net>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2025 Madhu.  All Rights Reserved.
;;;
;;; (SHRII CENTER RADIUS) a new construction of the classical shri
;;; yantra parameterised on a single angle (19.34 degrees), devised in
;;; feb-march 2014, which produces the coordinates of the triangles
;;;

(defpackage "MANDALA-MAGICK"
  (:use "CL" "SHRII")
  (:shadow "CONJUGATE" "INTERSECTION"))
(in-package "MANDALA-MAGICK")

(defmacro dbg (&rest syms)
  "\(DEBUG [\"DEBUG-MSG\"] [ITEMS...] \)
prints DEBUG-MSG: [ITEM=VAL ...] on standard output"
  (let ((leader (if (stringp (car syms)) (pop syms) "DEBUG")))
    (let ((format-string (format nil "~A: ~{~A=~~S~^, ~}~~&" leader syms)))
      `(format t ,format-string ,@syms))))

(eval-when (:load-toplevel :execute :compile-toplevel)

(defvar *wand* nil)
(defvar *filename* "x:")

(defvar *height* 400)
(defvar *width* 400)
(defvar *ndiv* 48)
(defvar *ndiv* 10)
(defvar *center* nil)
(defvar *length* nil)
(defvar *adiv* nil)
)

(defvar +dw-bindables+ '(height width center-x center-y ndiv))

(defmacro with-dims ((&key (height '*height*)
			   (width '*width*)
			   (ndiv '*ndiv*)
			   &allow-other-keys)
		     &body body)
  `(let* ((*height* ,height)
	  (*width* ,width)
	  (center-x (truncate *width* 2))
	  (center-y (truncate *height* 2))
	  (*center* (complex center-x center-y))
	  (*length* (min *width* *height*))
	  (*ndiv* ,ndiv)
	  (*adiv* (/ *length*  *ndiv*)))
     ,@body))

(defmacro with-cloned-dw ((dw orig-dw) &body body)
  ;; top level macro version, writes to global *wand*. another
  ;; macrolet is available within the body of with-wand
  (check-type dw symbol)
  `(magick:with-cloned-drawing-wand (,dw ,orig-dw)
     (multiple-value-prog1 (progn ,@body)
       (magick:draw-image *wand* ,dw))))

;; default canvas: background lightblue, fill white, stroke black
(defmacro with-wand ((&rest args &key
			    (height '*height*)
			    (wand-var '*wand*)
			    (width '*width*)
			    (wand-args '(:string "lightblue"))
			    (filename '*filename*)
			    dry-run
			    &allow-other-keys)
		     &body body)
  `(macrolet ((with-dw ((dw-var) &body body)
		`(magick:with-drawing-wand (,dw-var)
		   (magick:with-pixel-wand (pw :string "white")
		     (magick:pixel-set-alpha pw 0.0)
		     (magick:draw-set-fill-color ,dw-var pw))
		   (magick:with-pixel-wand (pw :string "black")
		     (magick:draw-set-stroke-color ,dw-var pw))
		   (multiple-value-prog1 (progn ,@body)
		     (magick:draw-image ,',wand-var ,dw-var))))
	      (with-cloned-dw ((dw-var orig-dw) &body body)
		`(magick:with-cloned-drawing-wand (,dw-var ,orig-dw)
		   (multiple-value-prog1 (progn ,@body)
		     (magick:draw-image ,',wand-var ,dw-var)))))
     (with-dims (,@args)
       (magick:with-magick-wand (,wand-var :create ,width ,height ,@wand-args)
	 (multiple-value-prog1 (progn ,@body)
	   (unless ,dry-run
	     (magick:write-image ,wand-var ,filename)))))))


;;; ----------------------------------------------------------------------
;;;
;;; transforms from normalized device coordinates
;;;

(defun get-wand-ndc-transform-ctx ()
  (get-transform-ctx  (new-point 0 0)
		      (new-point -1 1) (new-point 1 1)
		      *center*
		      (new-point 0 0)
		      (new-point *width* 0)))

#+nil
(with-wand (:dry-run t)
  (get-wand-ndc-transform-ctx))

(defun get-wand-transform-ctx ()
  (get-transform-ctx *center*
		     (new-point (- (x *center*) (/ *width* 2)) (+ (y *center*) (/ *height* 2)))
		     (new-point (+ (x *center*) (/ *width* 2)) (+ (y *center*) (/ *height* 2)))
		     *center*
		     (new-point 0 0)
		     (new-point *width* 0)))
#+nil
(with-wand (:dry-run t)
  (get-wand-transform-ctx))

(defmacro with-wand-transformed-coords ((var-list &key (ctx '(get-wand-transform-ctx))) &body body)
  (labels ((make-1let (var new-var)
	     `((,new-var (transformp ,var))
	       (,var ,new-var)))
	   (make-lets (vars new-vars body)
	     `(let* ,(loop for v in vars for n in new-vars
			   append (make-1let v n))
		,@body)))
    (let ((new-vars (mapcar (lambda (var) (gensym (string var))) var-list)))
      `(with-transform-ctx ,ctx
	 ,(make-lets var-list new-vars body)))))

#+nil
(with-wand (:dry-run t)
  (let ((p1 #C(0 0)))
    (with-wand-transformed-coords ((p1))
      p1)))


;;; ----------------------------------------------------------------------
;;;
;;;
;;;

(defun draw-circle (dw center radius0)
  (let* ((center-x (x center))
	 (center-y (y center))
	 (px (+ center-x (* radius0 (sin (/ +pi+ 4)))))
	 (py (+ center-y (* radius0 (cos (/ +pi+ 4))))))
    (magick:draw-circle dw center-x center-y px py)))

#+nil
(with-wand (:wand-var *wand* :filename "x:" :height 200 :width 200 :ndiv 10)
  (with-dw (dw)
    (draw-circle dw *center* (/ *length* 2))
    (with-cloned-dw (dw dw)
      (magick:with-pixel-wand (pw :string "white")
	(magick:pixel-set-alpha pw 1.0)
	(magick:draw-set-fill-color dw pw))
      (magick:with-pixel-wand (pw :string "red")
	(magick:draw-set-stroke-color dw pw))
      (magick:draw-set-stroke-width dw 1d0)
      (draw-circle dw *center* (* *adiv* 3)))
    (with-cloned-dw (dw1 dw)
      (magick:with-pixel-wand (pw :string "green")
	(magick:draw-set-fill-color dw pw))
      (magick:with-pixel-wand (pw :string "blue")
	(magick:draw-set-stroke-color dw pw))
      (draw-circle dw *center* (* *adiv* 2)))))

#+nil
(defun draw-line-except-axes (dw u v x y)
  (let ((x0 (x *center*)) (y0 (y *center*)))
    (unless (or (and (= u x0) (x y0))
		(and (= v x0) (= y y0)))
      (magick:draw-line dw u v x y))))

#+nil
(defun draw-connect-points (dw points)
  (loop for (u v) = (car points) then (list x y)
	for (x y) in (cdr points)
	for i from 0
	do (magick:draw-line dw u v x y)
	finally (magick:draw-line dw x y (car (car points)) (cadr (car points)))))

(defun c->l (list-of-complex-numbers &rest rest)
  "accept a list of points in a variety of formats and convert to a
canonical list of the form ((x y)...). input points may in the
following forms: (x . y) #C(x y) #(x y).
a single number x is interepreted as the point (x 0)."
  (flet ((proc (c)
	   (etypecase c
	      (number (list (x c) (y c)))
	      (vector (assert (= (length c) 2))
		      (coerce c 'list))
	      (cons
		   (if (and (cdr c) (atom (cdr c)))
		       (list (car c) (cdr c))
		       c)))))
    (if (atom list-of-complex-numbers)
	(if rest
	    (c->l (cons list-of-complex-numbers rest))
	    (proc list-of-complex-numbers))
	(mapcar #'proc list-of-complex-numbers))))

#||
(equal (c->l '(1 2)) '((1 0) (2 0)))
(equal (c->l '((1 2) (2 3))) '((1 2) (2 3)))
(equal (c->l #C(0 0)) '(0 0))
(equal (c->l 1) '(1 0))
(equal (c->l '(#C(0 0) (2 3) (1 . 3) 4)) '((0 0) (2 3) (1 3) (4 0)))
||#

(defun draw-connect-points (dw points &key (close t))
  (magick:draw-path-start dw)
  (setq points (c->l points))
  (destructuring-bind (u  v) (car points)
    (magick:draw-path-move-to-absolute dw u v)
    (loop for (x y) in (cdr points)
	  do (magick:draw-path-line-to-absolute dw x y))
    (when close (magick:draw-path-close dw))
    (magick:draw-path-finish dw)))

(defun draw-triangle (dw p1 p2 p3)
  (draw-connect-points dw
		       (loop for p in (list p1 p2 p3)
			     collect (list (x p) (y p)))))

(defun draw-shrii-ctx-trikonas (dw shrii-ctx)
  (loop for (a b c) in (retrieve-9-trikonas shrii-ctx)
	do (draw-triangle dw a b c)))

(defun draw-shrii-ctx-cakra-list-1 (dw ctx list)
  ;; list is a list of points that outline right hand side only the
  ;; particular cakra
  (with-board (:center (slot-value ctx 'center))
    (loop for (p0 p1 p2) = list then x  for x on (cddr list) by #'cddr
	  for (q0 q1 q2) =
	  (mapcar (lambda (p)
		    (complex (- (* 2 (x (slot-value ctx 'center)))
				(x p))
			     (y p)))
		  (list p0 p1 p2))
	  do
	  (draw-connect-points dw (list p0 p1 p2))
	  (draw-connect-points dw (list q0 q1 q2)))))

(defun draw-shrii-ctx-cakras (magick-wand shrii-ctx)
  (magick:with-cloned-magick-wands ((blue magick-wand)
				    (red magick-wand))
    (magick:set-size blue (magick:get-image-width magick-wand) (magick:get-image-height magick-wand))
    (magick:set-size red (magick:get-image-width magick-wand) (magick:get-image-height magick-wand))
    (magick:read-image red "radial-gradient:black-gray")
    (magick:read-image blue "radial-gradient:black-gray")
    (magick:gradient-compose-stops red (make-nstops 10 0))
    (magick:gradient-compose-stops blue (make-nstops 10 240))
    (loop for list in (retrieve-5-cakras shrii-ctx)
	  for col in (list red blue red blue red)
	  do
	  (magick:with-gradient-composition
	      (dw :magick-wand magick-wand :fill-gradient col :stroke-gradient col)
	    (draw-shrii-ctx-cakra-list-1 dw shrii-ctx list)))))

(defun draw-shrii (magick-wand center radius &key transform-ctx)
  (let ((ctx (call-solver #'solve-shrii center radius :transform-ctx transform-ctx)))
    #+nil
    (with-dw (dw) (draw-shrii-ctx-trikonas dw ctx))
    (draw-shrii-ctx-cakras magick-wand ctx)))

(defun draw-petal (dw radius1 radius2 alpha phi center)
  (assert (< radius1 radius2))
  (let* ((phi/2 (/ phi 2))
	 (angle1 (- alpha phi/2))
	 (angle2 (+ alpha phi/2))
	 (p0 (point radius1 angle1 center))
	 (p1 (point radius1 angle2 center))
	 (rx (- radius2 (* radius1 (cos phi/2))))
	 (ry (* radius1 (sin phi/2))))
    (with-wand-transformed-coords ((p0 p1))
      (magick:draw-path-start dw)
      (magick:draw-path-move-to-absolute dw (x p0) (y p0))
      (magick:draw-path-elliptic-arc-absolute
       dw rx ry (- 180 (radians-to-degrees alpha :clamp-180 t)) nil nil (x p1) (y p1))
      (magick:draw-path-elliptic-arc-absolute
       dw radius1 radius1 (radians-to-degrees alpha :clamp-180 t) nil t (x p0) (y p0))
      (magick:draw-path-close dw)
      (magick:draw-path-finish dw))))

(defun draw-ndala-petal (magick-wand npetal radius1 radius2 center)
  (magick:with-cloned-magick-wands ((red magick-wand))
    (magick:set-size red (magick:get-image-width magick-wand) (magick:get-image-height magick-wand))
    (magick:read-image red "radial-gradient:black-gray")
    (magick:gradient-compose-stops red (make-nstops 10 0))
    (loop with phi = (/ (* +pi+ 2 ) npetal)
	  for i below (/ npetal 1)
	  for angle = (* i phi)
	  do (magick:with-gradient-composition (dw :magick-wand magick-wand
						   :fill-gradient red
						   :stroke-gradient red)
	       (draw-petal dw radius1 radius2 angle phi center)))))

(defun draw-quadrangle (dw center h gap)
  "h from center"
  (let ((x0 (- (x center) (/ h 2)))
	(y0 (- (y center) (/ h 2)))
	(x1 (+ (x center) (/ h 2)))
	(y1 (- (y center) (/ h 2)))
	(x2 (+ (x center) (/ h 2)))
	(y2 (+ (y center) (/ h 2)))
	(x3 (- (x center) (/ h 2)))
	(y3 (+ (y center) (/ h 2)))
	(l (/ (- h gap) 2)))
    (magick:draw-line dw x0 y0 (+ x0 l) y0)
    (magick:draw-line dw (+ x0 l gap) y0 x1 y1)
    (magick:draw-line dw x1 y1 x1 (+ y1 l))
    (magick:draw-line dw x1 (+ y1 l gap) x2 y2)
    (magick:draw-line dw x2 y2 (- x2 l) y2)
    (magick:draw-line dw (- x2 l gap) y2 x3 y3)
    (magick:draw-line dw x3 y3 x3 (- y3 l))
    (magick:draw-line dw x3 (- y3 l gap) x0 y0)))


#+nil
(with-wand ()
  (with-dw (dw)
      (draw-quadrangle dw *center* 300 20)))


;;;
;;; Sastri & Ayyangar, Saundarya Lahari, Theosophical Publishing
;;; House, Adyar, 1937. footnote p.3 "in the construction in actuual
;;; practice: the height of the entire sri-cakra is 96 units of which
;;; 48 are taken up by the innermost circle, leaving 24 units at the
;;; top and 24 at the bottom. the 8-petalled and 16-petalled lotuses
;;; will touch the circles cutting the vertical diameter produced
;;; both-ways, at the 11th and 20th unit-distances from the upper and
;;; lower extremities of the diameter. Of the 4 remaining units the
;;; three concentric circles lying beyod the 16-petalled lotus take up
;;; one unit. the three units yet remaining mark the extremities of
;;; the 3 quadrangles forming the outermost boundary of the
;;; Sri-cakra. By marking off forty-three units from either extremity
;;; of the outermost quadrangle,the intervening space of ten units
;;; should be- rubbed off on the four sides of the three quadrangles
;;; forming the Bhu Grha. This will give the four gateways of the
;;; Cakra.

(defun %dgrid (dw nrows ncols)
  (magick:with-cloned-drawing-wand (dw dw)
    (magick:with-pixel-wand (pw :string "lightpink")
      (magick:pixel-set-alpha pw 1.0)
      (magick:draw-set-stroke-color dw pw)
      (magick:draw-set-stroke-width dw 0.5))
  (loop with ydiv = (float (/ *height* nrows))
	and xdiv = (float (/ *width* ncols))
	for i from 0 to nrows do
	(loop for j from 0 to ncols do
	      (magick:draw-line dw (* j xdiv) 0
				 (* j xdiv) *height*))
	(magick:draw-line dw 0 (* i ydiv)
			    *width* (* i ydiv)))
  (magick:draw-image *wand* dw)))

#+nil
(with-wand (:wand-var *wand* :filename "x:" :height 800 :width 800 :ndiv 96)
  (let* ((radius0 (* 48/2 *adiv*))
	 (radius1 (+ radius0 (* 11 *adiv*)))
	 (radius2 (+ radius0 (* 20 *adiv*))))
    (with-dw (dw)
      (%dgrid dw 96 96)
      (draw-circle dw *center* radius0)
      (draw-circle dw *center* radius1)
      (draw-circle dw *center* radius2))
    (draw-ndala-petal *wand* 8 radius0 radius1 *center*)
    (draw-ndala-petal *wand* 16 radius1 radius2 *center*)
    (draw-shrii *wand* (complex center-x center-y) radius0
		  :transform-ctx (get-wand-transform-ctx))
    (with-dw (dw)
      (loop for r from (+ radius2 *adiv*) by (/ *adiv* 3) repeat 3
	    do (draw-circle dw *center* r))
      ;;    (magick:draw-line dw 0 (/ *height* 2) *width* (/ *height* 2))
      ;;    (magick:draw-line dw (/ *width* 2) 0 (/ *width* 2) *height*))
      (magick:draw-line dw 0 center-y *width* center-y)
      (magick:draw-line dw center-x 0 center-x *height*)
      (let ((gap (* 10 *adiv*)))
	(draw-quadrangle dw *center* (* *adiv* (- 96 2)) gap)
	(draw-quadrangle dw *center* (* *adiv* (- 96 3)) gap)
	(draw-quadrangle dw *center* (* *adiv* (- 96 4)) gap)
	(mapcar 'float (list radius0 radius1 radius2)))
      (magick:with-pixel-wand (pw)
	(magick:rotate-image *wand* pw 0))
      )))

#||
cp ~/inbox/images/shri.ps /dev/shm/s.ps
ps2pdf /dev/shm/s.ps
mutool convert -o s.png s.pdf
identify s1.png
identify s2.png&
||#

(defun axes-image (wand &key vert-adj horz-adj rotate)
  (let* ((w (magick:get-image-width wand))
	 (h (magick:get-image-height wand))
	 (cx (/ w 2))
	 (cy (/ h 2))
	 (x (if horz-adj (+ cx horz-adj) cx))
	 (y (if vert-adj (+ cy vert-adj) cy)))
    (magick:with-pixel-wand (background)
      (when rotate (magick:rotate-image wand background rotate)))
    (magick:with-drawing-wand (dw)
      ;; vert
      (magick:draw-line dw 0 y w y)
      ;; horz
      (magick:draw-line dw x 0 x h)
      (magick:draw-image wand dw))))

(defun axes (img &rest args &key vert-adj horz-adj rotate)
  (declare (ignorable vert-adj horz-adj rotate))
  (magick:with-magick-wand (wand :load img)
    (apply #'axes-image wand args)
    (magick:write-image wand "x:")))

(defun annot-point (dw point &optional text)
  (let ((x  (x point)) (y (y point)))
    (magick:draw-annotation dw x y
			    (or text (format nil "~f,~f" x y)))))

#+nil
(with-wand (:wand-var *wand* :filename  ;; "x:"
	    "/dev/shm/label.png"
	    :height 500 :width 500 :ndiv 96
	    :wand-args (:string "black"))
  (let ((radius (/ *length* 2.3))
	(transform-ctx (get-wand-transform-ctx)))
    (with-dw (dw)
      (magick:with-pixel-wand (pw :string "goldenrod")
	(magick:draw-set-stroke-color dw pw))
      (loop for (a b c) in (shrii *center* radius :return-type 'triangles
				  :transform-ctx transform-ctx)
	    do (draw-triangle dw a b c))
      (let* ((plist (shrii *center* radius :return-type 'shrii:plist-points
			   :transform-ctx transform-ctx))
	     (keys (loop for (key nil) on plist by #'cddr collect key)))
	(magick:with-pixel-wand (pw :string "white")
	  (magick:draw-set-stroke-color dw pw))
	(magick:draw-set-font-family dw "Source Sans")
	(magick:draw-set-font-size dw 18)
	(loop for key in keys
	      do (if (find key '(f p a j l g v m d))
		     (with-cloned-dw (dw dw)
		       (magick:draw-set-text-decoration dw :underline)
		       (magick:with-pixel-wand (pw :string "skyblue")
			 (magick:draw-set-stroke-color dw pw))
		       (annot-point dw (getf plist key) (string key)))
		     (annot-point dw (getf plist key) (string key)))))
      (magick:draw-set-font-family dw "Courier New")
      (magick:draw-set-font-size dw 13)
      (let* ((plist (shrii (complex center-x center-y) radius :return-type 'shrii:plist)))
	(loop for (key val) on plist by #'cddr
	      if (search "LINE" (string key))
	      do (destructuring-bind (m c) val
		   (annot-point dw (complex 0 c) (string-downcase key))))))))


;;; ----------------------------------------------------------------------
;;;
;;; generate radial gradients based on hsv
;;;

(defun hsv-to-rgb-comp (h s v)		; dha?
  (declare (type (integer 0 360) h)
	   (type (integer 0 100) s v))
  (let ((h (/ h 60.0)) (s (/ s 100.0)) (v (/ v 100.0)))
    (multiple-value-bind (i f) (floor h)
      (if (evenp i) (setq f (- 1 f)))
      (let ((m (* v (- 1 s))) (n (* v (- 1 (* s f)))))
	(mapcar (lambda (x) (floor (* x 255)))
				      (ecase i
					((0 6) (list v n m))
					(1 (list n v m))
					(2 (list m v n))
					(3 (list m n v))
					(4 (list n m v))
					(5 (list v m n))))))))

(defun hsv-to-rgb (h s v)
  (let ((comp (hsv-to-rgb-comp h s v)))
    ;;(format nil "#~{~2,'0x~}" comp)
    (format nil "rgb(~{~a~^, ~})" comp)))

(defun make-nstops (n h)
  "Make N gradient stops for hue h of varying brightness (value)"
  (assert (< h 360))
  (loop for i from 0 to n
	for percent = (* 100 (/ i n))
	for percent-string = (format nil (if (integerp percent)
					     "~D%"
					     "~3,2F%")
				     percent)
	for s =  100
	for v =  (floor (- 100 percent))
	for color = (hsv-to-rgb h s v)
	collect (magick:make-stop :color color :offset percent-string)))

(defun make-nhues (m &optional (type :uniform) (random-state *random-state*))
  (ecase type
    (:uniform   (loop for i below m collect (floor (* i 360 (/ m)))))
    (:random (loop repeat m collect (random 360 random-state)))))

(defun make-mgradients-nstops (m n &optional (type :uniform))
  (loop for h in (make-nhues m type)
	collect (make-nstops n h)))

#+nil
(let ((nstops 6) (mgradients 18) (type :uniform))
  (with-wand (:dry-run nil :ndiv mgradients)
    (magick:set-size *wand* *width* *height*)
    (let ((gradient-specs (make-mgradients-nstops mgradients nstops)))
      (loop with y = 0 for i from 0 for x = (* i *adiv*)
	    for colour in gradient-specs
	    do (magick:with-cloned-magick-wand (grad *wand*)
		 (magick:read-image grad "radial-gradient:black-gray")
		 (magick:gradient-compose-stops grad colour)
		 (magick:with-gradient-composition
		     (dw :magick-wand *wand* :fill-gradient grad :stroke-gradient grad)
		   (magick:draw-rectangle
		    dw x y (+ x *adiv*) (+ y (/ *length* 2))))))
      (loop with x = 0 for i from 0 for y = (+ (/ *length* 2) (* i *adiv* 1/2))
	    for colour in gradient-specs
	    do (magick:with-cloned-magick-wand (grad *wand*)
		 (magick:read-image grad "radial-gradient:black-gray")
		 (magick:gradient-compose-stops grad colour)
		 (magick:with-gradient-composition
		     (dw :magick-wand *wand* :fill-gradient grad :stroke-gradient grad)
		   (magick:draw-rectangle
		    dw x y (+ x *length*) (+ y *adiv*))))))))

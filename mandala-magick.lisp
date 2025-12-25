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
	   (magick:write-image ,wand-var ,filename))))))

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

(defun draw-connect-points (dw points)
  (magick:draw-path-start dw)
  (destructuring-bind (u  v) (car points)
    (magick:draw-path-move-to-absolute dw u v)
    (loop for (x y) in (cdr points)
	  do (magick:draw-path-line-to-absolute dw x y))
    (magick:draw-path-close dw)
    (magick:draw-path-finish dw)))

(defun draw-triangle (dw p1 p2 p3)
  (draw-connect-points dw
		       (loop for p in (list p1 p2 p3)
			     collect (list (x p) (y p)))))

(defun draw-shrii (dw center radius)
  (magick:with-cloned-drawing-wand (dw dw)
    (magick:with-pixel-wands ((pw-black :string "black")
			      (pw-col1 :string "yellow")
			      (pw-col2 :string "blue")
			      (pw-col3 :string "gray")
			      (pw-col4 :string "cyan")
			      (pw-col5 :string "orange")
			      (pw-col6 :string "red"))

      #+nil
      (loop for (a b c) in (shrii center radius :return-type 'triangles)
	    do (draw-triangle dw a b c))

      ;;#+nil
      (loop for list in (shrii center radius)
	    for col in (list pw-col2 pw-col6 pw-col2 pw-col6 pw-col2)
	    do
	    (magick:draw-set-fill-color dw col)
	    (magick:draw-set-stroke-color dw pw-black)
	    (loop for (p0 p1 p2) = list then x  for x on (cddr list) by #'cddr
		  for (q0 q1 q2) =
		  (mapcar (lambda (p)
			    (complex (- (* 2 (x center)) (x p)) (y p)))
			  (list p0 p1 p2))
		  do
		  (draw-triangle dw p0 p1 p2)
		  (draw-triangle dw q0 q1 q2)))

      (magick:draw-image *wand* dw))))

(defun draw-petal (dw radius1 radius2 alpha phi center)
  (assert (< radius1 radius2))
  (with-board (:center center)
    (let* ((phi/2 (/ phi 2))
	   (angle1 (- alpha phi/2))
	   (angle2 (+ alpha phi/2))
	   (p0 (point radius1 angle1 center))
	   (p1 (point radius1 angle2 center))
	   (rx (- radius2 (* radius1 (cos phi/2))))
	   (ry (* radius1 (sin phi/2))))
      (magick:draw-path-start dw)
      (magick:draw-path-move-to-absolute dw (x p0) (y p0))
      (progn
	(magick:draw-path-elliptic-arc-absolute
	 dw rx ry (- 180 (radians-to-degrees alpha :clamp-180 t )) nil nil (x p1) (y p1))
	(magick:draw-path-elliptic-arc-absolute
	 dw radius1 radius1 (radians-to-degrees alpha :clamp-180 t) nil t (x p0) (y p0)))
      (magick:draw-path-close dw)
      (magick:draw-path-finish dw))))

(defun draw-ndala-petal (dw npetal radius1 radius2 center)
  (with-cloned-dw (dw dw)
    (magick:with-pixel-wand (pw :string "red")
      (magick:draw-set-fill-color dw pw)
      (magick:draw-set-stroke-color dw pw))
    (loop with phi = (/ (* +pi+ 2 ) npetal)
	  for i below (/ npetal 1)
	  for angle = (* i phi)
	  do (draw-petal dw radius1 radius2 angle phi center))))

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
  (with-dw (dw)
    (%dgrid dw 96 96)
    (let* ((radius0 (* 48/2 *adiv*))
	   (radius1 (+ radius0 (* 11 *adiv*)))
	   (radius2 (+ radius0 (* 20 *adiv*))))
      (draw-circle dw *center* radius0)
      (draw-circle dw *center* radius1)
      (draw-circle dw *center* radius2)
      (draw-ndala-petal dw 8 radius0 radius1 *center*)
      (draw-ndala-petal dw 16 radius1 radius2 *center*)
      (draw-shrii dw (complex center-x center-y) radius0)
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

#+nil
(with-wand (:width 703 :height 714)
  (magick:read-image *wand* "/home/madhu/cl/extern/lisp-magick-wand/examples/s2.png")
  (magick:with-pixel-wand (pw)
    (magick:rotate-image *wand* pw 180))
  (with-dw (dw)
    (magick:draw-line dw 0 center-y *width* center-y)
    (magick:draw-line dw center-x 0 center-x *height*)))

;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Thu Feb 12 16:25:44 2015 +0530 <enometh@meer.net>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2025 Madhu.  All Rights Reserved.
;;;
;;; (SHRII CENTER RADIUS) a new construction of the shri yantra
;;; parameterised on a single angle (19.34 degrees), devised in
;;; feb-march 2014, which produces the coordinates of the triangles.
;;;
(defpackage "SHRII"
  (:use "CL")
  (:shadow "CONJUGATE" "INTERSECTION")
  (:export "X" "Y"
   "+PI+"
   "DEGREES-MOD180"
   "RADIANS-MOD-PI"
   "RADIANS-TO-DEGREES"
   "DEGREES-TO-RADIANS"
   "WITH-BOARD"
   #:make-point
   #:new-line
   #:m
   #:c
   #:make-line
   #:conjugate
   #:reflectx
   #:intersection
   #:zpoint
   #:perpendicular
   #:point
   #:pcircle
   "SHRII" "TRIANGLES" "PLIST" "KRAMA"
   "PLIST-POINTS"
   #:A #:B #:C #:D #:E #:F #:G #:H #:I
   #:J #:L #:M #:P #:Q #:R #:V #:X #:Y
   #:s #:k #:o #:z #:w #:u #:n #:t1
   #:line0 #:line3 #:line7 #:line10 #:side1 #:side2 #:line2 #:line8 #:line5 #:line4
   #:side9 #:line6 #:side6 #:side8 #:side7 #:side3 #:line1 #:line9 #:side4 #:side5))
(in-package "SHRII")

(defvar +pi+ (coerce pi 'single-float))

(defun x (p) (realpart p))
(defun y (p) (imagpart p))

(defun degrees-mod180 (degrees)
  "Returns a value which lies in [-180 180]."
  (if (plusp degrees)
      (if (> (setq degrees (mod degrees 360)) 180)
	  (mod degrees -180)
	  degrees)
      (if (< (setq degrees (mod degrees -360)) -180)
	  (mod degrees 180)
	  degrees)))

(defun radians-mod-pi (rad)
  (if (plusp rad)
      (if (> (setq rad (mod rad (* 2 +pi+))) +pi+)
	  (mod rad (- +pi+))
	  rad)
      (if (< (setq rad (mod rad (* -2 +pi+)))  (- +pi+))
	  (mod rad +pi+)
	  rad)))

(defun radians-to-degrees (rad &key (clamp-180 t))
  (if clamp-180
      (degrees-mod180  (* 180 (/ rad +pi+)))
      (mod  (* 180 (/ rad +pi+)) 360)))

(defun degrees-to-radians (deg &key (clamp-pi t))
  (if clamp-pi
      (radians-mod-pi (* 1/180 deg +pi+))
      (mod (* 1/180 deg +pi+) (* 2 +pi+))))

(defvar +dw-board-bindables+ '(make-line new-line m c conjugate reflectx
			      intersection zpoint perpendicular point pcircle))

(defvar *center* #C(0 0))

(defun make-point (x y)
  (complex x y))

(defun new-line (m c)
   (list m c))

(defun point (r theta &optional (center *center*))
  (complex (+ (x center) (* r (cos theta)))
	   (- (y center) (* r (sin theta)))))

(defun m (line) "slope" (car line))
(defun c (line) "y-intercept" (cadr line))

(defun make-line (p1 p2)
  (unless (= (x p1) (x p2)) ; cant represent
    (let ((m (/ (- (y p2) (y p1)) (- (x p2) (x p1)))))
      (let ((c (cond ((= (x p1) 0) (y p1))
		     ((= (x p2) 0) (y p2))
		     (t (- (y p1) (* m (x p1)))))))
	(new-line m c)))))

(defun conjugate (p1 &optional (center *center*))
  (make-point (x p1) (- (* 2 (y center)) (y p1))))

(defun reflecty (p &optional (center *center*))
  (make-point (x p) (- (* 2 (y center)) (y p))))

(defun reflectx (p &optional (center *center*))
  (make-point (- (* 2 (x center)) (x p)) (y p)))

(defun intersection (line1 line2) ; point
  (destructuring-bind (m1 c1) line1
    (destructuring-bind (m2 c2) line2
      (unless (= m1 m2)  ; parallel lines dont intersect
	(let ((x (/ (- c1 c2) (- m2 m1))))
	  (make-point x (+ (* m1 x) c1)))))))

(defun zpoint (line &optional (center *center*) &aux (x (x center)))
  ;; point where `line' intersects "y axis"
  (make-point x (+ (* (m line) x) (c line))))

(defun perpendicular (line &optional (point *center*)) ; => new line
  (new-line #1=(/ (- (m line)))
	    (- (y point)
	       (* (x point) #1#))))

(defun pcircle (line radius &optional (center *center*)) ; => point (on the circle)
  "intersection of LINE (slope yintercept) on circle of RADIUS at CENTER"
  ;; (XQ - CX)^2 + (YQ - CY)^2  = R^2
  ;; (YQ - CY) = M *  (XQ - CX)
  (destructuring-bind (M C) line
    (if (zerop M)
	(let* ((y3 c)
	       (x3 (+ (x center)
		      (sqrt (- (expt radius 2.0)
			       (expt (abs (- Y3
					     (y center)))
				     2.0))))))
	  (make-point x3 y3))
	(let* ((x (+ (x center)
		     (/ radius (sqrt (1+ (* M M))))))
	       (y (+ C (* M x))))
	  (make-point x y)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun %with-board-flet-bindings (syms)
  (loop for sym in syms
	for sym-name = (symbol-name sym)
	for shrii-sym = (find-symbol sym-name "SHRII")
	do (assert (find shrii-sym +dw-board-bindables+))
	do (assert (not (eql sym shrii-sym)))
	collect `(,sym (&rest args) (apply #',shrii-sym args)))))

#+nil
(%with-board-flet-bindings '(cl:intersection cl:conjugate))

(defmacro with-board ((&key (center '*center*) shadow) &body body)
  "evaluate BODY with *CENTER* bound to CENTER.  BODY can use the
functions listed in +DW-BOARD-BINDABLES+.  SHADOW if supplied should
be a list of symbols (which do not belong to the SHRII package)
which (nevertheless) have the same name as a symbol in the
+DW-BOARD-BINDABLES+ list.  These are bound via flet to call the
corresponding function in the SHRII package during the execution of
BODY."
  `(let ((*center* ,center))
     (flet ,(%with-board-flet-bindings shadow)
       ,@body)))

#||
(with-board (:center #(200 200)) nil)
(with-board () nil)
(with-board (:center #(200 200) :shadow (cl:conjugate)) nil)
||#

(defmacro plistify (list-of-symbols)
  (cons 'list (loop for x in list-of-symbols append `(',x ,x))))


;;; MODELING NOTES
;;;
;;; there are 11 lines lines0 to line10. line0 and line10 are tangent
;;; at the top and bottom of the circle. there are nine triangles
;;; ($t1..$t9) these have their bases on (line1..line9). [shrishtthi]
;;; $t1 $t2 $t3 $t4 $t5 face downward $t7 $t8 $t9 face upward. each
;;; triangle is also determined by a side (side1..side9) which is the
;;; side adjacent to (line1..line9).

(defun shrii (center radius &key return-type)
  (check-type return-type (or null (member plist triangles krama plist-points)))
  (with-board (:center center)
    (let ((deg -19.43943)
	  line0 line3 line7 line10 side1 side2 line2 line8 line5 line4
	  side9 line6 side6 side8 side7 side3 line1 line9 side4 side5)

      (let* ((POINT0 ;; +nil(POINT RADIUS (/ +PI+ 2) center)
	      (make-point (x center) (- (y center) radius)))
	     (rad (degrees-to-radians deg))
	     (M (float (tan rad) 1.0))
	     (X (+ (x center)
		   (sqrt (/ (* radius radius)
			    (+ 1 (* M M))))))
	     (Y (+ (y center) (* M (- x (x center)))))
	     Q H G I R B A C F E)

	(SETQ Q (make-point X Y))
	(SETQ LINE0 (new-line 0 (y POINT0)))
	(setq line10 (new-line 0 (y (CONJUGATE POINT0))))
	(setq line3 (new-line 0 (y Q)))
	;; (isoceles '$t3 (zpoint line10) $q)
	(setq side3 (make-line (zpoint line10) Q))

	(setq H (intersection side3 (make-line center (conjugate Q))))
	(setq LINE7 (new-line 0 (y H)))
	;; (isoceles '$t7 (zpoint line0) (pcircle line7))
	(setq side7 (make-line (zpoint line0) (pcircle line7 radius)))

	(setq G (intersection side7
			      (perpendicular (make-line Q (zpoint line0)))))
	(setq line2 (new-line 0 (y G)))
	(setq I (intersection line3 side7))
	(setq R (intersection line2 (make-line Q (zpoint line0))))
	(setq side2 (make-line R I))
	;; (isoceles '$t2 (zpoint side2) $r)
	(setq line9 (new-line 0 (y (zpoint side2))))
	(setq B (intersection line7 side2))
	(setq side9 (make-line B (zpoint line3)))
	;; (isoceles '$t9 (zpoint line3) (intersection line9 side9))
	(setq A (intersection side9 side3))
	(setq line8 (new-line 0 (y A)))

	(setq C (intersection (make-line center H) side9))
	(setq line6 (new-line 0 (y C)))
	(setq side6 (make-line (zpoint line2) (intersection line6 side2))) ;p
	;; (isoceles '$t6 (zpoint line2) (intersection line6 side2))
	(setq side4 (make-line C (zpoint line8)))
	(setq side1 (make-line G (zpoint line6)))

	(setq F (intersection side1 line3))
	(setq side8 (make-line F H))
	(setq line1 (new-line 0 (y (zpoint side8)))) ; v
	;; (isoceles '$t1 (zpoint line6) (intersection side1 line1))
	;; (isoceles '$t8 (zpoint line1) (intersection side8 line8))
	(setq E (intersection side6 side1))
	(setq line4 (new-line 0 (y E)))
	(setq side4 (make-line (zpoint line8) (intersection line4 side8)))
	;; (isoceles ' $t4 (zpoint line8) (intersection line4 side8))
	(setq line5 (new-line 0 (y (intersection side9 side1))))
	(setq side5 (make-line (zpoint line7) (intersection line5 side6)))
	;; (isoceles '$t5  (zpoint line7) (intersection line5 side6))

	(when (eql return-type 'plist)
	  (return-from shrii (plistify
			      (line0 line3 line7 line10 side1 side2 line2 line8 line5 line4
				     side9 line6 side6 side8 side7 side3 line1 line9 side4 side5
				     Q H G I R B A C F E X Y))))

	(let (s j k l o z w d u p v m n t1)
	  (setq t1 (zpoint line0))
	  (setq s (intersection line1 side1))
	  r
	  q
	  (setq j (intersection line7 side7))
	  (setq k (intersection line8 side8))
	  (setq l (intersection line9 side9))
	  (setq o (zpoint line10))
	  (setq z (zpoint line8))
	  a h b
	  (setq w (zpoint line6))
	  (setq d (intersection line5 side9))
	  (setq U (zpoint line3))
	  (setq p (intersection line6 side2))
	  (setq v (zpoint line1))
	  s
	  (setq m (intersection line4 side8))
	  (setq n (intersection line5 side5))

	  (when (eql return-type 'plist-points)
	    (return-from shrii
	      (plistify (#|F A G|# P J L V M D
				     Q H G I R B A C F E X Y
				     T1 O Z W U V S N K ))))

	  (when (eql return-type 'triangles)
	    (return-from shrii (list
				(list t1 j (reflectx j))
				(list v k (reflectx k))
				(list u l (reflectx l))
				(list (zpoint line2) p (reflectx p))
				(list (reflectx s) s w)
				(list (reflectx r) r (zpoint line9))
				(list (reflectx q) q o)
				(list (reflectx m) m z)
				(list (reflectx n) n (zpoint line7)))))
	  (when (eql return-type 'krama)
	    (return-from shrii  nil)))

	(list
	 (list (zpoint line0)  (zpoint line1)
	       (intersection side7 line1)
	       (intersection side1 line1)
	       (intersection side7 line2)
	       (intersection side2 line2)
	       (intersection side7 line3)
	       (intersection side3 line3)
	       (intersection side7 side3)
	       (intersection side7 line7)
	       (intersection side3 line7)
	       (intersection side8 line8)
	       (intersection side3 line8)
	       (intersection side9 line9)
	       (intersection side3 line9)
	       (zpoint line9) (zpoint line10))

	 (list (zpoint line1) (zpoint line2)
	       (intersection side8 line2)
	       (intersection side1 line2)
	       (intersection side8 line3)
	       (intersection side7 line3)
	       (intersection side2 side8)
	       (intersection side3 line7)
	       (intersection side2 line7)
	       (intersection side3 line8)
	       (intersection side2 line8)
	       (zpoint line9) (zpoint line8))

	 (list (zpoint line2) (zpoint line3)
	       (intersection side6 line3)
	       (intersection side8 line3)
	       (intersection side6 line4)
	       (intersection side8 line4)
	       (intersection side6 side4)
	       (intersection side2 line6)
	       (intersection side4 line6)
	       (intersection side2 line7)
	       (intersection side4 line7)
	       (zpoint line8) (zpoint line7))

	 (list (zpoint line3)  (zpoint line4)
	       (intersection side9 line4)
	       (intersection side6 line4)
	       (intersection side9 line5)
	       (intersection side6 line5)
	       (intersection side9 side5)
	       (intersection side4 line6)
	       (intersection side5 line6)
	       (zpoint line7) (zpoint line6))
	 (list (zpoint line5) (intersection side9 line5) (zpoint line6)))))))

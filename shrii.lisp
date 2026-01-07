;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Thu Feb 12 16:25:44 2015 +0530 <enometh@meer.net>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2025 Madhu.  All Rights Reserved.
;;;
;;; (SHRII CENTER RADIUS) a new construction of the shri yantra
;;; parameterised on a single angle (-19.43943 degrees), devised in
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
   #:new-line
   #:new-point
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
   #:side9 #:line6 #:side6 #:side8 #:side7 #:side3 #:line1 #:line9 #:side4 #:side5
   "CENTER" "RADIUS"
   "SHRII-CTX" "MAKE-SHRII-CTX" "WITH-CTX-SLOTS"
   "SOLVE-SHRII" "CALL-SOLVER"
   "*FLOAT-TOLERANCE*" "APPROX=" "VERIFY-SOLVED"
   "RETRIEVE-5-CAKRAS" "RETRIEVE-9-TRIKONAS"
   "CLIP-FRAME" "GET-TRANSFORM-CTX-CLIP-FRAMES" "MAKE-CLIP-FRAME"
   "TRANSFORM-CTX" "GET-TRANSFORM-CTX" "WITH-TRANSFORM-CTX"
   "TRANSFORMP" "TRANSFORML" "TRANSFORM-SHRII-CTX"
))
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

(defun new-point (x y)
  (complex x y))

(defun new-line (m c)
   (list m c))

; if Y axis grows downwards. the point has to be transformed before it
; can be rendered. see WITH-BOARD.
(defun point (r theta &optional (center *center*))
  (new-point (+ (x center) (* r (cos theta)))
	     (+ (y center) (* r (sin theta)))))

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
  (new-point (x p1) (- (* 2 (y center)) (y p1))))

(defun reflecty (p &optional (center *center*))
  (new-point (x p) (- (* 2 (y center)) (y p))))

(defun reflectx (p &optional (center *center*))
  (new-point (- (* 2 (x center)) (x p)) (y p)))

(defun intersection (line1 line2) ; point
  (destructuring-bind (m1 c1) line1
    (destructuring-bind (m2 c2) line2
      (unless (= m1 m2)  ; parallel lines dont intersect
	(let ((x (/ (- c1 c2) (- m2 m1))))
	  (new-point x (+ (* m1 x) c1)))))))

(defun zpoint (line &optional (center *center*) &aux (x (x center)))
  ;; point where `line' intersects "y axis"
  (new-point x (+ (* (m line) x) (c line))))

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
	  (new-point x3 y3))
	(let* ((x (+ (x center)
		     (/ radius (sqrt (1+ (* M M))))))
	       (y (+ C (* M x))))
	  (new-point x y)))))

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
BODY.

The polar representation of (POINT) on a rectangular board of length w
and side h centered at center (cx,cy) implies a coordinate system with
these corners (t:top l:left b:bottom r:right)

tl: cx-w/2,cy+h/2           cx,cy+h/2    tr: cx+w/2,cy+h/2
    cx-w/2,cy            c: cx,cy            cx+w/2,cy
bl: cx-w/2,cy-h/2           cx,cy-h/2    br: cx+w/2,cy-h/2

To render these points on, say, a canvas with corners tl: (0,0)
tr: (0,w) bl: (0,h) br: (w,h) centered at c, one would have to use a
transform-ctx to transform the points. in this case (sx sy tx ty)
== (1 -1 0 0). e.g.

(let ((w 600) (h 400) (c #C(300 200)))
  (with-board (:center c)
    (get-transform-ctx c	   ; center of board coordinate system
		       (new-point (- (x c) (/ w 2)) (+ (y c) (/ h 2))) ;tl
		       (new-point (+ (x c) (/ w 2)) (+ (y c) (/ h 2))) ;tr
		       c   ; center of final display coordinate system
		       (new-point 0 0)	;tl
		       (new-point w 0)	;tr
		       ))) ; (1 -1 0 0)

(let ((p (new-point 100 250)))
  (with-transform-ctx '(-1 -1 0 0)
    (transformp p))) ;#C(-100 -250) to plot on the canvas
"
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

(eval-when (:compile-toplevel :load-toplevel :execute)
(defvar +shrii-params+
  '(line0 line3 line7 line10 side1 side2 line2 line8 line5 line4
    side9 line6 side6 side8 side7 side3 line1 line9 side4 side5
    Q H G I R B A C F E
    s j k l o z w d u p v m n t1)))

(defmacro defshriictx ()
  `(defstruct (shrii-ctx (:predicate shrii-ctxp))
     (center #C(0.0 0.0))
     (radius 1.0)
     transform-ctx ;;(transform-ctx '(1 1 0 0))
     ,@+shrii-params+))
(defshriictx)

#||
(defvar *shrii-ctx* nil)
(defmacro defshriictxaccessors (&optional (var '*shrii-ctx*))
  `(progn
     ,@(loop for slot-name in +shrii-params+
	     collect `(define-symbol-macro ,slot-name (slot-value ,var ',slot-name)))))
(defshriictxaccessors)
||#

(defmacro with-ctx-slots (ctx &body body)
  `(with-slots ,(loop for slot-name in `(center radius ,@+shrii-params+)
		      collect slot-name)
       ,ctx
     (declare (ignorable center radius ,@+shrii-params+))
     ,@body))



#||
(setq $s1 (make-shrii-ctx))
(setf (slot-value $s1 'line0) 10)
(with-ctx-slots $s1 line0)
||#

;;;
;;; transformations
;;;

(defun get-transform-ctx (c1 tl1 tr1 c2 tl2 tr2)
  "Return a transform-ctx of the form (sx sy tx ty) which will transform
from a frame specified by points center c1 top-left tl1 top-right tr1,
to a frame specified similarly by points c2 tl2 tr2.
e.g: ndc:
    -1,1           1,1
            0,0
    -1,-1          1,-1
port:
    0,0            w,0
          w/2,h/2
    0,h            w,h
(get-transform-ctx #C(0 0) #C(-1 1) #C(1 1)
                   #C(200 200) #C(0 0) #C(400 0)) ;=> (200 -200 200 200)
"
  (assert (= (y tl1) (y tr1)))
  (assert (= (y tl2) (y tr2)))
  (let* ((w1 (- (x tr1) (x tl1)))
	 (h1 (* 2 (- (y c1) (y tl1))))
	 (w2 (- (x tr2) (x tl2)))
	 (h2 (* 2 (- (y c2) (y tl2))))
	 (sx (/ w2 w1))
	 (sy (/ h2 h1))
	 (tx (- (x c2) (* (x c1) sx)))
	 (ty (- (y c2) (* (y c1) sy))))
    (list sx sy tx ty)))

;; instead of computing the center, specify 3 of 4 corners of the two
;; rectangles
(defstruct (clip-frame (:type list)) tl bl tr)

(defun get-transform-ctx-clip-frames (old-clipframe new-clipframe)
  (destructuring-bind (tl1 bl1 tr1) old-clipframe
    (let ((w1 (- (x tr1) (x tl1)))
	  (h1 (- (y bl1) (y tl1))))
      (destructuring-bind (tl2 bl2 tr2) new-clipframe
	(let ((w2 (- (x tr2) (x tl2)))
	      (h2 (- (y bl2) (y tl2))))
	  (let ((c1 (new-point (+ (x tl1) (/ w1 2))
			       (+ (y tl1) (/ h1 2))))
		(c2 (new-point (+ (x tl2) (/ w2 2))
			       (+ (y tl2) (/ h2 2)))))
	    (get-transform-ctx c1 tl1 tr1 c2 tl2 tr2)))))))

(defmacro with-transform-ctx (transform-ctx &body body)
  "A transform context is a list (sx sy tx ty) that specifies the
horizontal and vertical scaling and translation factors respectively."
  `(destructuring-bind (sx sy tx ty) ,transform-ctx
     (labels ((translatep (p)
		(new-point (+ tx (x p)) (+ ty (y p))))
	      (scalep (p)
		(new-point (* sx (x p)) (* sy (y p))))
	      (transformp (p)
		(translatep (scalep p)))
	      (transforml (l)
		(destructuring-bind (m c) l ;; y-intercept = m * 0 + c
		  (let ((d (transformp (new-point 0 c))))
		    (if (zerop m)
			(new-line m (y d))
			;; 0 = x-intercept * m + c
			(let ((e (transformp (new-point (/ (- c) m) 0))))
			  (make-line d e)))))))
       ,@body)))

(defun transform-shrii-ctx (ctx transform-ctx &optional
			    (ret (copy-shrii-ctx ctx)))
  "Transform slots of SHRII-CTX (which are specified in some
device coordinate system) to the coordinate system indicated by
TRANSFORM-CTX which is a list (sx sy tx ty) which specifies the
horizontal and vertical scaling and translation factors"
  (with-transform-ctx transform-ctx
    (setf (slot-value ret 'center)
	  (translatep (slot-value ctx 'center)))
    (setf (slot-value ret 'radius)
	  (y (translatep
	      (new-point 0 (slot-value ctx 'radius)))))
    (loop for i in +shrii-params+
	  for val = (slot-value ctx i)
	  do (cond ((null val) (assert (null (slot-value ret i))))
		   (t (setf (slot-value ret i)
			    (if (or (search "LINE" (string i))
				    (search "SIDE" (string i)))
				(transforml val)
				(transformp val)))))))
  ret)

(defun solve-shrii (ctx &optional (deg -19.43943))
  "my 2014 construction based on a single parameter `Q'"
  (with-ctx-slots ctx
    (with-board (:center center) ;fix multiple rebindings of `center'
      ;; T1=POINT0 = (POINT RADIUS (/ +PI+ 2) center)
      (setq t1 (point radius (/ +pi+ 2)))
      (setq q (reflecty (point radius (degrees-to-radians deg))))
      (progn
	(SETQ LINE0 (new-line 0 (y T1)))
	(setq line10 (new-line 0 (y (CONJUGATE T1))))
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
	;; (isoceles '$t4 (zpoint line8) (intersection line4 side8))
	(setq line5 (new-line 0 (y (intersection side9 side1))))
	(setq side5 (make-line (zpoint line7) (intersection line5 side6)))
	;; (isoceles '$t5  (zpoint line7) (intersection line5 side6))
	ctx))))

(defun call-solver (solver center radius &key
		    transform-ctx
		    (ctx (apply #'make-shrii-ctx :center center
				:radius radius
				(if transform-ctx
				    `(:transform-ctx ,transform-ctx)))))

  "SOLVER is a function that takes a SHRII-CTX initialized with CENTER and RADIUS and computes the remaining slots."
  (if (slot-value ctx 'center)
      (assert (= center (slot-value ctx 'center)))
      (setf (slot-value ctx 'center) center))
  (if (slot-value ctx 'radius)
      (assert (= radius (slot-value ctx 'radius)))
      (setf (slot-value 'ctx radius) radius))
  (funcall solver ctx)
  (let ((transform-ctx (slot-value ctx 'transform-ctx)))
    (when (and transform-ctx (not (equal transform-ctx '(1 1 0 0))))
      (transform-shrii-ctx ctx transform-ctx ctx))
    ctx))

(defvar *float-tolerance* 0.0005)

(defun approx= (a b)
  (< (abs (- a b)) *float-tolerance*))

(defmacro csetq (var form &rest more-forms)
  "If VAR (symbol) is non-NIL, check if it is approximately equal to
FORM (evaluated), otherwise if VAR is NIL, set it to FORM (evaluated)."
  (check-type var symbol)
  `(let ((.val. ,form))
     (if ,var
	 (unless (and (approx= (x ,var) (x .val.))
		      (approx= (y ,var) (y .val.)))
	   (with-simple-restart (cont "Cont")
	     (error "~A set to ~A but expected to be ~A" ',var ,var .val.)))
	 (setq ,var .val.))
     ,@(and more-forms `((csetq ,@more-forms)))))

#+nil
(let ((a 1.05) (*float-tolerance* 0.01))
  (csetq a 1.04))

#+nil
(slynk:eval-in-emacs '(put 'csetq 'common-lisp-indent-function nil))

(defun verify-solved (shrii-ctx)
  (with-ctx-slots shrii-ctx
    (with-board (:center center)
      (csetq t1 (zpoint line0))
      (csetq s (intersection line1 side1))
      (csetq r (intersection line2 side2))
      (csetq q (intersection line3 side3))
      (csetq j (intersection line7 side7))
      (csetq k (intersection line8 side8))
      (csetq l (intersection line9 side9))
      (csetq o (zpoint line10))
      (csetq z (zpoint line8))

      (csetq a (intersection line8 side9))
      (csetq a (intersection line8 side3))
      (csetq a (intersection side3 side9))

      (csetq h (intersection side3 side8))
      (csetq h (intersection line7 side3))
      (csetq h (intersection line7 side8))

      (csetq b (intersection line7 side9))
      (csetq b (intersection line7 side2))
      (csetq b (intersection side2 side9))

      ;; todo
      (setq w (zpoint line6))
      (setq d (intersection line5 side9))
      (setq U (zpoint line3))
      (setq p (intersection line6 side2))
      (setq v (zpoint line1))

      (setq s (intersection line1 side1))
      ;; todo
      (setq m (intersection line4 side8))
      (setq n (intersection line5 side5)))))

(defun retrieve-9-trikonas (shrii-ctx)
  (verify-solved shrii-ctx)
  (with-ctx-slots shrii-ctx
    (with-board (:center center)
      (list
       (list t1 j (reflectx j))
       (list v k (reflectx k))
       (list u l (reflectx l))
       (list (zpoint line2) p (reflectx p))
       (list (reflectx s) s w)
       (list (reflectx r) r (zpoint line9))
       (list (reflectx q) q o)
       (list (reflectx m) m z)
       (list (reflectx n) n (zpoint line7))))))

(defun retrieve-5-cakras (shrii-ctx)
  (verify-solved shrii-ctx)
  (with-ctx-slots shrii-ctx
    (with-board (:center center)
      (list (list (zpoint line0)  (zpoint line1)
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
	    (list (zpoint line5) (intersection side9 line5) (zpoint line6))))))

(defun shrii (center radius &key return-type transform-ctx)
  "Obsolete"
  (check-type return-type (or null (member plist triangles krama plist-points)))
  (let* ((ctx (call-solver #'solve-shrii center radius :transform-ctx transform-ctx))
	 (q (slot-value ctx 'q))
	 (x (x q))
	 (y (y q)))
    (with-ctx-slots ctx
      (with-board (:center center)
	(when (eql return-type 'plist)
	  (return-from shrii (plistify
			      (line0 line3 line7 line10 side1 side2 line2 line8 line5 line4
				     side9 line6 side6 side8 side7 side3 line1 line9 side4 side5
				     Q H G I R B A C F E X Y ))))
	(let ()
	  (when (eql return-type 'plist-points)
	    (verify-solved ctx)
	    (return-from shrii
	      (plistify (#|F A G|# P J L V M D
				     Q H G I R B A C F E X Y
				     T1 O Z W U V S N K ))))

	  (when (eql return-type 'triangles)
	    (return-from shrii (retrieve-9-trikonas ctx)))

	  (when (eql return-type 'krama)
	    (return-from shrii  nil)))
	(retrieve-5-cakras ctx)))))

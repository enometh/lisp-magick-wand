;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Mon Jan 05 11:17:32 2026 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2026 Madhu.  All Rights Reserved.
;;;
;;; check the data presented for the "Classical Sri Yantra" by G.Huet.
;;; http://yquem.inria.fr/~huet/PUBLIC/Nivat.ps (2002-12-31, last
;;; retrieved 2011-05-07)

(in-package "MANDALA-MAGICK")

;;;
;;; huet-coords is huet's solution to the Classical Sri Yantra "where
;;; the unit circle alpha is taken as unit length". presumably this is
;;; a coordinate system where the origin is in the bottom center and
;;; the center of the circle is 0.0, 0.5. we have to convert this to
;;; ndc

(defvar +huet-coords+ '(YF 0.668 XF 0.126 YP 0.463 XA 0.187 YJ 0.398 YL 0.165 YA 0.265 YG 0.769 YV 0.887 YM 0.603 YD 0.551))

(defun split-var (s)
  (let ((direction (find-symbol (subseq (string s) 0 1) "SHRII"))
	(variable  (find-symbol (subseq (string s) 1 2) "SHRII")))
    (assert direction) (assert variable)
    (list variable direction)))

(defvar +huet-vars+ ; ndc
  (loop for (s val) on +huet-coords+ by #'cddr
	for (variable direction) = (split-var s)
	do (setf (get variable direction)
		 (case direction
		   (x val)
		   (y (- val 0.5))))
	collect variable))

;;#+nil
(mapcan (lambda (s) (list s (cons (get s 'x) (get s 'y)))) +huet-vars+)
;; => (D (NIL . 0.0510) M (NIL . 0.103) V (NIL . 0.387) G (NIL . 0.269) L (NIL . -0.335) J (NIL . -0.102) A  (0.187 . -0.235) P (NIL . -0.037) F (0.126 . 0.168))

(defvar $huet-classic (make-shrii-ctx :center #C(0 .0) :radius .5))

(defun compute-huet (ctx) ;le comput
  (with-ctx-slots ctx
    (with-board (:center center :shadow (conjugate intersection))
      (setq line0 (new-line 0 0.5))
      (setq t1 (zpoint line0))
      (setq line10 (new-line 0 -0.5))
      (setq o (zpoint line10))
      (setq line1 (new-line 0 (get 'v 'y)))
      (setq v (zpoint line1))
      (setq line2 (new-line 0 (get 'g 'y)))
      (setq line3 (new-line 0 (get 'f 'y)))
      (setq f (new-point (get 'f 'x) (get 'f 'y)))
      (setq line4 (new-line 0 (get 'm 'y)))
      (setq line5 (new-line 0 (get 'd 'y)))
      (setq line6 (new-line 0 (get 'p 'y)))
      (setq line7 (new-line 0 (get 'j 'y)))
      (setq line8 (new-line 0 (get 'a 'y)))
      (setq a (new-point (get 'a 'x) (get 'a 'y)))
      (setq z (zpoint line8))
      (assert (= (y z) (y a)))
      (setq line9 (new-line 0 (get 'l 'y)))

      ;; t3 side3 (QO) line3
      (setq o (new-point 0 -.5))
      ;; q is on the circle. y=0.5*sin(thetaQ)=YF
      ;; y = 0.5* sin(thetaQ)
      (setq q (new-point (* (cos (asin (* 2 (get 'f 'y)))) .5)
			  (get 'f 'y)))
      ;; {xq}^2 + {yq}^2 = .25
      ;; (get 'f 'y)=.168  (sqrt (- .25 (expt .168 2)))
      (setq side3 (make-line o q))

      ;; t7 side7 (T1-GI-J) line7
      ;; {xj}^2 + {yj}^2 = .25;
      (setq j (new-point (sqrt (- (expt .5 2) (expt (get 'j 'y) 2)))
			  (get 'j 'y)))
      (setq side7 (make-line t1 j))

      ;; t9 side9 (U-DBA-L) line9
      (setq u (zpoint line3))
      (setq side9 (make-line u a))
      (setq l (intersection side9 line9))
      (assert (= (get 'l 'y) (y l)))

      ;; t1 side1 (S-GFED-W) line1
      (setq w (zpoint line6))
      (setq side1 (make-line f w))
      ;; t2 side2 (R-IP-B) line2
      (setq i (intersection side7 line3))
      (setq b (intersection side9 line7))
      (setq side2 (make-line i b))
      ;; t6 side6 (P-NE-zpoint(Line2)) line6
      (setq p (intersection side2 line6))
      (setq side6 (make-line p (zpoint line2)))
      ;; t5 side5 (N-Zoint(line7) line5
      (setq n (intersection line5 side6))
      (setq side5 (make-line n (zpoint line7)))
      ;; t8 side8 (V-FMH-K) line8
      (setq side8 (make-line v f))
      (setq k (intersection side8 line8))
      ;; t4 side4 (M-N-Z) line4
      (setq m (intersection line4 side8))
      (setq side4 (make-line m z))

      (setq s (intersection line1 side1))
      (setq r (intersection line2 side2)))))

;;#+nil
(compute-huet $huet-classic)

#+nil
(let ((*float-tolerance* 0.01)) ;; serious problems!
  (with-wand (:wand-var *wand* :filename "x:"
	      :height 800 :width 750
	      :wand-args (:string "white"))
    (with-dw (dw)
      (transform-ndc dw)
      (draw-shrii-ctx-cakras dw $huet-classic)
      (axes-image *wand*))
    (with-dw (dw)
      (draw-circle dw (transform-ndcp #C(0 0))
		   (x (scale-ndcp #C(.5 0)))))))

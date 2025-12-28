;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Fri Dec 26 09:56:14 2025 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2025 Madhu.  All Rights Reserved.
;;;
;;;
;;; Add the ability for wand drawing functions to use color gradients
;;; as "fill" and "stroke" targets (instead of just colors, as in
;;; svg)..
;;;
(in-package "LISP-MAGICK-WAND")

#||
Refs:
https://github.com/ImageMagick/ImageMagick/discussions/8449#discussioncomment-15014963

snibgo> As fmw42 says, other approaches are possible. One image could be the background (eg white), another image entirely a gradient, a third image a mask that defines the shape, then use `-composite` to combine the images. If we also want to create a "stroke" around the shape, we might use a stroke image as input, and create a mask from that image.

https://jqmagick.imagemagick.org/discourse-server/viewtopic.php%3Ft=31808
"How to fill a primitive with a gradient color?"
||#

(defun call-draw-function-with-gradient-composition
    (magick-wand wand-drawing-function fill-gradient stroke-gradient
     &key clip-to-self (x 0) (y 0))
  (labels ((dodraw (typ gradient-wand)
	     (with-cloned-magick-wand (src gradient-wand)
	       (with-pixel-wands ((black-pixel :string "black")
				  (white-pixel :string "white"))
		 (with-pixel-wand (opacity)
		   (pixel-wand-set-alpha opacity 1.0)
		   (colorize-image src black-pixel opacity))
		 (with-drawing-wand (dw)
		   (ecase typ
		     (:fill
		      (draw-set-fill-color dw white-pixel)
		      (draw-set-stroke-color dw black-pixel))
		     (:stroke
		      (draw-set-fill-color dw black-pixel)
		      (draw-set-stroke-color dw white-pixel)))
		   (funcall wand-drawing-function dw)
		   (draw-image src dw))
		 (set-image-alpha-channel src :off))
	       (composite-image gradient-wand src :copy-opacity clip-to-self x y))
	     (composite-image magick-wand gradient-wand :over clip-to-self x y))
	   (dograd (typ gradient)
	     (etypecase gradient
	       (string
		(with-magick-wand (background)
		  (set-size background
			    (get-image-width magick-wand)
			    (get-image-height magick-wand))
		  (read-image background gradient)
		  (dodraw typ background)))
	       (t (dodraw typ gradient)))))
    (when fill-gradient (dograd :fill fill-gradient))
    (when stroke-gradient (dograd :stroke stroke-gradient))))

(defmacro with-gradient-composition ((drawing-wand-var
				      &key magick-wand fill-gradient stroke-gradient
				      clip-to-self (x 0) (y 0))
				     &body body)
  `(flet ((doit (,drawing-wand-var) ,@body))
     (call-draw-function-with-gradient-composition ,magick-wand #'doit ,fill-gradient ,stroke-gradient :clip-to-self ,clip-to-self :x ,x :y ,y)))

(export 'with-gradient-composition 'lisp-magick-wand)

#||
(defun ellips (dw)
  (magick:draw-set-stroke-width dw 3)
  (magick:draw-path-start dw)
  (magick:draw-path-move-to-absolute dw 30 40)
  (magick:draw-path-elliptic-arc-absolute dw 30 20 20 nil nil 70 20)
  (magick:draw-path-elliptic-arc-absolute dw 30 20 20 t nil 30 40)
  (magick:draw-path-close dw)
  (magick:draw-path-finish dw))

(with-magick-wand (img)
  (set-size img 100 70)
  (read-image img "xc:white")
  (with-gradient-composition (dw :magick-wand img :fill-gradient "gradient:white-skyblue" :stroke-gradient "gradient:red-blue")
    (ellips dw))
  (write-image img "x:"))
||#


#|| CMDLINE NOTES
(slynk:eval-in-emacs
 `(progn
    (setenv "drawstring" "path 'M 30,40 A 30,20 20 0,0 70,20 A 30,20 20 1,0 30,40 Z'")
    (setenv "size" "100x70")))
(slynk:eval-in-emacs
  `(progn
     (setenv "drawstring"  "ellipse 250,250 150,150 0,360")
     (setenv "size" "500x500")))

sxiv /tmp/ellipse-fill&
magick \( -size "${size}" -background none gradient:white-skyblue \
   \( +clone  -fill black -colorize 100  -strokewidth 10 -stroke black -fill white -draw "${drawstring}" -alpha off \) \
       -compose CopyOpacity -composite \)  /tmp/ellipse-fill.png

sxiv /tmp/ellipse-stroke.png&
magick \( -size "${size}" -background none gradient:red-blue \
   \( +clone  -fill black -colorize 100  -strokewidth 10 -stroke white -fill black -draw "${drawstring}" -alpha off \) \
       -compose CopyOpacity -composite \)  /tmp/ellipse-stroke.png

sxiv /tmp/ellipse.png&
magick  -size ${size} -background none canvas: /tmp/ellipse-fill.png  -compose Over -composite /tmp/ellipse-stroke.png -compose Over -composite /tmp/ellipse.png

# without cloning the gradient
magick \( -size "${size}" -background none gradient:white-skyblue \) \
       \( -size "${size}" -background none canvas: -fill black -colorize 100  -strokewidth 10 -stroke black -fill white -draw "${drawstring}" -alpha off \) \
       -compose CopyOpacity -composite /tmp/ellipse-fill.png

magick \( -size "${size}" -background none gradient:red-blue \) \
       \( -size "${size}" -background none canvas: -fill black -colorize 100  -strokewidth 10 -stroke white -fill black -draw "${drawstring}" -alpha off \) \
       -compose CopyOpacity -composite /tmp/ellipse-stroke.png
||#


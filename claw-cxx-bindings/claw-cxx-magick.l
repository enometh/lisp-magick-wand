;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Sun Oct 12 11:15:08 2025 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2025 Madhu.  All Rights Reserved.
;;;
;;; scratch worksheet for generating and working with bindings
;;; enometh/claw-cxx

(in-package "CL-USER")
(load "~/cl/extern/Github/lisp-magick-wand/magick.system")
(mk:oos "magick" :load)

#+nil
(progn
(load "~/cl/extern/claw-cxx/claw-cxx.system")
(require 'claw-cxx/util))

(defvar *magick-bindings-dir* (mk::system-relative-pathname :magick "claw-cxx-bindings/7.1.0/gen/"))

#+nil
(setq *magick-bindings-dir* "/dev/shm/claw-cxx-imagemagick/")

(make:defsystem :claw-cxx-imagemagick.bindings
  :source-pathname #1=#.*magick-bindings-dir*
  :binary-pathname (let (#+nil(*binary-directory-fasl-root*
			  (merge-pathnames "fasl/" #1#)))
		     (binary-directory #1# ))
  :source-extension "lisp"
  :depends-on ("uiop" "cffi")
  :components ((:module "bindings"
		:components
		((:file "x86_64-pc-linux-gnu" :if-feature (:and :x86-64 :linux))
		 #+nil
		 (:file "i686-pc-linux-gnu" :if-feature (:and :x86 :linux))))))

#+nil
(mk:oos :claw-cxx-imagemagick.bindings :load :compile-during-load t)

#+nil
(mapcar (lambda (x) (cffi::foreign-library-pathname x))
	(mapcar (lambda (x) (cffi::get-foreign-library x))
		(cffi:find-foreign-library "MagickWand")))
;; also loads MagickCore


;;; ----------------------------------------------------------------------
;;;
;;; generate bindings
;;;

#+nil
(require 'claw-cxx)

#+nil
(unless (cffi:find-foreign-library "libresect.so")
  (cffi:load-foreign-library "libresect.so"))

#+nil
(unless (cffi:find-foreign-library "libresect.so")
  (cffi:load-foreign-library "/dev/shm/libresect/build/libresect.so"))

#||
pkg-config --cflags ImageMagick-7.Q16HDRI
-fopenmp -DMAGICKCORE_HDRI_ENABLE=1 -DMAGICKCORE_QUANTUM_DEPTH=16 -I/usr/include/ImageMagick-7
pkg-config --cflags  Magick++-7.Q16HDRI
-fopenmp -DMAGICKCORE_HDRI_ENABLE=1 -DMAGICKCORE_QUANTUM_DEPTH=16 -I/usr/include/ImageMagick-7
pkg-config --cflags  MagickCore-7.Q16HDRI
||#

#+nil
(claw.wrapper:defwrapper
    ("claw-cxx-imagemagick"
     (:base-path "/dev/shm/claw-cxx-imagemagick/"
      #+nil #. (merge-pathnames "gen/" *magick-bindings-dir*))
     (:headers  "MagickWand/MagickWand.h" "MagickCore/MagickCore.h")
     (:system-includes "/usr/include/ImageMagick-7")
     (:defines "MAGICKCORE_HDRI_ENABLE" "1"  "MAGICKCORE_QUANTUM_DEPTH" "16")
     (:targets
      ((:and :x86-64 :linux) "x86_64-pc-linux-gnu")
      #+nil
      ((:and :x86 :linux) "i686-pc-linux-gnu"))
     (:include-definitions ".*")
     #+nil
     (:exclude-definitions "^_*libcpp" "_H$"
      "^_*LIBCPP"
      "^(INFINITY|MATH_ERR(EXCEPT|NO)|HUGE_VAL[FL]?)$"
      "math_errhandling"
      "^M_([12]_)?(SQRT|PI|LOG|LN)"
      "^NAN$" "_SOURCE$"
      "^FP_"
      "^_*(a?cos|a?sin|a?tan|cbrt|ceil|copysign|drem|erf|exp2?|fabs|fdim|finite|floor|fma|fmax|fmin|fmod|fpclassify|frexp|gamma|hypot|ilogb|iseqsig|isinf|isnan|issignaling|j0|j1|jn|ldexp|lgamma|llrint|llround|log|log10|log1p|log2|logb|l?rint|l?round|mod|nan|nearbyint|nextafter|nexttoward|pow|remainder|remquo|scalb|signgam|signbit|significand|sqrt|tgamma|trunc|y0|y1|yn)[fhl]?"
      "^gamma$" "^gammaf$" "^gammal$"
      "^lgamma$" "^lgammaf$" "^lgammal$"
      "^lgamma_r$" "^lgammaf_r$" "^lgammal_r$"
      "^drem$" "^dremf$" "^dreml$"
      "^y0$" "^y0f$" "^y0l$"
      "^y1$" "^y1f$" "^y1l$"
      "^yn$" "^ynf$" "^ynl$"
      "^j0$" "^j0f$" "^j0l$"
      "^j1$" "^j1f$" "^j1l$"
      "^jn$" "^jnf$" "^jnl$"
      "^scalb$" "^scalbf$" "^scalbl$"
      "^significand$" "^significandf$" "^significandl$"
      "^finite$" "^finitef$" "^finitel$"
      "^isinf$" "^isinff$" "^isinfl$"
      "^isnan$" "^isnanf$" "^isnanl$"
      "_t$" "^__(?!claw)"
      ;; See https://gitlab.com/embeddable-common-lisp/ecl/-/issues/753
      "^frexp$" "^frexpf$" "^modf$" "^modff$" "^nan$" "^nanf$" "^remquo$" "^remquof$"))
  :in-package "CLAW-CXX-IMAGEMAGICK"
  :trim-enum-prefix t
  :recognize-bitfields t
  :recognize-strings t
  :recognize-arrays t
  :with-adapter (:dynamic :path "lib/adapter.c"))

#+nil
(claw:generate-wrapper "claw-cxx-imagemagick")

#||
(progn
  (setq $wrapper-def (gethash :claw-cxx-imagemagick claw.wrapper::*wrapper-registry*))
  (setq $opts-def (car $wrapper-def))
  (setq $conf-def (cdr $wrapper-def))
  (setq $opts (claw.wrapper::eval-opts :test-bool $opts-def)))

;; claw.wrapper:describe-foreign-library
(claw.resect::write-uber-header
 (claw.wrapper::wrapper-options-headers $opts)
 "/tmp/1.h"
 (claw.wrapper::wrapper-options-defines $opts))

(claw.wrapper::wrapper-options-defines $opts)
(setq $bindings-table
      (let (claw.wrapper::*always-generate*)
	(claw.wrapper::make-bindings-table :test-bool $opts $conf-def)))
(length (claw.wrapper::wrapper-options-targets $opts))
(claw.wrapper::target-options-triple (car (claw.wrapper::wrapper-options-targets $opts)))
||#


;;; ----------------------------------------------------------------------
;;;
;;;
;;;
#+nil
(let ((mk::*reload-systems-from-disk* nil))
  (mk:oos :claw-cxx-imagemagick.bindings :load))

#+nil
(sort (mapcar 'symbol-name
	      (claw.util:use-spec-package "CLAW-CXX-IMAGEMAGICK"
					  "CL"
					  :dry-run-p t))
      #'string<)


(defpackage "MAGICK-USER"
  (:use "CL" "CLAW-CXX-IMAGEMAGICK")
  (:shadow "ABORT" "ABS" "ACOS" "ACOSH" "ADJOIN" "ASIN" "ASINH" "ATAN" "ATANH" "CLOSE" "COS" "COSH" "COUNT" "DEBUG" "DIRECTORY" "ERROR" "EXP" "FILL" "FLOOR" "FORMAT" "LENGTH" "LIST" "LOG" "MAX" "MERGE" "METHOD" "MIN" "RANDOM" "READ" "REM" "REMOVE" "ROUND" "SHADOW" "SIN" "SINH" "SQRT" "STREAM" "TAN" "TANH" "TIME" "TYPE" "VALUES" "WRITE"))

(in-package "MAGICK-USER")
(cl:error "JUNK AT EOF")

(setq $foo (cffi:convert-to-foreign "foo" :string))
(magick-core-genesis $foo 1)

(setq $e (acquire-exception-info))


(setq $i (clone-image-info (cffi:null-pointer)))

(cffi:lisp-string-to-foreign  "/home/madhu/out.bmp1"
			      (cffi:foreign-slot-value $i 'image-info 'filename)
			      +magick-path-extent+)

#+nil
(cffi:foreign-string-to-lisp
 (cffi:foreign-slot-value $i 'image-info 'filename))

(setq $img (read-image $i $e))
(magickcore-check-exception $e :force t)
(cffi-extra:slots $e 'exception-info)
(cffi:null-pointer-p(cffi:foreign-slot-value $e 'exception-info 'exceptions))
(reset-linked-list-iterator(cffi:foreign-slot-value $e 'exception-info 'exceptions))
(get-next-value-in-linked-list(cffi:foreign-slot-value $e 'exception-info 'exceptions))


(defun magickcore-check-exception (e &key force)
  (unless (or force (eql (cffi:foreign-slot-value e 'exception-info 'severity)
			 :undefined-exception))
    (let (severities reasons
	  (exceptions
	   (cffi:foreign-slot-value e 'exception-info 'exceptions)))
      (reset-linked-list-iterator exceptions)
      (loop for p = (get-next-value-in-linked-list exceptions)
	    while (not (cffi:null-pointer-p p)) do
	    (push (cffi:foreign-slot-value p 'exception-info 'severity) severities)
	    (push (cffi:foreign-slot-value p 'exception-info 'reason) reasons))
      (clear-magick-exception e)
      (cl:error 'magick:magick-wand-error :message reasons :type severities))))


(cffi:foreign-slot-value $e 'exception-info 'severity)
(cffi:foreign-slot-value $e 'exception-info 'reason)
(cffi:foreign-slot-value $e 'exception-info 'description)
(load "~/cl/cffi-extra.lisp")
(cffi-extra:slot-names  'exception-info)
(cffi-extra:slots $e 'exception-info)
(catch-exception $e)
(get-image-view-exception
(get-locale-exception-message :blob-error "foo")


#||
objdump -T  /usr/lib64/libMagickCore-7.Q16HDRI.so |\
	grep  'g    DF' |\
 egrep -v '(MagickError)|(MagickFatalError)|(MagickWarning)|(ThrowException)'  |\
 awk '{printf $7; printf "\n" }';
 awk '{ printf("#define %s  PrependMagickMethod(%s)\n", $7, $7); }' | sort
||#

(export
 (loop for x being each external-symbol of "CLAW-CXX-IMAGEMAGICK"
       when (eql x (find-symbol (symbol-name x) "MAGICK-USER"))
       collect x)
 "MAGICK-USER")

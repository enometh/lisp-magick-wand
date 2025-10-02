(in-package :lisp-magick-wand)
(PROGN (CFFI:DEFINE-FOREIGN-LIBRARY
         (:LIB-MAGIC-WAND-64 :SEARCH-PATH "/tmp/im712-64/lib/")
         (:UNIX "libMagickWand-7.Q64HDRI.so"))
       (CFFI:DEFINE-FOREIGN-LIBRARY
         (:LIB-MAGIC-WAND-16 :SEARCH-PATH "/tmp/im7120-16/lib/")
         (:UNIX "libMagickWand-7.Q16HDRI.so"))
       (CFFI:DEFINE-FOREIGN-LIBRARY
         (:LIB-MAGIC-WAND-8 :SEARCH-PATH "/tmp/im712-8/lib/")
         (:UNIX "libMagickWand-7.Q8HDRI.so"))
       (CFFI:DEFINE-FOREIGN-LIBRARY
         (:LIB-MAGIC-WAND-32 :SEARCH-PATH "/tmp/im710-32/lib/")
         (:UNIX "libMagickWand-7.Q32HDRI.so")))


(defvar cl-user::$magick-dll-variant :lib-magic-wand-64)

(cffi:load-foreign-library cl-user::$magick-dll-variant)

(unless (search "HDRI.so" (namestring (cffi:foreign-library-pathname (cffi::get-foreign-library  cl-user::$magick-dll-variant))))
  (pushnew 'no-hdri *features*))

#||
(type-of (cffi::get-foreign-library 'lib-magic-wand-64))
(cffi::foreign-library-search-path  (cffi::get-foreign-library 'lib-magic-wand-64))
||#



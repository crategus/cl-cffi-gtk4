(in-package :gtk-test)

(def-suite gtk-picture :in gtk-suite)
(in-suite gtk-picture)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkContentFit                                      Since 4.8



;;;     GtkPicture

(test gtk-picture-class
  ;; Type check
  (is (g:type-is-object "GtkPicture"))
  ;; Check the registered name
  (is (eq 'gtk:picture
          (glib:symbol-for-gtype "GtkPicture")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkPicture")
          (g:gtype (cffi:foreign-funcall "gtk_picture_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkPicture")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkPicture")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (list-interfaces "GtkPicture")))
  ;; Check the properties
  (is (equal '("alternative-text" "can-shrink" "content-fit" "file"
               "keep-aspect-ratio" "paintable")
             (list-properties "GtkPicture")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkPicture")))
  ;; CSS information
  (is (string= "picture"
               (gtk:widget-class-css-name "GtkPicture")))
  (is (string=
"picture:dir(ltr)
"
               (gtk:style-context-to-string
                   (gtk:widget-style-context (make-instance 'gtk:picture))
                   :none)))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkPicture" GTK-PICTURE
                       (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                        ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
                        :TYPE-INITIALIZER "gtk_picture_get_type")
                       ((ALTERNATIVE-TEXT GTK-PICTURE-ALTERNATIVE-TEXT
                         "alternative-text" "gchararray" T T)
                        (CAN-SHRINK GTK-PICTURE-CAN-SHRINK "can-shrink"
                         "gboolean" T T)
                        (CONTENT-FIT GTK-PICTURE-CONTENT-FIT "content-fit"
                         "GtkContentFit" T T)
                        (FILE GTK-PICTURE-FILE "file" "GFile" T T)
                        (KEEP-ASPECT-RATIO GTK-PICTURE-KEEP-ASPECT-RATIO
                         "keep-aspect-ratio" "gboolean" T T)
                        (PAINTABLE GTK-PICTURE-PAINTABLE "paintable"
                         "GdkPaintable" T T)))
             (gobject:get-g-type-definition "GtkPicture"))))

;;; --- Properties -------------------------------------------------------------

;;;     alternative-text
;;;     can-shrink
;;;     content-fit                                        Since 4.8
;;;     file
;;;     keep-aspect-ratio
;;;     paintable

;;; --- Functions --------------------------------------------------------------

;;;     gtk_picture_new
;;;     gtk_picture_new_for_paintable
;;;     gtk_picture_new_for_pixbuf
;;;     gtk_picture_new_for_file
;;;     gtk_picture_new_for_filename
;;;     gtk_picture_new_for_resource
;;;     gtk_picture_set_pixbuf
;;;     gtk_picture_set_filename
;;;     gtk_picture_set_resource

;;; --- 2023-5-29 --------------------------------------------------------------

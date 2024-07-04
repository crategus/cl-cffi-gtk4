(in-package :gtk-test)

(def-suite gtk-picture :in gtk-suite)
(in-suite gtk-picture)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkContentFit                                      Since 4.8

(test gtk-content-fit
  ;; Check type
  (is (g:type-is-enum "GtkContentFit"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkContentFit")
          (g:gtype (cffi:foreign-funcall "gtk_content_fit_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:content-fit
          (glib:symbol-for-gtype "GtkContentFit")))
  ;; Check names
  (is (equal '("GTK_CONTENT_FIT_FILL" "GTK_CONTENT_FIT_CONTAIN"
               "GTK_CONTENT_FIT_COVER" "GTK_CONTENT_FIT_SCALE_DOWN")
             (gtk-test:list-enum-item-name "GtkContentFit")))
  ;; Check values
  (is (equal '(0 1 2 3)
             (gtk-test:list-enum-item-value "GtkContentFit")))
  ;; Check nick names
  (is (equal '("fill" "contain" "cover" "scale-down")
             (gtk-test:list-enum-item-nick "GtkContentFit")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkContentFit" GTK-CONTENT-FIT
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "gtk_content_fit_get_type")
                                     (:FILL 0)
                                     (:CONTAIN 1)
                                     (:COVER 2)
                                     (:SCALE-DOWN 3))
             (gobject:get-g-type-definition "GtkContentFit"))))

;;;     GtkPicture

(test gtk-picture-class
  ;; Check type
  (is (g:type-is-object "GtkPicture"))
  ;; Check registered name
  (is (eq 'gtk:picture
          (glib:symbol-for-gtype "GtkPicture")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkPicture")
          (g:gtype (cffi:foreign-funcall "gtk_picture_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkPicture")))
  ;; Check children
  (is (equal '()
             (gtk-test:list-children "GtkPicture")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (gtk-test:list-interfaces "GtkPicture")))
  ;; Check properties
  (is (equal '("alternative-text" "can-shrink" "content-fit" "file"
               "keep-aspect-ratio" "paintable")
             (gtk-test:list-properties "GtkPicture")))
  ;; Check signals
  (is (equal '()
             (gtk-test:list-signals "GtkPicture")))
  ;; Check CSS information
  (is (string= "picture"
               (gtk:widget-class-css-name "GtkPicture")))
  ;; Check accessible role
  (is (eq :img (gtk:widget-class-accessible-role "GtkPicture")))
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

(test gtk-picture-properties
  (let ((picture (make-instance 'gtk:picture)))
    (is-false (gtk:picture-alternative-text picture))
    (is-true (gtk:picture-can-shrink picture))
    (is (eq :contain (gtk:picture-content-fit picture)))
    (is-false (gtk:picture-file picture))
    (is-true (gtk:picture-keep-aspect-ratio picture))
    (is-false (gtk:picture-paintable picture))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_picture_new

(test gtk-picture-new
  (is (typep (gtk:picture-new) 'gtk:picture)))

;;;     gtk_picture_new_for_paintable

(test gtk-picture-new-for-paintable
  (let ((paintable (gdk:texture-new-from-filename
                       (sys-path "resource/gtk-logo-24.png"))))
    (is (typep (gtk:picture-new-for-paintable paintable) 'gtk:picture))))

;;;     gtk_picture_new_for_pixbuf

(test gtk-picture-new-for-pixbuf
  (let ((*gtk-warn-deprecated* nil))
    (let ((pixbuf (gdk:pixbuf-new-from-file
                      (sys-path "resource/gtk-logo-24.png"))))
      (is (typep (gtk:picture-new-for-pixbuf pixbuf) 'gtk:picture)))))

;;;     gtk_picture_new_for_file

;;;     gtk_picture_new_for_filename

(test gtk-picture-new-for-filename
  (let ((filename (sys-path "resource/gtk-logo-24.png")))
    (is (typep (gtk:picture-new-for-filename filename) 'gtk:picture))))

;;;     gtk_picture_new_for_resource

(test gtk-picture-new-for-resource
  (gio:with-g-resources (resource (sys-path "resource/rtest-resource.gresource"))
    (let ((path "/com/crategus/test/gtk-logo-24.png"))
    (is (typep (gtk:picture-new-for-resource path) 'gtk:picture)))))

;;;     gtk_picture_set_pixbuf
;;;     gtk_picture_set_filename
;;;     gtk_picture_set_resource

;;; 2024-4-25

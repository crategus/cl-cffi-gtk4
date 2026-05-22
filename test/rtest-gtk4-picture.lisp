(in-package :gtk-test)

(def-suite gtk-picture :in gtk-display-widgets)
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
             (glib-test:list-enum-item-names "GtkContentFit")))
  ;; Check values
  (is (equal '(0 1 2 3)
             (glib-test:list-enum-item-values "GtkContentFit")))
  ;; Check nick names
  (is (equal '("fill" "contain" "cover" "scale-down")
             (glib-test:list-enum-item-nicks "GtkContentFit")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkContentFit" GTK:CONTENT-FIT
                                    (:EXPORT T
                                     :TYPE-INITIALIZER "gtk_content_fit_get_type")
                                    (:FILL 0)
                                    (:CONTAIN 1)
                                    (:COVER 2)
                                    (:SCALE-DOWN 3))
             (gobject:get-gtype-definition "GtkContentFit"))))

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
             (glib-test:list-children "GtkPicture")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (glib-test:list-interfaces "GtkPicture")))
  ;; Check properties
  (is (equal '("alternative-text" "can-shrink" "content-fit" "file"
               "isolate-contents" "keep-aspect-ratio" "paintable")
             (glib-test:list-properties "GtkPicture")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkPicture")))
  ;; Check CSS information
  (is (string= "picture"
               (gtk:widget-class-css-name "GtkPicture")))
  ;; Check accessible role
  (is (eq :img (gtk:widget-class-accessible-role "GtkPicture")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkPicture" GTK:PICTURE
                      (:SUPERCLASS GTK:WIDGET
                       :EXPORT T
                       :INTERFACES
                       ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
                       :TYPE-INITIALIZER "gtk_picture_get_type")
                      ((ALTERNATIVE-TEXT PICTURE-ALTERNATIVE-TEXT
                        "alternative-text" "gchararray" T T)
                       (CAN-SHRINK PICTURE-CAN-SHRINK
                        "can-shrink" "gboolean" T T)
                       (CONTENT-FIT PICTURE-CONTENT-FIT
                        "content-fit" "GtkContentFit" T T)
                       (FILE PICTURE-FILE "file" "GFile" T T)
                       (ISOLATE-CONTENTS PICTURE-ISOLATE-CONTENTS
                        "isolate-contents" "gboolean" T T)
                       (KEEP-ASPECT-RATIO PICTURE-KEEP-ASPECT-RATIO
                        "keep-aspect-ratio" "gboolean" T T)
                       (PAINTABLE PICTURE-PAINTABLE
                        "paintable" "GdkPaintable" T T)))
             (gobject:get-gtype-definition "GtkPicture"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-picture-properties
  (glib-test:with-check-memory (picture)
    (is (typep (setf picture (make-instance 'gtk:picture)) 'gtk:picture))
    ;; Property ALTERNATIVE-TEXT
    (is (equal '(:READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkPicture" "alternative-text")))
    (is-false (gtk:picture-alternative-text picture))
    (is (string= "text" (setf (gtk:picture-alternative-text picture) "text")))
    ;; Property CAN-SHRINK
    (is (equal '(:READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkPicture" "can-shrink")))
    (is-true (gtk:picture-can-shrink picture))
    (is-false (setf (gtk:picture-can-shrink picture) nil))
    ;; Property CONTENT-FIT since 4.22
    (is (equal '(:READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkPicture" "content-fit")))
    (is (eq :contain (gtk:picture-content-fit picture)))
    (is (eq :fill (setf (gtk:picture-content-fit picture) :fill)))
    ;; Property FILE
    (is (equal '(:READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkPicture" "file")))
    (let ((path (glib-sys:sys-path "test/resource/ducky.png")))
      (is-false (gtk:picture-file picture))
      (is (typep (setf (gtk:picture-file picture)
                       (g:file-new-for-path path)) 'g:object))
      (is-false (setf (gtk:picture-file picture) nil)))
    ;; Property ISOLATE-CONTENTS since 4.22
    #+gtk-4-22
    (is (equal '(:READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkPicture" "isolate-contents")))
    #+gtk-4-22
    (is-false (setf (gtk:picture-isolate-contents picture) nil))
    #+gtk-4-22
    (is-false (gtk:picture-isolate-contents picture))
    ;; Property KEEP-ASPECT-RATIO deprecated 4.8
    (is (equal '(:DEPRECATED :READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkPicture" "keep-aspect-ratio")))
    (is-false (gtk:picture-keep-aspect-ratio picture))
    (is-true (setf (gtk:picture-keep-aspect-ratio picture) t))
    ;; Property PAINTABLE
    (is (equal '(:READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkPicture" "paintable")))
    (let* ((path (glib-sys:sys-path "test/resource/ducky.png"))
           (paintable (gdk:texture-new-from-filename path)))
      (is-false (gtk:picture-paintable picture))
      (is (typep (setf (gtk:picture-paintable picture) paintable) 'gdk:paintable))
      (is-false (setf (gtk:picture-paintable picture) nil)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_picture_new

(test gtk-picture-new
  (glib-test:with-check-memory (picture)
    (is (typep (setf picture (gtk:picture-new)) 'gtk:picture))
    (is (typep (setf picture (make-instance 'gtk:picture)) 'gtk:picture))
    (is (typep (setf picture (g:object-new "GtkPicture")) 'gtk:picture))))

;;;     gtk_picture_new_for_paintable

(test gtk-picture-new-for-paintable
  (glib-test:with-check-memory (picture)
    (let* ((path (glib-sys:sys-path "test/resource/gtk-logo-24.png"))
           (paintable (gdk:texture-new-from-filename path)))
      (is (typep (setf picture
                       (gtk:picture-new-for-paintable paintable)) 'gtk:picture))
      (is-false (setf (gtk:picture-paintable picture) nil)))))

;;;     gtk_picture_new_for_pixbuf

(test gtk-picture-new-for-pixbuf
  (let ((gtk-init:*warn-deprecated* nil))
    (glib-test:with-check-memory (picture)
      (let* ((path (glib-sys:sys-path "test/resource/gtk-logo-24.png"))
             (pixbuf (gdk:pixbuf-new-from-file path)))
        (is (typep (setf picture
                         (gtk:picture-new-for-pixbuf pixbuf)) 'gtk:picture))
        (is-false (setf (gtk:picture-paintable picture) nil))))))

;;;     gtk_picture_new_for_file

(test gtk-picture-new-for-file
  (glib-test:with-check-memory (picture)
    (let* ((path (glib-sys:sys-path "test/resource/gtk-logo-24.png"))
           (file (g:file-new-for-path path)))
      (is (typep (setf picture
                       (gtk:picture-new-for-file file)) 'gtk:picture))
      (is-false (setf (gtk:picture-file picture) nil)))))

;;;     gtk_picture_new_for_filename

(test gtk-picture-new-for-filename
  (glib-test:with-check-memory (picture)
    (let ((filename (glib-sys:sys-path "test/resource/gtk-logo-24.png")))
      (is (typep (setf picture
                       (gtk:picture-new-for-filename filename)) 'gtk:picture)))))

;;;     gtk_picture_new_for_resource

(test gtk-picture-new-for-resource
  (glib-test:with-check-memory (picture)
    (gio:with-resource (resource (glib-sys:sys-path "test/rtest-resource.gresource"))
      (let ((path "/com/crategus/test/gtk-logo-24.png"))
        (is (typep (setf picture (gtk:picture-new-for-resource path)) 'gtk:picture))))))

;;;     gtk_picture_set_pixbuf

(test gtk-picture-set-pixbuf
  (let ((gtk-init:*warn-deprecated* nil))
    (glib-test:with-check-memory (picture)
      (let* ((path (glib-sys:sys-path "test/resource/gtk-logo-24.png"))
             (pixbuf (gdk:pixbuf-new-from-file path)))
        (is (typep (setf picture (gtk:picture-new)) 'gtk:picture))
        (is-false (gtk:picture-set-pixbuf picture pixbuf))
        (is-false (setf (gtk:picture-paintable picture) nil))))))

;;;     gtk_picture_set_filename

(test gtk-picture-set-filename
  (glib-test:with-check-memory (picture)
    (let ((filename (glib-sys:sys-path "test/resource/gtk-logo-24.png")))
      (is (typep (setf picture (gtk:picture-new)) 'gtk:picture))
      (is-false (gtk:picture-set-filename picture filename)))))

;;;     gtk_picture_set_resource

(test gtk-picture-set-resource
  (glib-test:with-check-memory (picture)
    (gio:with-resource (resource (glib-sys:sys-path "test/rtest-resource.gresource"))
      (let ((path "/com/crategus/test/gtk-logo-24.png"))
        (is (typep (setf picture (gtk:picture-new)) 'gtk:picture))
        (is-false (gtk:picture-set-resource picture path))))))

;;; 2026-05-17

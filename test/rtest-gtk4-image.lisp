(in-package :gtk-test)

(def-suite gtk-image :in gtk-suite)
(in-suite gtk-image)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkImageType

(test gtk-image-type
  ;; Check type
  (is (g:type-is-enum "GtkImageType"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkImageType")
          (g:gtype (cffi:foreign-funcall "gtk_image_type_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:image-type
          (glib:symbol-for-gtype "GtkImageType")))
  ;; Check names
  (is (equal '("GTK_IMAGE_EMPTY" "GTK_IMAGE_ICON_NAME" "GTK_IMAGE_GICON"
               "GTK_IMAGE_PAINTABLE")
             (list-enum-item-name "GtkImageType")))
  ;; Check values
  (is (equal '(0 1 2 3)
             (list-enum-item-value "GtkImageType")))
  ;; Check nick names
  (is (equal '("empty" "icon-name" "gicon" "paintable")
             (list-enum-item-nick "GtkImageType")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkImageType"
                             GTK-IMAGE-TYPE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_image_type_get_type")
                             (:EMPTY 0)
                             (:ICON-NAME 1)
                             (:GICON 2)
                             (:PAINTABLE 3))
             (gobject:get-g-type-definition "GtkImageType"))))

;;;     GtkImage

(test gtk-image-class
  ;; Check type
  (is (g:type-is-object "GtkImage"))
  ;; Check registered name
  (is (eq 'gtk:image
          (glib:symbol-for-gtype "GtkImage")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkImage")
          (g:gtype (cffi:foreign-funcall "gtk_image_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkImage")))
  ;; Check children
  (is (equal '()
             (list-children "GtkImage")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (list-interfaces "GtkImage")))
  ;; Check class properties
  (is (equal '("file" "gicon" "icon-name" "icon-size" "paintable" "pixel-size"
               "resource" "storage-type" "use-fallback")
             (list-properties "GtkImage")))
  ;; Check list of signals
  (is (equal '()
             (list-signals "GtkImage")))
  ;; Check CSS information
  (is (string= "image"
               (gtk:widget-class-css-name "GtkImage")))
  ;; Check accessible role
  (is (eq :img (gtk:widget-class-accessible-role "GtkImage")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkImage" GTK-IMAGE
                       (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                        ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
                        :TYPE-INITIALIZER "gtk_image_get_type")
                       ((FILE GTK-IMAGE-FILE "file" "gchararray" T T)
                        (GICON GTK-IMAGE-GICON "gicon" "GIcon" T T)
                        (ICON-NAME GTK-IMAGE-ICON-NAME "icon-name" "gchararray"
                         T T)
                        (ICON-SIZE GTK-IMAGE-ICON-SIZE "icon-size"
                         "GtkIconSize" T T)
                        (PAINTABLE GTK-IMAGE-PAINTABLE "paintable"
                         "GdkPaintable" T T)
                        (PIXEL-SIZE GTK-IMAGE-PIXEL-SIZE "pixel-size" "gint" T
                         T)
                        (RESOURCE GTK-IMAGE-RESOURCE "resource" "gchararray" T
                         T)
                        (STORAGE-TYPE GTK-IMAGE-STORAGE-TYPE "storage-type"
                         "GtkImageType" T NIL)
                        (USE-FALLBACK GTK-IMAGE-USE-FALLBACK "use-fallback"
                         "gboolean" T T)))
             (gobject:get-g-type-definition "GtkImage"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-image-properties
  (let ((image (make-instance 'gtk:image)))
    (is-false (gtk:image-file image))
    (is-false (gtk:image-gicon image))
    (is-false (gtk:image-icon-name image))
    (is (eq :inherit (gtk:image-icon-size image)))
    (is-false (gtk:image-paintable image))
    (is (= -1 (gtk:image-pixel-size image)))
    (is-false (gtk:image-resource image))
    (is (eq :empty (gtk:image-storage-type image)))
    (is-false (gtk:image-use-fallback image))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_image_new

(test gtk-image-new
  (let ((image (gtk:image-new)))
    (is-false (gtk:image-file image))
    (is-false (gtk:image-gicon image))
    (is-false (gtk:image-icon-name image))
    (is (eq :inherit (gtk:image-icon-size image)))
    (is-false (gtk:image-paintable image))
    (is (= -1 (gtk:image-pixel-size image)))
    (is-false (gtk:image-resource image))
    (is (eq :empty (gtk:image-storage-type image)))
    (is-false (gtk:image-use-fallback image))))

;;;     gtk_image_new_from_file

(test gtk-image-new-from-file
  (let ((image (gtk:image-new-from-file (sys-path "resource/gtk-logo-24.png"))))
    (is (string= "gtk-logo-24"
                 (pathname-name (gtk:image-file image))))
    (is-false (gtk:image-gicon image))
    (is-false (gtk:image-icon-name image))
    (is (eq :inherit (gtk:image-icon-size image)))
    (is (typep (gtk:image-paintable image) 'gdk:texture))
    (is (= -1 (gtk:image-pixel-size image)))
    (is-false (gtk:image-resource image))
    (is (eq :paintable (gtk:image-storage-type image)))
    (is-false (gtk:image-use-fallback image))))

;;;     gtk_image_new_from_resource

(test gtk-image-new-from-resource
  (gio:with-g-resources (resource (sys-path "resource/rtest-resource.gresource"))
    (let ((image (gtk:image-new-from-resource
                     "/com/crategus/test/gtk-logo-24.png")))
      (is-false (gtk:image-file image))
      (is-false (gtk:image-gicon image))
      (is-false (gtk:image-icon-name image))
      (is (eq :inherit (gtk:image-icon-size image)))
      (is (typep (gtk:image-paintable image) 'gdk:texture))
      (is (= -1 (gtk:image-pixel-size image)))
      (is (string= "/com/crategus/test/gtk-logo-24.png"
                   (gtk:image-resource image)))
      (is (eq :paintable (gtk:image-storage-type image)))
      (is-false (gtk:image-use-fallback image)))))

;;;     gtk_image_new_from_pixbuf

(test gtk-image-new-from-pixbuf
  (let ((*gtk-warn-deprecated* nil))
    (let* ((pixbuf (gdk:pixbuf-new-from-file
                       (sys-path "resource/gtk-logo-24.png")))
           (image (gtk:image-new-from-pixbuf pixbuf)))
      (is (typep pixbuf 'gdk:pixbuf))
      (is-false (gtk:image-file image))
      (is-false (gtk:image-gicon image))
      (is-false (gtk:image-icon-name image))
      (is (eq :inherit (gtk:image-icon-size image)))
      (is (typep (gtk:image-paintable image) 'gdk:texture))
      (is (= -1 (gtk:image-pixel-size image)))
      (is-false (gtk:image-resource image))
      (is (eq :paintable (gtk:image-storage-type image)))
      (is-false (gtk:image-use-fallback image)))))

;;;     gtk_image_new_from_paintable

(test gtk-image-new-from-paintable
  (let* ((paintable (gdk:texture-new-from-filename
                        (sys-path "resource/gtk-logo-24.png")))
         (image (gtk:image-new-from-paintable paintable)))
    (is (typep paintable 'gdk:texture))
    (is (typep paintable 'gdk:paintable))
    (is-false (gtk:image-file image))
    (is-false (gtk:image-gicon image))
    (is-false (gtk:image-icon-name image))
    (is (eq :inherit (gtk:image-icon-size image)))
    (is (typep (gtk:image-paintable image) 'gdk:texture))
    (is (= -1 (gtk:image-pixel-size image)))
    (is-false (gtk:image-resource image))
    (is (eq :paintable (gtk:image-storage-type image)))
    (is-false (gtk:image-use-fallback image))))

;;;     gtk_image_new_from_icon_name

(test gtk-image-new-from-icon-name
  (let* ((image (gtk:image-new-from-icon-name "window-close")))
    (is-false (gtk:image-file image))
    (is-false (gtk:image-gicon image))
    (is (string= "window-close" (gtk:image-icon-name image)))
    (is (eq :inherit (gtk:image-icon-size image)))
    (is-false (gtk:image-paintable image))
    (is (= -1 (gtk:image-pixel-size image)))
    (is-false (gtk:image-resource image))
    (is (eq :icon-name (gtk:image-storage-type image)))
    (is-false (gtk:image-use-fallback image))))

;;;     gtk_image_new_from_gicon

(test gtk-image-new-from-gicon
  (let* ((gicon (g:icon-new-for-string "window-close"))
         (image (gtk:image-new-from-gicon gicon)))
    (is-false (gtk:image-file image))
    (is (typep (gtk:image-gicon image) 'g:themed-icon))
    (is-false (gtk:image-icon-name image))
    (is (eq :inherit (gtk:image-icon-size image)))
    (is-false (gtk:image-paintable image))
    (is (= -1 (gtk:image-pixel-size image)))
    (is-false (gtk:image-resource image))
    (is (eq :gicon (gtk:image-storage-type image)))
    (is-false (gtk:image-use-fallback image))))

;;;     gtk_image_clear
;;;     gtk_image_set_from_file
;;;     gtk_image_set_from_resource

;;;     gtk_image_set_from_pixbuf

(test gtk-image-set-from-pixbuf
  (let ((*gtk-warn-deprecated* nil))
    (let* ((pixbuf (gdk:pixbuf-new-from-file
                       (sys-path "resource/gtk-logo-24.png")))
           (image (gtk:image-new)))
      (is-false (gtk:image-set-from-pixbuf image pixbuf))
      (is (typep pixbuf 'gdk:pixbuf))
      (is-false (gtk:image-file image))
      (is-false (gtk:image-gicon image))
      (is-false (gtk:image-icon-name image))
      (is (eq :inherit (gtk:image-icon-size image)))
      (is (typep (gtk:image-paintable image) 'gdk:texture))
      (is (= -1 (gtk:image-pixel-size image)))
      (is-false (gtk:image-resource image))
      (is (eq :paintable (gtk:image-storage-type image)))
      (is-false (gtk:image-use-fallback image)))))

;;;     gtk_image_set_from_paintable
;;;     gtk_image_set_from_icon_name
;;;     gtk_image_set_from_gicon

;;; 2024-4-25

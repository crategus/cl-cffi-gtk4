(in-package :gtk-test)

(def-suite gtk-image :in gtk-suite)
(in-suite gtk-image)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkImageType

(test image-type
  ;; Check the type
  (is (g:type-is-enum "GtkImageType"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkImageType")
          (g:gtype (foreign-funcall "gtk_image_type_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gtk:image-type
          (gobject:symbol-for-gtype "GtkImageType")))
  ;; Check the names
  (is (equal '("GTK_IMAGE_EMPTY" "GTK_IMAGE_ICON_NAME" "GTK_IMAGE_GICON"
               "GTK_IMAGE_PAINTABLE")
             (mapcar #'gobject:enum-item-name
                     (gobject:get-enum-items "GtkImageType"))))
  ;; Check the values
  (is (equal '(0 1 2 3)
             (mapcar #'gobject:enum-item-value
                     (gobject:get-enum-items "GtkImageType"))))
  ;; Check the nick names
  (is (equal '("empty" "icon-name" "gicon" "paintable")
             (mapcar #'enum-item-nick
                     (get-enum-items "GtkImageType"))))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GtkImageType"
                             GTK-IMAGE-TYPE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_image_type_get_type")
                             (:EMPTY 0)
                             (:ICON-NAME 1)
                             (:GICON 2)
                             (:PAINTABLE 3))
             (gobject:get-g-type-definition "GtkImageType"))))

;;;     GtkImage

(test image-class
  ;; Type check
  (is (g:type-is-object "GtkImage"))
  ;; Check the registered name
  (is (eq 'gtk:image
          (gobject:symbol-for-gtype "GtkImage")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkImage")
          (g:gtype (foreign-funcall "gtk_image_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkImage")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkImage")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (list-interfaces "GtkImage")))
  ;; Check the class properties
  (is (equal '("file" "gicon" "icon-name" "icon-size" "paintable" "pixel-size"
               "resource" "storage-type" "use-fallback")
             (list-properties "GtkImage")))
  ;; Check the list of signals
  (is (equal '()
             (list-signals "GtkImage")))
  ;; CSS information
  (is (string= "image"
               (gtk:widget-class-css-name "GtkImage")))
  (is (string=
"image:dir(ltr)
"
               (gtk:style-context-to-string
                   (gtk:widget-style-context (make-instance 'gtk:image))
                   :none)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkImage" GTK-IMAGE
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

;;;     file
;;;     gicon
;;;     icon-name
;;;     icon-size
;;;     paintable
;;;     pixel-size
;;;     resource
;;;     storage-type
;;;     use-fallback

(test image-properties
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

(test image-new
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

(test image-new-from-file
  (let ((image (gtk:image-new-from-file (sys-path "test/gtk-logo-24.png"))))
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

(test image-new-from-resource
  (with-g-resource (resource (sys-path "test/rtest-resource.gresource"))
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

(test image-new-from-pixbuf
  (let* ((pixbuf (gdk:pixbuf-new-from-file
                     (sys-path "test/gtk-logo-24.png")))
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
    (is-false (gtk:image-use-fallback image))))

;;;     gtk_image_new_from_paintable

(test image-new-from-paintable
  (let* ((paintable (gdk:texture-new-from-filename
                        (sys-path "test/gtk-logo-24.png")))
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

(test image-new-from-icon-name
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

(test image-new-from-gicon
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
;;;     gtk_image_set_from_paintable
;;;     gtk_image_set_from_icon_name
;;;     gtk_image_set_from_gicon

;;; 2022-11-12

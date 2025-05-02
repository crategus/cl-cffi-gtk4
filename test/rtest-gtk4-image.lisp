(in-package :gtk-test)

(def-suite gtk-image :in gtk-display-widgets)
(in-suite gtk-image)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkImageType

(test gtk-image-type
  ;; Check type
  (is (g:type-is-enum "GtkImageType"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkImageType")
          (g:gtype (cffi:foreign-funcall "gtk_image_type_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:image-type
          (glib:symbol-for-gtype "GtkImageType")))
  ;; Check names
  (is (equal '("GTK_IMAGE_EMPTY" "GTK_IMAGE_ICON_NAME" "GTK_IMAGE_GICON"
               "GTK_IMAGE_PAINTABLE")
             (glib-test:list-enum-item-names "GtkImageType")))
  ;; Check values
  (is (equal '(0 1 2 3)
             (glib-test:list-enum-item-values "GtkImageType")))
  ;; Check nick names
  (is (equal '("empty" "icon-name" "gicon" "paintable")
             (glib-test:list-enum-item-nicks "GtkImageType")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkImageType" GTK:IMAGE-TYPE
                                    (:EXPORT T
                                     :TYPE-INITIALIZER "gtk_image_type_get_type")
                                    (:EMPTY 0)
                                    (:ICON-NAME 1)
                                    (:GICON 2)
                                    (:PAINTABLE 3))
             (gobject:get-gtype-definition "GtkImageType"))))

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
             (glib-test:list-children "GtkImage")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (glib-test:list-interfaces "GtkImage")))
  ;; Check class properties
  (is (equal '("file" "gicon" "icon-name" "icon-size" "paintable" "pixel-size"
               "resource" "storage-type" "use-fallback")
             (glib-test:list-properties "GtkImage")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkImage")))
  ;; Check CSS name
  (is (string= "image"
               (gtk:widget-class-css-name "GtkImage")))
  ;; Check accessible role
  (is (eq :img (gtk:widget-class-accessible-role "GtkImage")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkImage" GTK:IMAGE
                      (:SUPERCLASS GTK:WIDGET
                       :EXPORT T
                       :INTERFACES
                       ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
                       :TYPE-INITIALIZER "gtk_image_get_type")
                      ((FILE IMAGE-FILE "file" "gchararray" T T)
                       (GICON IMAGE-GICON "gicon" "GIcon" T T)
                       (ICON-NAME IMAGE-ICON-NAME "icon-name" "gchararray" T T)
                       (ICON-SIZE IMAGE-ICON-SIZE "icon-size" "GtkIconSize" T T)
                       (PAINTABLE IMAGE-PAINTABLE "paintable" "GdkPaintable" T T)
                       (PIXEL-SIZE IMAGE-PIXEL-SIZE "pixel-size" "gint" T T)
                       (RESOURCE IMAGE-RESOURCE "resource" "gchararray" T T)
                       (STORAGE-TYPE IMAGE-STORAGE-TYPE
                        "storage-type" "GtkImageType" T NIL)
                       (USE-FALLBACK IMAGE-USE-FALLBACK
                        "use-fallback" "gboolean" T T)))
             (gobject:get-gtype-definition "GtkImage"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-image-properties
  (glib-test:with-check-memory (image)
    (is (typep (setf image (make-instance 'gtk:image)) 'gtk:image))
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
  (glib-test:with-check-memory (image)
    (is (typep (setf image (gtk:image-new)) 'gtk:image))
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
  (glib-test:with-check-memory (image)
    (let ((path (glib-sys:sys-path "test/resource/gtk-logo-24.png")))
      (is (typep (setf image (gtk:image-new-from-file path)) 'gtk:image))
      (is (string= "gtk-logo-24"
                   (pathname-name (gtk:image-file image))))
      (is-false (gtk:image-gicon image))
      (is-false (gtk:image-icon-name image))
      (is (eq :inherit (gtk:image-icon-size image)))
      (is (typep (gtk:image-paintable image) 'gdk:texture))
      (is (= -1 (gtk:image-pixel-size image)))
      (is-false (gtk:image-resource image))
      (is (eq :paintable (gtk:image-storage-type image)))
      (is-false (gtk:image-use-fallback image))
      ;; Remove references
      (is-false (setf (gtk:image-paintable image) nil)))))

;;;     gtk_image_new_from_resource

(test gtk-image-new-from-resource
  (glib-test:with-check-memory (image)
    (let ((path (glib-sys:sys-path "test/rtest-resource.gresource")))
      (gio:with-resource (resource path)
        (is (typep (setf image (gtk:image-new-from-resource
                         "/com/crategus/test/gtk-logo-24.png")) 'gtk:image))
        (is-false (gtk:image-file image))
        (is-false (gtk:image-gicon image))
        (is-false (gtk:image-icon-name image))
        (is (eq :inherit (gtk:image-icon-size image)))
        (is (typep (gtk:image-paintable image) 'gdk:texture))
        (is (= -1 (gtk:image-pixel-size image)))
        (is (string= "/com/crategus/test/gtk-logo-24.png"
                     (gtk:image-resource image)))
        (is (eq :paintable (gtk:image-storage-type image)))
        (is-false (gtk:image-use-fallback image))
        ;; Remove references
        (is-false (setf (gtk:image-paintable image) nil))))))

;;;     gtk_image_new_from_pixbuf

(test gtk-image-new-from-pixbuf
  (glib-test:with-check-memory ((pixbuf 2) image :strong 1)
    (let* ((*gtk-warn-deprecated* nil)
           (path (glib-sys:sys-path "test/resource/gtk-logo-24.png")))
      (is (typep (setf pixbuf (gdk:pixbuf-new-from-file path)) 'gdk:pixbuf))
      (is (typep (setf image (gtk:image-new-from-pixbuf pixbuf)) 'gtk:image))
      (is (typep pixbuf 'gdk:pixbuf))
      (is-false (gtk:image-file image))
      (is-false (gtk:image-gicon image))
      (is-false (gtk:image-icon-name image))
      (is (eq :inherit (gtk:image-icon-size image)))
      (is (typep (gtk:image-paintable image) 'gdk:texture))
      (is (= -1 (gtk:image-pixel-size image)))
      (is-false (gtk:image-resource image))
      (is (eq :paintable (gtk:image-storage-type image)))
      (is-false (gtk:image-use-fallback image))
      ;; Remove references
      (is-false (gtk:image-clear image)))))

;;;     gtk_image_new_from_paintable

(test gtk-image-new-from-paintable
  (glib-test:with-check-memory (paintable image)
    (let ((path (glib-sys:sys-path "test/resource/gtk-logo-24.png")))
      (is (typep (setf paintable
                       (gdk:texture-new-from-filename path)) 'gdk:paintable))
      (is (typep (setf image
                       (gtk:image-new-from-paintable paintable)) 'gtk:image))
    (is-false (gtk:image-file image))
    (is-false (gtk:image-gicon image))
    (is-false (gtk:image-icon-name image))
    (is (eq :inherit (gtk:image-icon-size image)))
    (is (typep (gtk:image-paintable image) 'gdk:texture))
    (is (= -1 (gtk:image-pixel-size image)))
    (is-false (gtk:image-resource image))
    (is (eq :paintable (gtk:image-storage-type image)))
    (is-false (gtk:image-use-fallback image))
    ;; Remove references
    (is-false (setf (gtk:image-paintable image) nil)))))

;;;     gtk_image_new_from_icon_name

(test gtk-image-new-from-icon-name
  (glib-test:with-check-memory (image)
    (is (typep (setf image
                     (gtk:image-new-from-icon-name "window-close")) 'gtk:image))
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
  (glib-test:with-check-memory (gicon image)
    (is (typep (setf gicon (g:icon-new-for-string "window-close")) 'g:icon))
    (is (typep (setf image (gtk:image-new-from-gicon gicon)) 'gtk:image))
    (is-false (gtk:image-file image))
    (is (typep (gtk:image-gicon image) 'g:themed-icon))
    (is-false (gtk:image-icon-name image))
    (is (eq :inherit (gtk:image-icon-size image)))
    (is-false (gtk:image-paintable image))
    (is (= -1 (gtk:image-pixel-size image)))
    (is-false (gtk:image-resource image))
    (is (eq :gicon (gtk:image-storage-type image)))
    (is-false (gtk:image-use-fallback image))
    ;; Remove references
    (is-false (setf (gtk:image-gicon image) nil))))

;;;     gtk_image_clear

(test gtk-image-clear
  (glib-test:with-check-memory (image)
    (let ((path (glib-sys:sys-path "test/resource/gtk-logo-24.png")))
      (is (typep (setf image (gtk:image-new-from-file path)) 'gtk:image))
      (is (eq :paintable (gtk:image-storage-type image)))
      ;; Clear the image
      (is-false (gtk:image-clear image))
      (is (eq :empty (gtk:image-storage-type image))))))

;;;     gtk_image_set_from_file

(test gtk-image-set-from-file
  (glib-test:with-check-memory (image)
    (let ((path (glib-sys:sys-path "test/resource/gtk-logo-24.png")))
      (is (typep (setf image (gtk:image-new)) 'gtk:image))
      (is-false (gtk:image-set-from-file image path))
      (is (string= "gtk-logo-24"
                   (pathname-name (gtk:image-file image))))
      (is-false (gtk:image-gicon image))
      (is-false (gtk:image-icon-name image))
      (is (eq :inherit (gtk:image-icon-size image)))
      (is (typep (gtk:image-paintable image) 'gdk:texture))
      (is (= -1 (gtk:image-pixel-size image)))
      (is-false (gtk:image-resource image))
      (is (eq :paintable (gtk:image-storage-type image)))
      (is-false (gtk:image-use-fallback image))
      ;; Remove references
      (is-false (setf (gtk:image-paintable image) nil)))))

;;;     gtk_image_set_from_resource

(test gtk-image-set-from-resource
  (glib-test:with-check-memory (image)
    (let ((path (glib-sys:sys-path "test/rtest-resource.gresource")))
      (is (typep (setf image (gtk:image-new)) 'gtk:image))
      (gio:with-resource (resource path)
        (is-false (gtk:image-set-from-resource
                          image
                          "/com/crategus/test/gtk-logo-24.png"))
        (is-false (gtk:image-file image))
        (is-false (gtk:image-gicon image))
        (is-false (gtk:image-icon-name image))
        (is (eq :inherit (gtk:image-icon-size image)))
        (is (typep (gtk:image-paintable image) 'gdk:texture))
        (is (= -1 (gtk:image-pixel-size image)))
        (is (string= "/com/crategus/test/gtk-logo-24.png"
                     (gtk:image-resource image)))
        (is (eq :paintable (gtk:image-storage-type image)))
        (is-false (gtk:image-use-fallback image))
        ;; Remove references
        (is-false (setf (gtk:image-paintable image) nil))))))

;;;     gtk_image_set_from_pixbuf

(test gtk-image-set-from-pixbuf
  (glib-test:with-check-memory ((pixbuf 2) image :strong 1)
    (let ((*gtk-warn-deprecated* nil)
          (path (glib-sys:sys-path "test/resource/gtk-logo-24.png")))
      (is (typep (setf pixbuf (gdk:pixbuf-new-from-file path)) 'gdk:pixbuf))
      (is (typep (setf image (gtk:image-new)) 'gtk:image))
      (is-false (gtk:image-set-from-pixbuf image pixbuf))
      (is-false (gtk:image-file image))
      (is-false (gtk:image-gicon image))
      (is-false (gtk:image-icon-name image))
      (is (eq :inherit (gtk:image-icon-size image)))
      (is (typep (gtk:image-paintable image) 'gdk:texture))
      (is (= -1 (gtk:image-pixel-size image)))
      (is-false (gtk:image-resource image))
      (is (eq :paintable (gtk:image-storage-type image)))
      (is-false (gtk:image-use-fallback image))
      ;; Remove references
      (is-false (setf (gtk:image-paintable image) nil)))))

;;;     gtk_image_set_from_paintable

(test gtk-image-set-from-paintable
  (glib-test:with-check-memory (paintable image)
    (let ((path (glib-sys:sys-path "test/resource/gtk-logo-24.png")))
      (is (typep (setf paintable
                       (gdk:texture-new-from-filename path)) 'gdk:paintable))
      (is (typep (setf image (gtk:image-new)) 'gtk:image))
      (is-false (gtk:image-set-from-paintable image paintable))
      (is-false (gtk:image-file image))
      (is-false (gtk:image-gicon image))
      (is-false (gtk:image-icon-name image))
      (is (eq :inherit (gtk:image-icon-size image)))
      (is (typep (gtk:image-paintable image) 'gdk:texture))
      (is (= -1 (gtk:image-pixel-size image)))
      (is-false (gtk:image-resource image))
      (is (eq :paintable (gtk:image-storage-type image)))
      (is-false (gtk:image-use-fallback image))
      ;; Remove references
      (is-false (setf (gtk:image-paintable image) nil)))))

;;;     gtk_image_set_from_icon_name

(test gtk-image-set-from-icon-name
  (glib-test:with-check-memory (image)
    (is (typep (setf image (gtk:image-new)) 'gtk:image))
    (is-false (gtk:image-set-from-icon-name image "window-close"))
    (is-false (gtk:image-file image))
    (is-false (gtk:image-gicon image))
    (is (string= "window-close" (gtk:image-icon-name image)))
    (is (eq :inherit (gtk:image-icon-size image)))
    (is-false (gtk:image-paintable image))
    (is (= -1 (gtk:image-pixel-size image)))
    (is-false (gtk:image-resource image))
    (is (eq :icon-name (gtk:image-storage-type image)))
    (is-false (gtk:image-use-fallback image))))

;;;     gtk_image_set_from_gicon

(test gtk-image-set-from-gicon
  (glib-test:with-check-memory (gicon image)
    (is (typep (setf gicon (g:icon-new-for-string "window-close")) 'g:icon))
    (is (typep (setf image (gtk:image-new)) 'gtk:image))
    (is-false (gtk:image-set-from-gicon image gicon))
    (is-false (gtk:image-file image))
    (is (typep (gtk:image-gicon image) 'g:themed-icon))
    (is-false (gtk:image-icon-name image))
    (is (eq :inherit (gtk:image-icon-size image)))
    (is-false (gtk:image-paintable image))
    (is (= -1 (gtk:image-pixel-size image)))
    (is-false (gtk:image-resource image))
    (is (eq :gicon (gtk:image-storage-type image)))
    (is-false (gtk:image-use-fallback image))
    ;; Remove references
    (is-false (setf (gtk:image-gicon image) nil))))

;;; 2025-4-26

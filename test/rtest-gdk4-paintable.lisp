(in-package :gtk-test)

(def-suite gdk-paintable :in gdk-suite)
(in-suite gdk-paintable)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkPaintableFlags

(test gdk-paintable-flags
  ;; Check type
  (is (g:type-is-flags "GdkPaintableFlags"))
  ;; Check registered name
  (is (eq 'gdk:paintable-flags
          (glib:symbol-for-gtype "GdkPaintableFlags")))
  ;; Check type initializer
  (is (eq (g:gtype "GdkPaintableFlags")
          (g:gtype (cffi:foreign-funcall "gdk_paintable_flags_get_type" :size))))
  ;; Check names
  (is (equal '("GDK_PAINTABLE_STATIC_SIZE" "GDK_PAINTABLE_STATIC_CONTENTS")
             (glib-test:list-flags-item-names "GdkPaintableFlags")))
  ;; Check values
  (is (equal '(1 2)
             (glib-test:list-flags-item-values "GdkPaintableFlags")))
  ;; Check nick names
  (is (equal '("size" "contents")
             (glib-test:list-flags-item-nicks "GdkPaintableFlags")))
  ;; Check flags definition
  (is (equal '(GOBJECT:DEFINE-GFLAGS "GdkPaintableFlags" GDK:PAINTABLE-FLAGS
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                       "gdk_paintable_flags_get_type")
                                      (:SIZE 1)
                                      (:CONTENTS 2))
             (gobject:get-gtype-definition "GdkPaintableFlags"))))

;;;     GdkSnapshot

(test gdk-snapshot-class
  ;; Check type
  (is (g:type-is-object "GdkSnapshot"))
  ;; Check registered name
  (is (eq 'gdk:snapshot
          (glib:symbol-for-gtype "GdkSnapshot")))
  ;; Check type initializer
  (is (eq (g:gtype "GdkSnapshot")
          (g:gtype (cffi:foreign-funcall "gdk_snapshot_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GdkSnapshot")))
  ;; Check children
  (is (equal '("GtkSnapshot")
             (glib-test:list-children "GdkSnapshot")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GdkSnapshot")))
  ;; Check properties
  (is (equal '()
             (glib-test:list-properties "GdkSnapshot")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GdkSnapshot")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GdkSnapshot" GDK:SNAPSHOT
                       (:SUPERCLASS G:OBJECT
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gdk_snapshot_get_type")
                       NIL)
             (gobject:get-gtype-definition "GdkSnapshot"))))

;;;     GdkPaintable

(test gdk-paintable-interface
  ;; Check type
  (is (g:type-is-interface "GdkPaintable"))
  ;; Check registered name
  (is (eq 'gdk:paintable
          (glib:symbol-for-gtype "GdkPaintable")))
  ;; Check type initializer
  (is (eq (g:gtype "GdkPaintable")
          (g:gtype (cffi:foreign-funcall "gdk_paintable_get_type" :size))))
  ;; Check interface prerequisites
  (is (equal '("GObject")
             (glib-test:list-interface-prerequisites "GdkPaintable")))
  ;; Check interface properties
  (is (equal '()
             (glib-test:list-interface-properties "GdkPaintable")))
  ;; Check signals
  (is (equal '("invalidate-contents" "invalidate-size")
             (glib-test:list-signals "GdkPaintable")))
  ;; Check interface definition
  (is (equal '(GOBJECT:DEFINE-GINTERFACE "GdkPaintable" GDK:PAINTABLE
                       (:EXPORT T
                        :TYPE-INITIALIZER "gdk_paintable_get_type"))
             (gobject:get-gtype-definition "GdkPaintable"))))

;;; --- Signals ----------------------------------------------------------------

;;;     invalidate-contents
;;;     invalidate-size

;;; --- Functions --------------------------------------------------------------

;;;     gdk_paintable_get_current_image
;;;     gdk_paintable_snapshot

;;;     gdk_paintable_get_flags

(test gdk-paintable-flags
  (let* ((path (glib-sys:sys-path "test/resource/ducky.png"))
         (paintable (gdk:texture-new-from-filename path)))
    (is (equal '(:static-size :static-contents)
               (gdk:paintable-flags paintable)))))

;;;     gdk_paintable_get_intrinsic_width
;;;     gdk_paintable_get_intrinsic_height
;;;     gdk_paintable_get_intrinsic_aspect_ratio

(test gdk-paintable-intrinsic-width/height/aspect-ratio
  (let* ((path (glib-sys:sys-path "test/resource/ducky.png"))
         (paintable (gdk:texture-new-from-filename path)))
    (is (= 489 (gdk:paintable-intrinsic-width paintable)))
    (is (= 537 (gdk:paintable-intrinsic-height paintable)))
    (is (approx-equal 0.91061d0
                      (gdk:paintable-intrinsic-aspect-ratio paintable)))))

;;;     gdk_paintable_compute_concrete_size
;;;     gdk_paintable_invalidate_contents
;;;     gdk_paintable_invalidate_size
;;;     gdk_paintable_new_empty

;;; 2024-9-19

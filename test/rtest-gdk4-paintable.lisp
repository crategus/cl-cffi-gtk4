(in-package :gtk-test)

(def-suite gdk-paintable :in gdk-suite)
(in-suite gdk-paintable)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkPaintableInterface
;;;     GdkPaintableFlags
;;;     GdkSnapshot

;;;     GdkPaintable

(test gdk-paintable-interface
  ;; Type check
  (is (g:type-is-interface "GdkPaintable"))
  ;; Check the registered name
  (is (eq 'gdk:paintable
          (glib:symbol-for-gtype "GdkPaintable")))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkPaintable")
          (g:gtype (cffi:foreign-funcall "gdk_paintable_get_type" :size))))
  ;; Check the interface prerequisites
  (is (equal '("GObject")
             (list-interface-prerequisites "GdkPaintable")))
  ;; Check the interface properties
  (is (equal '()
             (list-interface-properties "GdkPaintable")))
  ;; Check the signals
  (is (equal '("invalidate-contents" "invalidate-size")
             (list-signals "GdkPaintable")))
  ;; Get the interface definition
  (is (equal '(GOBJECT:DEFINE-G-INTERFACE "GdkPaintable" GDK-PAINTABLE
                            (:EXPORT T :TYPE-INITIALIZER
                             "gdk_paintable_get_type"))
             (gobject:get-g-type-definition "GdkPaintable"))))

;;; --- Signals ----------------------------------------------------------------

;;;     invalidate-contents
;;;     invalidate-size

;;; --- Functions --------------------------------------------------------------

;;;     gdk_paintable_get_current_image
;;;     gdk_paintable_snapshot
;;;     gdk_paintable_get_flags
;;;     gdk_paintable_get_intrinsic_width
;;;     gdk_paintable_get_intrinsic_height
;;;     gdk_paintable_get_intrinsic_aspect_ratio
;;;     gdk_paintable_compute_concrete_size
;;;     gdk_paintable_invalidate_contents
;;;     gdk_paintable_invalidate_size
;;;     gdk_paintable_new_empty

;;; --- 2023-7-30 --------------------------------------------------------------

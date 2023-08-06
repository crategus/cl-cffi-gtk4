(in-package :gtk-test)

(def-suite gdk-paintable :in gdk-suite)
(in-suite gdk-paintable)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkPaintableFlags

(test gdk-paintable-flags
  ;; Check the type
  (is (g:type-is-flags "GdkPaintableFlags"))
  ;; Check the registered name
  (is (eq 'gdk:paintable-flags
          (glib:symbol-for-gtype "GdkPaintableFlags")))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkPaintableFlags")
          (g:gtype (cffi:foreign-funcall "gdk_paintable_flags_get_type" 
                                         :size))))
  ;; Check the names
  (is (equal '("GDK_PAINTABLE_STATIC_SIZE" "GDK_PAINTABLE_STATIC_CONTENTS")
             (list-flags-item-name "GdkPaintableFlags")))
  ;; Check the values
  (is (equal '(1 2)
             (list-flags-item-value "GdkPaintableFlags")))
  ;; Check the nick names
  (is (equal '("size" "contents")
             (list-flags-item-nick "GdkPaintableFlags")))
  ;; Check the flags definition
  (is (equal '(GOBJECT:DEFINE-G-FLAGS "GdkPaintableFlags"
                                      GDK-PAINTABLE-FLAGS
                                      (:EXPORT T 
                                       :TYPE-INITIALIZER
                                       "gdk_paintable_flags_get_type")
                                      (:SIZE 1)
                                      (:CONTENTS 2))
             (gobject:get-g-type-definition "GdkPaintableFlags"))))

;;;     GdkSnapshot

(test gdk-snapshot-class
  ;; Type check
  (is (g:type-is-object "GdkSnapshot"))
  ;; Check the registered name
  (is (eq 'gdk:snapshot
          (glib:symbol-for-gtype "GdkSnapshot")))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkSnapshot")
          (g:gtype (cffi:foreign-funcall "gdk_snapshot_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GdkSnapshot")))
  ;; Check the children
  (is (equal '("GtkSnapshot")
             (list-children "GdkSnapshot")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GdkSnapshot")))
  ;; Check the properties
  (is (equal '()
             (list-properties "GdkSnapshot")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GdkSnapshot")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GdkSnapshot" GDK-SNAPSHOT
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                                :TYPE-INITIALIZER "gdk_snapshot_get_type")
                               NIL)
             (gobject:get-g-type-definition "GdkSnapshot"))))

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

(test gdk-paintable-flags
  (let* ((path (sys-path "resource/ducky.png"))
         (paintable (gdk:texture-new-from-filename path)))
    (is (equal '(:static-size :static-contents) 
               (gdk:paintable-flags paintable)))))

;;;     gdk_paintable_get_intrinsic_width
;;;     gdk_paintable_get_intrinsic_height
;;;     gdk_paintable_get_intrinsic_aspect_ratio

(test gdk-paintable-intrinsic-width/height/aspect-ratio
  (let* ((path (sys-path "resource/ducky.png"))
         (paintable (gdk:texture-new-from-filename path)))
    (is (= 489 (gdk:paintable-intrinsic-width paintable)))
    (is (= 537 (gdk:paintable-intrinsic-height paintable)))
    (is (approx-equal 0.9d0 
                      (gdk:paintable-intrinsic-aspect-ratio paintable)))))

;;;     gdk_paintable_compute_concrete_size
;;;     gdk_paintable_invalidate_contents
;;;     gdk_paintable_invalidate_size
;;;     gdk_paintable_new_empty

;;; --- 2023-7-30 --------------------------------------------------------------

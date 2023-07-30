(in-package :gtk-test)

(def-suite gdk-drag-surface :in gdk-suite)
(in-suite gdk-drag-surface)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkDragSurface

(test drag-surface-interface
  ;; Type check
  (is (g:type-is-interface "GdkDragSurface"))
  ;; Check the registered name
  (is (eq 'gdk:drag-surface
          (glib:symbol-for-gtype "GdkDragSurface")))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkDragSurface")
          (g:gtype (cffi:foreign-funcall "gdk_drag_surface_get_type" :size))))
  ;; Check interface Prerequisites
  (is (equal '("GdkSurface")
             (list-interface-prerequisites "GdkDragSurface")))
  ;; Get the names of the interface properties.
  (is (equal '()
             (list-interface-properties "GdkDragSurface")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GdkDragSurface")))
  ;; Get the interface definition
  (is (equal '(GOBJECT:DEFINE-G-INTERFACE "GdkDragSurface" GDK-DRAG-SURFACE
                            (:EXPORT T :TYPE-INITIALIZER
                             "gdk_drag_surface_get_type"))
             (gobject:get-g-type-definition "GdkDragSurface"))))

;;; --- Functions --------------------------------------------------------------

;;;     gdk_drag_surface_present

;;; --- 2023-7-30 --------------------------------------------------------------

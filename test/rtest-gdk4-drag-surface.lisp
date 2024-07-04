(in-package :gtk-test)

(def-suite gdk-drag-surface :in gdk-suite)
(in-suite gdk-drag-surface)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkDragSurfaceSize

;;;     GdkDragSurface

(test gdk-drag-surface-interface
  ;; Check type
  (is (g:type-is-interface "GdkDragSurface"))
  ;; Check registered name
  (is (eq 'gdk:drag-surface
          (glib:symbol-for-gtype "GdkDragSurface")))
  ;; Check type initializer
  (is (eq (g:gtype "GdkDragSurface")
          (g:gtype (cffi:foreign-funcall "gdk_drag_surface_get_type" :size))))
  ;; Check interface Prerequisites
  (is (equal '("GdkSurface")
             (gtk-test:list-interface-prerequisites "GdkDragSurface")))
  ;; Check interface properties
  (is (equal '()
             (gtk-test:list-interface-properties "GdkDragSurface")))
  ;; Check signals
  (is (equal '("compute-size")
             (gtk-test:list-signals "GdkDragSurface")))
  ;; Check interface definition
  (is (equal '(GOBJECT:DEFINE-G-INTERFACE "GdkDragSurface" GDK-DRAG-SURFACE
                            (:EXPORT T :TYPE-INITIALIZER
                             "gdk_drag_surface_get_type"))
             (gobject:get-g-type-definition "GdkDragSurface"))))

;;; --- Signals ----------------------------------------------------------------

;;;     compute-size

(test gtk-drag-surface-compute-size-signal
  (let ((query (g:signal-query (g:signal-lookup "compute-size"
                                                "GdkDragSurface"))))
    (is (string= "compute-size" (g:signal-query-signal-name query)))
    (is (string= "GdkDragSurface"
                 (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '("GdkDragSurfaceSize")
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

;;; --- Functions --------------------------------------------------------------

;;;     gdk_drag_surface_present
;;;     gdk_drag_surface_size_set_size

;;; 2024-7-4

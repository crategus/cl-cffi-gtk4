(in-package :gtk-test)

(def-suite gdk-drag-surface :in gdk-suite)
(in-suite gdk-drag-surface)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkDragSurfaceSize

;;;     GdkDragSurface

(test gdk-drag-surface-interface
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
  (is (equal '("compute-size")
             (list-signals "GdkDragSurface")))
  ;; Get the interface definition
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

;;; --- 2023-11-4 --------------------------------------------------------------

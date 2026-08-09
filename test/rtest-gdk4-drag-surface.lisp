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
             (glib-test:list-interface-prerequisites "GdkDragSurface")))
  ;; Check interface properties
  (is (equal '()
             (glib-test:list-interface-properties "GdkDragSurface")))
  ;; Check signals
  (is (equal '("compute-size")
             (glib-test:list-signals "GdkDragSurface")))
  ;; Check interface definition
  (is (equal '(GOBJECT:DEFINE-GINTERFACE "GdkDragSurface" GDK:DRAG-SURFACE
                      (:EXPORT T
                       :TYPE-INITIALIZER "gdk_drag_surface_get_type"))
             (gobject:get-gtype-definition "GdkDragSurface"))))

;;; --- Signals ----------------------------------------------------------------

;;;     compute-size

(test gdk-drag-surface-compute-size-signal
  (let* ((name "compute-size")
         (gtype (g:gtype "GdkDragSurface"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("GdkDragSurfaceSize")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Functions --------------------------------------------------------------

;;;     gdk_drag_surface_present
;;;     gdk_drag_surface_size_set_size

;;; 2026-07-24

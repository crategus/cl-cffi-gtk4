(in-package :gtk-test)

(def-suite gsk-renderer :in gsk-suite)
(in-suite gsk-renderer)

;; TODO: The class GskGpuRenderer is used, but not documented and not
;; implemented for Lisp.
;;
;; Check the implementation of the classes and documentation again.

;;; --- Types and Values -------------------------------------------------------

;;;     GskRenderer

(test gsk-renderer-class
  ;; Check type
  (is (g:type-is-object "GskRenderer"))
  ;; Check registered name
  (is (eq 'gsk:renderer
          (glib:symbol-for-gtype "GskRenderer")))
  ;; Check type initializer
  (is (eq (g:gtype "GskRenderer")
          (g:gtype (cffi:foreign-funcall "gsk_renderer_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GskRenderer")))
  ;; Check children
  #-gtk-4-18
  (is (equal '("GskCairoRenderer" "GskGLRenderer" "GskGpuRenderer")
             (glib-test:list-children "GskRenderer")))
  #+gtk-4-18
  (is (equal '("GskCairoRenderer" "GskGpuRenderer" "GskNglRenderer")
             (glib-test:list-children "GskRenderer")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GskRenderer")))
  ;; Check properties
  (is (equal '("realized" "surface")
             (glib-test:list-properties "GskRenderer")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GskRenderer")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GskRenderer" GSK:RENDERER
                       (:SUPERCLASS GOBJECT:OBJECT
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gsk_renderer_get_type")
                       ((REALIZED RENDERER-REALIZED "realized" "gboolean" T
                         NIL)
                        (SURFACE RENDERER-SURFACE "surface" "GdkSurface" T
                         NIL)))
             (gobject:get-gtype-definition "GskRenderer"))))

;;;     GskCairoRenderer

(test gsk-cairo-renderer-class
  ;; Check type
  (is (g:type-is-object "GskCairoRenderer"))
  ;; Check registered name
  (is (eq 'gsk:cairo-renderer
          (glib:symbol-for-gtype "GskCairoRenderer")))
  ;; Check type initializer
  (is (eq (g:gtype "GskCairoRenderer")
          (g:gtype (cffi:foreign-funcall "gsk_cairo_renderer_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GskRenderer")
          (g:type-parent "GskCairoRenderer")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GskCairoRenderer")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GskCairoRenderer")))
  ;; Check properties
  (is (equal '()
             (glib-test:list-properties "GskCairoRenderer")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GskCairoRenderer")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GskCairoRenderer" GSK:CAIRO-RENDERER
                       (:SUPERCLASS GSK:RENDERER
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gsk_cairo_renderer_get_type")
                       NIL)
             (gobject:get-gtype-definition "GskCairoRenderer"))))

;;;     GskGLRenderer

(test gsk-gl-renderer-class
  ;; Check type
  (is (g:type-is-object "GskGLRenderer"))
  ;; Check registered name
  (is (eq 'gsk:gl-renderer
          (glib:symbol-for-gtype "GskGLRenderer")))
  ;; Check type initializer
  (is (eq (g:gtype "GskGLRenderer")
          (g:gtype (cffi:foreign-funcall "gsk_gl_renderer_get_type" :size))))
  ;; Check parent
  #-gtk-4-18
  (is (eq (g:gtype "GskRenderer")
          (g:type-parent "GskGLRenderer")))
  #+gtk-4-18
  (is (eq (g:gtype "GskGpuRenderer")
          (g:type-parent "GskGLRenderer")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GskGLRenderer")))
  ;; Check interfaces
  (is (equal '("GdkDmabufDownloader")
             (glib-test:list-interfaces "GskGLRenderer")))
  ;; Check properties
  (is (equal '()
             (glib-test:list-properties "GskGLRenderer")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GskGLRenderer")))
  ;; Check class definition
  #-gtk-4-18
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GskGLRenderer" GSK:GL-RENDERER
                       (:SUPERCLASS GSK:RENDERER
                        :EXPORT T
                        :INTERFACES ("GdkDmabufDownloader"))
                       NIL)
             (gobject:get-gtype-definition "GskGLRenderer")))
  #+gtk-4-18
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GskGLRenderer" GSK:GL-RENDERER
                       (:SUPERCLASS GSK-GPU-RENDERER
                        :EXPORT T
                        :INTERFACES ("GdkDmabufDownloader"))
                       NIL)
             (gobject:get-gtype-definition "GskGLRenderer"))))

;;;     GskNGLRenderer

#-gtk-4-18
(test gsk-ngl-renderer-class
  ;; Check type
  (is (g:type-is-object "GskNglRenderer"))
  ;; Check registered name
  (is (eq 'gsk:ngl-renderer
          (glib:symbol-for-gtype "GskNglRenderer")))
  ;; Check type initializer
  (is (eq (g:gtype "GskNglRenderer")
          (g:gtype (cffi:foreign-funcall "gsk_ngl_renderer_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GskGpuRenderer")
          (g:type-parent "GskNglRenderer")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GskNglRenderer")))
  ;; Check interfaces
  (is (equal '("GdkDmabufDownloader")
             (glib-test:list-interfaces "GskNglRenderer")))
  ;; Check properties
  (is (equal '()
             (glib-test:list-properties "GskNglRenderer")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GskNglRenderer")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GskNglRenderer" GSK:NGL-RENDERER
                       (:SUPERCLASS GSK-GPU-RENDERER
                        :EXPORT T
                        :INTERFACES ("GdkDmabufDownloader")
                        :TYPE-INITIALIZER "gsk_ngl_renderer_get_type")
                       NIL)
             (gobject:get-gtype-definition "GskNglRenderer"))))

#+gtk-4-18
(test gsk-ngl-renderer-class
  ;; Check type
  (is (g:type-is-object "GskNglRenderer"))
  ;; Check registered name
  (is (eq 'gsk:ngl-renderer
          (glib:symbol-for-gtype "GskNglRenderer")))
  ;; Check type initializer
  (is (eq (g:gtype "GskNglRenderer")
          (g:gtype (cffi:foreign-funcall "gsk_ngl_renderer_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GskRenderer")
          (g:type-parent "GskNglRenderer")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GskNglRenderer")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GskNglRenderer")))
  ;; Check properties
  (is (equal '()
             (glib-test:list-properties "GskNglRenderer")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GskNglRenderer")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GskNglRenderer" GSK:NGL-RENDERER
                       (:SUPERCLASS GSK:RENDERER
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gsk_ngl_renderer_get_type")
                       NIL)
             (gobject:get-gtype-definition "GskNglRenderer"))))

;;; --- Properties -------------------------------------------------------------

;;;     realized
;;;     surface

(test gsk-renderer-properties
  (when *first-run-testsuite*
    (glib-test:with-check-memory (surface renderer :strong 1)
      (setf surface (gdk:surface-new-toplevel (gdk:display-default)))
      (setf renderer (gsk:renderer-new-for-surface surface))
      (is-true (gsk:renderer-realized renderer))
      (is (typep (gsk:renderer-surface renderer) 'gdk:surface))
      ;; Remove references
      (is-false (gsk:renderer-unrealize renderer))
      (is-false (gsk:renderer-is-realized renderer))
      (is-false (gdk:surface-destroy surface)))))

;;; --- Functions --------------------------------------------------------------

;;;     gsk_renderer_new_for_surface

(test gsk-renderer-new-for-surface
  (when *first-run-testsuite*
    (glib-test:with-check-memory (surface renderer :strong 1)
      (setf surface (gdk:surface-new-toplevel (gdk:display-default)))
      (is (typep (setf renderer
                       (gsk:renderer-new-for-surface surface)) 'gsk:renderer))
      ;; Remove references
      (is-false (gsk:renderer-unrealize renderer))
      (is-false (gsk:renderer-is-realized renderer))
      (is-false (gdk:surface-destroy surface)))))

;;;     gsk_renderer_realize
;;;     gsk_renderer_unrealize
;;;     gsk_renderer_is_realized

(test gsk-renderer-is-realized
  (when *first-run-testsuite*
    (glib-test:with-check-memory (surface renderer :strong 1)
      (setf surface (gdk:surface-new-toplevel (gdk:display-default)))
      (setf renderer (gsk:renderer-new-for-surface surface))
      (is-true (gsk:renderer-is-realized renderer))
      ;; Remove references
      (is-false (gsk:renderer-unrealize renderer))
      (is-false (gsk:renderer-is-realized renderer))
      (is-false (gdk:surface-destroy surface)))))

;;;     gsk_renderer_render
;;;     gsk_renderer_render_texture

;;;     gsk_cairo_renderer_new

(test gsk-cairo-renderer-new
  (glib-test:with-check-memory (renderer)
    (is (typep (setf renderer
                     (gsk:cairo-renderer-new)) 'gsk:cairo-renderer))
    ;; Check memory management
    (is-false (gsk:renderer-is-realized renderer))))

;;;     gsk_gl_renderer_new
;;;     gsk_ngl_renderer_new

;;; 2025-10-26

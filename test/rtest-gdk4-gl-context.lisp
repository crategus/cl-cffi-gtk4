(in-package :gtk-test)

(def-suite gdk-gl-context :in gdk-suite)
(in-suite gdk-gl-context)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkGLAPI

(test gdk-gl-api
  ;; Check type
  (is (g:type-is-flags "GdkGLAPI"))
  ;; Check registered name
  (is (eq 'gdk:gl-api
          (glib:symbol-for-gtype "GdkGLAPI")))
  ;; Check type initializer
  (is (eq (g:gtype "GdkGLAPI")
          (g:gtype (cffi:foreign-funcall "gdk_gl_api_get_type" :size))))
  ;; Check names
  (is (equal '("GDK_GL_API_GL" "GDK_GL_API_GLES")
             (glib-test:list-flags-item-names "GdkGLAPI")))
  ;; Check values
  (is (equal '(1 2)
             (glib-test:list-flags-item-values "GdkGLAPI")))
  ;; Check nick names
  (is (equal '("gl" "gles")
             (glib-test:list-flags-item-nicks "GdkGLAPI")))
  ;; Check flags definition
  (is (equal '(GOBJECT:DEFINE-GFLAGS "GdkGLAPI" GDK:GL-API
                       (:EXPORT T)
                       (:GL 1)
                       (:GLES 2))
             (gobject:get-gtype-definition "GdkGLAPI"))))

;;;     GdkGLContext

(test gdk-cl-context-class
  ;; Check type
  (is (g:type-is-object "GdkGLContext"))
  ;; Check registered name
  (is (eq 'gdk:gl-context
          (glib:symbol-for-gtype "GdkGLContext")))
  ;; Check type initializer
  (is (eq (g:gtype "GdkGLAPI")
          (g:gtype (cffi:foreign-funcall "gdk_gl_api_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GdkDrawContext")
          (g:type-parent "GdkGLContext")))
  ;; Check children
  (is (equal '("GdkWaylandGLContext" "GdkX11GLContext")
             (glib-test:list-children "GdkGLContext")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GObject")))
  ;; Check properties
  (is (equal '("allowed-apis" "api" "shared-context")
             (glib-test:list-properties "GdkGLContext")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GdkGLContext")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GdkGLContext" GDK:GL-CONTEXT
                       (:SUPERCLASS GDK:DRAW-CONTEXT
                        :EXPORT T
                        :INTERFACES NIL)
                       ((ALLOWED-APIS GL-CONTEXT-ALLOWED-APIS "allowed-apis"
                         "GdkGLAPI" T T)
                        (API GL-CONTEXT-API "api" "GdkGLAPI" T NIL)
                        (SHARED-CONTEXT GL-CONTEXT-SHARED-CONTEXT
                         "shared-context" "GdkGLContext" T NIL)))
             (gobject:get-gtype-definition "GdkGLContext"))))

;;; --- Properties -------------------------------------------------------------

;;;     allowed-apis                                       Since 4.6
;;;     api                                                Since 4.6
;;;     shared-context                                     Deprecated 4.4

;;; --- Functions --------------------------------------------------------------

;;;     gdk_gl_context_get_display
;;;     gdk_gl_context_get_surface
;;;     gdk_gl_context_get_version
;;;     gdk_gl_context_set_required_version
;;;     gdk_gl_context_get_required_version
;;;     gdk_gl_context_set_debug_enabled
;;;     gdk_gl_context_get_debug_enabled
;;;     gdk_gl_context_set_forward_compatible
;;;     gdk_gl_context_get_forward_compatible
;;;     gdk_gl_context_set_use_es
;;;     gdk_gl_context_get_use_es
;;;     gdk_gl_context_is_legacy
;;;     gdk_gl_context_realize
;;;     gdk_gl_context_make_current
;;;     gdk_gl_context_get_current
;;;     gdk_gl_context_clear_current

;;;     gdk_gl_context_is_shared                           Since 4.4

;;; 2024-11-29

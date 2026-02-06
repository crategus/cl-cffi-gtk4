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
  #-windows
  (is (subsetp (glib-test:list-children "GdkGLContext")
               '("GdkWaylandGLContext" "GdkX11GLContext") :test #'string=))
  #+windows
  (is (equal '("GdkWin32GLContext")
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

(test gdk-gl-context-properties
  (glib-test:with-check-memory ((surface 2) context :strong 2)
    (is (typep (setf surface
                     (gdk:surface-new-toplevel *display-default*))
               'gdk:surface))
    (is (typep (setf context
                     (gdk:surface-create-gl-context surface)) 'gdk:gl-context))
    (is (equal '(:GL :GLES) (gdk:gl-context-allowed-apis context)))
    (is-false (gdk:gl-context-api context))
    (is-false (gdk:gl-context-shared-context context))
    ;; Remove references
    (is-false (gdk:surface-destroy surface))))

;;; --- Functions --------------------------------------------------------------

;;;     gdk_gl_context_get_display
;;;     gdk_gl_context_get_surface
;;;     gdk_gl_context_get_version
;;;     gdk_gl_context_realize

(test gdk-gl-context-display/surface/version
  (glib-test:with-check-memory ((surface 2) context :strong 2)
    (is (typep (setf surface
                     (gdk:surface-new-toplevel *display-default*))
               'gdk:surface))
    (is (typep (setf context
                     (gdk:surface-create-gl-context surface)) 'gdk:gl-context))
    (is-true (gdk:gl-context-realize context))
    (is (eq *display-default* (gdk:gl-context-display context)))
    (is (eq surface (gdk:gl-context-surface context)))
    (is (= 3 (gdk:gl-context-version context)))
    ;; Remove references
    (is-false (gdk:surface-destroy surface))))

;;;     gdk_gl_context_set_required_version
;;;     gdk_gl_context_get_required_version

(test gdk-gl-context-required-version
  (glib-test:with-check-memory ((surface 2) context :strong 2)
    (is (typep (setf surface
                     (gdk:surface-new-toplevel *display-default*))
               'gdk:surface))
    (is (typep (setf context
                     (gdk:surface-create-gl-context surface)) 'gdk:gl-context))
    (is (equal '(2 1)
               (multiple-value-list
                   (setf (gdk:gl-context-required-version context) '(2 1)))))
    (is-true (gdk:gl-context-realize context))
    (is (equal '(3 2)
               (multiple-value-list (gdk:gl-context-required-version context))))
    ;; Remove references
    (is-false (gdk:surface-destroy surface))))

;;;     gdk_gl_context_set_debug_enabled
;;;     gdk_gl_context_get_debug_enabled

(test gdk-gl-context-debug-enabled
  (glib-test:with-check-memory ((surface 2) context :strong 2)
    (is (typep (setf surface
                     (gdk:surface-new-toplevel *display-default*))
               'gdk:surface))
    (is (typep (setf context
                     (gdk:surface-create-gl-context surface)) 'gdk:gl-context))
    (is-true (setf (gdk:gl-context-debug-enabled context) t))
    (is-true (gdk:gl-context-realize context))
    (is-true (gdk:gl-context-debug-enabled context))
    ;; Remove references
    (is-false (gdk:surface-destroy surface))))

;;;     gdk_gl_context_set_forward_compatible
;;;     gdk_gl_context_get_forward_compatible

(test gdk-gl-context-forward-compatible
  (glib-test:with-check-memory ((surface 2) context :strong 2)
    (is (typep (setf surface
                     (gdk:surface-new-toplevel *display-default*))
               'gdk:surface))
    (is (typep (setf context
                     (gdk:surface-create-gl-context surface)) 'gdk:gl-context))
    (is-true (setf (gdk:gl-context-forward-compatible context) t))
    (is-true (gdk:gl-context-realize context))
    (is-true (gdk:gl-context-forward-compatible context))
    ;; Remove references
    (is-false (gdk:surface-destroy surface))))

;;;     gdk_gl_context_set_use_es
;;;     gdk_gl_context_get_use_es

(test gdk-gl-context-use-es.1
  (glib-test:with-check-memory ((surface 2) context :strong 2)
    (is (typep (setf surface
                     (gdk:surface-new-toplevel *display-default*))
               'gdk:surface))
    (is (typep (setf context
                     (gdk:surface-create-gl-context surface)) 'gdk:gl-context))
    (is-true (gdk:gl-context-realize context))
    (is-true (gdk:gl-context-use-es context))
    ;; Remove references
    (is-false (gdk:surface-destroy surface))))

(test gdk-gl-context-use-es.2
  (glib-test:with-check-memory ((surface 2) context :strong 2)
    (is (typep (setf surface
                     (gdk:surface-new-toplevel *display-default*))
               'gdk:surface))
    (is (typep (setf context
                     (gdk:surface-create-gl-context surface)) 'gdk:gl-context))
    (is (= 0 (setf (gdk:gl-context-use-es context) 0)))
    (is-true (gdk:gl-context-realize context))
    (is-false (gdk:gl-context-use-es context))
    ;; Remove references
    (is-false (gdk:surface-destroy surface))))

;;;     gdk_gl_context_is_legacy

(test gdk-gl-context-is-legacy
  (glib-test:with-check-memory ((surface 2) context :strong 2)
    (is (typep (setf surface
                     (gdk:surface-new-toplevel *display-default*))
               'gdk:surface))
    (is (typep (setf context
                     (gdk:surface-create-gl-context surface)) 'gdk:gl-context))
    (is-true (gdk:gl-context-realize context))
    (is-false (gdk:gl-context-is-legacy context))
    ;; Remove references
    (is-false (gdk:surface-destroy surface))))

;;;     gdk_gl_context_is_shared                           Since 4.4

(test gdk-gl-context-is-shared
  (glib-test:with-check-memory ((surface 2) context :strong 2)
    (is (typep (setf surface
                     (gdk:surface-new-toplevel *display-default*))
               'gdk:surface))
    (is (typep (setf context
                     (gdk:surface-create-gl-context surface)) 'gdk:gl-context))
    (is-true (gdk:gl-context-realize context))
    (is-true (gdk:gl-context-is-shared context context))
    ;; Remove references
    (is-false (gdk:surface-destroy surface))))

;;;     gdk_gl_context_make_current
;;;     gdk_gl_context_get_current
;;;     gdk_gl_context_clear_current

(test gdk-gl-context-make/get/clear-current
  (glib-test:with-check-memory ((surface 2) context :strong 2)
    (is (typep (setf surface
                     (gdk:surface-new-toplevel *display-default*))
               'gdk:surface))
    (is (typep (setf context
                     (gdk:surface-create-gl-context surface)) 'gdk:gl-context))
    (is-false (gdk:gl-context-clear-current))
    (is-false (gdk:gl-context-current))
    (is-false (gdk:gl-context-make-current context))
    (is (eq context (gdk:gl-context-current)))
    (is-false (gdk:gl-context-clear-current))
    (is-false (gdk:gl-context-current))
    ;; Remove references
    (is-false (gdk:surface-destroy surface))))

;;; 2026-02-05

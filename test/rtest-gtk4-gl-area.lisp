(in-package :gtk-test)

(def-suite gtk-gl-area :in gtk-suite)
(in-suite gtk-gl-area)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkGLArea

(test gtk-gl-area-class
  ;; Check type
  (is (g:type-is-object "GtkGLArea"))
  ;; Check registered name
  (is (eq 'gtk:gl-area
          (glib:symbol-for-gtype "GtkGLArea")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkGLArea")
          (g:gtype (cffi:foreign-funcall "gtk_gl_area_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkGLArea")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkGLArea")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (glib-test:list-interfaces "GtkGLArea")))
  ;; Check properties
  (is (equal '("allowed-apis" "api" "auto-render" "context" "has-depth-buffer"
               "has-stencil-buffer" "use-es")
             (glib-test:list-properties "GtkGLArea")))
  ;; Check signals
  (is (equal '("create-context" "render" "resize")
             (glib-test:list-signals "GtkGLArea")))
  ;; Check CSS name
  (is (string= "widget"
               (gtk:widget-class-css-name "GtkGLArea")))
  ;; Check accessible role
  (is (eq :widget (gtk:widget-class-accessible-role "GtkGLArea")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkGLArea" GTK:GL-AREA
                       (:SUPERCLASS GTK:WIDGET
                        :EXPORT T
                        :INTERFACES
                        ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"))
                       ((ALLOWED-APIS GL-AREA-ALLOWED-APIS
                         "allowed-apis" "GdkGLAPI" T T)
                        (API GL-AREA-API "api" "GdkGLAPI" T NIL)
                        (AUTO-RENDER GL-AREA-AUTO-RENDER
                         "auto-render" "gboolean" T T)
                        (CONTEXT GL-AREA-CONTEXT "context" "GdkGLContext" T NIL)
                        (HAS-DEPTH-BUFFER GL-AREA-HAS-DEPTH-BUFFER
                         "has-depth-buffer" "gboolean" T T)
                        (HAS-STENCIL-BUFFER GL-AREA-HAS-STENCIL-BUFFER
                         "has-stencil-buffer" "gboolean" T T)
                        (USE-ES GL-AREA-USE-ES "use-es" "gboolean" T T)))
             (gobject:get-gtype-definition "GtkGLArea"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-gl-area-properties
  (let ((area (make-instance 'gtk:gl-area)))
    (is (equal '(:gl :gles) (gtk:gl-area-allowed-apis area)))
    (is-false (gtk:gl-area-api area))
    (is-true (gtk:gl-area-auto-render area))
    (is-false (gtk:gl-area-context area))
    (is-false (gtk:gl-area-has-depth-buffer area))
    (is-false (gtk:gl-area-has-stencil-buffer area))
    (is-false (gtk:gl-area-use-es area))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_gl_area_new
;;;     gtk_gl_area_make_current
;;;     gtk_gl_area_queue_render
;;;     gtk_gl_area_attach_buffers
;;;     gtk_gl_area_set_error
;;;     gtk_gl_area_get_error
;;;     gtk_gl_area_get_required_version
;;;     gtk_gl_area_set_required_version

;;; 2024-10-26

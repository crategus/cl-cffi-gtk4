(in-package :gtk-test)

(def-suite gdk-cairo-context :in gdk-suite)
(in-suite gdk-cairo-context)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkCairoContext

(test g-object-class
  ;; Type check
  (is (g:type-is-object "GdkCairoContext"))
  ;; Check the registered name
  (is (eq 'gdk:cairo-context
          (gobject:symbol-for-gtype "GdkCairoContext")))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkCairoContext")
          (g:gtype (cffi:foreign-funcall "gdk_cairo_context_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GdkDrawContext")
          (g:type-parent "GdkCairoContext")))
  ;; Check the children
  (is (equal '("GdkWaylandCairoContext")
             (list-children "GdkCairoContext")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GdkCairoContext")))
  ;; Check the properties
  (is (equal '()
             (list-properties "GdkCairoContext")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GdkCairoContext")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GdkCairoContext" GDK-CAIRO-CONTEXT
                       (:SUPERCLASS GDK-DRAW-CONTEXT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gdk_cairo_context_get_type")
                       NIL)
             (gobject:get-g-type-definition "GdkCairoContext"))))

;;; --- Functions --------------------------------------------------------------

;;;     gdk_cairo_context_cairo_create

(test gdk-cairo-context-cairo-create
  (let* ((surface (gdk:surface-new-toplevel (gdk:display-default)))
         (context (gdk:surface-create-cairo-context surface)))
    (is (g:type-is-a (g:type-from-instance surface) "GdkSurface"))
    (is (g:type-is-a (g:type-from-instance context) "GdkCairoContext"))
    ;; TODO: We get a NULL Cairo context. Improve this !?
    (is (cffi:null-pointer-p (gdk:cairo-context-cairo-create context)))))

;;; --- 2023-4-7 ---------------------------------------------------------------

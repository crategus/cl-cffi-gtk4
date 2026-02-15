(in-package :gtk-test)

(def-suite gdk-cairo-context :in gdk-suite)
(in-suite gdk-cairo-context)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkCairoContext

(test gdk-cairo-context-class
  ;; Check type
  (is (g:type-is-object "GdkCairoContext"))
  ;; Check registered name
  (is (eq 'gdk:cairo-context
          (glib:symbol-for-gtype "GdkCairoContext")))
  ;; Check type initializer
  (is (eq (g:gtype "GdkCairoContext")
          (g:gtype (cffi:foreign-funcall "gdk_cairo_context_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GdkDrawContext")
          (g:type-parent "GdkCairoContext")))
  ;; Check children
  #-windows
  (is (equal '("GdkWaylandCairoContext" "GdkX11CairoContext")
             (glib-test:list-children "GdkCairoContext")))
  #+WINDOWS
  (is (equal '("GdkWin32CairoContext")
             (glib-test:list-children "GdkCairoContext")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GdkCairoContext")))
  ;; Check properties
  (is (equal '()
             (glib-test:list-properties "GdkCairoContext")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GdkCairoContext")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GdkCairoContext" GDK:CAIRO-CONTEXT
                      (:SUPERCLASS GDK:DRAW-CONTEXT
                       :EXPORT T
                       :INTERFACES NIL
                       :TYPE-INITIALIZER "gdk_cairo_context_get_type")
                      NIL)
             (gobject:get-gtype-definition "GdkCairoContext"))))

;;; --- Functions --------------------------------------------------------------

;;;     gdk_cairo_context_cairo_create

(test gdk-cairo-context-cairo-create
  (let ((gtk-init:*warn-deprecated* nil))
    (let ((surface (gdk:surface-new-toplevel (gdk:display-default))))
      (is (typep (gdk:surface-create-cairo-context surface) 'gdk:cairo-context))
      (is-false (gdk:surface-destroy surface)))))

;;; 2025-12-05

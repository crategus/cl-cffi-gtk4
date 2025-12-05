(in-package :gtk-test)

(def-suite gdk-draw-context :in gdk-suite)
(in-suite gdk-draw-context)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkDrawContext

(test gdk-draw-context-class
  ;; Check type
  (is (g:type-is-object "GdkDrawContext"))
  ;; Check registered name
  (is (eq 'gdk:draw-context
          (glib:symbol-for-gtype "GdkDrawContext")))
  ;; Check type initializer
  (is (eq (g:gtype "GdkDrawContext")
          (g:gtype (cffi:foreign-funcall "gdk_draw_context_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GdkDrawContext")))
  ;; Check children
  (is (equal '("GdkCairoContext" "GdkGLContext" "GdkVulkanContext")
             (glib-test:list-children "GdkDrawContext")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GdkDrawContext")))
  ;; Check properties
  (is (equal '("display" "surface")
             (glib-test:list-properties "GdkDrawContext")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GdkDrawContext")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GdkDrawContext" GDK:DRAW-CONTEXT
                       (:SUPERCLASS GOBJECT:OBJECT
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gdk_draw_context_get_type")
                       ((DISPLAY DRAW-CONTEXT-DISPLAY "display" "GdkDisplay" T
                         NIL)
                        (SURFACE DRAW-CONTEXT-SURFACE "surface" "GdkSurface" T
                         NIL)))
             (gobject:get-gtype-definition "GdkDrawContext"))))

;;; --- Properties -------------------------------------------------------------

;;;     display
;;;     surface

(test gdk-draw-context-properties
  (let ((*gtk-warn-deprecated* nil))
    (let* ((surface (gdk:surface-new-toplevel (gdk:display-default)))
           ;; Create a Cairo context to check the properties
           (context (gdk:surface-create-cairo-context surface)))
      (is (eq (gdk:display-default) (gdk:draw-context-display context)))
      (is (eq surface (gdk:draw-context-surface context)))
      (is-false (gdk:surface-destroy surface)))))

;;; --- Functions --------------------------------------------------------------

;;;     gdk_draw_context_begin_frame                        Deprecated 4.16
;;;     gdk_draw_context_end_frame                          Deprecated 4.16
;;;     gdk_draw_context_is_in_frame                        Deprecated 4.16
;;;     gdk_draw_context_get_frame_region                   Deprecated 4.16

;;; 2025-12-05

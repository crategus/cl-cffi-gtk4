(in-package :gtk-test)

(def-suite gdk-drop :in gdk-suite)
(in-suite gdk-drop)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkDrop

(test gdk-drop-class
  ;; Check type
  (is (g:type-is-object "GdkDrop"))
  ;; Check registered name
  (is (eq 'gdk:drop
          (glib:symbol-for-gtype "GdkDrop")))
  ;; Check type initializer
  (is (eq (g:gtype "GdkDrop")
          (g:gtype (cffi:foreign-funcall "gdk_drop_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GdkDrop")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GdkDrop")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GdkDrop")))
  ;; Check properties
  (is (equal '("actions" "device" "display" "drag" "formats" "surface")
             (glib-test:list-properties "GdkDrop")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GdkDrop")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GdkDrop" GDK:DROP
                      (:SUPERCLASS GOBJECT:OBJECT
                       :EXPORT T
                       :INTERFACES NIL
                       :TYPE-INITIALIZER "gdk_drop_get_type")
                      ((ACTIONS DROP-ACTIONS "actions" "GdkDragAction" T NIL)
                       (DEVICE DROP-DEVICE "device" "GdkDevice" T NIL)
                       (DISPLAY DROP-DISPLAY "display" "GdkDisplay" T NIL)
                       (DRAG DROP-DRAG "drag" "GdkDrag" T NIL)
                       (FORMATS DROP-FORMATS "formats" "GdkContentFormats" T NIL)
                       (SURFACE DROP-SURFACE "surface" "GdkSurface" T NIL)))
             (gobject:get-gtype-definition "GdkDrop"))))

;;; ---- Properties ------------------------------------------------------------

;;;     actions
;;;     device
;;;     display
;;;     drag
;;;     formats
;;;     surface

;;; --- Functions --------------------------------------------------------------

;;;     gdk_drop_status
;;;     gdk_drop_finish
;;;     gdk_drop_read_async
;;;     gdk_drop_read_finish
;;;     gdk_drop_read_value_async
;;;     gdk_drop_read_value_finish

;;; 2026-08-09

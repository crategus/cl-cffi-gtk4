(in-package :gtk-test)

(def-suite gtk-gesture-drag :in gtk-suite)
(in-suite gtk-gesture-drag)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkGestureDrag

(test gesture-drag-class
  ;; Type check
  (is (g:type-is-object "GtkGestureDrag"))
  ;; Check the registered name
  (is (eq 'gtk:gesture-drag
          (gobject:symbol-for-gtype "GtkGestureDrag")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkGestureDrag")
          (g:gtype (foreign-funcall "gtk_gesture_drag_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkGestureSingle")
          (g:type-parent "GtkGestureDrag")))
  ;; Check the children
  (is (equal '("GtkGesturePan")
             (list-children "GtkGestureDrag")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkGestureDrag")))
  ;; Check the properties
  (is (equal '()
             (list-properties "GtkGestureDrag")))
  ;; Check the signals
  (is (equal '("drag-begin" "drag-end" "drag-update")
             (list-signals "GtkGestureDrag")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkGestureDrag" GTK-GESTURE-DRAG
                       (:SUPERCLASS GTK-GESTURE-SINGLE :EXPORT T :INTERFACES
                        NIL :TYPE-INITIALIZER "gtk_gesture_drag_get_type")
                       NIL)
             (gobject:get-g-type-definition "GtkGestureDrag"))))

;;; --- Signals ----------------------------------------------------------------

;;;     drag-begin
;;;     drag-end
;;;     drag-update

;;; --- Functions --------------------------------------------------------------

;;;     gtk_gesture_drag_new
;;;     gtk_gesture_drag_get_start_point
;;;     gtk_gesture_drag_get_offset

;;; 2022-11-12

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
          (glib:symbol-for-gtype "GtkGestureDrag")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkGestureDrag")
          (g:gtype (cffi:foreign-funcall "gtk_gesture_drag_get_type" :size))))
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
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkGestureDrag" GTK-GESTURE-DRAG
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

(test gtk-gesture-drag-new
  (is (typep (gtk:gesture-drag-new) 'gtk:gesture-drag)))

;;;     gtk_gesture_drag_get_start_point

(test gtk-gesture-drag-start-point
  (let ((gesture (gtk:gesture-drag-new)))
    (is-false (gtk:gesture-drag-start-point gesture))))

;;;     gtk_gesture_drag_get_offset

(test gtk-gesture-drag-offset
  (let ((gesture (gtk:gesture-drag-new)))
    (is-false (gtk:gesture-drag-offset gesture))))

;;; --- 2023-5-29 --------------------------------------------------------------

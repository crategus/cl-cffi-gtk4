(in-package :gtk-test)

(def-suite gtk-gesture-drag :in gtk-suite)
(in-suite gtk-gesture-drag)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkGestureDrag

(test gesture-drag-class
  ;; Check type
  (is (g:type-is-object "GtkGestureDrag"))
  ;; Check registered name
  (is (eq 'gtk:gesture-drag
          (glib:symbol-for-gtype "GtkGestureDrag")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkGestureDrag")
          (g:gtype (cffi:foreign-funcall "gtk_gesture_drag_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkGestureSingle")
          (g:type-parent "GtkGestureDrag")))
  ;; Check children
  (is (equal '("GtkGesturePan")
             (gtk-test:list-children "GtkGestureDrag")))
  ;; Check interfaces
  (is (equal '()
             (gtk-test:list-interfaces "GtkGestureDrag")))
  ;; Check properties
  (is (equal '()
             (gtk-test:list-properties "GtkGestureDrag")))
  ;; Check signals
  (is (equal '("drag-begin" "drag-end" "drag-update")
             (gtk-test:list-signals "GtkGestureDrag")))
  ;; Check class definition
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

;;; 2024-7-4

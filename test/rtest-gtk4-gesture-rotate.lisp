(in-package :gtk-test)

(def-suite gtk-gesture-rotate :in gtk-suite)
(in-suite gtk-gesture-rotate)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkGestureRotate

(test gesture-rotate-class
  ;; Check type
  (is (g:type-is-object "GtkGestureRotate"))
  ;; Check registered name
  (is (eq 'gtk:gesture-rotate
          (glib:symbol-for-gtype "GtkGestureRotate")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkGestureRotate")
          (g:gtype (cffi:foreign-funcall "gtk_gesture_rotate_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkGesture")
          (g:type-parent "GtkGestureRotate")))
  ;; Check children
  (is (equal '()
             (gtk-test:list-children "GtkGestureRotate")))
  ;; Check interfaces
  (is (equal '()
             (gtk-test:list-interfaces "GtkGestureRotate")))
  ;; Check properties
  (is (equal '()
             (gtk-test:list-properties "GtkGestureRotate")))
  ;; Check signals
  (is (equal '("angle-changed")
             (gtk-test:list-signals "GtkGestureRotate")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkGestureRotate" GTK-GESTURE-ROTATE
                       (:SUPERCLASS GTK-GESTURE :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_gesture_rotate_get_type")
                       NIL)
             (gobject:get-g-type-definition "GtkGestureRotate"))))

;;; --- Signals ----------------------------------------------------------------

;;;     angle-changed

;;; --- Functions --------------------------------------------------------------

;;;     gtk_gesture_rotate_new

(test gtk-gestsure-rotate-new
  (is (typep (gtk:gesture-rotate-new) 'gtk:gesture-rotate)))

;;;     gtk_gesture_rotate_get_angle_delta

(test gtk-gesture-rotate-angle-delta
  (let ((gesture (gtk:gesture-rotate-new)))
    (is (= 0.0d0 (gtk:gesture-rotate-angle-delta gesture)))))

;;; 2024-2-19

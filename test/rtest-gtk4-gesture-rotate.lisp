(in-package :gtk-test)

(def-suite gtk-gesture-rotate :in gtk-suite)
(in-suite gtk-gesture-rotate)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkGestureRotate

(test gesture-rotate-class
  ;; Type check
  (is (g:type-is-object "GtkGestureRotate"))
  ;; Check the registered name
  (is (eq 'gtk:gesture-rotate
          (gobject:symbol-for-gtype "GtkGestureRotate")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkGestureRotate")
          (g:gtype (cffi:foreign-funcall "gtk_gesture_rotate_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkGesture")
          (g:type-parent "GtkGestureRotate")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkGestureRotate")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkGestureRotate")))
  ;; Check the properties
  (is (equal '()
             (list-properties "GtkGestureRotate")))
  ;; Check the signals
  (is (equal '("angle-changed")
             (list-signals "GtkGestureRotate")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkGestureRotate" GTK-GESTURE-ROTATE
                       (:SUPERCLASS GTK-GESTURE :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_gesture_rotate_get_type")
                       NIL)
             (gobject:get-g-type-definition "GtkGestureRotate"))))

;;; --- Signals ----------------------------------------------------------------

;;;     angle-changed

;;; --- Functions --------------------------------------------------------------

;;;     gtk_gesture_rotate_new
;;;     gtk_gesture_rotate_get_angle_delta

;;; --- 2023-3-18 --------------------------------------------------------------

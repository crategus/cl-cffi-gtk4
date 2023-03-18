(in-package :gtk-test)

(def-suite gtk-gesture-swipe :in gtk-suite)
(in-suite gtk-gesture-swipe)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkGestureSwipe

(test gesture-swipe-class
  ;; Type check
  (is (g:type-is-object "GtkGestureSwipe"))
  ;; Check the registered name
  (is (eq 'gtk:gesture-swipe
          (gobject:symbol-for-gtype "GtkGestureSwipe")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkGestureSwipe")
          (g:gtype (cffi:foreign-funcall "gtk_gesture_swipe_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkGestureSingle")
          (g:type-parent "GtkGestureSwipe")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkGestureSwipe")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkGestureSwipe")))
  ;; Check the properties
  (is (equal '()
             (list-properties "GtkGestureSwipe")))
  ;; Check the signals
  (is (equal '("swipe")
             (list-signals "GtkGestureSwipe")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkGestureSwipe" GTK-GESTURE-SWIPE
                       (:SUPERCLASS GTK-GESTURE-SINGLE :EXPORT T :INTERFACES
                        NIL :TYPE-INITIALIZER "gtk_gesture_swipe_get_type")
                       NIL)
             (gobject:get-g-type-definition "GtkGestureSwipe"))))

;;; --- Signals ----------------------------------------------------------------

;;;     swipe

;;; --- Functions --------------------------------------------------------------

;;;     gtk_gesture_swipe_new
;;;     gtk_gesture_swipe_get_velocity

;;; --- 2023-3-18 --------------------------------------------------------------

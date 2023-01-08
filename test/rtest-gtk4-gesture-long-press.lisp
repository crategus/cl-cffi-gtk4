(in-package :gtk-test)

(def-suite gtk-gesture-long-press :in gtk-suite)
(in-suite gtk-gesture-long-press)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkGestureLongPress

(test gesture-long-press-class
  ;; Type check
  (is (g:type-is-object "GtkGestureLongPress"))
  ;; Check the registered name
  (is (eq 'gtk:gesture-long-press
          (gobject:symbol-for-gtype "GtkGestureLongPress")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkGestureLongPress")
          (g:gtype (foreign-funcall "gtk_gesture_long_press_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkGestureSingle")
          (g:type-parent "GtkGestureLongPress")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkGestureLongPress")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkGestureLongPress")))
  ;; Check the properties
  (is (equal '("delay-factor")
             (list-properties "GtkGestureLongPress")))
  ;; Check the signals
  (is (equal '("cancelled" "pressed")
             (list-signals "GtkGestureLongPress")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkGestureLongPress" GTK-GESTURE-LONG-PRESS
                       (:SUPERCLASS GTK-GESTURE-SINGLE :EXPORT T :INTERFACES
                        NIL :TYPE-INITIALIZER
                        "gtk_gesture_long_press_get_type")
                       ((DELAY-FACTOR GTK-GESTURE-LONG-PRESS-DELAY-FACTOR
                         "delay-factor" "gdouble" T T)))
             (gobject:get-g-type-definition "GtkGestureLongPress"))))

;;; --- Properties -------------------------------------------------------------

;;;     delay-factor

;;; --- Signals ----------------------------------------------------------------

;;;     cancelled
;;;     pressed

;;; --- Functions --------------------------------------------------------------

;;;     gtk_gesture_long_press_new

;;; 2022-11-12

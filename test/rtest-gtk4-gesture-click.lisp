(in-package :gtk-test)

(def-suite gtk-gesture-click :in gtk-suite)
(in-suite gtk-gesture-click)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkGestureClick

(test gesture-click-class
  ;; Type check
  (is (g:type-is-object "GtkGestureClick"))
  ;; Check the registered name
  (is (eq 'gtk:gesture-click
          (glib:symbol-for-gtype "GtkGestureClick")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkGestureClick")
          (g:gtype (cffi:foreign-funcall "gtk_gesture_click_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkGestureSingle")
          (g:type-parent "GtkGestureClick")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkGestureClick")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkGestureClick")))
  ;; Check the properties
  (is (equal '()
             (list-properties "GtkGestureClick")))
  ;; Check the signals
  (is (equal '("pressed" "released" "stopped" "unpaired-release")
             (list-signals "GtkGestureClick")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkGestureClick" GTK-GESTURE-CLICK
                       (:SUPERCLASS GTK-GESTURE-SINGLE :EXPORT T :INTERFACES
                        NIL :TYPE-INITIALIZER "gtk_gesture_click_get_type")
                       NIL)
             (gobject:get-g-type-definition "GtkGestureClick"))))

;;; --- Signals ----------------------------------------------------------------

;;;     pressed
;;;     released
;;;     stopped
;;;     unpaired-release

;;; --- Functions --------------------------------------------------------------

;;;     gtk_gesture_click_new

;;; --- 2023-5-29 --------------------------------------------------------------

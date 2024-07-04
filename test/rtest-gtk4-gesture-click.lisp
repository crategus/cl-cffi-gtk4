(in-package :gtk-test)

(def-suite gtk-gesture-click :in gtk-suite)
(in-suite gtk-gesture-click)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkGestureClick

(test gesture-click-class
  ;; Check type
  (is (g:type-is-object "GtkGestureClick"))
  ;; Check registered name
  (is (eq 'gtk:gesture-click
          (glib:symbol-for-gtype "GtkGestureClick")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkGestureClick")
          (g:gtype (cffi:foreign-funcall "gtk_gesture_click_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkGestureSingle")
          (g:type-parent "GtkGestureClick")))
  ;; Check children
  (is (equal '()
             (gtk-test:list-children "GtkGestureClick")))
  ;; Check interfaces
  (is (equal '()
             (gtk-test:list-interfaces "GtkGestureClick")))
  ;; Check properties
  (is (equal '()
             (gtk-test:list-properties "GtkGestureClick")))
  ;; Check signals
  (is (equal '("pressed" "released" "stopped" "unpaired-release")
             (gtk-test:list-signals "GtkGestureClick")))
  ;; Check class definition
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

;;; 2024-7-4

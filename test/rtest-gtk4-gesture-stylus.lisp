(in-package :gtk-test)

(def-suite gtk-gesture-stylus :in gtk-suite)
(in-suite gtk-gesture-stylus)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkGestureStylus

(test gtk-gesture-stylus-class
  ;; Type check
  (is (g:type-is-object "GtkGestureStylus"))
  ;; Check the registered name
  (is (eq 'gtk:gesture-stylus
          (glib:symbol-for-gtype "GtkGestureStylus")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkGestureStylus")
          (g:gtype (cffi:foreign-funcall "gtk_gesture_stylus_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkGestureSingle")
          (g:type-parent "GtkGestureStylus")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkGestureStylus")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkGestureStylus")))
  ;; Check the properties
  (is (equal '("stylus-only")
             (list-properties "GtkGestureStylus")))
  ;; Check the signals
  (is (equal '("down" "motion" "proximity" "up")
             (list-signals "GtkGestureStylus")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkGestureStylus" GTK-GESTURE-STYLUS
                       (:SUPERCLASS GTK-GESTURE-SINGLE :EXPORT T :INTERFACES
                        NIL :TYPE-INITIALIZER "gtk_gesture_stylus_get_type")
                       ((STYLUS-ONLY GTK-GESTURE-STYLUS-STYLUS-ONLY
                         "stylus-only" "gboolean" T T)))
             (gobject:get-g-type-definition "GtkGestureStylus"))))

;;; --- Properties -------------------------------------------------------------

;;;     stylus-only                                        Since 4.10

(test gtk-gesture-stylus-properties
  (let ((gesture (make-instance 'gtk:gesture-stylus)))
    (is-true (gtk:gesture-stylus-stylus-only gesture))))

;;; --- Signals ----------------------------------------------------------------

;;;     down
;;;     motion
;;;     proximity
;;;     up

;;; --- Functions --------------------------------------------------------------

;;;     gtk_gesture_stylus_new

(test gtk-gesture-stylus-new
  (is (typep (gtk:gesture-stylus-new) 'gtk:gesture-stylus)))

;;;     gtk_gesture_stylus_get_axis

(test gtk-gesture-stylus-axis
  (let ((gesture (gtk:gesture-stylus-new)))
    (is-false (gtk:gesture-stylus-axis gesture :x))))

;;;     gtk_gesture_stylus_get_axes
;;;     gtk_gesture_stylus_get_backlog

;;;     gtk_gesture_stylus_get_device_tool

(test gtk-gesture-stylus-device-tool
  (let ((gesture (gtk:gesture-stylus-new)))
    (is-false (gtk:gesture-stylus-device-tool gesture))))

;;; 2024-2-21

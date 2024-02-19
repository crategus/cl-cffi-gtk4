(in-package :gtk-test)

(def-suite gtk-gesture-single :in gtk-suite)
(in-suite gtk-gesture-single)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkGestureSingle

(test gtk-gesture-single-class
  ;; Type check
  (is (g:type-is-object "GtkGestureSingle"))
  ;; Check the registered name
  (is (eq 'gtk:gesture-single
          (glib:symbol-for-gtype "GtkGestureSingle")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkGestureSingle")
          (g:gtype (cffi:foreign-funcall "gtk_gesture_single_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkGesture")
          (g:type-parent "GtkGestureSingle")))
  ;; Check the children
  (is (equal '("GtkDragSource" "GtkGestureClick" "GtkGestureDrag"
               "GtkGestureLongPress" "GtkGestureStylus" "GtkGestureSwipe")
             (list-children "GtkGestureSingle")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkGestureSingle")))
  ;; Check the properties
  (is (equal '("button" "exclusive" "touch-only")
             (list-properties "GtkGestureSingle")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkGestureSingle")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkGestureSingle" GTK-GESTURE-SINGLE
                       (:SUPERCLASS GTK-GESTURE :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_gesture_single_get_type")
                       ((BUTTON GTK-GESTURE-SINGLE-BUTTON "button" "guint" T T)
                        (EXCLUSIVE GTK-GESTURE-SINGLE-EXCLUSIVE "exclusive"
                         "gboolean" T T)
                        (TOUCH-ONLY GTK-GESTURE-SINGLE-TOUCH-ONLY "touch-only"
                         "gboolean" T T)))
             (gobject:get-g-type-definition "GtkGestureSingle"))))

;;; --- Properties -------------------------------------------------------------

;;;     button
;;;     exclusive
;;;     touch-only

(test gtk-gesture-single-properties
  (let ((gesture (make-instance 'gtk:gesture-single)))
    (is (= 1 (gtk:gesture-single-button gesture)))
    (is-false (gtk:gesture-single-exclusive gesture))
    (is-false (gtk:gesture-single-touch-only gesture))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_gesture_single_get_current_button

(test gtk-gesture-single-current-button
  (let ((gesture (make-instance 'gtk:gesture-single)))
    (is (= 0 (gtk:gesture-single-current-button gesture)))))

;;;     gtk_gesture_single_get_current_sequence

(test gtk-gesture-single-current-sequence
  (let ((gesture (make-instance 'gtk:gesture-single)))
    (is-false (gtk:gesture-single-current-sequence gesture))))

;;; 2024-2-19

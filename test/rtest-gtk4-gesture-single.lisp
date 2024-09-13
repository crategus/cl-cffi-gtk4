(in-package :gtk-test)

(def-suite gtk-gesture-single :in gtk-suite)
(in-suite gtk-gesture-single)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkGestureSingle

(test gtk-gesture-single-class
  ;; Check type
  (is (g:type-is-object "GtkGestureSingle"))
  ;; Check registered name
  (is (eq 'gtk:gesture-single
          (glib:symbol-for-gtype "GtkGestureSingle")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkGestureSingle")
          (g:gtype (cffi:foreign-funcall "gtk_gesture_single_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkGesture")
          (g:type-parent "GtkGestureSingle")))
  ;; Check children
  (is (equal '("GtkDragSource" "GtkGestureClick" "GtkGestureDrag"
               "GtkGestureLongPress" "GtkGestureStylus" "GtkGestureSwipe")
             (gtk-test:list-children "GtkGestureSingle")))
  ;; Check interfaces
  (is (equal '()
             (gtk-test:list-interfaces "GtkGestureSingle")))
  ;; Check properties
  (is (equal '("button" "exclusive" "touch-only")
             (gtk-test:list-properties "GtkGestureSingle")))
  ;; Check signals
  (is (equal '()
             (gtk-test:list-signals "GtkGestureSingle")))
  ;; Check class definition
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

;;; 2024-7-27

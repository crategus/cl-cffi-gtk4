(in-package :gtk-test)

(def-suite gtk-gesture-single :in gtk-suite)
(in-suite gtk-gesture-single)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkGestureSingle

(test gesture-single-class
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
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkGestureSingle" GTK-GESTURE-SINGLE
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

;;; --- Functions --------------------------------------------------------------

;;;     gtk_gesture_single_get_current_button
;;;     gtk_gesture_single_get_current_sequence

;;; --- 2023-5-29 --------------------------------------------------------------

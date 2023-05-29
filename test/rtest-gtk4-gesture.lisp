(in-package :gtk-test)

(def-suite gtk-gesture :in gtk-suite)
(in-suite gtk-gesture)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkEventSequenceState

(test event-sequence-state
  ;; Check the type
  (is (g:type-is-enum "GtkEventSequenceState"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkEventSequenceState")
          (g:gtype (cffi:foreign-funcall "gtk_event_sequence_state_get_type"
                                         :size))))
  ;; Check the registered name
  (is (eq 'gtk:event-sequence-state
          (glib:symbol-for-gtype "GtkEventSequenceState")))
  ;; Check the names
  (is (equal '("GTK_EVENT_SEQUENCE_NONE" "GTK_EVENT_SEQUENCE_CLAIMED"
               "GTK_EVENT_SEQUENCE_DENIED")
             (list-enum-item-name "GtkEventSequenceState")))
  ;; Check the values
  (is (equal '(0 1 2)
             (list-enum-item-value "GtkEventSequenceState")))
  ;; Check the nick names
  (is (equal '("none" "claimed" "denied")
             (list-enum-item-nick "GtkEventSequenceState")))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GtkEventSequenceState"
                             GTK-EVENT-SEQUENCE-STATE
                             (:EXPORT T
                              :TYPE-INITIALIZER
                              "gtk_event_sequence_state_get_type")
                             (:NONE 0)
                             (:CLAIMED 1)
                             (:DENIED 2))
             (gobject:get-g-type-definition "GtkEventSequenceState"))))

;;;     GtkGesture

(test gesture-class
  ;; Type check
  (is (g:type-is-object "GtkGesture"))
  ;; Check the registered name
  (is (eq 'gtk:gesture
          (glib:symbol-for-gtype "GtkGesture")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkGesture")
          (g:gtype (cffi:foreign-funcall "gtk_gesture_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkEventController")
          (g:type-parent "GtkGesture")))
  ;; Check the children
  (is (equal '("GtkGestureRotate" "GtkGestureSingle" "GtkGestureZoom")
             (list-children "GtkGesture")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkGesture")))
  ;; Check the class properties
  (is (equal '("n-points")
             (list-properties "GtkGesture")))
  ;; Check the list of signals
  (is (equal '("begin" "cancel" "end" "sequence-state-changed" "update")
             (list-signals "GtkGesture")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkGesture" GTK-GESTURE
                       (:SUPERCLASS GTK-EVENT-CONTROLLER :EXPORT T :INTERFACES
                        NIL :TYPE-INITIALIZER "gtk_gesture_get_type")
                       ((N-POINTS GTK-GESTURE-N-POINTS "n-points" "guint" T
                         NIL)))
             (gobject:get-g-type-definition "GtkGesture"))))

;;; --- Properties -------------------------------------------------------------

(test gesture-propertiers
  (let ((gesture (make-instance 'gtk:gesture-click)))
    (is (= 1 (gtk:gesture-n-points gesture)))))

;;; --- Signals ----------------------------------------------------------------

;;;     begin
;;;     cancel
;;;     end
;;;     sequence-state-changed
;;;     update

;;; --- Functions --------------------------------------------------------------

;;;     gtk_gesture_get_device

(test gesture-device
  (let ((gesture (make-instance 'gtk:gesture-click)))
    (is-false (gtk:gesture-device gesture))))

;;;     gtk_gesture_is_active

(test gesture-is-active
  (let ((gesture (make-instance 'gtk:gesture-click)))
    (is-false (gtk:gesture-is-active gesture))))

;;;     gtk_gesture_is_recognized

(test gesture-is-recognized
  (let ((gesture (make-instance 'gtk:gesture-click)))
    (is-false (gtk:gesture-is-recognized gesture))))

;;;     gtk_gesture_get_sequence_state
;;;     gtk_gesture_set_sequence_state
;;;     gtk_gesture_set_state
;;;     gtk_gesture_get_sequences
;;;     gtk_gesture_handles_sequence
;;;     gtk_gesture_get_last_updated_sequence
;;;     gtk_gesture_get_last_event
;;;     gtk_gesture_get_point
;;;     gtk_gesture_get_bounding_box
;;;     gtk_gesture_get_bounding_box_center
;;;     gtk_gesture_group
;;;     gtk_gesture_ungroup
;;;     gtk_gesture_get_group
;;;     gtk_gesture_is_grouped_with

;; --- 2023-5-29 ---------------------------------------------------------------

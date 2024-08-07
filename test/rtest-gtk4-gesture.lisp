(in-package :gtk-test)

(def-suite gtk-gesture :in gtk-suite)
(in-suite gtk-gesture)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkEventSequenceState

(test event-sequence-state
  ;; Check type
  (is (g:type-is-enum "GtkEventSequenceState"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkEventSequenceState")
          (g:gtype (cffi:foreign-funcall "gtk_event_sequence_state_get_type"
                                         :size))))
  ;; Check registered name
  (is (eq 'gtk:event-sequence-state
          (glib:symbol-for-gtype "GtkEventSequenceState")))
  ;; Check names
  (is (equal '("GTK_EVENT_SEQUENCE_NONE" "GTK_EVENT_SEQUENCE_CLAIMED"
               "GTK_EVENT_SEQUENCE_DENIED")
             (gtk-test:list-enum-item-name "GtkEventSequenceState")))
  ;; Check values
  (is (equal '(0 1 2)
             (gtk-test:list-enum-item-value "GtkEventSequenceState")))
  ;; Check nick names
  (is (equal '("none" "claimed" "denied")
             (gtk-test:list-enum-item-nick "GtkEventSequenceState")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkEventSequenceState"
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
  ;; Check type
  (is (g:type-is-object "GtkGesture"))
  ;; Check registered name
  (is (eq 'gtk:gesture
          (glib:symbol-for-gtype "GtkGesture")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkGesture")
          (g:gtype (cffi:foreign-funcall "gtk_gesture_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkEventController")
          (g:type-parent "GtkGesture")))
  ;; Check children
  (is (equal '("GtkGestureRotate" "GtkGestureSingle" "GtkGestureZoom")
             (gtk-test:list-children "GtkGesture")))
  ;; Check interfaces
  (is (equal '()
             (gtk-test:list-interfaces "GtkGesture")))
  ;; Check class properties
  (is (equal '("n-points")
             (gtk-test:list-properties "GtkGesture")))
  ;; Check signals
  (is (equal '("begin" "cancel" "end" "sequence-state-changed" "update")
             (gtk-test:list-signals "GtkGesture")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkGesture" GTK-GESTURE
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

(test gtk-gesture-sequences
  (let ((gesture (make-instance 'gtk:gesture-click)))

    (is-false (gtk:gesture-sequences gesture))
))

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

;; --- 2023-10-2 ---------------------------------------------------------------

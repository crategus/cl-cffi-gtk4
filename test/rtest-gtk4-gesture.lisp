(in-package :gtk-test)

(def-suite gtk-gesture :in gtk-suite)
(in-suite gtk-gesture)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkEventSequenceState

(test gtk-event-sequence-state
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
             (glib-test:list-enum-item-names "GtkEventSequenceState")))
  ;; Check values
  (is (equal '(0 1 2)
             (glib-test:list-enum-item-values "GtkEventSequenceState")))
  ;; Check nick names
  (is (equal '("none" "claimed" "denied")
             (glib-test:list-enum-item-nicks "GtkEventSequenceState")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkEventSequenceState"
                                    GTK:EVENT-SEQUENCE-STATE
                       (:EXPORT T
                        :TYPE-INITIALIZER "gtk_event_sequence_state_get_type")
                       (:NONE 0)
                       (:CLAIMED 1)
                       (:DENIED 2))
             (gobject:get-gtype-definition "GtkEventSequenceState"))))

;;;     GtkGesture

(test gtk-gesture-class
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
             (glib-test:list-children "GtkGesture")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkGesture")))
  ;; Check class properties
  (is (equal '("n-points")
             (glib-test:list-properties "GtkGesture")))
  ;; Check signals
  (is (equal '("begin" "cancel" "end" "sequence-state-changed" "update")
             (glib-test:list-signals "GtkGesture")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkGesture" GTK:GESTURE
                       (:SUPERCLASS GTK:EVENT-CONTROLLER
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_gesture_get_type")
                       ((N-POINTS GESTURE-N-POINTS "n-points" "guint" T NIL)))
             (gobject:get-gtype-definition "GtkGesture"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-gesture-propertiers
  (let ((gesture (make-instance 'gtk:gesture-click)))
    (is (= 1 (gtk:gesture-n-points gesture)))))

;;; --- Signals ----------------------------------------------------------------

;;;     begin

(test gtk-gesture-begin-signal
  (let* ((name "begin")
         (gtype (g:gtype "GtkGesture"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    ;; Check parameter types
    (is (equal '("GdkEventSequence")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     cancel

(test gtk-gesture-cancel-signal
  (let* ((name "cancel")
         (gtype (g:gtype "GtkGesture"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    ;; Check parameter types
    (is (equal '("GdkEventSequence")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     end

(test gtk-gesture-end-signal
  (let* ((name "end")
         (gtype (g:gtype "GtkGesture"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    ;; Check parameter types
    (is (equal '("GdkEventSequence")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     sequence-state-changed

(test gtk-gesture-sequence-state-changed-signal
  (let* ((name "sequence-state-changed")
         (gtype (g:gtype "GtkGesture"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    ;; Check parameter types
    (is (equal '("GdkEventSequence" "GtkEventSequenceState")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     update

(test gtk-gesture-update-signal
  (let* ((name "update")
         (gtype (g:gtype "GtkGesture"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    ;; Check parameter types
    (is (equal '("GdkEventSequence")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_gesture_get_device

(test gtk-gesture-device
  (let ((gesture (gtk:gesture-click-new)))
    (is-false (gtk:gesture-device gesture))))

;;;     gtk_gesture_is_active

(test gtk-gesture-is-active
  (let ((gesture (gtk:gesture-click-new)))
    (is-false (gtk:gesture-is-active gesture))))

;;;     gtk_gesture_is_recognized

(test gtk-gesture-is-recognized
  (let ((gesture (gtk:gesture-click-new)))
    (is-false (gtk:gesture-is-recognized gesture))))

;;;     gtk_gesture_get_sequence_state
;;;     gtk_gesture_set_sequence_state
;;;     gtk_gesture_set_state

;;;     gtk_gesture_get_sequences

(test gtk-gesture-sequences
  (let ((gesture (gtk:gesture-click-new)))
    (is-false (gtk:gesture-sequences gesture))))

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

;; 2024-9-20

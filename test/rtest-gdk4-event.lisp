(in-package :gtk-test)

(def-suite gdk-event :in gdk-suite)
(in-suite gdk-event)

;;; --- Types and Values -------------------------------------------------------

;;;     GDK_CURRENT_TIME
;;;     GDK_PRIORITY_EVENTS
;;;     GDK_PRIORITY_REDRAW

;;;     GDK_EVENT_PROPAGATE
;;;     GDK_EVENT_STOP

(test gdk-event-propagate
  (is-false +gdk-event-propagate+)
  (is-true +gdk-event-stop+))

;;;     GDK_BUTTON_PRIMARY
;;;     GDK_BUTTON_MIDDLE
;;;     GDK_BUTTON_SECONDARY

;;;     GdkEventType
;;;     GdkKeymapKey
;;;     GdkKeyMatch
;;;     GdkTouchpadGesturePhase
;;;     GdkScrollDirection
;;;     GdkCrossingMode
;;;     GdkNotifyType
;;;
;;;     GdkEventSequence

;;;     GdkEvent

#+nil
(test event-class
  ;; Type check
  (is (g:type-is-object "GdkEvent"))
  ;; Check the registered name
  (is (eq 'gdk:event
          (glib:symbol-for-gtype "GdkEvent")))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkEvent")
          (g:gtype (cffi:foreign-funcall "gdk_event_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GdkEvent")))
  ;; Check the children
  (is (equal '("GdkButtonEvent" "GdkCrossingEvent" "GdkDNDEvent"
               "GdkDeleteEvent" "GdkFocusEvent" "GdkGrabBrokenEvent"
               "GdkKeyEvent" "GdkMotionEvent" "GdkPadEvent" "GdkProximityEvent"
               "GdkScrollEvent" "GdkTouchEvent" "GdkTouchpadEvent")
             (list-children "GdkEvent")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GdkEvent")))
  ;; Check the properties
  (is (equal '()
             (list-properties "GdkEvent")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GdkEvent")))
  ;; Check the class definition
  (is (equal '()
             (gobject:get-g-type-definition "GdkEvent"))))

;;;     GdkButtonEvent
;;;     GdkScrollEvent
;;;     GdkMotionEvent
;;;     GdkKeyEvent
;;;     GdkFocusEvent
;;;     GdkCrossingEvent
;;;     GdkGrabBrokenEvent
;;;     GdkDeleteEvent
;;;     GdkDNDEvent
;;;     GdkTouchEvent
;;;     GdkTouchpadEvent
;;;     GdkPadEvent
;;;     GdkProximityEvent
;;;
;;; Functions
;;;
;;;     gdk_event_ref
;;;     gdk_event_unref
;;;     gdk_event_get_event_type
;;;     gdk_event_get_surface
;;;     gdk_event_get_device
;;;     gdk_event_get_device_tool
;;;     gdk_event_get_time
;;;     gdk_event_get_display
;;;     gdk_event_get_seat
;;;     gdk_event_get_event_sequence
;;;     gdk_event_get_modifier_state
;;;     gdk_event_get_position
;;;     gdk_event_get_axes
;;;     gdk_event_get_axis
;;;     gdk_event_get_history
;;;     gdk_event_get_pointer_emulated
;;;     gdk_event_triggers_context_menu
;;;
;;;     gdk_button_event_get_button
;;;
;;;     gdk_scroll_event_get_direction
;;;     gdk_scroll_event_get_deltas
;;;     gdk_scroll_event_is_stop
;;;
;;;     gdk_key_event_get_keyval
;;;     gdk_key_event_get_keycode
;;;     gdk_key_event_get_consumed_modifiers
;;;     gdk_key_event_get_layout
;;;     gdk_key_event_get_level
;;;     gdk_key_event_is_modifier
;;;     gdk_key_event_matches
;;;     gdk_key_event_get_match
;;;
;;;     gdk_focus_event_get_in
;;;
;;;     gdk_touch_event_get_emulating_pointer
;;;
;;;     gdk_crossing_event_get_mode
;;;     gdk_crossing_event_get_detail
;;;     gdk_crossing_event_get_focus
;;;
;;;     gdk_grab_broken_event_get_grab_surface
;;;     gdk_grab_broken_event_get_implicit
;;;
;;;     gdk_dnd_event_get_drop
;;;
;;;     gdk_touchpad_event_get_gesture_phase
;;;     gdk_touchpad_event_get_n_fingers
;;;     gdk_touchpad_event_get_deltas
;;;     gdk_touchpad_event_get_pinch_angle_delta
;;;     gdk_touchpad_event_get_pinch_scale
;;;
;;;     gdk_pad_event_get_axis_value
;;;     gdk_pad_event_get_button
;;;     gdk_pad_event_get_group_mode
;;;
;;;     gdk_events_get_angle
;;;     gdk_events_get_center
;;;     gdk_events_get_distance

;;; --- 2023-5-29 --------------------------------------------------------------

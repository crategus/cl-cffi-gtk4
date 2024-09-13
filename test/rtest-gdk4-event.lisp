(in-package :gtk-test)

(def-suite gdk-event :in gdk-suite)
(in-suite gdk-event)

;;; --- Types and Values -------------------------------------------------------

;;;     GDK_CURRENT_TIME

(test gdk-current-time
  (is (= 0 gdk:+current-time+)))

;;;     GDK_PRIORITY_EVENTS
;;;     GDK_PRIORITY_REDRAW

;;;     GDK_EVENT_PROPAGATE
;;;     GDK_EVENT_STOP

(test gdk-event-propagate
  (is-false gdk:+event-propagate+)
  (is-true gdk:+event-stop+))

;;;     GDK_BUTTON_PRIMARY
;;;     GDK_BUTTON_MIDDLE
;;;     GDK_BUTTON_SECONDARY

;;;     GdkEventType

(test gdk-event-type
  ;; Check type
  (is (g:type-is-enum "GdkEventType"))
  ;; Check type initializer
  (is (eq (g:gtype "GdkEventType")
          (g:gtype (cffi:foreign-funcall "gdk_event_type_get_type" :size))))
  ;; Check registered name
  (is (eq 'gdk:event-type
          (glib:symbol-for-gtype "GdkEventType")))
  ;; Check names
  (is (equal '("GDK_DELETE" "GDK_MOTION_NOTIFY" "GDK_BUTTON_PRESS"
               "GDK_BUTTON_RELEASE" "GDK_KEY_PRESS" "GDK_KEY_RELEASE"
               "GDK_ENTER_NOTIFY" "GDK_LEAVE_NOTIFY" "GDK_FOCUS_CHANGE"
               "GDK_PROXIMITY_IN" "GDK_PROXIMITY_OUT" "GDK_DRAG_ENTER"
               "GDK_DRAG_LEAVE" "GDK_DRAG_MOTION" "GDK_DROP_START" "GDK_SCROLL"
               "GDK_GRAB_BROKEN" "GDK_TOUCH_BEGIN" "GDK_TOUCH_UPDATE"
               "GDK_TOUCH_END" "GDK_TOUCH_CANCEL" "GDK_TOUCHPAD_SWIPE"
               "GDK_TOUCHPAD_PINCH" "GDK_PAD_BUTTON_PRESS"
               "GDK_PAD_BUTTON_RELEASE" "GDK_PAD_RING" "GDK_PAD_STRIP"
               "GDK_PAD_GROUP_MODE" "GDK_TOUCHPAD_HOLD" "GDK_EVENT_LAST")
             (gtk-test:list-enum-item-name "GdkEventType")))
  ;; Check values
  (is (equal '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24
               25 26 27 28 29)
             (gtk-test:list-enum-item-value "GdkEventType")))
  ;; Check nick names
  (is (equal '("delete" "motion-notify" "button-press" "button-release"
               "key-press" "key-release" "enter-notify" "leave-notify"
               "focus-change" "proximity-in" "proximity-out" "drag-enter"
               "drag-leave" "drag-motion" "drop-start" "scroll" "grab-broken"
               "touch-begin" "touch-update" "touch-end" "touch-cancel"
               "touchpad-swipe" "touchpad-pinch" "pad-button-press"
               "pad-button-release" "pad-ring" "pad-strip" "pad-group-mode"
               "touchpad-hold" "event-last")
             (gtk-test:list-enum-item-nick "GdkEventType")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GdkEventType" GDK-EVENT-TYPE
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "gdk_event_type_get_type")
                                     (:DELETE 0)
                                     (:MOTION-NOTIFY 1)
                                     (:BUTTON-PRESS 2)
                                     (:BUTTON-RELEASE 3)
                                     (:KEY-PRESS 4)
                                     (:KEY-RELEASE 5)
                                     (:ENTER-NOTIFY 6)
                                     (:LEAVE-NOTIFY 7)
                                     (:FOCUS-CHANGE 8)
                                     (:PROXIMITY-IN 9)
                                     (:PROXIMITY-OUT 10)
                                     (:DRAG-ENTER 11)
                                     (:DRAG-LEAVE 12)
                                     (:DRAG-MOTION 13)
                                     (:DROP-START 14)
                                     (:SCROLL 15)
                                     (:GRAB-BROKEN 16)
                                     (:TOUCH-BEGIN 17)
                                     (:TOUCH-UPDATE 18)
                                     (:TOUCH-END 19)
                                     (:TOUCH-CANCEL 20)
                                     (:TOUCHPAD-SWIPE 21)
                                     (:TOUCHPAD-PINCH 22)
                                     (:PAD-BUTTON-PRESS 23)
                                     (:PAD-BUTTON-RELEASE 24)
                                     (:PAD-RING 25)
                                     (:PAD-STRIP 26)
                                     (:PAD-GROUP-MODE 27)
                                     (:TOUCHPAD-HOLD 28)
                                     (:EVENT-LAST 29))
             (gobject:get-g-type-definition "GdkEventType"))))

;;;     GdkKeymapKey

;;;     GdkKeyMatch

(test gdk-key-match
  ;; Check type
  (is (g:type-is-enum "GdkKeyMatch"))
  ;; Check type initializer
  (is (eq (g:gtype "GdkKeyMatch")
          (g:gtype (cffi:foreign-funcall "gdk_key_match_get_type" :size))))
  ;; Check registered name
  (is (eq 'gdk:key-match
          (glib:symbol-for-gtype "GdkKeyMatch")))
  ;; Check names
  (is (equal '("GDK_KEY_MATCH_NONE" "GDK_KEY_MATCH_PARTIAL"
               "GDK_KEY_MATCH_EXACT")
             (gtk-test:list-enum-item-name "GdkKeyMatch")))
  ;; Check values
  (is (equal '(0 1 2)
             (gtk-test:list-enum-item-value "GdkKeyMatch")))
  ;; Check nick names
  (is (equal '("none" "partial" "exact")
             (gtk-test:list-enum-item-nick "GdkKeyMatch")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GdkKeyMatch" GDK-KEY-MATCH
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "gdk_key_match_get_type")
                                     (:NONE 0)
                                     (:PARTIAL 1)
                                     (:EXACT 2))
             (gobject:get-g-type-definition "GdkKeyMatch"))))

;;;     GdkTouchpadGesturePhase

(test gdk-touchpad-gesture-phase
  ;; Check type
  (is (g:type-is-enum "GdkTouchpadGesturePhase"))
  ;; Check type initializer
  (is (eq (g:gtype "GdkTouchpadGesturePhase")
          (g:gtype (cffi:foreign-funcall "gdk_touchpad_gesture_phase_get_type"
                                         :size))))
  ;; Check registered name
  (is (eq 'gdk:touchpad-gesture-phase
          (glib:symbol-for-gtype "GdkTouchpadGesturePhase")))
  ;; Check names
  (is (equal '("GDK_TOUCHPAD_GESTURE_PHASE_BEGIN"
               "GDK_TOUCHPAD_GESTURE_PHASE_UPDATE"
               "GDK_TOUCHPAD_GESTURE_PHASE_END"
               "GDK_TOUCHPAD_GESTURE_PHASE_CANCEL")
             (gtk-test:list-enum-item-name "GdkTouchpadGesturePhase")))
  ;; Check values
  (is (equal '(0 1 2 3)
             (gtk-test:list-enum-item-value "GdkTouchpadGesturePhase")))
  ;; Check nick names
  (is (equal '("begin" "update" "end" "cancel")
             (gtk-test:list-enum-item-nick "GdkTouchpadGesturePhase")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GdkTouchpadGesturePhase"
                                     GDK-TOUCHPAD-GESTURE-PHASE
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "gdk_touchpad_gesture_phase_get_type")
                                     (:BEGIN 0)
                                     (:UPDATE 1)
                                     (:END 2)
                                     (:CANCEL 3))
             (gobject:get-g-type-definition "GdkTouchpadGesturePhase"))))

;;;     GdkScrollDirection

(test gdk-scroll-direction
  ;; Check type
  (is (g:type-is-enum "GdkScrollDirection"))
  ;; Check type initializer
  (is (eq (g:gtype "GdkScrollDirection")
          (g:gtype (cffi:foreign-funcall "gdk_scroll_direction_get_type"
                                         :size))))
  ;; Check registered name
  (is (eq 'gdk:scroll-direction
          (glib:symbol-for-gtype "GdkScrollDirection")))
  ;; Check names
  (is (equal '("GDK_SCROLL_UP" "GDK_SCROLL_DOWN" "GDK_SCROLL_LEFT"
               "GDK_SCROLL_RIGHT" "GDK_SCROLL_SMOOTH")
             (gtk-test:list-enum-item-name "GdkScrollDirection")))
  ;; Check values
  (is (equal '(0 1 2 3 4)
             (gtk-test:list-enum-item-value "GdkScrollDirection")))
  ;; Check nick names
  (is (equal '("up" "down" "left" "right" "smooth")
             (gtk-test:list-enum-item-nick "GdkScrollDirection")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GdkScrollDirection" GDK-SCROLL-DIRECTION
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "gdk_scroll_direction_get_type")
                                     (:UP 0)
                                     (:DOWN 1)
                                     (:LEFT 2)
                                     (:RIGHT 3)
                                     (:SMOOTH 4))
             (gobject:get-g-type-definition "GdkScrollDirection"))))

;;;     GdkCrossingMode

(test gdk-crossing-mode
  ;; Check type
  (is (g:type-is-enum "GdkCrossingMode"))
  ;; Check type initializer
  (is (eq (g:gtype "GdkCrossingMode")
          (g:gtype (cffi:foreign-funcall "gdk_crossing_mode_get_type" :size))))
  ;; Check registered name
  (is (eq 'gdk:crossing-mode
          (glib:symbol-for-gtype "GdkCrossingMode")))
  ;; Check names
  (is (equal '("GDK_CROSSING_NORMAL" "GDK_CROSSING_GRAB" "GDK_CROSSING_UNGRAB"
               "GDK_CROSSING_GTK_GRAB" "GDK_CROSSING_GTK_UNGRAB"
               "GDK_CROSSING_STATE_CHANGED" "GDK_CROSSING_TOUCH_BEGIN"
               "GDK_CROSSING_TOUCH_END" "GDK_CROSSING_DEVICE_SWITCH")
             (gtk-test:list-enum-item-name "GdkCrossingMode")))
  ;; Check values
  (is (equal '(0 1 2 3 4 5 6 7 8)
             (gtk-test:list-enum-item-value "GdkCrossingMode")))
  ;; Check nick names
  (is (equal '("normal" "grab" "ungrab" "gtk-grab" "gtk-ungrab" "state-changed"
               "touch-begin" "touch-end" "device-switch")
             (gtk-test:list-enum-item-nick "GdkCrossingMode")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GdkCrossingMode" GDK-CROSSING-MODE
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "gdk_crossing_mode_get_type")
                                     (:NORMAL 0)
                                     (:GRAB 1)
                                     (:UNGRAB 2)
                                     (:GTK-GRAB 3)
                                     (:GTK-UNGRAB 4)
                                     (:STATE-CHANGED 5)
                                     (:TOUCH-BEGIN 6)
                                     (:TOUCH-END 7)
                                     (:DEVICE-SWITCH 8))
             (gobject:get-g-type-definition "GdkCrossingMode"))))

;;;     GdkScrollUnit                                      Since 4.8

(test gdk-scroll-unit
  ;; Check type
  (is (g:type-is-enum "GdkScrollUnit"))
  ;; Check type initializer
  (is (eq (g:gtype "GdkScrollUnit")
          (g:gtype (cffi:foreign-funcall "gdk_scroll_unit_get_type" :size))))
  ;; Check registered name
  (is (eq 'gdk:scroll-unit
          (glib:symbol-for-gtype "GdkScrollUnit")))
  ;; Check names
  (is (equal '("GDK_SCROLL_UNIT_WHEEL" "GDK_SCROLL_UNIT_SURFACE")
             (gtk-test:list-enum-item-name "GdkScrollUnit")))
  ;; Check values
  (is (equal '(0 1)
             (gtk-test:list-enum-item-value "GdkScrollUnit")))
  ;; Check nick names
  (is (equal '("wheel" "surface")
             (gtk-test:list-enum-item-nick "GdkScrollUnit")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GdkScrollUnit" GDK-SCROLL-UNIT
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "gdk_scroll_unit_get_type")
                                     (:WHEEL 0)
                                     (:SURFACE 1))
             (gobject:get-g-type-definition "GdkScrollUnit"))))

;;;     GdkNotifyType

(test gdk-notify-type
  ;; Check type
  (is (g:type-is-enum "GdkNotifyType"))
  ;; Check type initializer
  (is (eq (g:gtype "GdkNotifyType")
          (g:gtype (cffi:foreign-funcall "gdk_notify_type_get_type" :size))))
  ;; Check registered name
  (is (eq 'gdk:notify-type
          (glib:symbol-for-gtype "GdkNotifyType")))
  ;; Check names
  (is (equal '("GDK_NOTIFY_ANCESTOR" "GDK_NOTIFY_VIRTUAL" "GDK_NOTIFY_INFERIOR"
               "GDK_NOTIFY_NONLINEAR" "GDK_NOTIFY_NONLINEAR_VIRTUAL"
               "GDK_NOTIFY_UNKNOWN")
             (gtk-test:list-enum-item-name "GdkNotifyType")))
  ;; Check values
  (is (equal '(0 1 2 3 4 5)
             (gtk-test:list-enum-item-value "GdkNotifyType")))
  ;; Check nick names
  (is (equal '("ancestor" "virtual" "inferior" "nonlinear" "nonlinear-virtual"
               "unknown")
             (gtk-test:list-enum-item-nick "GdkNotifyType")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GdkNotifyType"
    GDK-NOTIFY-TYPE
    (:EXPORT T :TYPE-INITIALIZER "gdk_notify_type_get_type")
  (:ANCESTOR 0)
  (:VIRTUAL 1)
  (:INFERIOR 2)
  (:NONLINEAR 3)
  (:NONLINEAR-VIRTUAL 4)
  (:UNKNOWN 5))
             (gobject:get-g-type-definition "GdkNotifyType"))))

;;;     GdkEventSequence

(test gdk-event-sequence-boxed
  ;; Check type
  (is (g:type-is-boxed "GdkEventSequence"))
  ;; Check type initializer
  (is (eq (g:gtype "GdkEventSequence")
          (g:gtype (cffi:foreign-funcall "gdk_event_sequence_get_type" :size))))
  ;; Check registered name
  (is (eq 'gdk:event-sequence
          (glib:symbol-for-gtype "GdkEventSequence"))))

;;;     GdkEvent

#+nil
(test event-class
  ;; Check type
  (is (g:type-is-object "GdkEvent"))
  ;; Check registered name
  (is (eq 'gdk:event
          (glib:symbol-for-gtype "GdkEvent")))
  ;; Check type initializer
  (is (eq (g:gtype "GdkEvent")
          (g:gtype (cffi:foreign-funcall "gdk_event_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GdkEvent")))
  ;; Check children
  (is (equal '("GdkButtonEvent" "GdkCrossingEvent" "GdkDNDEvent"
               "GdkDeleteEvent" "GdkFocusEvent" "GdkGrabBrokenEvent"
               "GdkKeyEvent" "GdkMotionEvent" "GdkPadEvent" "GdkProximityEvent"
               "GdkScrollEvent" "GdkTouchEvent" "GdkTouchpadEvent")
             (gtk-test:list-children "GdkEvent")))
  ;; Check interfaces
  (is (equal '()
             (gtk-test:list-interfaces "GdkEvent")))
  ;; Check properties
  (is (equal '()
             (gtk-test:list-properties "GdkEvent")))
  ;; Check signals
  (is (equal '()
             (gtk-test:list-signals "GdkEvent")))
  ;; Check class definition
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

;;; 2024-1-6

;;; ----------------------------------------------------------------------------
;;; gdk4.event.lisp
;;;
;;; The documentation of this file is taken from the GDK 4 Reference Manual
;;; Version 4.10 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 - 2024 Dieter Kaiser
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;; ----------------------------------------------------------------------------
;;;
;;; Events
;;;
;;;     Functions for handling events from the window system
;;;
;;; Types and Values
;;;
;;;     GDK_CURRENT_TIME
;;;     GDK_PRIORITY_EVENTS
;;;     GDK_PRIORITY_REDRAW
;;;     GDK_EVENT_PROPAGATE
;;;     GDK_EVENT_STOP
;;;     GDK_BUTTON_PRIMARY
;;;     GDK_BUTTON_MIDDLE
;;;     GDK_BUTTON_SECONDARY
;;;
;;;     GdkEventType
;;;     GdkKeymapKey
;;;     GdkKeyMatch
;;;     GdkTouchpadGesturePhase
;;;     GdkScrollDirection
;;;     GdkCrossingMode
;;;     GDKScrollUnit                                      Since 4.8
;;;     GdkNotifyType
;;;
;;;     GdkEventSequence
;;;
;;;     GdkEvent
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
;;;
;;; Object Hierarchy
;;;
;;;    GdkEvent
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GDK_CURRENT_TIME
;;; ----------------------------------------------------------------------------

(defconstant +current-time+ 0
 #+liber-documentation
 "@version{2024-4-1}
  @begin{short}
    Represents the current time, and can be used anywhere a time is expected.
  @end{short}")

#+liber-documentation
(setf (liber:alias-for-variable '+current-time+) "Constant")

(export '+current-time+)

;;; ----------------------------------------------------------------------------
;;; GDK_PRIORITY_EVENTS
;;;
;;; #define GDK_PRIORITY_EVENTS (G_PRIORITY_DEFAULT)
;;;
;;; This is the priority that events from the X server are given in the GLib
;;; Main Loop.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GDK_PRIORITY_REDRAW
;;;
;;; #define GDK_PRIORITY_REDRAW     (G_PRIORITY_HIGH_IDLE + 20)
;;;
;;; This is the priority that the idle handler processing surface updates is
;;; given in the GLib Main Loop.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GDK_EVENT_PROPAGATE
;;; ----------------------------------------------------------------------------

(defconstant +event-propagate+ nil
 #+liber-documentation
 "@version{2024-4-1}
  @variable-value{@em{false}}
  @begin{short}
    Use this value as the return value for continuing the propagation of an
    event handler.
  @end{short}
  @see-variable{gdk:+event-stop+}")

#+liber-documentation
(setf (liber:alias-for-variable '+event-propagate+) "Constant")

(export '+event-propagate+)

;;; ----------------------------------------------------------------------------
;;; GDK_EVENT_STOP
;;; ----------------------------------------------------------------------------

(defconstant +event-stop+ t
 #+liber-documentation
 "@version{2024-4-1}
  @variable-value{@em{true}}
  @begin{short}
    Use this value as the return value for stopping the propagation of an event
    handler.
  @end{short}
  @begin[Example]{dictionary}
    This event handler for the @code{\"close-request\"} signal of a window
    stops the propagation of the event and the window is not closed.
    @begin{pre}
(g:signal-connect window \"close-request\"
                  (lambda (widget event)
                    (declare (ignore widget event))
                    gdk:+event-stop+))
    @end{pre}
  @end{dictionary}
  @see-variable{gdk:+event-propagate+}")

#+liber-documentation
(setf (liber:alias-for-variable '+event-stop+) "Constant")

(export '+event-stop+)

;;; ----------------------------------------------------------------------------
;;; GDK_BUTTON_PRIMARY
;;;
;;; #define GDK_BUTTON_PRIMARY      (1)
;;;
;;; The primary button. This is typically the left mouse button, or the right
;;; button in a left-handed setup.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GDK_BUTTON_MIDDLE
;;;
;;; #define GDK_BUTTON_MIDDLE       (2)
;;;
;;; The middle button.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GDK_BUTTON_SECONDARY
;;;
;;; #define GDK_BUTTON_SECONDARY    (3)
;;;
;;; The secondary button. This is typically the right mouse button, or the left
;;; button in a left-handed setup.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GdkEventType
;;; ----------------------------------------------------------------------------

(gobject:define-g-enum "GdkEventType" event-type
  (:export t
   :type-initializer "gdk_event_type_get_type")
  (:nothing -1)
  :delete
  :motion-notify
  :button-press
  :button-release
  :key-press
  :key-release
  :enter-notify
  :leave-notify
  :focus-change
  :proximity-in
  :proximity-out
  :drag-enter
  :drag-leave
  :drag-motion
  :drop-start
  :scroll
  :grab-broken
  :touch-begin
  :touch-update
  :touch-end
  :touch-cancel
  :touchpad-swipe
  :touchpad-pinch
  :pad-button-press
  :pad-button-release
  :pad-ring
  :pad-strip
  :pad-group-mode
  #+gtk-4-6
  :touchpad-hold
  :event-last)

#+liber-documentation
(setf (liber:alias-for-symbol 'event-type)
      "GEnum"
      (liber:symbol-documentation 'event-type)
 "@version{2023-12-17}
  @short{Specifies the type of a @class{gdk:event} instance.}
  @begin{pre}
(gobject:define-g-enum \"GdkEventType\" event-type
  (:export t
   :type-initializer \"gdk_event_type_get_type\")
  (:nothing -1)
  :delete
  :motion-notify
  :button-press
  :button-release
  :key-press
  :key-release
  :enter-notify
  :leave-notify
  :focus-change
  :proximity-in
  :proximity-out
  :drag-enter
  :drag-leave
  :drag-motion
  :drop-start
  :scroll
  :grab-broken
  :touch-begin
  :touch-update
  :touch-end
  :touch-cancel
  :touchpad-swipe
  :touchpad-pinch
  :pad-button-press
  :pad-button-release
  :pad-ring
  :pad-strip
  :pad-group-mode
  :touchpad-hold
  :event-last)
  @end{pre}
  @begin[code]{table}
    @entry[:nothing]{A special code to indicate a null event.}
    @entry[:delete]{The window manager has requested that the toplevel window
      be hidden or destroyed, usually when the user clicks on a special icon
      in the title bar.}
    @entry[:motion-notify]{The pointer, usually a mouse, has moved.}
    @entry[:button-press]{A mouse button has been pressed.}
    @entry[:button-release]{A mouse button has been released.}
    @entry[:key-press]{A key has been pressed.}
    @entry[:key-release]{A key has been released.}
    @entry[:enter-notifiy]{The pointer has entered the window.}
    @entry[:leave-notify]{The pointer has left the window.}
    @entry[:focus-change]{The keyboard focus has entered or left the window.}
    @entry[:proximity-in]{An input device has moved into contact with a
      sensing surface, e.g. a touchscreen or graphics tablet.}
    @entry[:proximity-out]{An input device has moved out of contact with a
      sensing surface.}
    @entry[:drag-enter]{The mouse has entered the window while a drag is in
      progress.}
    @entry[:drag-leave]{The mouse has left the window while a drag is in
      progress.}
    @entry[:drag-motion]{The mouse has moved in the window while a drag is in
      progress.}
    @entry[:drop-start]{A drop operation onto the window has started.}
    @entry[:scroll]{The scroll wheel was turned.}
    @entry[:grab-broken]{A pointer or keyboard grab was broken.}
    @entry[:touch-begin]{A new touch event sequence has just started.}
    @entry[:touch-update]{A touch event sequence has been updated.}
    @entry[:touch-end]{A touch event sequence has finished.}
    @entry[:touch-cancel]{A touch event sequence has been canceled.}
    @entry[:touchpad-swipe]{A touchpad swipe gesture event, the current state
      is determined by its phase field.}
    @entry[:touchpad-pinch]{A touchpad pinch gesture event, the current state
      is determined by its phase field.}
    @entry[:pad-button-press]{A tablet pad button press event.}
    @entry[:pad-button-release]{A tablet pad button release event.}
    @entry[:pad-ring]{A tablet pad axis event from a \"ring\".}
    @entry[:pad-strip]{A tablet pad axis event from a \"strip\".}
    @entry[:pad-group-mode]{A tablet pad group mode change.}
    @entry[:touchpad-hold]{A touchpad hold gesture event, the current state is
      determined by its phase field. Since 4.6}
    @entry[:event-last]{Marks the end of the @symbol{gdk:event-type}
      enumeration.}
  @end{table}
  @see-class{gdk:event}")

;;; ----------------------------------------------------------------------------
;;; struct GdkKeymapKey
;;;
;;; struct GdkKeymapKey {
;;;   guint keycode;
;;;   int   group;
;;;   int   level;
;;; };
;;;
;;; A GdkKeymapKey is a hardware key that can be mapped to a keyval.
;;;
;;; guint keycode;
;;;     the hardware keycode. This is an identifying number for a physical key.
;;;
;;; int group;
;;;     indicates movement in a horizontal direction. Usually groups are used
;;;     for two different languages. In group 0, a key might have two English
;;;     characters, and in group 1 it might have two Hebrew characters. The
;;;     Hebrew characters will be printed on the key next to the English
;;;     characters.
;;;
;;; int level;
;;;     indicates which symbol on the key will be used, in a vertical direction.
;;;     So on a standard US keyboard, the key with the number “1” on it also has
;;;     the exclamation point ("!") character on it. The level indicates whether
;;;     to use the “1” or the “!” symbol. The letter keys are considered to have
;;;     a lowercase letter at level 0, and an uppercase letter at level 1,
;;;     though only the uppercase letter is printed.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GdkKeyMatch
;;; ----------------------------------------------------------------------------

(gobject:define-g-enum "GdkKeyMatch" key-match
  (:export t
   :type-initializer "gdk_key_match_get_type")
  (:none 0)
  (:partial 1)
  (:exact 2))

#+liber-documentation
(setf (liber:alias-for-symbol 'key-match)
      "GEnum"
      (liber:symbol-documentation 'key-match)
 "@version{2023-12-17}
  @begin{short}
    Describes how well an event matches a given keyval and modifiers.
  @end{short}
  The @symbol{gdk:key-match} values are returned by the
  @fun{gdk:key-event-matches} function.
  @begin{pre}
(gobject:define-g-enum \"GdkKeyMatch\" key-match
  (:export t
   :type-initializer \"gdk_key_match_get_type\")
  (:none 0)
  (:partial 1)
  (:exact 2))
  @end{pre}
  @begin[code]{table}
    @entry[:none]{The key event does not match.}
    @entry[:partial]{The key event matches if keyboard state (specifically, the
      currently active group) is ignored.}
    @entry[:exact]{The key event matches.}
  @end{table}
  @see-class{gdk:key-event}
  @see-function{gdk:key-event-matches}")

;;; ----------------------------------------------------------------------------
;;; enum GdkTouchpadGesturePhase
;;; ----------------------------------------------------------------------------

(gobject:define-g-enum "GdkTouchpadGesturePhase" touchpad-gesture-phase
  (:export t
   :type-initializer "gdk_touchpad_gesture_phase_get_type")
  (:begin 0)
  (:update 1)
  (:end 2)
  (:cancel 3))

#+liber-documentation
(setf (liber:alias-for-symbol 'touchpad-gesture-phase)
      "GEnum"
      (liber:symbol-documentation 'touchpad-gesture-phase)
 "@version{2023-12-17}
  @begin{short}
    The @symbol{gdk:touchpad-gesture-phase} enumeration specifies the current
    state of a touchpad gesture.
  @end{short}
  All gestures are guaranteed to begin with an event with @code{:begin}
  phase, followed by 0 or several events with @code{:update} phase.

  A finished gesture may have 2 possible outcomes, an event with @code{:end}
  phase will be emitted when the gesture is considered successful, this should
  be used as the hint to perform any permanent changes.

  Cancelled gestures may be so for a variety of reasons, due to hardware or the
  compositor, or due to the gesture recognition layers hinting the gesture did
  not finish resolutely, e.g. a 3rd finger being added during a pinch gesture.
  In these cases, the last event will report the @code{:cancel} phase, this
  should be used as a hint to undo any visible/permanent changes that were done
  throughout the progress of the gesture.
  @begin{pre}
(gobject:define-g-enum \"GdkTouchpadGesturePhase\" touchpad-gesture-phase
  (:export t
   :type-initializer \"gdk_touchpad_gesture_phase_get_type\")
  (:begin 0)
  (:update 1)
  (:end 2)
  (:cancel 3))
  @end{pre}
  @begin[code]{table}
    @entry[:begin]{The gesture has begun.}
    @entry[:update]{The gesture has been updated.}
    @entry[:end]{The gesture was finished, changes should be permanently
      applied.}
    @entry[:cancel]{The gesture was cancelled, all changes should be undone.}
  @end{table}
  @see-class{gdk:touchpad-event}")

;;; ----------------------------------------------------------------------------
;;; enum GdkScrollDirection
;;; ----------------------------------------------------------------------------

(gobject:define-g-enum "GdkScrollDirection" scroll-direction
  (:export t
   :type-initializer "gdk_scroll_direction_get_type")
  (:up 0)
  (:down 1)
  (:left 2)
  (:right 3)
  (:smooth 4))

#+liber-documentation
(setf (liber:alias-for-symbol 'scroll-direction)
      "GEnum"
      (liber:symbol-documentation 'scroll-direction)
 "@version{2023-12-17}
  @short{Specifies the direction for a @class{gdk:scroll-event} event.}
  @begin{pre}
(gobject:define-g-enum \"GdkScrollDirection\" scroll-direction
  (:export t
   :type-initializer \"gdk_scroll_direction_get_type\")
  (:up 0)
  (:down 1)
  (:left 2)
  (:right 3)
  (:smooth 4))
  @end{pre}
  @begin[code]{table}
    @entry[:up]{The window is scrolled up.}
    @entry[:down]{The window is scrolled down.}
    @entry[:left]{The window is scrolled to the left.}
    @entry[:right]{The window is scrolled to the right.}
    @entry[:smooth]{The scrolling is determined by the delta values in the
      @class{gdk:scroll-event} event. See the @fun{gdk:scroll-event-deltas}
      function.}
  @end{table}
  @see-class{gdk:scroll-event}
  @see-function{gdk:scroll-event-deltas}")

;;; ----------------------------------------------------------------------------
;;; enum GdkCrossingMode
;;; ----------------------------------------------------------------------------

(gobject:define-g-enum "GdkCrossingMode" crossing-mode
  (:export t
   :type-initializer "gdk_crossing_mode_get_type")
  :normal
  :grab
  :ungrab
  :gtk-grab
  :gtk-ungrab
  :state-changed
  :touch-begin
  :touch-end
  :device-switch)

#+liber-documentation
(setf (liber:alias-for-symbol 'crossing-mode)
      "GEnum"
      (liber:symbol-documentation 'crossing-mode)
 "@version{2023-12-17}
  @short{Specifies the crossing mode for enter and leave events.}
  @begin{pre}
(gobject:define-g-enum \"GdkCrosssingMode\" crossing-mode
  (:export t
   :type-initializer \"gdk_crossing_mode_get_type\")
  :normal
  :grab
  :ungrab
  :gtk-grab
  :gtk-ungrab
  :state-changed
  :touch-begin
  :touch-end
  :device-switch)
  @end{pre}
  @begin[code]{table}
    @entry[:normal]{Crossing because of pointer motion.}
    @entry[:grab]{Crossing because a grab is activated.}
    @entry[:ungrab]{Crossing because a grab is deactivated.}
    @entry[:gtk-grab]{Crossing because a GTK grab is activated.}
    @entry[:gtk-ungrab]{Crossing because a GTK grab is deactivated.}
    @entry[:state-changed]{Crossing because a GTK widget changed state, e.g.
      sensitivity.}
    @entry[:touch-begin]{Crossing because a touch sequence has begun, this
      event is synthetic as the pointer might have not left the window.}
    @entry[:touch-end]{Crossing because a touch sequence has ended, this event
      is synthetic as the pointer might have not left the window.}
    @entry[:device-switch]{Crossing because of a device switch, i.e. a mouse
      taking control of the pointer after a touch device, this event is
      synthetic as the pointer did not leave the window.}
  @end{table}
  @see-class{gdk:crossing-event}")

;;; ----------------------------------------------------------------------------
;;; enum GdkScrollUnit
;;; ----------------------------------------------------------------------------

#+gtk-4-8
(gobject:define-g-enum "GdkScrollUnit" scroll-unit
  (:export t
   :type-initializer "gdk_scroll_unit_get_type")
  :wheel
  :surface)

#+(and gtk-4-8 liber-documentation)
(setf (liber:alias-for-symbol 'scroll-unit)
      "GEnum"
      (liber:symbol-documentation 'scroll-unit)
 "@version{2023-12-17}
  @begin{short}
    Specifies the unit of scroll deltas.
  @end{short}
  When you get the @code{:wheel} value, a delta of 1.0 means 1 wheel detent
  click in the south direction, 2.0 means 2 wheel detent clicks in the south
  direction. This is the same logic for negative values but in the north
  direction.

  If you get the @code{:surface} value, are managing a scrollable view and get
  a value of 123, you have to scroll 123 surface logical pixels right if it is
  @code{delta_x} or down if it is @code{delta_y}. This is the same logic for
  negative values but you have to scroll left instead of right if it is
  @code{delta_x} and up instead of down if it is @code{delta_y}.

  1 surface logical pixel is equal to 1 real screen pixel multiplied by the
  final scale factor of your graphical interface, the product of the desktop
  scale factor and eventually a custom scale factor in your application.
  @begin{pre}
(gobject:define-g-enum \"GdkScrollUnit\" scroll-unit
  (:export t
   :type-initializer \"gdk_scroll_unit_get_type\")
  :wheel
  :surface)
  @end{pre}
  @begin[code]{table}
    @entry[:wheel]{The delta is in number of wheel clicks.}
    @entry[:surface]{The delta is in surface pixels to scroll directly on
      screen.}
  @end{table}
  Since 4.8
  @see-function{gtk:event-controller-scroll-unit}")

;;; ----------------------------------------------------------------------------
;;; enum GdkNotifyType
;;; ----------------------------------------------------------------------------

(gobject:define-g-enum "GdkNotifyType" notify-type
  (:export t
   :type-initializer "gdk_notify_type_get_type")
  (:ancestor 0)
  :virtual
  :inferior
  :nonlinear
  :nonlinear-virtual
  :unknown)

#+liber-documentation
(setf (liber:alias-for-symbol 'notify-type)
      "GEnum"
      (liber:symbol-documentation 'notify-type)
 "@version{2023-12-17}
  @short{Specifies the kind of crossing for enter and leave events.}
  See the X11 protocol specification of @code{LeaveNotify} for full details of
  crossing event generation.
  @begin{pre}
(gobject:define-g-enum notify-type
  (:export t
   :type-initializer \"gdk_notify_type_get_type\")
  (:ancestor 0)
  :virtual
  :inferior
  :nonlinear
  :nonlinear-virtual
  :unknown)
  @end{pre}
  @begin[code]{table}
    @entry[:ancestor]{The window is entered from an ancestor or left towards
      an ancestor.}
    @entry[:virtual]{The pointer moves between an ancestor and an inferior of
      the window.}
    @entry[:inferior]{The window is entered from an inferior or left towards
      an inferior.}
    @entry[:nonlinear]{The window is entered from or left towards a window
      which is neither an ancestor nor an inferior.}
    @entry[:nonlinear-virtual]{The pointer moves between two windows which are
      not ancestors of each other and the window is part of the ancestor chain
      between one of these windows and their least common ancestor.}
    @entry[:unknown]{An unknown type of enter/leave event occurred.}
  @end{table}
  @see-class{gdk:crossing-event}")

;;; ----------------------------------------------------------------------------
;;; GdkEventSequence
;;; ----------------------------------------------------------------------------

(glib:define-g-boxed-opaque event-sequence "GdkEventSequence"
  :export t
  :type-initializer "gdk_event_sequence_get_type"
  :alloc (error "GdkEventSequence cannot be created from the Lisp side."))

#+liber-documentation
(setf (liber:alias-for-class 'event-sequence)
      "GBoxed"
      (documentation 'event-sequence 'type)
 "@version{2023-12-17}
  @begin{short}
    The @class{gdk:event-sequence} structure is an opaque type representing a
    sequence of related touch events.
  @end{short}
  See the @fun{gdk:event-event-sequence} function.
  @see-function{gdk:event-event-sequence}")

(export 'event-sequence)

;;; ----------------------------------------------------------------------------
;;; GdkEvent
;;;
;;; This section describes functions dealing with events from the window system.
;;;
;;; ----------------------------------------------------------------------------

(cffi:define-foreign-type event ()
  ()
  (:actual-type :pointer)
  (:simple-parser event))

(defmethod cffi:translate-to-foreign (proxy (type event))
  proxy)

(defmethod cffi:translate-from-foreign (native (type event))
  native)

#+liber-documentation
(setf (liber:alias-for-class 'event)
      "GdkEvent"
      (documentation 'event 'type)
 "@version{#2023-7-25}
  @begin{short}
    The base type of an event.
  @end{short}
  In GTK applications the events are handled automatically by toplevel widgets
  and passed on to the event controllers of appropriate widgets, so these
 functions are rarely needed.")

(export 'event)

;;; ----------------------------------------------------------------------------
;;; GdkButtonEvent
;;; ----------------------------------------------------------------------------

(cffi:define-foreign-type button-event (event)
  ()
  (:simple-parser button-event))

#+liber-documentation
(setf (liber:alias-for-class 'button-event)
      "GdkEvent"
      (documentation 'button-event 'type)
 "@version{#2023-7-25}
  @begin{short}
    An event related to a button on a pointer device.
  @end{short}
  @see-class{gdk:event}")

(export 'button-event)

;;; ----------------------------------------------------------------------------
;;; GdkScrollEvent
;;; ----------------------------------------------------------------------------

(cffi:define-foreign-type scroll-event (event)
  ()
  (:simple-parser sroll-event))

#+liber-documentation
(setf (liber:alias-for-class 'scroll-event)
      "GdkEvent"
      (documentation 'scroll-event 'type)
 "@version{#2023-7-25}
  @begin{short}
    An event related to a scrolling motion.
  @end{short}
  @see-class{gdk:event}")

(export 'scroll-event)

;;; ----------------------------------------------------------------------------
;;; GdkMotionEvent
;;; ----------------------------------------------------------------------------

(cffi:define-foreign-type motion-event (event)
  ()
  (:simple-parser motion-event))

#+liber-documentation
(setf (liber:alias-for-class 'motion-event)
      "GdkEvent"
      (documentation 'motion-event 'type)
 "@version{#2023-7-25}
  @begin{short}
    An event related to a pointer or touch device motion.
  @end{short}
  @see-class{gdk:event}")

(export 'motion-event)

;;; ----------------------------------------------------------------------------
;;; GdkKeyEvent
;;; ----------------------------------------------------------------------------

(cffi:define-foreign-type key-event (event)
  ()
  (:simple-parser key-event))

#+liber-documentation
(setf (liber:alias-for-class 'key-event)
      "GdkEvent"
      (documentation 'key-event 'type)
 "@version{#2023-7-25}
  @begin{short}
    An event related to a key-based device.
  @end{short}
  @see-class{gdk:event}")

(export 'key-event)

;;; ----------------------------------------------------------------------------
;;; GdkFocusEvent
;;; ----------------------------------------------------------------------------

(cffi:define-foreign-type focus-event (event)
  ()
  (:simple-parser focus-event))

#+liber-documentation
(setf (liber:alias-for-class 'focus-event)
      "GdkEvent"
      (documentation 'focus-event 'type)
 "@version{#2023-7-25}
  @begin{short}
    An event related to a focus change.
  @end{short}
  @see-class{gdk:event}")

(export 'focus-event)

;;; ----------------------------------------------------------------------------
;;; GdkCrossingEvent
;;; ----------------------------------------------------------------------------

(cffi:define-foreign-type crossing-event (event)
  ()
  (:simple-parser crossing-event))

#+liber-documentation
(setf (liber:alias-for-class 'crossing-event)
      "GdkEvent"
      (documentation 'crossing-event 'type)
 "@version{#2023-7-25}
  @begin{short}
    An event caused by a pointing device moving between surfaces.
  @end{short}
  @see-class{gdk:event}")

(export 'crossing-event)

;;; ----------------------------------------------------------------------------
;;; GdkGrabBrokenEvent
;;; ----------------------------------------------------------------------------

(cffi:define-foreign-type grab-broken-event (event)
  ()
  (:simple-parser grab-broken-event))

#+liber-documentation
(setf (liber:alias-for-class 'grab-broken-event)
      "GdkEvent"
      (documentation 'grab-broken-event 'type)
 "@version{#2023-7-25}
  @begin{short}
    An event related to a broken windowing system grab.
  @end{short}
  @see-class{gdk:event}")

(export 'grab-broken-event)

;;; ----------------------------------------------------------------------------
;;; GdkDeleteEvent
;;; ----------------------------------------------------------------------------

(cffi:define-foreign-type delete-event (event)
  ()
  (:simple-parser delete-event))

#+liber-documentation
(setf (liber:alias-for-class 'delete-event)
      "GdkEvent"
      (documentation 'delete-event 'type)
 "@version{#2023-7-25}
  @begin{short}
    An event related to closing a toplevel surface.
  @end{short}
  @see-class{gdk:event}")

(export 'delete-event)

;;; ----------------------------------------------------------------------------
;;; GdkDNDEvent
;;; ----------------------------------------------------------------------------

(cffi:define-foreign-type dnd-event (event)
  ()
  (:simple-parser dnd-event))

#+liber-documentation
(setf (liber:alias-for-class 'dnd-event)
      "GdkEvent"
      (documentation 'dnd-event 'type)
 "@version{#2023-7-25}
  @begin{short}
    An event related to drag and drop operations.
  @end{short}
  @see-class{gdk:event}")

(export 'dnd-event)

;;; ----------------------------------------------------------------------------
;;; GdkTouchEvent
;;; ----------------------------------------------------------------------------

(cffi:define-foreign-type touch-event (event)
  ()
  (:simple-parser touch-event))

#+liber-documentation
(setf (liber:alias-for-class 'touch-event)
      "GdkEvent"
      (documentation 'touch-event 'type)
 "@version{#2023-7-25}
  @begin{short}
    An event related to a touch-based device.
  @end{short}
  @see-class{gdk:event}")

(export 'touch-event)

;;; ----------------------------------------------------------------------------
;;; GdkTouchpadEvent
;;; ----------------------------------------------------------------------------

(cffi:define-foreign-type touchpad-event (event)
  ()
  (:simple-parser touchpad-event))

#+liber-documentation
(setf (liber:alias-for-class 'touchpad-event)
      "GdkEvent"
      (documentation 'touchpad-event 'type)
 "@version{#2023-7-25}
  @begin{short}
    An event related to a touchpad device.
  @end{short}
  @see-class{gdk:event}")

(export 'touchpad-event)

;;; ----------------------------------------------------------------------------
;;; GdkPadEvent
;;; ----------------------------------------------------------------------------

(cffi:define-foreign-type pad-event (event)
  ()
  (:simple-parser pad-event))

#+liber-documentation
(setf (liber:alias-for-class 'pad-event)
      "GdkEvent"
      (documentation 'pad-event 'type)
 "@version{#2023-7-25}
  @begin{short}
    An event related to a pad-based device.
  @end{short}
  @see-class{gdk:event}")

(export 'pad-event)

;;; ----------------------------------------------------------------------------
;;; GdkProximityEvent
;;; ----------------------------------------------------------------------------

(cffi:define-foreign-type proximity-event (event)
  ()
  (:simple-parser proximity-event))

#+liber-documentation
(setf (liber:alias-for-class 'proximity-event)
      "GdkEvent"
      (documentation 'proximity-event 'type)
 "@version{#2023-7-25}
  @begin{short}
    An event related to the proximity of a tool to a device.
  @end{short}
  @see-class{gdk:event}")

(export 'proximity-event)

;;; ----------------------------------------------------------------------------
;;; gdk_event_ref ()
;;;
;;; GdkEvent *
;;; gdk_event_ref (GdkEvent *event);
;;;
;;; Increase the ref count of event .
;;;
;;; event :
;;;     a GdkEvent
;;;
;;; Returns :
;;;     event .
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_event_unref ()
;;;
;;; void
;;; gdk_event_unref (GdkEvent *event);
;;;
;;; Decrease the ref count of event , and free it if the last reference is
;;; dropped.
;;;
;;; event :
;;;     a GdkEvent.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_event_type ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_event_get_event_type" event-event-type) event-type
 #+liber-documentation
 "@version{#2023-7-25}
  @argument[event]{a @class{gdk:event} instance}
  @return{The @symbol{gdk:event-type} value.}
  @short{Retrieves the type of the event.}
  @see-class{gdk:event}
  @see-symbol{gdk:event-type}"
  (event event))

(export 'event-event-type)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_surface ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_event_get_surface" event-surface) (g:object surface)
 #+liber-documentation
 "@version{#2023-7-25}
  @argument[event]{a @class{gdk:event} instance}
  @return{The @class{gdk:surface} object.}
  @short{Extracts the surface associated with an event.}
  @see-class{gdk:event}
  @see-symbol{gdk:surface}"
  (event event))

(export 'event-surface)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_device ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_event_get_device" event-device) (g:object device)
 #+liber-documentation
 "@version{#2023-7-25}
  @argument[event]{a @class{gdk:event} instance}
  @return{The @class{gdk:device} object.}
  @short{Returns the device of an event.}
  @see-class{gdk:event}
  @see-symbol{gdk:device}"
  (event event))

(export 'event-device)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_device_tool ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_event_get_device_tool" event-device-tool)
    (g:object device-tool)
 #+liber-documentation
 "@version{#2023-7-25}
  @argument[event]{a @class{gdk:event} instance}
  @return{The @class{gdk:device-tool} object, or @code{nil}.}
  @begin{short}
    If the event was generated by a device that supports different tools, e.g.
    a tablet, this function will return a @class{gdk:device-tool} object
    representing the tool that caused the event.
  @end{short}
  Otherwise, @code{nil} will be returned.

  Note: The @class{gdk:device-tool} objects will be constant during the
  application lifetime, if settings must be stored persistently across runs,
  see the @fun{gdk:device-tool-serial} function.
  @see-class{gdk:event}
  @see-symbol{gdk:device-tool}
  @see-function{gdk:device-tool-serial}"
  (event event))

(export 'event-device-tool)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_time ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_event_get_time" event-time) :uint32
 #+liber-documentation
 "@version{#2023-7-25}
  @argument[event]{a @class{gdk:event} instance}
  @return{The unsigned integer with the time stamp field from @arg{event}.}
  @begin{short}
    Returns the time stamp from @arg{event}, if there is one, otherwise returns
    @variable{gdk:+current-time+}.
  @end{short}
  @see-class{gdk:event}
  @see-variable{gdk:+current-time+}"
  (event event))

(export 'event-time)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_display ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_event_get_display" event-display) (g:object display)
 #+liber-documentation
 "@version{#2023-7-25}
  @argument[event]{a @class{gdk:event} instance}
  @return{The @class{gdk:display} object.}
  @short{Retrieves the display associated to the event.}
  @see-class{gdk:event}
  @see-variable{gdk:display}"
  (event event))

(export 'event-display)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_seat ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_event_get_seat" event-seat) (g:object seat)
 #+liber-documentation
 "@version{#2023-7-25}
  @argument[event]{a @class{gdk:event} instance}
  @return{The @class{gdk:seat} object.}
  @short{Returns the seat that originated the event.}
  @see-class{gdk:event}
  @see-class{gdk:seat}"
  (event event))

(export 'event-seat)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_event_sequence ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_event_get_event_sequence" event-event-sequence)
    (g:boxed event-sequence)
 #+liber-documentation
 "@version{#2023-7-25}
  @argument[event]{a @class{gdk:event} instance}
  @return{The @class{gdk:event-sequence} instance that the event belongs to.}
  @begin{short}
    If the event is a touch event, returns the @class{gdk:event-sequence}
    instance to which the event belongs. Otherwise, return @code{nil}.
  @end{short}
  @see-class{gdk:event}
  @see-class{gdk:event-sequence}"
  (event event))

(export 'event-event-sequence)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_modifier_state ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_event_get_modifier_state" event-modifier-state)
    modifier-type
 #+liber-documentation
 "@version{#2023-7-25}
  @argument[event]{a @class{gdk:event} instance}
  @return{The @symbol{gdk:modifier-type} value with the modifier state of
    @arg{event}.}
  @short{Returns the modifier state field of an event.}
  @see-class{gdk:event}
  @see-symbol{gdk:modifier-type}"
  (event event))

(export 'event-modifier-state)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_position ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_event_get_position" %event-position) :boolean
  (event event)
  (x (:pointer :double))
  (y (:pointer :double)))

(defun event-position (event)
 #+liber-documentation
 "@version{#2023-7-25}
  @argument[event]{a @class{gdk:event} instance}
  @begin{return}
    @arg{x} - a double float with the event surface x coordinate @br{}
    @arg{y} - a double float with the event surface y coordinate
  @end{return}
  @short{Extract the event surface relative x/y coordinates from an event.}
  @see-class{gdk:event}"
  (cffi:with-foreign-objects ((x :double) (y :double))
    (when (%event-position event x y)
      (values (cffi:mem-ref x :double)
              (cffi:mem-ref y :double)))))

(export 'event-position)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_axes ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_event_get_axes" %event-axes) :boolean
  (event event)
  (axes :pointer)
  (n-axes (:pointer :int)))

(defun event-axes (event)
 #+liber-documentation
 "@version{#2023-7-25}
  @argument[event]{a @class{gdk:event} instance}
  @return{The list of double float with the values for all axes.}
  @short{Extracts all axis values from an event.}
  @see-class{gdk:event}"
  (cffi:with-foreign-objects ((axes-ptr :pointer) (n-axes-ptr :int))
    (when (%event-axes event axes-ptr n-axes-ptr)
      (let ((axes (cffi:mem-ref axes-ptr :pointer))
            (n-axes (cffi:mem-ref n-axes-ptr :int)))
        (iter (for i from 0 below n-axes)
              (collect (cffi:mem-aref axes :double i))
              (finally (g:free axes-ptr)))))))

(export 'event-axes)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_axis ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_event_get_axis" %event-axis) :boolean
  (event event)
  (axis-use axis-use)
  (value (:pointer :double)))

(defun event-axis (event axis-use)
 #+liber-documentation
 "@version{#2023-7-25}
  @argument[event]{a @class{gdk:event} instance}
  @argument[axis-use]{a @symbol{gdk:axis-use} value with the axis use to look
    for}
  @return{The double float with the axis value.}
  @begin{short}
    Extract the axis value for a particular axis use from an event instance.
  @end{short}
  @see-class{gdk:event}
  @see-symbol{gdk:axis-use}"
  (cffi:with-foreign-object (value :double)
    (when (%event-axis event axis-use value)
      (cffi:mem-ref value :double))))

(export 'event-axis)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_history ()
;;;
;;; GdkTimeCoord *
;;; gdk_event_get_history (GdkEvent *event,
;;;                        guint *out_n_coords);
;;;
;;; Retrieves the history of the event , as a list of time and coordinates.
;;;
;;; The history includes events that are not delivered to the application
;;; because they occurred in the same frame as event .
;;;
;;; Note that only motion and scroll events record history, and motion events
;;; only if one of the mouse buttons is down.
;;;
;;; event :
;;;     a motion or scroll GdkEvent
;;;
;;; out_n_coords :
;;;     Return location for the length of the returned array.
;;;
;;; Returns :
;;;     an array of time and coordinates.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_pointer_emulated ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_event_get_pointer_emulated" event-pointer-emulated) :boolean
 #+liber-documentation
 "@version{#2023-7-25}
  @argument[event]{a @class{gdk:event} instance}
  @return{@em{True} if the event is emulated.}
  @begin{short}
    Returns whether this event is an emulated pointer event, typically from a
    touch event, as opposed to a real one.
  @end{short}
  @see-class{gdk:event}"
  (event event))

(export 'event-pointer-emulated)

;;; ----------------------------------------------------------------------------
;;; gdk_event_triggers_context_menu ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_event_triggers_context_menu" event-triggers-context-menu)
    :boolean
 #+liber-documentation
 "@version{#2023-7-25}
  @argument[event]{a @class{gdk:event} instance}
  @return{@em{True} if the event should trigger a context menu.}
  @begin{short}
    This function returns whether an event should trigger a context menu,
    according to platform conventions.
  @end{short}
  The right mouse button always triggers context menus.
  @see-class{gdk:event}"
  (event event))

(export 'event-triggers-context-menu)

;;; ----------------------------------------------------------------------------
;;; gdk_button_event_get_button ()
;;; ----------------------------------------------------------------------------

;; TODO: We check for the correct event type. Implement this more general.

(cffi:defcfun ("gdk_button_event_get_button" %button-event-button) :uint
  (event event))

(defun button-event-button (event)
 #+liber-documentation
 "@version{#2023-7-25}
  @argument[event]{a @class{gdk:event} instance}
  @return{The unsigned integer with the button of @arg{event}.}
  @short{Extract the button number from a button event.}
  @see-class{gdk:event}"
  (let ((etype (event-event-type event)))
    (when (or (eq :button-press etype) (eq :button-release etype))
      (%button-event-button event))))

(export 'button-event-button)

;;; ----------------------------------------------------------------------------
;;; gdk_scroll_event_get_direction ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_scroll_event_get_direction" scroll-event-direction)
    scroll-direction
 #+liber-documentation
 "@version{#2023-7-25}
  @argument[event]{a @class{gdk:event} instance}
  @return{The @symbol{gdk:scroll-direction} value with the direction of
    @arg{event}.}
  @short{Extracts the direction of a scroll event.}
  @see-class{gdk:event}
  @see-symbol{gdk:scroll-direction}"
  (event event))

(export 'scroll-event-direction)

;;; ----------------------------------------------------------------------------
;;; gdk_scroll_event_get_deltas ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_scroll_event_get_deltas" %scroll-event-deltas) :void
  (event event)
  (xdelta :double)
  (ydelta :double))

(defun scroll-event-deltas (event)
 #+liber-documentation
 "@version{#2023-7-25}
  @argument[event]{a @class{gdk:event} instance}
  @begin{return}
    @arg{xdelta} - a double float with the x scroll delta @br{}
    @arg{ydelta} - a double float with the y scroll delta
  @end{return}
  @begin{short}
    Extracts the scroll deltas of a scroll event.
  @end{short}
  The deltas will be zero unless the scroll direction is the @code{:smooth}
  value of the @symbol{gdk:scroll-direction} enumeration.
  @see-class{gdk:event}
  @see-symbol{gdk:scroll-direction}"
  (cffi:with-foreign-objects ((xdelta :double) (ydelta :double))
    (%scroll-event-deltas event xdelta ydelta)
    (values (cffi:mem-ref xdelta :double)
            (cffi:mem-ref ydelta :double))))

(export 'scroll-event-deltas)

;;; ----------------------------------------------------------------------------
;;; gdk_scroll_event_is_stop ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_scroll_event_is_stop" scroll-event-is-stop) :boolean
 #+liber-documentation
 "@version{#2023-7-25}
  @argument[event]{a @class{gdk:event} instance}
  @return{@em{True} if the event is a scroll stop event.}
  @begin{short}
    Check whether a scroll event is a stop scroll event.
  @end{short}
  Scroll sequences with smooth scroll information may provide a stop scroll
  event once the interaction with the device finishes, e.g. by lifting a finger.
  This stop scroll event is the signal that a widget may trigger kinetic
  scrolling based on the current velocity.

  Stop scroll events always have a delta of 0/0.
  @see-class{gdk:event}"
  (event event))

(export 'scroll-event-is-stop)

;;; ----------------------------------------------------------------------------
;;; gdk_key_event_get_keyval ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_key_event_get_keyval" key-event-keyval) :uint
 #+liber-documentation
 "@version{#2023-7-25}
  @argument[event]{a @class{gdk:event} instance}
  @return{The unsigned integer with the keyval of @arg{event}.}
  @short{Extracts the keyval from a key event.}
  @see-class{gdk:event}"
  (event event))

(export 'key-event-keyval)

;;; ----------------------------------------------------------------------------
;;; gdk_key_event_get_keycode ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_key_event_get_keycode" key-event-keycode) :uint
 #+liber-documentation
 "@version{#2023-7-25}
  @argument[event]{a @class{gdk:event} instance}
  @return{The unsigned integer with the keycode of @arg{event}.}
  @short{Extracts the keycode from a key event.}
  @see-class{gdk:event}"
  (event event))

(export 'key-event-keycode)

;;; ----------------------------------------------------------------------------
;;; gdk_key_event_get_consumed_modifiers ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_key_event_get_consumed_modifiers"
               key-event-consumed-modifiers) modifier-type
 #+liber-documentation
 "@version{#2023-7-25}
  @argument[event]{a @class{gdk:event} instance}
  @return{The @symbol{gdk:modifier-type} value.}
  @short{Extracts the consumed modifiers from a key event.}
  @see-class{gdk:event}
  @see-symbol{gdk:modifier-type}"
  (event event))

(export 'key-event-consumed-modifiers)

;;; ----------------------------------------------------------------------------
;;; gdk_key_event_get_layout ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_key_event_get_layout" key-event-layout) :uint
 #+liber-documentation
 "@version{#2023-7-25}
  @argument[event]{a @class{gdk:event} instance}
  @return{The unsigned integer with the layout of @arg{event}.}
  @short{Extracts the layout from a key event.}
  @see-class{gdk:event}"
  (event event))

(export 'key-event-layout)

;;; ----------------------------------------------------------------------------
;;; gdk_key_event_get_level ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_key_event_get_level" key-event-level) :uint
 #+liber-documentation
 "@version{#2023-7-25}
  @argument[event]{a @class{gdk:event} instance}
  @return{The unsigned integer with the shift level of @arg{event}.}
  @short{Extracts the shift level from a key event.}
  @see-class{gdk:event}"
  (event event))

(export 'key-event-level)

;;; ----------------------------------------------------------------------------
;;; gdk_key_event_is_modifier ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_key_event_is_modifier" key-event-is-modifier) :boolean
 #+liber-documentation
 "@version{#2023-7-25}
  @argument[event]{a @class{gdk:event} instance}
  @return{@em{True} if the event is for a modifier key.}
  @short{Extracts whether the key event is for a modifier key.}
  @see-class{gdk:event}"
  (event event))

(export 'key-event-is-modifier)

;;; ----------------------------------------------------------------------------
;;; gdk_key_event_matches ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_key_event_matches" key-event-matches) key-match
 #+liber-documentation
 "@version{#2023-7-23}
  @argument[event]{a @class{gdk:event} instance}
  @argument[keyval]{an unsigned integer with the keyval to match}
  @argument[modifiers]{a @symbol{gdk:modifier-type} value to match}
  @return{The @symbol{gdk:key-match} value describing whether @arg{event}
    matches.}
  @begin{short}
    Matches a key event against a keyboard shortcut that is specified as a
    keyval and modifiers.
  @end{short}
  Partial matches are possible where the combination matches if the currently
  active group is ignored.

  Note that we ignore the @kbd{Caps Lock} key for matching.
  @see-class{gdk:event}
  @see-symbol{gdk:key-match}"
  (event event)
  (keyval :uint)
  (modifiers modifier-type))

(export 'key-event-matches)

;;; ----------------------------------------------------------------------------
;;; gdk_key_event_get_match ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_key_event_get_match" %key-event-match) :boolean
  (event event)
  (keyval (:pointer :uint))
  (modifiers (:pointer modifier-type)))

(defun key-event-match (event)
 #+liber-documentation
 "@version{#2023-7-25}
  @argument[event]{a @class{gdk:event} instance}
  @begin{return}
    @arg{keyval} - an unsigned integer with the keyval @br{}
    @arg{modifiers} - a @symbol{gdk:modifier-type} value
  @end{return}
  @begin{short}
    Gets a keyval and modifier combination that will cause the
    @fun{gdk:key-event-matches} function to successfully match the given event.
  @end{short}
  @see-class{gdk:event}
  @see-symbol{gdk:modifier-type}"
  (cffi:with-foreign-objects ((keyval :uint)
                              (modifiers 'modifier-type))
    (when (%key-event-match event keyval modifiers)
      (values (cffi:mem-ref keyval :uint)
              (cffi:mem-ref modifiers 'modifier-type)))))

(export 'key-event-match)

;;; ----------------------------------------------------------------------------
;;; gdk_focus_event_get_in ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_focus_event_get_in" focus-event-in) :boolean
 #+liber-documentation
 "@version{#2023-7-25}
  @argument[event]{a @class{gdk:event} instance}
  @return{@em{True} if the focus is entering.}
  @begin{short}
    Extracts whether this event is about focus entering or leaving the surface.
  @end{short}
  @see-class{gdk:event}"
  (event event))

(export 'focus-event-in)

;;; ----------------------------------------------------------------------------
;;; gdk_touch_event_get_emulating_pointer ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_touch_event_get_emulating_pointer"
               touch-event-emulating-pointer) :boolean
 #+liber-documentation
 "@version{#2023-7-25}
  @argument[event]{a @class{gdk:event} instance}
  @return{@em{True} if @arg{event} is emulating.}
  @begin{short}
    Extracts whether a touch event is emulating a pointer event.
  @end{short}
  @see-class{gdk:event}"
  (event event))

(export 'touch-event-emulating-pointer)

;;; ----------------------------------------------------------------------------
;;; gdk_crossing_event_get_mode ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_crossing_event_get_mode" crossing-event-mode) crossing-mode
 #+liber-documentation
 "@version{#2023-7-25}
  @argument[event]{a @class{gdk:event} instance}
  @return{The @symbol{gdk:crossing-mode} value with the mode of @arg{event}.}
  @short{Extracts the crossing mode from a crossing event.}
  @see-class{gdk:event}
  @see-symbol{gdk:crossing-mode}"
  (event event))

(export 'crossing-event-mode)

;;; ----------------------------------------------------------------------------
;;; gdk_crossing_event_get_detail ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_crossing_event_get_detail" crossing-event-detail)
    notify-type
 #+liber-documentation
 "@version{#2023-7-25}
  @argument[event]{a @class{gdk:event} instance}
  @return{The @symbol{gdk:notify-type} value with the detail of @arg{event}.}
  @short{Extracts the notify detail from a crossing event.}
  @see-class{gdk:event}
  @see-symbol{gdk:notify-type}"
  (event event))

(export 'crossing-event-detail)

;;; ----------------------------------------------------------------------------
;;; gdk_crossing_event_get_focus ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_crossing_event_get_focus" crossing-event-focus) :boolean
 #+liber-documentation
 "@version{#2023-7-25}
  @argument[event]{a @class{gdk:event} instance}
  @return{@em{True} if the surface is the focus surface.}
  @short{Checks if the event surface is the focus surface.}
  @see-class{gdk:event}"
  (event event))

(export 'crossing-event-focus)

;;; ----------------------------------------------------------------------------
;;; gdk_grab_broken_event_get_grab_surface ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_grab_broken_event_get_grab_surface"
               grab-broken-event-grab-surface) (g:object surface)
 #+liber-documentation
 "@version{#2023-7-25}
  @argument[event]{a @class{gdk:event} instance}
  @return{The @class{gdk:surface} object with the grab surface of @arg{event}.}
  @short{Extracts the grab surface from a grab broken event.}
  @see-class{gdk:event}
  @see-class{gdk:surface}"
  (event event))

(export 'grab-broken-event-grab-surface)

;;; ----------------------------------------------------------------------------
;;; gdk_grab_broken_event_get_implicit ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_grab_boken_event_get_implicit" grab-broken-event-implicit)
    :boolean
 #+liber-documentation
 "@version{#2023-7-25}
  @argument[event]{a @class{gdk:event} instance}
  @return{@em{True} if the an implicit grab was broken.}
  @short{Checks whether the grab broken event is for an implicit grab.}
  @see-class{gdk:event}"
  (event event))

(export 'grab-broken-event-implicit)

;;; ----------------------------------------------------------------------------
;;; gdk_dnd_event_get_drop ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_dnd_event_get_drop" dnd-event-drop) (g:object drop)
 #+liber-documentation
 "@version{#2023-7-25}
  @argument[event]{a @class{gdk:event} instance}
  @return{The @class{gdk:drop} object.}
  @short{Gets the @class{gdk:drop} object from a DND event.}
  @see-class{gdk:event}
  @see-class{gdk:drop}"
  (event event))

(export 'dnd-event-drop)

;;; ----------------------------------------------------------------------------
;;; gdk_touchpad_event_get_gesture_phase ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_touchpad_event_get_gesture_phase"
               touchpad-event-gesture-phase) touchpad-gesture-phase
 #+liber-documentation
 "@version{#2023-7-25}
  @argument[event]{a @class{gdk:event} instance}
  @return{The @symbol{gdk:touchpad-gesture-phase} value with the gesture phase
    of @arg{event}.}
  @short{Extracts the touchpad gesture phase from a touchpad event.}
  @see-class{gdk:event}
  @see-symbol{gdk:touchpad-gesture-phase}"
  (event event))

(export 'touchpad-event-gesture-phase)

;;; ----------------------------------------------------------------------------
;;; gdk_touchpad_event_get_n_fingers ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_touchpad_event_get_n_fingers" touchpad-event-n-fingers)
    :uint
 #+liber-documentation
 "@version{#2023-7-25}
  @argument[event]{a @class{gdk:event} instance}
  @return{The unsigned integer with the number of fingers for @arg{event}.}
  @short{Extracts the number of fingers from a touchpad event.}
  @see-class{gdk:event}"
  (event event))

(export 'touchpad-event-n-fingers)

;;; ----------------------------------------------------------------------------
;;; gdk_touchpad_event_get_deltas ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_touchpad_event_get_deltas" %touchpad-event-deltas) :void
  (event event)
  (dx (:pointer :double))
  (dy (:pointer :double)))

(defun touchpad-event-deltas (event)
 #+liber-documentation
 "@version{#2023-7-25}
  @argument[event]{a @class{gdk:event} instance}
  @begin{return}
    @arg{dx} - a double float for dx @br{}
    @arg{dy} - a double float for dy
  @end{return}
  @short{Extracts delta information from a touchpad event.}
  @see-class{gdk:event}"
  (cffi:with-foreign-objects ((dx :double) (dy :double))
    (%touchpad-event-deltas event dx dy)
    (values (cffi:mem-ref dx :double)
            (cffi:mem-ref dy :double))))

(export 'touchpad-event-deltas)

;;; ----------------------------------------------------------------------------
;;; gdk_touchpad_event_get_pinch_angle_delta ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_touchpad_event_get_pinch_angle_delta"
               touchpad-event-pinch-angle-delta) :double
 #+liber-documentation
 "@version{#2023-7-25}
  @argument[event]{a @class{gdk:event} instance}
  @return{The double float with the angle delta of @arg{event}.}
  @short{Extracts the angle delta from a touchpad pinch event.}
  @see-class{gdk:event}"
  (event event))

(export 'touchpad-event-pinch-angle-delta)

;;; ----------------------------------------------------------------------------
;;; gdk_touchpad_event_get_pinch_scale ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_touchpad_event_get_pinch_scale" touchpad-event-pinch-scale)
    :double
 #+liber-documentation
 "@version{#2023-7-25}
  @argument[event]{a @class{gdk:event} instance}
  @return{The double float with the scale of @arg{event}.}
  @short{Extracts the scale from a touchpad pinch event.}
  @see-class{gdk:event}"
  (event event))

(export 'touchpad-event-pinch-scale)

;;; ----------------------------------------------------------------------------
;;; gdk_pad_event_get_axis_value ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_pad_event_get_axis_value" %pad-event-axis-value) :void
  (event event)
  (index (:pointer :uint))
  (value (:pointer :double)))

(defun pad-event-axis-value (event)
 #+liber-documentation
 "@version{#2023-5-25}
  @argument[event]{a @class{gdk:event} instance}
  @begin{return}
    @arg{index} - an unsigned integer with the axis index @br{}
    @arg{value} - a double float with the axis value
  @end{return}
  @short{Extracts the information from a pad strip or ring event.}
  @see-class{gdk:event}"
  (cffi:with-foreign-objects ((index :uint) (value :double))
    (%pad-event-axis-value event index value)
    (values (cffi:mem-ref index :uint)
            (cffi:mem-ref value :double))))

(export 'pad-event-axis-value)

;;; ----------------------------------------------------------------------------
;;; gdk_pad_event_get_button ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_pad_event_get_button" pad-event-button) :uint
 #+liber-documentation
 "@version{#2023-7-25}
  @argument[event]{a @class{gdk:event} instance}
  @return{The unsigned integer with the button of @arg{event}.}
  @short{Extracts information about the pressed button from a pad event.}
  @see-class{gdk:event}"
  (event event))

(export 'pad-event-button)

;;; ----------------------------------------------------------------------------
;;; gdk_pad_event_get_group_mode ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_pad_event_get_group_mode" %pad-event-group-mode) :void
  (event event)
  (group (:pointer :uint))
  (mode (:pointer :uint)))

(defun pad-event-group-mode (event)
 #+liber-documentation
 "@version{#2023-7-25}
  @argument[event]{a @class{gdk:event} instance}
  @begin{return}
    @arg{group} - an unsigned integer with the group @br{}
    @arg{mode} - an unsigned integer wiht the mode
  @end{return}
  @short{Extracts group and mode information from a pad event.}
  @see-class{gdk:event}"
  (cffi:with-foreign-objects ((group :uint) (mode :uint))
    (%pad-event-group-mode event group mode)
    (values (cffi:mem-ref group :uint)
            (cffi:mem-ref mode :uint))))

(export 'pad-event-group-mode)

;;; ----------------------------------------------------------------------------
;;; gdk_events_get_angle ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_events_get_angle" %events-angle) :boolean
  (event1 event)
  (event2 event)
  (angle (:pointer :double)))

(defun events-angle (event1 event2)
 #+liber-documentation
 "@version{#2023-5-25}
  @argument[event1]{a @class{gdk:event} instance}
  @argument[event2]{a @class{gdk:event} instance}
  @return{The double float with the relative angle between both events.}
  @begin{short}
    If both events contain X/Y information, this function will return the
    relative angle from @arg{event1} to @arg{event2}.
  @end{short}
  The rotation direction for positive angles is from the positive X axis
  towards the positive Y axis.
  @see-class{gdk:event}"
  (cffi:with-foreign-object (angle :double)
    (when (%events-angle event1 event2 angle)
      (cffi:mem-ref angle :double))))

(export 'events-angle)

;;; ----------------------------------------------------------------------------
;;; gdk_events_get_center ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_events_get_center" %events-center) :boolean
  (event1 event)
  (event2 event)
  (x (:pointer :double))
  (y (:pointer :double)))

(defun events-center (event1 event2)
 #+liber-documentation
 "@version{#2023-5-25}
  @argument[event1]{a @class{gdk:event} instance}
  @argument[event2]{a @class{gdk:event} instance}
  @begin{return}
    @arg{x} - a double float with the x coordinate of the center @br{}
    @arg{y} - a double float with the y coordinate of the center
  @end{return}
  @begin{short}
    If both events contain X/Y information, the center of both coordinates will
    be returned in @arg{x} and @arg{y}.
  @end{short}
  @see-class{gdk:event}"
  (cffi:with-foreign-objects ((x :double) (y :double))
    (when (%events-center event1 event2 x y)
      (values (cffi:mem-ref x :double)
              (cffi:mem-ref y :double)))))

(export 'events-center)

;;; ----------------------------------------------------------------------------
;;; gdk_events_get_distance ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_events_get_distance" %events-distance) :boolean
  (event1 event)
  (event2 event)
  (distance (:pointer :double)))

(defun events-distance (event1 event2)
 #+liber-documentation
 "@version{#2023-5-25}
  @argument[event1]{a @class{gdk:event} instance}
  @argument[event2]{a @class{gdk:event} instance}
  @return{The double float with the distance.}
  @begin{short}
    If both events have X/Y information, the distance between both coordinates,
    as in a straight line going from @arg{event1} to @arg{event2}, will be
    returned.
  @end{short}
  @see-class{gdk:event}"
  (cffi:with-foreign-object (distance :double)
    (when (%events-distance event1 event2 distance)
      (cffi:mem-ref distance :double))))

(export 'events-distance)

;;; --- End of file gdk4.event.lisp --------------------------------------------

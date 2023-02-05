;;; ----------------------------------------------------------------------------
;;; gdk4.event.lisp
;;;
;;; The documentation of this file is taken from the GDK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 - 2023 Dieter Kaiser
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License for Lisp
;;; as published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version and with a preamble to
;;; the GNU Lesser General Public License that clarifies the terms for use
;;; with Lisp programs and is referred as the LLGPL.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program and the preamble to the Gnu Lesser
;;; General Public License.  If not, see <http://www.gnu.org/licenses/>
;;; and <http://opensource.franz.com/preamble.html>.
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
;;;
;;; #define GDK_CURRENT_TIME     0L
;;;
;;; Represents the current time, and can be used anywhere a time is expected.
;;; ----------------------------------------------------------------------------

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
;;;
;;; #define GDK_EVENT_PROPAGATE     (FALSE)
;;;
;;; Use this macro as the return value for continuing the propagation of an
;;; event handler.
;;; ----------------------------------------------------------------------------

(defconstant +gdk-event-propagate+ nil
 #+liber-documentation
 "@version{#2021-12-13}
  @variable-value{@em{false}}
  @begin{short}
    Use this value as the return value for continuing the propagation of an
    event handler.
  @end{short}
  @see-variable{+gdk-event-stop+}")

#+liber-documentation
(setf (liber:alias-for-variable '+gdk-event-propagate+) "Constant")

(export '+gdk-event-propagate+)

;;; ----------------------------------------------------------------------------
;;; GDK_EVENT_STOP
;;; ----------------------------------------------------------------------------

(defconstant +gdk-event-stop+ t
 #+liber-documentation
 "@version{#2021-12-13}
  @variable-value{@em{true}}
  @begin{short}
    Use this value as the return value for stopping the propagation of an event
    handler.
  @end{short}
  @begin[Example]{dictionary}
    This event handler for the \"close-request\" signal of a window stops the
    propagation of the event and the window is not closed.
    @begin{pre}
(g:signal-connect window \"close-request\"
                  (lambda (widget event)
                    (declare (ignore widget event))
                    +gdk-event-stop+))
    @end{pre}
  @end{dictionary}
  @see-variable{+gdk-event-propagate+}")

#+liber-documentation
(setf (liber:alias-for-variable '+gdk-event-stop+) "Constant")

(export '+gdk-event-stop+)

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

(define-g-enum "GdkEventType" event-type
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
 "@version{#2021-12-13}
  @short{Specifies the type of a @class{gdk-event} instance.}
  @begin{pre}
(define-g-enum \"GdkEventType\" event-type
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
    @entry[:event-last]{Marks the end of the @sym{gdk-event-type} enumeration.}
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

(define-g-enum "GdkKeyMatch" key-match
  (:export t
   :type-initializer "gdk_key_match_get_type")
  (:none 0)
  (:partial 1)
  (:exact 2))

#+liber-documentation
(setf (liber:alias-for-symbol 'key-match)
      "GEnum"
      (liber:symbol-documentation 'key-match)
 "@version{#2022-11-26}
  @begin{short}
    Describes how well an event matches a given keyval and modifiers.
  @end{short}
  The @symbol{gdk:key-match} values are returned by the
  @fun{gdk:key-event-matches} function.
  @begin{pre}
(define-g-enum \"GdkKeyMatch\" key-match
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

(define-g-enum "GdkTouchpadGesturePhase" touchpad-gesture-phase
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
 "@version{#2021-12-13}
  @begin{short}
    The @sym{gdk:touchpad-gesture-phase} enumeration specifies the current
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
(define-g-enum \"GdkTouchpadGesturePhase\" touchpad-gesture-phase
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
  @see-class{gdk:touchpad-pad-event}")

;;; ----------------------------------------------------------------------------
;;; enum GdkScrollDirection
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkScrollDirection" scroll-direction
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
 "@version{#2021-12-13}
  @short{Specifies the direction for a @class{gdk:event-scroll} event.}
  @begin{pre}
(define-g-enum \"GdkScrollDirection\" scroll-direction
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
      @class{gdk-event-scroll} event. See the @fun{gdk:event-scroll-deltas}
      function.}
  @end{table}
  @see-class{gdk:event-scroll}
  @see-function{gdk:event-scroll-deltas}")

;;; ----------------------------------------------------------------------------
;;; enum GdkCrossingMode
;;;
;;;

;;; ----------------------------------------------------------------------------

(define-g-enum "GdkCrossingMode" crossing-mode
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

#+cl-cffi-gtk-documentation
(setf (liber:alias-for-symbol 'crossing-mode)
      "GEnum"
      (liber:symbol-documentation 'crossing-mode)
 "@version{#2021-12-13}
  @short{Specifies the crossing mode for enter and leave events.}
  @begin{pre}
(define-g-enum \"GdkCrosssingMode\" crossing-mode
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
;;; enum GdkNotifyType
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkNotifyType" notify-type
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
 "@version{#2021-12-13}
  @short{Specifies the kind of crossing for enter and leave events.}
  See the X11 protocol specification of @code{LeaveNotify} for full details of
  crossing event generation.
  @begin{pre}
(define-g-enum notify-type
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

(define-g-boxed-opaque event-sequence "GdkEventSequence"
  :alloc (error "GdkEventSequence cannot be created from the Lisp side."))

#+liber-documentation
(setf (liber:alias-for-class 'event-sequence)
      "GBoxed"
      (documentation 'event-sequence 'type)
 "@version{#2022-7-21}
  @begin{short}
    The @sym{event-sequence} structure is an opaque type representing a sequence
    of related touch events.
  @end{short}
  See the @fun{gdk:event-event-sequence} function.
  @see-function{gdk:event-event-sequence}")

(export 'event-sequence)

;;; ----------------------------------------------------------------------------
;;; GdkEvent
;;;
;;; This section describes functions dealing with events from the window system.
;;;
;;; In GTK applications the events are handled automatically by toplevel widgets
;;; and passed on to the event controllers of appropriate widgets, so these
;;; functions are rarely needed.
;;; ----------------------------------------------------------------------------

;; FIXME: The implementation is wrong. GdkEvent is not a object, but has its
;; own fundamental type.

(define-g-object-class "GdkEvent" event
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gdk_event_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; GdkButtonEvent
;;;
;;; typedef struct _GdkButtonEvent GdkButtonEvent;
;;;
;;; An event related to a button on a pointer device/
;;; ----------------------------------------------------------------------------

(define-g-object-class "GdkButtonEvent" button-event
  (:superclass event
   :export t
   :interfaces nil
   :type-initializer "gdk_button_event_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; GdkScrollEvent
;;;
;;; typedef struct _GdkScrollEvent GdkScrollEvent;
;;;
;;; An event related to a scrolling motion.
;;; ----------------------------------------------------------------------------

(define-g-object-class "GdkScrollEvent" scroll-event
  (:superclass event
   :export t
   :interfaces nil
   :type-initializer "gdk_scroll_event_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; GdkMotionEvent
;;;
;;; typedef struct _GdkMotionEvent GdkMotionEvent;
;;;
;;; An event related to a pointer or touch device motion.
;;; ----------------------------------------------------------------------------

(define-g-object-class "GdkMotionEvent" motion-event
  (:superclass event
   :export t
   :interfaces nil
   :type-initializer "gdk_motion_event_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; GdkKeyEvent
;;;
;;; typedef struct _GdkKeyEvent GdkKeyEvent;
;;;
;;; An event related to a key-based device.
;;; ----------------------------------------------------------------------------

(define-g-object-class "GdkKeyEvent" key-event
  (:superclass event
   :export t
   :interfaces nil
   :type-initializer "gdk_key_event_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; GdkFocusEvent
;;;
;;; typedef struct _GdkFocusEvent GdkFocusEvent;
;;;
;;; An event related to a focus change.
;;; ----------------------------------------------------------------------------

(define-g-object-class "GdkFocusEvent" focus-event
  (:superclass event
   :export t
   :interfaces nil
   :type-initializer "gdk_focus_event_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; GdkCrossingEvent
;;;
;;; typedef struct _GdkCrossingEvent GdkCrossingEvent;
;;;
;;; An event caused by a pointing device moving between surfaces.
;;; ----------------------------------------------------------------------------

(define-g-object-class "GdkCrossingEvent" crossing-event
  (:superclass event
   :export t
   :interfaces nil
   :type-initializer "gdk_crossing_event_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; GdkGrabBrokenEvent
;;;
;;; typedef struct _GdkGrabBrokenEvent GdkGrabBrokenEvent;
;;;
;;; An event related to a broken windowing system grab.
;;; ----------------------------------------------------------------------------

(define-g-object-class "GdkGrabBrokenEvent" grab-broken-event
  (:superclass event
   :export t
   :interfaces nil
   :type-initializer "gdk_grab_broken_event_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; GdkDeleteEvent
;;;
;;; typedef struct _GdkDeleteEvent GdkDeleteEvent;
;;;
;;; An event related to closing a top-level surface.
;;; ----------------------------------------------------------------------------

(define-g-object-class "GdkDeleteEvent" delete-event
  (:superclass event
   :export t
   :interfaces nil
   :type-initializer "gdk_delete_event_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; GdkDNDEvent
;;;
;;; typedef struct _GdkDNDEvent GdkDNDEvent;
;;;
;;; An event related to drag and drop operations.
;;; ----------------------------------------------------------------------------

(define-g-object-class "GdkDNDEvent" dnd-event
  (:superclass event
   :export t
   :interfaces nil
   :type-initializer "gdk_dnd_event_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; GdkTouchEvent
;;;
;;; typedef struct _GdkTouchEvent GdkTouchEvent;
;;;
;;; An event related to a touch-based device.
;;; ----------------------------------------------------------------------------

(define-g-object-class "GdkTouchEvent" touch-event
  (:superclass event
   :export t
   :interfaces nil
   :type-initializer "gdk_touch_event_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; GdkTouchpadEvent
;;;
;;; typedef struct _GdkTouchpadEvent GdkTouchpadEvent;
;;;
;;; An event related to a touchpad device.
;;; ----------------------------------------------------------------------------

(define-g-object-class "GdkTouchpadEvent" touchpad-event
  (:superclass event
   :export t
   :interfaces nil
   :type-initializer "gdk_touchpad_event_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; GdkPadEvent
;;;
;;; typedef struct _GdkPadEvent GdkPadEvent;
;;;
;;; An event related to a pad-based device.
;;; ----------------------------------------------------------------------------

(define-g-object-class "GdkPadEvent" pad-event
  (:superclass event
   :export t
   :interfaces nil
   :type-initializer "gdk_pad_event_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; GdkProximityEvent
;;;
;;; typedef struct _GdkProximityEvent GdkProximityEvent;
;;;
;;; An event related to the proximity of a tool to a device.
;;; ----------------------------------------------------------------------------

(define-g-object-class "GdkProximityEvent" proximity-event
  (:superclass event
   :export t
   :interfaces nil
   :type-initializer "gdk_proximity_event_get_type")
  nil)

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
;;;
;;; GdkEventType
;;; gdk_event_get_event_type (GdkEvent *event);
;;;
;;; Retrieves the type of the event.
;;;
;;; event :
;;;     a GdkEvent
;;;
;;; Returns :
;;;     a GdkEventType
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_event_get_event_type" event-event-type) event-type
  (event (g:object event)))

(export 'event-event-type)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_surface ()
;;;
;;; GdkSurface *
;;; gdk_event_get_surface (GdkEvent *event);
;;;
;;; Extracts the GdkSurface associated with an event.
;;;
;;; event :
;;;     a GdkEvent
;;;
;;; Returns :
;;;     The GdkSurface associated with the event.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_event_get_surface" event-surface) (g:object surface)
  (event (g:object event)))

(export 'event-surface)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_device ()
;;;
;;; GdkDevice *
;;; gdk_event_get_device (GdkEvent *event);
;;;
;;; Returns the device of an event.
;;;
;;; event :
;;;     a GdkEvent.
;;;
;;; Returns :
;;;     a GdkDevice.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_event_get_device" event-device) (g:object device)
  (event (g:object event)))

(export 'event-device)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_device_tool ()
;;;
;;; GdkDeviceTool *
;;; gdk_event_get_device_tool (GdkEvent *event);
;;;
;;; If the event was generated by a device that supports different tools (eg. a
;;; tablet), this function will return a GdkDeviceTool representing the tool
;;; that caused the event. Otherwise, NULL will be returned.
;;;
;;; Note: the GdkDeviceTools will be constant during the application lifetime,
;;; if settings must be stored persistently across runs, see
;;; gdk_device_tool_get_serial()
;;;
;;; event :
;;;     a GdkEvent
;;;
;;; Returns :
;;;     The current device tool, or NULL.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_event_get_device_tool" event-device-tool) (g:object device-tool)
  (event (g:object event)))

(export 'event-device-tool)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_time ()
;;;
;;; guint32
;;; gdk_event_get_time (GdkEvent *event);
;;;
;;; Returns the time stamp from event , if there is one; otherwise returns
;;; GDK_CURRENT_TIME.
;;;
;;; event :
;;;     a GdkEvent
;;;
;;; Returns :
;;;     time stamp field from event
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_event_get_time" event-time) :uint32
  (event (g:object event)))

(export 'event-time)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_display ()
;;;
;;; GdkDisplay *
;;; gdk_event_get_display (GdkEvent *event);
;;;
;;; Retrieves the GdkDisplay associated to the event .
;;;
;;; event :
;;;     a GdkEvent
;;;
;;; Returns :
;;;     a GdkDisplay.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_event_get_display" event-display) (g:object display)
  (event (g:object event)))

(export 'event-display)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_seat ()
;;;
;;; GdkSeat *
;;; gdk_event_get_seat (GdkEvent *event);
;;;
;;; Returns the seat that originated the event.
;;;
;;; event :
;;;     a GdkEvent
;;;
;;; Returns :
;;;     a GdkSeat.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_event_get_seat" event-seat) (g:object seat)
  (event (g:object seat)))

(export 'event-seat)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_event_sequence ()
;;;
;;; GdkEventSequence *
;;; gdk_event_get_event_sequence (GdkEvent *event);
;;;
;;; If event is a touch event, returns the GdkEventSequence to which the event
;;; belongs. Otherwise, return NULL.
;;;
;;; event :
;;;     a GdkEvent
;;;
;;; Returns :
;;;     the event sequence that the event belongs to.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_event_get_event_sequence" event-event-sequence)
    (g:boxed event-sequence)
  (event (g:object event)))

(export 'event-event-sequence)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_modifier_state ()
;;;
;;; GdkModifierType
;;; gdk_event_get_modifier_state (GdkEvent *event);
;;;
;;; Returns the modifier state field of an event.
;;;
;;; event :
;;;     a GdkEvent
;;;
;;; Returns :
;;;     the modifier state of event
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_event_get_modifier_state" event-modifier-state) modifier-type
  (event (g:object event)))

(export 'event-modifier-state)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_position ()
;;;
;;; gboolean
;;; gdk_event_get_position (GdkEvent *event,
;;;                         double *x,
;;;                         double *y);
;;;
;;; Extract the event surface relative x/y coordinates from an event.
;;;
;;; event :
;;;     a GdkEvent
;;;
;;; x :
;;;     location to put event surface x coordinate.
;;;
;;; y :
;;;     location to put event surface y coordinate.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_event_get_position" %event-position) :boolean
  (event (g:object event))
  (x (:pointer :double))
  (y (:pointer :double)))

(defun event-position (event)
  (with-foreign-objects ((x :double) (y :double))
    (%event-position event x y)
    (values (cffi:mem-ref x :double)
            (cffi:mem-ref y :double))))

(export 'event-position)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_axes ()
;;;
;;; gboolean
;;; gdk_event_get_axes (GdkEvent *event,
;;;                     double **axes,
;;;                     guint *n_axes);
;;;
;;; Extracts all axis values from an event.
;;;
;;; event :
;;;     a GdkEvent
;;;
;;; axes :
;;;     the array of values for all axes.
;;;
;;; n_axes :
;;;     the length of array.
;;;
;;; Returns :
;;;     TRUE on success, otherwise FALSE
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_axis ()
;;;
;;; gboolean
;;; gdk_event_get_axis (GdkEvent *event,
;;;                     GdkAxisUse axis_use,
;;;                     double *value);
;;;
;;; Extract the axis value for a particular axis use from an event structure.
;;;
;;; event :
;;;     a GdkEvent
;;;
;;; axis_use :
;;;     the axis use to look for
;;;
;;; value :
;;;     location to store the value found.
;;;
;;; Returns :
;;;     TRUE if the specified axis was found, otherwise FALSE
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_event_get_axis" %event-axis) :boolean
  (event (g:object event))
  (axis-use axis-use)
  (value (:pointer :double)))

(defun event-axis (event axis-use)
  (with-foreign-object (value :double)
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
;;;
;;; gboolean
;;; gdk_event_get_pointer_emulated (GdkEvent *event);
;;;
;;; Returns whether this event is an 'emulated' pointer event (typically from a
;;; touch event), as opposed to a real one.
;;;
;;; event :
;;;     a GdkEvent
;;;
;;; Returns :
;;;     TRUE if this event is emulated
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_event_get_pointer_emulated" event-pointer-emulated) :boolean
  (event (g:object event)))

(export 'event-pointer-emulated)

;;; ----------------------------------------------------------------------------
;;; gdk_event_triggers_context_menu ()
;;;
;;; gboolean
;;; gdk_event_triggers_context_menu (GdkEvent *event);
;;;
;;; This function returns whether a GdkEvent should trigger a context menu,
;;; according to platform conventions. The right mouse button always triggers
;;; context menus.
;;;
;;; This function should always be used instead of simply checking for
;;; event->button == GDK_BUTTON_SECONDARY.
;;;
;;; event :
;;;     a GdkEvent, currently only button events are meaningful values
;;;
;;; Returns :
;;;     TRUE if the event should trigger a context menu.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_event_triggers_context_menu" event-triggers-context-menu)
    :boolean
  (event (g:object event)))

(export 'event-triggers-context-menu)

;;; ----------------------------------------------------------------------------
;;; gdk_button_event_get_button ()
;;;
;;; guint
;;; gdk_button_event_get_button (GdkEvent *event);
;;;
;;; Extract the button number from a button event.
;;;
;;; event :
;;;     a button event.
;;;
;;; Returns :
;;;     the button of event
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_button_event_get_button" button-event-button) :uint
  (event (g:object event)))

(export 'button-event-button)

;;; ----------------------------------------------------------------------------
;;; gdk_scroll_event_get_direction ()
;;;
;;; GdkScrollDirection
;;; gdk_scroll_event_get_direction (GdkEvent *event);
;;;
;;; Extracts the direction of a scroll event.
;;;
;;; event :
;;;     a scroll event.
;;;
;;; Returns :
;;;     the scroll direction of event
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_scroll_event_get_direction" scroll-event-direction)
    scroll-direction
  (event (g:object event)))

(export 'scroll-event-direction)

;;; ----------------------------------------------------------------------------
;;; gdk_scroll_event_get_deltas ()
;;;
;;; void
;;; gdk_scroll_event_get_deltas (GdkEvent *event,
;;;                              double *delta_x,
;;;                              double *delta_y);
;;;
;;; Extracts the scroll deltas of a scroll event.
;;;
;;; The deltas will be zero unless the scroll direction is GDK_SCROLL_SMOOTH.
;;;
;;; event :
;;;     a scroll event.
;;;
;;; delta_x :
;;;     return location for x scroll delta.
;;;
;;; delta_y :
;;;     return location for y scroll delta.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_scroll_event_get_deltas" %scroll-event-deltas) :void
  (event (g:object event))
  (xdelta :double)
  (ydelta :double))

(defun scroll-event-deltas (event)
  (with-foreign-objects ((xdelta :double) (ydelta :double))
    (%scroll-event-deltas event xdelta ydelta)
    (values (cffi:mem-ref xdelta :double)
            (cffi:mem-ref ydelta :double))))

(export 'scroll-event-deltas)

;;; ----------------------------------------------------------------------------
;;; gdk_scroll_event_is_stop ()
;;;
;;; gboolean
;;; gdk_scroll_event_is_stop (GdkEvent *event);
;;;
;;; Check whether a scroll event is a stop scroll event. Scroll sequences with
;;; smooth scroll information may provide a stop scroll event once the
;;; interaction with the device finishes, e.g. by lifting a finger. This stop
;;; scroll event is the signal that a widget may trigger kinetic scrolling based
;;; on the current velocity.
;;;
;;; Stop scroll events always have a delta of 0/0.
;;;
;;; event :
;;;     a scroll event.
;;;
;;; Returns
;;;     TRUE if the event is a scroll stop event
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_scroll_event_is_stop" scroll-event-is-stop) :boolean
  (event (g:object event)))

(export 'scroll-event-is-stop)

;;; ----------------------------------------------------------------------------
;;; gdk_key_event_get_keyval ()
;;;
;;; guint
;;; gdk_key_event_get_keyval (GdkEvent *event);
;;;
;;; Extracts the keyval from a key event.
;;;
;;; event :
;;;     a key event.
;;;
;;; Returns :
;;;     the keyval of event
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_key_event_get_keyval" key-event-keyval) :uint
  (event (g:object event)))

(export 'key-event-keyval)

;;; ----------------------------------------------------------------------------
;;; gdk_key_event_get_keycode ()
;;;
;;; guint
;;; gdk_key_event_get_keycode (GdkEvent *event);
;;;
;;; Extracts the keycode from a key event.
;;;
;;; event :
;;;     a key event.
;;;
;;; Returns :
;;;     the keycode of event
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_key_event_get_keycode" key-event-keycode) :uint
  (event (g:object event)))

(export 'key-event-keycode)

;;; ----------------------------------------------------------------------------
;;; gdk_key_event_get_consumed_modifiers ()
;;;
;;; GdkModifierType
;;; gdk_key_event_get_consumed_modifiers (GdkEvent *event);
;;;
;;; Extracts the consumed modifiers from a key event.
;;;
;;; event :
;;;     a key event.
;;;
;;; Returns :
;;;     the consumed modifiers or event
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_key_event_get_consumed_modifiers"
           key-event-consumed-modifiers) modifier-type
  (event (g:object event)))

(export 'key-event-consumed-modifiers)

;;; ----------------------------------------------------------------------------
;;; gdk_key_event_get_layout ()
;;;
;;; guint
;;; gdk_key_event_get_layout (GdkEvent *event);
;;;
;;; Extracts the layout from a key event.
;;;
;;; event :
;;;     a key event.
;;;
;;; Returns :
;;;     the layout of event
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_key_event_get_layout" key-event-layout) :uint
  (event (g:object event)))

(export 'key-event-layout)

;;; ----------------------------------------------------------------------------
;;; gdk_key_event_get_level ()
;;;
;;; guint
;;; gdk_key_event_get_level (GdkEvent *event);
;;;
;;; Extracts the shift level from a key event.
;;;
;;; event :
;;;     a key event.
;;;
;;; Returns :
;;;     the shift level of event
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_key_event_get_level" key-event-level) :uint
  (event (g:object event)))

(export 'key-event-level)

;;; ----------------------------------------------------------------------------
;;; gdk_key_event_is_modifier ()
;;;
;;; gboolean
;;; gdk_key_event_is_modifier (GdkEvent *event);
;;;
;;; Extracts whether the key event is for a modifier key.
;;;
;;; event :
;;;     a key event.
;;;
;;; Returns :
;;;     TRUE if the event is for a modifier key
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_key_event_is_modifier" key-event-is-modifier) :boolean
  (event (g:object event)))

(export 'key-event-is-modifier)

;;; ----------------------------------------------------------------------------
;;; gdk_key_event_matches ()
;;;
;;; GdkKeyMatch
;;; gdk_key_event_matches (GdkEvent *event,
;;;                        guint keyval,
;;;                        GdkModifierType modifiers);
;;;
;;; Matches a key event against a keyboard shortcut that is specified as a
;;; keyval and modifiers. Partial matches are possible where the combination
;;; matches if the currently active group is ignored.
;;;
;;; Note that we ignore Caps Lock for matching.
;;;
;;; event :
;;;     a key GdkEvent.
;;;
;;; keyval :
;;;     the keyval to match
;;;
;;; modifiers :
;;;     the modifiers to match
;;;
;;; Returns :
;;;     a GdkKeyMatch value describing whether event matches
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_key_event_matches" key-event-matches) key-match
  (event (g:object event))
  (keyval :uint)
  (modifiers modifier-type))

(export 'key-event-matches)

;;; ----------------------------------------------------------------------------
;;; gdk_key_event_get_match ()
;;;
;;; gboolean
;;; gdk_key_event_get_match (GdkEvent *event,
;;;                          guint *keyval,
;;;                          GdkModifierType *modifiers);
;;;
;;; Gets a keyval and modifier combination that will cause
;;; gdk_key_event_matches() to successfully match the given event.
;;;
;;; event :
;;;     a key GdkEvent.
;;;
;;; keyval :
;;;     return location for a keyval.
;;;
;;; modifiers :
;;;     return location for modifiers.
;;;
;;; Returns :
;;;     TRUE on success
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_key_event_get_match" %key-event-match) :boolean
  (event (g:object event))
  (keyval (:pointer :uint))
  (modifiers (:pointer modifier-type)))

(defun key-event-match (event)
  (with-foreign-objects ((keyval :uint)
                         (modifiers 'modifier-type))
    (when (%key-event-match event keyval modifiers)
      (values (cffi:mem-ref keyval :uint)
              (cffi:mem-ref modifiers 'modifier-type)))))

(export 'key-event-match)

;;; ----------------------------------------------------------------------------
;;; gdk_focus_event_get_in ()
;;;
;;; gboolean
;;; gdk_focus_event_get_in (GdkEvent *event);
;;;
;;; Extracts whether this event is about focus entering or leaving the surface.
;;;
;;; event :
;;;     a focus change event.
;;;
;;; Returns :
;;;     TRUE of the focus is entering
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_focus_event_get_in" focus-event-in) :boolean
  (event (g:object event)))

(export 'focus-event-in)

;;; ----------------------------------------------------------------------------
;;; gdk_touch_event_get_emulating_pointer ()
;;;
;;; gboolean
;;; gdk_touch_event_get_emulating_pointer (GdkEvent *event);
;;;
;;; Extracts whether a touch event is emulating a pointer event.
;;;
;;; event :
;;;     a touch event.
;;;
;;; Returns :
;;;     TRUE if event is emulating
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_touch_event_get_emulating_pointer" touch-event-emulating-pointer)
    :boolean
  (event (g:object event)))

(export 'touch-event-emulating-pointer)

;;; ----------------------------------------------------------------------------
;;; gdk_crossing_event_get_mode ()
;;;
;;; GdkCrossingMode
;;; gdk_crossing_event_get_mode (GdkEvent *event);
;;;
;;; Extracts the crossing mode from a crossing event.
;;;
;;; event :
;;;     a crossing event.
;;;
;;; Returns :
;;;     the mode of event
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_crossing_event_get_mode" crossing-event-mode) crossing-mode
  (event (g:object event)))

(export 'crossing-event-mode)

;;; ----------------------------------------------------------------------------
;;; gdk_crossing_event_get_detail ()
;;;
;;; GdkNotifyType
;;; gdk_crossing_event_get_detail (GdkEvent *event);
;;;
;;; Extracts the notify detail from a crossing event.
;;;
;;; event :
;;;     a crossing event.
;;;
;;; Returns :
;;;     the notify detail of event
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_crossing_event_get_detail" crossing-event-detail) notify-type
  (event (g:object event)))

(export 'crossing-event-detail)

;;; ----------------------------------------------------------------------------
;;; gdk_crossing_event_get_focus ()
;;;
;;; gboolean
;;; gdk_crossing_event_get_focus (GdkEvent *event);
;;;
;;; Checks if the event surface is the focus surface.
;;;
;;; event :
;;;     a crossing event.
;;;
;;; Returns ;
;;;     TRUE if the surface is the focus surface
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_crossing_event_get_focus" crossing-event-focus) :boolean
  (event (g:object event)))

(export 'crossing-event-focus)

;;; ----------------------------------------------------------------------------
;;; gdk_grab_broken_event_get_grab_surface ()
;;;
;;; GdkSurface *
;;; gdk_grab_broken_event_get_grab_surface (GdkEvent *event);
;;;
;;; Extracts the grab surface from a grab broken event.
;;;
;;; event :
;;;     a grab broken event.
;;;
;;; Returns :
;;;     the grab surface of event .
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_grab_broken_event_get_grab_surface"
           grab-broken-event-grab-surface) (g:object surface)
  (event (g:object event)))

(export 'grab-broken-event-grab-surface)

;;; ----------------------------------------------------------------------------
;;; gdk_grab_broken_event_get_implicit ()
;;;
;;; gboolean
;;; gdk_grab_broken_event_get_implicit (GdkEvent *event);
;;;
;;; Checks whether the grab broken event is for an implicit grab.
;;;
;;; event :
;;;     a grab broken event.
;;;
;;; Returns :
;;;     TRUE if the an implicit grab was broken
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_grab_boken_event_get_implicit" grab-broken-event-implicit)
    :boolean
  (event (g:object event)))

(export 'grab-broken-event-implicit)

;;; ----------------------------------------------------------------------------
;;; gdk_dnd_event_get_drop ()
;;;
;;; GdkDrop *
;;; gdk_dnd_event_get_drop (GdkEvent *event);
;;;
;;; Gets the GdkDrop from a DND event.
;;;
;;; event :
;;;     a DND event.
;;;
;;; Returns :
;;;     the drop.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_dnd_event_get_drop" dnd-event-drop) (g:object drop)
  (event (g:object event)))

(export 'dnd-event-drop)

;;; ----------------------------------------------------------------------------
;;; gdk_touchpad_event_get_gesture_phase ()
;;;
;;; GdkTouchpadGesturePhase
;;; gdk_touchpad_event_get_gesture_phase (GdkEvent *event);
;;;
;;; Extracts the touchpad gesture phase from a touchpad event.
;;;
;;; event :
;;;     a touchpad GdkEvent.
;;;
;;; Returns :
;;;     the gesture phase of event
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_touchpad_event_get_gesture_phase"
           touchpad-event-gesture-phase) touchpad-gesture-phase
  (event (g:object event)))

(export 'touchpad-event-gesture-phase)

;;; ----------------------------------------------------------------------------
;;; gdk_touchpad_event_get_n_fingers ()
;;;
;;; guint
;;; gdk_touchpad_event_get_n_fingers (GdkEvent *event);
;;;
;;; Extracts the number of fingers from a touchpad event.
;;;
;;; event :
;;;     a touchpad event.
;;;
;;; Returns
;;;     the number of fingers for event
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_touchpad_event_get_n_fingers" touchpad-event-n-fingers) :uint
  (event (g:object event)))

(export 'touchpad-event-n-fingers)

;;; ----------------------------------------------------------------------------
;;; gdk_touchpad_event_get_deltas ()
;;;
;;; void
;;; gdk_touchpad_event_get_deltas (GdkEvent *event, double *dx, double *dy);
;;;
;;; Extracts delta information from a touchpad event.
;;;
;;; event :
;;;     a touchpad event.
;;;
;;; dx :
;;;     return location for x.
;;;
;;; dy :
;;;     return location for y.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_touchpad_event_get_deltas" %touchpad-event-deltas) :void
  (event (g:object event))
  (dx (:pointer :double))
  (dy (:pointer :double)))

(defun touchpad-event-deltas (event)
  (with-foreign-objects ((dx :double) (dy :double))
    (%touchpad-event-deltas event dx dy)
    (values (cffi:mem-ref dx :double)
            (cffi:mem-ref dy :double))))

(export 'touchpad-event-deltas)

;;; ----------------------------------------------------------------------------
;;; gdk_touchpad_event_get_pinch_angle_delta ()
;;;
;;; double
;;; gdk_touchpad_event_get_pinch_angle_delta (GdkEvent *event);
;;;
;;; Extracts the angle delta from a touchpad pinch event.
;;;
;;; event :
;;;     a touchpad pinch event.
;;;
;;; Returns :
;;;     the angle delta of event
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_touchpad_event_get_pinch_angle_delta"
           touchpad-event-pinch-angle-delta) :double
  (event (g:object event)))

(export 'touchpad-event-pinch-angle-delta)

;;; ----------------------------------------------------------------------------
;;; gdk_touchpad_event_get_pinch_scale ()
;;;
;;; double
;;; gdk_touchpad_event_get_pinch_scale (GdkEvent *event);
;;;
;;; Extracts the scale from a touchpad pinch event.
;;;
;;; event :
;;;     a touchpad pinch event.
;;;
;;; Returns :
;;;     the scale of event
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_touchpad_event_get_pinch_scale" touchpad-event-pinch-scale)
    :double
  (event (g:object event)))

(export 'touchpad-event-pinch-scale)

;;; ----------------------------------------------------------------------------
;;; gdk_pad_event_get_axis_value ()
;;;
;;; void
;;; gdk_pad_event_get_axis_value (GdkEvent *event,
;;;                               guint *index,
;;;                               double *value);
;;;
;;; Extracts the information from a pad strip or ring event.
;;;
;;; event :
;;;     a pad strip or ring event.
;;;
;;; index :
;;;     Return location for the axis index.
;;;
;;; value :
;;;     Return location for the axis value.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pad_event_get_axis_value" %pad-event-axis-value) :void
  (event (g:object event))
  (index (:pointer :uint))
  (value (:pointer :double)))

(defun pad-event-axis-value (event)
  (with-foreign-objects ((index :uint) (value :double))
    (%pad-event-axis-value event index value)
    (values (cffi:mem-ref index :uint)
            (cffi:mem-ref value :double))))

(export 'pad-event-axis-value)

;;; ----------------------------------------------------------------------------
;;; gdk_pad_event_get_button ()
;;;
;;; guint
;;; gdk_pad_event_get_button (GdkEvent *event);
;;;
;;; Extracts information about the pressed button from a pad event.
;;;
;;; event :
;;;     a pad button event.
;;;
;;; Returns :
;;;     the button of event
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pad_event_get_button" pad-event-button) :uint
  (event (g:object event)))

(export 'pad-event-button)

;;; ----------------------------------------------------------------------------
;;; gdk_pad_event_get_group_mode ()
;;;
;;; void
;;; gdk_pad_event_get_group_mode (GdkEvent *event,
;;;                               guint *group,
;;;                               guint *mode);
;;;
;;; Extracts group and mode information from a pad event.
;;;
;;; event :
;;;     a pad event.
;;;
;;; group :
;;;     return location for the group.
;;;
;;; mode :
;;;     return location for the mode.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pad_event_get_group_mode" %pad-event-group-mode) :void
  (event (g:object event))
  (group (:pointer :uint))
  (mode (:pointer :uint)))

(defun pad-event-group-mode (event)
  (with-foreign-objects ((group :uint) (mode :uint))
    (%pad-event-group-mode event group mode)
    (values (cffi:mem-ref group :uint)
            (cffi:mem-ref mode :uint))))

(export 'pad-event-group-mode)

;;; ----------------------------------------------------------------------------
;;; gdk_events_get_angle ()
;;;
;;; gboolean
;;; gdk_events_get_angle (GdkEvent *event1,
;;;                       GdkEvent *event2,
;;;                       double *angle);
;;;
;;; If both events contain X/Y information, this function will return TRUE and
;;; return in angle the relative angle from event1 to event2 . The rotation
;;; direction for positive angles is from the positive X axis towards the
;;; positive Y axis.
;;;
;;; event1 :
;;;     first GdkEvent
;;;
;;; event2 :
;;;     second GdkEvent
;;;
;;; angle :
;;;     return location for the relative angle between both events.
;;;
;;; Returns :
;;;     TRUE if the angle could be calculated.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_events_get_angle" %events-angle) :boolean
  (event1 (g:object event))
  (event2 (g:object event))
  (angle (:pointer :double)))

(defun events-angle (event1 event2)
  (with-foreign-object (angle :double)
    (when (%events-angle event1 event2 angle)
      (cffi:mem-ref angle :double))))

(export 'events-angle)

;;; ----------------------------------------------------------------------------
;;; gdk_events_get_center ()
;;;
;;; gboolean
;;; gdk_events_get_center (GdkEvent *event1,
;;;                        GdkEvent *event2,
;;;                        double *x,
;;;                        double *y);
;;;
;;; If both events contain X/Y information, the center of both coordinates will
;;; be returned in x and y .
;;;
;;; event1 :
;;;     first GdkEvent
;;;
;;; event2 :
;;;     second GdkEvent
;;;
;;; x :
;;;     return location for the X coordinate of the center.
;;;
;;; y :
;;;     return location for the Y coordinate of the center.
;;;
;;; Returns :
;;;     TRUE if the center could be calculated.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_events_get_center" %events-center) :boolean
  (event1 (g:object event))
  (event2 (g:object event))
  (x (:pointer :double))
  (y (:pointer :double)))

(defun events-center (event1 event2)
  (with-foreign-objects ((x :double) (y :double))
    (when (%events-center event1 event2 x y)
      (values (cffi:mem-ref x :double)
              (cffi:mem-ref y :double)))))

(export 'events-center)

;;; ----------------------------------------------------------------------------
;;; gdk_events_get_distance ()
;;;
;;; gboolean
;;; gdk_events_get_distance (GdkEvent *event1,
;;;                          GdkEvent *event2,
;;;                          double *distance);
;;;
;;; If both events have X/Y information, the distance between both coordinates
;;; (as in a straight line going from event1 to event2 ) will be returned.
;;;
;;; event1 :
;;;     first GdkEvent
;;;
;;; event2 :
;;;     second GdkEvent
;;;
;;; distance :
;;;     return location for the distance.
;;;
;;; Returns :
;;;     TRUE if the distance could be calculated.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_events_get_distance" %events-distance) :boolean
  (event1 (g:object event))
  (event2 (g:object event))
  (distance (:pointer :double)))

(defun events-distance (event1 event2)
  (with-foreign-object (distance :double)
    (when (%events-distance event1 event2 distance)
      (cffi:mem-ref distance :double))))

(export 'events-distance)

;;; --- End of file gdk4.event.lisp --------------------------------------------

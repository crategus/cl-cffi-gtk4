;;; ----------------------------------------------------------------------------
;;; gtk4.gesture.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.12 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2019 - 2024 Dieter Kaiser
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
;;; GtkGesture
;;;
;;;     Base class for gestures
;;;
;;; Types and Values
;;;
;;;     GtkGesture
;;;     GtkEventSequenceState
;;;
;;; Functions
;;;
;;;     gtk_gesture_get_device
;;;     gtk_gesture_is_active
;;;     gtk_gesture_is_recognized
;;;     gtk_gesture_get_sequence_state
;;;     gtk_gesture_set_sequence_state                     Deprecated 4.10
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
;;;
;;; Properties
;;;
;;;     n-points
;;;
;;; Signals
;;;
;;;     begin
;;;     cancel
;;;     end
;;;     sequence-state-changed
;;;     update
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkEventController
;;;         ╰── GtkGesture
;;;             ├── GtkGestureSingle
;;;             ├── GtkGestureRotate
;;;             ╰── GtkGestureZoom
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkEventSequenceState
;;; ----------------------------------------------------------------------------

(gobject:define-g-enum "GtkEventSequenceState" event-sequence-state
  (:export t
   :type-initializer "gtk_event_sequence_state_get_type")
  (:none 0)
  (:claimed 1)
  (:denied 2))

#+liber-documentation
(setf (liber:alias-for-symbol 'event-sequence-state)
      "GEnum"
      (liber:symbol-documentation 'event-sequence-state)
 "@version{#2022-8-22}
  @begin{declaration}
    @begin{pre}
(gobject:define-g-enum \"GtkEventSequenceState\" event-sequence-state
  (:export t
   :type-initializer \"gtk_event_sequence_state_get_type\")
  (:none 0)
  (:claimed 1)
  (:denied 2))
    @end{pre}
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:none]{The sequence is handled, but not grabbed.}
      @entry[:claimed]{The sequence is handled and grabbed.}
      @entry[:denied]{The sequence is denied.}
    @end{table}
  @end{values}
  @begin{short}
    Describes the state of a @class{gdk:event-sequence} instance in a
    @class{gtk:gesture} object.
  @end{short}
  @see-class{gtk:gesture}
  @see-class{gdk:event-sequence}")

;;; ----------------------------------------------------------------------------
;;; GtkGesture
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkGesture" gesture
  (:superclass event-controller
   :export t
   :interfaces nil
   :type-initializer "gtk_gesture_get_type")
  ((n-points
    gesture-n-points
    "n-points" "guint" t nil)))

#+liber-documentation
(setf (documentation 'gesture 'type)
 "@version{#2022-8-22}
  @begin{short}
    The @class{gtk:gesture} object is the base object for gesture recognition.
  @end{short}
  Although this object is quite generalized to serve as a base for multi-touch
  gestures, it is suitable to implement single-touch and pointer-based gestures.

  The number of touches that a @class{gtk:gesture} object need to be recognized
  is controlled by the @code{n-points} property, if a gesture is keeping track
  of less or more than that number of sequences, it will not check whether the
  gesture is recognized.

  As soon as the gesture has the expected number of touches, the gesture will
  run the \"check\" signal regularly on input events until the gesture is
  recognized, the criteria to consider a gesture as recognized is left to
  @class{gtk:gesture} subclasses.

  A recognized gesture will then emit the following signals:
  @begin{itemize}
    @item{The \"begin\" signal when the gesture is recognized.}
    @item{A number of \"update\" signals, whenever an input event is processed.}
    @item{The \"end\" signal when the gesture is no longer recognized.}
  @end{itemize}
  @subheading{Event propagation}
  In order to receive events, a gesture needs to set a propagation phase through
  the @fun{gtk:event-controller-propagation-phase} function.

  In the capture phase, events are propagated from the toplevel down to the
  target widget, and gestures that are attached to containers above the widget
  get a chance to interact with the event before it reaches the target.

  In the bubble phase, events are propagated up from the target widget to the
  toplevel, and gestures that are attached to containers above the widget get a
  chance to interact with events that have not been handled yet.

  @subheading{States of a sequence}
  Whenever input interaction happens, a single event may trigger a cascade of
  gestures, both across the parents of the widget receiving the event and in
  parallel within an individual widget. It is a responsibility of the widgets
  using those gestures to set the state of touch sequences accordingly in order
  to enable cooperation of gestures around the event sequences triggering those.

  Within a widget, gestures can be grouped through the @fun{gtk:gesture-group}
  function, grouped gestures synchronize the state of sequences, so calling the
  @fun{gtk:gesture-set-state} function on one will effectively propagate the
  state throughout the group.

  By default, all sequences start out in the @code{:none} state, sequences in
  this state trigger the gesture event handler, but event propagation will
  continue unstopped by gestures.

  If a sequence enters into the @code{:denied} state, the gesture group will
  effectively ignore the sequence, letting events go unstopped through the
  gesture, but the \"slot\" will still remain occupied while the touch is
  active.

  If a sequence enters in the @code{:claimed} state, the gesture group will
  grab all interaction on the sequence, by:
  @begin{itemize}
    @item{Setting the same sequence to @code{:denied} on every other gesture
      group within the widget, and every gesture on parent widgets in the
      propagation chain.}
    @item{calling \"cancel\" on every gesture in widgets underneath in the
      propagation chain.}
    @item{Stopping event propagation after the gesture group handles the event.}
  @end{itemize}
  Note: if a sequence is set early to @code{:claimed} on
  @code{:touch-begin}/@code{:button-press} (so those events are captured before
  reaching the event widget, this implies @code{:phase-capture}), one similar
  event will emulated if the sequence changes to @code{:denied}. This way event
  coherence is preserved before event propagation is unstopped again.

  Sequence states can not be changed freely, see the
  @fun{gtk:gesture-set-state} function to know about the possible lifetimes of
  a @class{gdk:event-sequence} instance.

  @subheading{Touchpad gestures}
  On the platforms that support it, the @class{gtk:gesture} object will handle
  transparently touchpad gesture events. The only precautions users of the
  @class{gtk:gesture} object should do to enable this support are:
  @begin{itemize}
    @item{If the gesture has @code{:phase-none}, ensuring events of type
      @code{:touchpad-swipe} and @code{:touchpad-pinch} are handled by the
      @class{gtk:gesture} object}
  @end{itemize}
  @begin[Signal Details]{dictionary}
    @subheading{The \"begin\" signal}
      @begin{pre}
lambda (gesture sequence)    :run-last
      @end{pre}
      The signal is emitted when the gesture is recognized. This means the
      number of touch sequences matches @code{n-points}, and the \"check\"
      handler(s) returned @em{true}. Note: These conditions may also happen when
      an extra touch, e.g. a third touch on a 2-touches gesture, is lifted, in
      that situation sequence will not pertain to the current set of active
      touches, so do not rely on this being true.
      @begin[code]{table}
        @entry[gesture]{The @class{gtk:gesture} object which received the
          signal.}
        @entry[sequence]{The @class{gdk;event-sequence} event that made the
          gesture to be recognized.}
      @end{table}
    @subheading{The \"cancel\" signal}
      @begin{pre}
lambda (gesture sequence)    :run-last
      @end{pre}
      The signal is emitted whenever a sequence is cancelled. This usually
      happens on active touches when the @fun{gtk:event-controller-reset}
      function is called on gesture, manually, due to grabs ..., or the
      individual sequence was claimed by controllers of the parent widgets, see
      the @fun{gtk:gesture-set-sequence-state}) function. The @arg{gesture}
      argument must forget everything about @arg{sequence} as a reaction to the
      signal.
      @begin[code]{table}
        @entry[gesture]{The @class{gtk:gesture} object which received the
          signal.}
        @entry[sequence]{The @class{gdk:event-sequence} event that was
          cancelled.}
      @end{table}
    @subheading{The \"end\" signal}
      @begin{pre}
lambda (gesture sequence)    :run-last
      @end{pre}
      The signal is emitted when gesture either stopped recognizing the event
      sequences as something to be handled, the \"check\" handler returned
      @em{false}, or the number of touch sequences became higher or lower than
      @code{n-points}. Note: The @arg{sequence} argument might not pertain to
      the group of sequences that were previously triggering recognition on
      gesture, i.e. a just pressed touch sequence that exceeds @code{n-points}.
      This situation may be detected by checking through the
      @fun{gtk:gesture-handles-sequence} function.
      @begin[code]{table}
        @entry[gesture]{The @class{gtk:gesture} object which received the
          signal.}
        @entry[sequence]{The @class{gdk:event-sequence} that made gesture
          recognition to finish.}
      @end{table}
    @subheading{The \"sequence-state-changed\" signal}
      @begin{pre}
lambda (gesture sequence state)    :run-last
      @end{pre}
      The signal is emitted whenever a sequence state changes. See the
      @fun{gtk:gesture-set-sequence-state} function to know more about the
      expectable sequence lifetimes.
      @begin[code]{table}
        @entry[gesture]{The @class{gtk:gesture} object which received the
          signal.}
        @entry[sequence]{The @class{gdk:event-sequence} event that was
          cancelled.}
        @entry[state]{The new @symbol{gtk:event-sequence-state} value.}
      @end{table}
    @subheading{The \"update\" signal}
      @begin{pre}
lambda (gesture sequence)    :run-last
      @end{pre}
      The signal is emitted whenever an event is handled while the gesture is
      recognized. The @arg{sequence} argument is guaranteed to pertain to the
      set of active touches.
      @begin[code]{table}
        @entry[gesture]{The @class{gtk:gesture} object which received the
          signal.}
        @entry[sequence]{The @class{gdk:event-sequence} event that was updated.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk:gesture-n-points}
  @see-class{gtk:event-controller}
  @see-class{gtk:gesture-single}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:gesture-n-points ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "n-points" 'gesture) t)
 "The @code{n-points} property of type @code{:int}
  (Read / Write / Construct only) @br{}
  The number of touch points that trigger recognition on this gesture. @br{}
  Allowed values: >= 1 @br{}
  Default value: 1 @br{}")

#+liber-documentation
(setf (liber:alias-for-function 'gesture-n-points)
      "Accessor"
      (documentation 'gesture-n-points 'function)
 "@version{#2022-8-22}
  @syntax{(gtk:gesture-n-points object) => n-points)}
  @argument[object]{a @class{gtk:gesture} object}
  @argument[n-points]{an integer with the number of touch points}
  @begin{short}
    Accessor of the @slot[gtk:gesture]{n-points} slot of the
    @class{gtk:gesture} class.
  @end{short}
  The number of touch points that trigger recognition on this gesture.
  @see-class{gtk:gesture}")

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_get_device
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_gesture_get_device" gesture-device) (g:object gdk:device)
 #+liber-documentation
 "@version{#2022-8-22}
  @argument[gesture]{a @class{gtk:gesture} object}
  @return{The @class{gdk:device} object, or @code{nil}.}
  @begin{short}
    Returns the logical @class{gdk:device} object that is currently operating
    on @arg{gesture}, or @code{nil} if the gesture is not being interacted.
  @end{short}
  @see-class{gtk:gesture}"
  (gesture (g:object gesture)))

(export 'gesture-device)

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_is_active
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_gesture_is_active" gesture-is-active) :boolean
 #+liber-documentation
 "@version{#2022-8-21}
  @argument[gesture]{a @class{gtk:gesture} object}
  @return{@em{True} if @arg{gesture} is active.}
  @begin{short}
    Returns @em{true} if the gesture is currently active.
  @end{short}
  A gesture is active meanwhile there are touch sequences interacting with
  the gesture.
  @see-class{gtk:gesture}"
  (gesture (g:object gesture)))

(export 'gesture-is-active)

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_is_recognized
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_gesture_is_recognized" gesture-is-recognized) :boolean
 #+liber-documentation
 "@version{#2022-8-22}
  @argument[gesture]{a @class{gtk:gesture} object}
  @return{@em{True} if @arg{gesture} is recognized.}
  @begin{short}
    Returns @em{true} if the gesture is currently recognized.
  @end{short}
  A gesture is recognized if there are as many interacting touch sequences as
  required by the gesture.
  @see-class{gtk:gesture}"
  (gesture (g:object gesture)))

(export 'gesture-is-recognized)

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_get_sequence_state
;;; gtk_gesture_set_sequence_state
;;; ----------------------------------------------------------------------------

(defun (setf gesture-sequence-state) (value gesture sequence)
  (when (cffi:foreign-funcall "gtk_gesture_set_sequence_state"
                              (g:object gesture) gesture
                              (g:boxed gdk:event-sequence) sequence
                              event-sequence-state value
                              :boolean)
    value))

(cffi:defcfun ("gtk_gesture_get_sequence_state" gesture-sequence-state)
    event-sequence-state
 #+liber-documentation
 "@version{#2023-10-2}
  @syntax{(gtk:gesture-sequence-state gesture sequence) => state}
  @syntax{(setf (gtk:gesture-sequence-state gesture sequence) state)}
  @argument[gesture]{a @class{gtk:gesture} object}
  @argument[sequence]{a @class{gdk:event-sequence} object}
  @argument[state]{a @symbol{gtk:event-sequence-state} value}
  @begin{short}
    Accessor of the sequence state of the gesture.
  @end{short}
  The @fun{gtk:gesture-sequence-state} function returns the sequence state, as
  seen by the gesture. The @setf{gtk:gesture-sequence-state} function sets the
  state of @arg{sequence} in the gesture.
  @begin[Warning]{dictionary}
    The @setf{gtk:gesture-sequence-state} function is deprecated since 4.10.
    Use the @fun{gtk:gesture-set-state} function.
  @end{dictionary}
  @see-class{gtk:gesture}
  @see-class{gdk:event-sequence}
  @see-function{gtk:gesture-set-state}"
  (gesture (g:object gesture))
  (sequence (g:boxed gdk:event-sequence)))

(export 'gesture-sequence-state)

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_set_state
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_gesture_set_state" gesture-set-state) :boolean
 #+liber-documentation
 "@version{#2023-10-2}
  @argument[gesture]{a @class{gtk:gesture} object}
  @argument[state]{a @symbol{gtk:event-sequence-state} value}
  @return{@em{True} if the state of at least one sequence was changed
    successfully.}
  @begin{short}
    Sets the state of all sequences that the gesture is currently interacting
    with.
  @end{short}
  Sequences start in @code{:none} state, and whenever they change state, they
  can never go back to that state. Likewise, sequences in @code{:denied} state
  cannot turn back to a not denied state. With these rules, the lifetime of an
  event sequence is constrained to the next four:
  @begin{itemize}
    @item{None}
    @item{None → Denied}
    @item{None → Claimed}
    @item{None → Claimed → Denied}
  @end{itemize}
  Note: Due to event handling ordering, it may be unsafe to set the state on
  another gesture within a \"begin\" signal handler, as the callback might be
  executed before the other gesture knows about the sequence. A safe way to
  perform this could be:
  @begin{pre}
static void
first_gesture_begin_cb (GtkGesture       *first_gesture,
                        GdkEventSequence *sequence,
                        gpointer          user_data)
{
  gtk_gesture_set_sequence_state (first_gesture,
                                  sequence, GTK_EVENT_SEQUENCE_CLAIMED);
  gtk_gesture_set_sequence_state (second_gesture,
                                  sequence, GTK_EVENT_SEQUENCE_DENIED);
@}

static void
second_gesture_begin_cb (GtkGesture       *second_gesture,
                         GdkEventSequence *sequence,
                         gpointer          user_data)
{
  if (gtk_gesture_get_sequence_state (first_gesture, sequence)
       == GTK_EVENT_SEQUENCE_CLAIMED)
    gtk_gesture_set_sequence_state (second_gesture,
                                    sequence, GTK_EVENT_SEQUENCE_DENIED);
@}
  @end{pre}
  If both gestures are in the same group, just set the state on the gesture
  emitting the event, the sequence will be already be initialized to the
  group's global state when the second gesture processes the event.
  @see-class{gtk:gesture}
  @see-symbol{gtk:event-sequence-state}"
  (gesture (g:object gesture))
  (state event-sequence-state))

(export 'gesture-set-state)

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_get_sequences
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_gesture_get_sequences" gesture-sequences)
    (g:list-t (g:boxed gdk:event-sequence))
 #+liber-documentation
 "@version{#2023-10-21}
  @argument[gesture]{a @class{gtk:gesture} object}
  @return{The list of @class{gdk:event-sequences} instances.}
  @begin{short}
    Returns the list of @class{gdk:event-sequence} instances currently being
    interpreted by @arg{gesture}.
  @end{short}
  @see-class{gtk:gesture}
  @see-class{gdk:event-sequence}"
  (gesture (g:object gesture)))

(export 'gesture-sequences)

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_handles_sequence
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_gesture_handles_sequence" gesture-handles-sequence) :boolean
 #+liber-documentation
 "@version{#2023-10-21}
  @argument[gesture]{a @class{gtk:gesture} object}
  @argument[sequence]{a @class{gdk:event-sequence} instance, or @code{nil}}
  @return{@em{True} if @arg{gesture} is handling @arg{sequence}, @em{false}
    otherwise.}
  @begin{short}
    Returns @em{true} if @arg{gesture} is currently handling events
    corresponding to @arg{sequence}.
  @end{short}
  @see-class{gtk:gesture}
  @see-class{gdk:event-sequence}"
  (gesture (g:object gesture))
  (sequence (g:boxed gdk:event-sequence)))

(export 'gesture-handles-sequence)

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_get_last_updated_sequence
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_gesture_get_last_updated_sequence"
               gesture-last-updated-sequence) (g:boxed gdk:event-sequence)
 #+liber-documentation
 "@version{#2023-10-21}
  @argument[gesture]{a @class{gtk:gesture} object}
  @return{The last updated @class{gdk:event-sequence} instance.}
  @begin{short}
    Returns the @class{gdk:event-sequence} instance that was last updated on
    @arg{gesture}.
  @end{short}
  @see-class{gtk:gesture}
  @see-class{gdk:event-sequence}"
  (gesture (g:object gesture)))

(export 'gesture-last-updated-sequence)

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_get_last_event
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_gesture_get_last_event" gesture-last-event) gdk:event
 #+liber-documentation
 "@version{#2023-10-21}
  @argument[gesture]{a @class{gtk:gesture} object}
  @argument[sequence]{a @class{gdk:event-sequence} instance}
  @return{The last @class{gdk:event} instance from @arg{sequence}.}
  @begin{short}
    Returns the last event that was processed for @arg{sequence}.
  @end{short}
  Note that the returned pointer is only valid as long as the sequence is still
  interpreted by the gesture. If in doubt, you should make a copy of the event.
  @see-class{gtk:gesture}
  @see-class{gdk:event}
  @see-class{gdk:event-sequence}"
  (gesture (g:object gesture))
  (sequence (g:boxed gdk:event-sequence)))

(export 'gesture-last-event)

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_get_point
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_gesture_get-point" %gesture-point) :boolean
  (gesture (g:object gesture))
  (sequence (g:boxed gdk:event-sequence))
  (x (:pointer :double))
  (y (:pointer :double)))

(defun gesture-point (gesture sequence)
 #+liber-documentation
 "@version{#2023-10-21}
  @argument[gesture]{a @class{gtk:gesture} object}
  @argument[sequence]{a @class{gdk:event-sequence} instance, or @code{nil}
    for pointer events}
  @begin{return}
    @arg{x} -- a double float with the x axis of the sequence coordinates @br{}
    @arg{y} -- a double float with the y axis of the sequence coordinates
  @end{return}
  @begin{short}
    If @arg{sequence} is currently being interpreted by @arg{gesture}, this
    function returns x and y with the last coordinates stored for that event
    sequence.
  @end{short}
  The coordinates are always relative to the widget allocation.
  @see-class{gtk:gesture}
  @see-class{gdk:event-sequence}"
  (cffi:with-foreign-objects ((x :double) (y :double))
    (when (%gesture-point gesture sequence x y)
      (values (cffi:mem-ref x :double)
              (cffi:mem-ref y :double)))))

(export 'gesture-point)

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_get_bounding_box
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_gesture_get_bounding_box" %gesture-bounding-box) :boolean
  (gesture (g:object gesture))
  (rect (g:boxed gdk:rectangle)))

(defun gesture-bounding-box (gesture)
 #+liber-documentation
 "@version{#2023-10-21}
  @argument[gesture]{a @class{gtk:gesture} object}
  @return{The @class{gdk:rectangle} instance with the bounding box containing
    all active touches.}
  @begin{short}
    If there are touch sequences being currently handled by @arg{gesture}, this
    function returns the bounding box containing all active touches.
  @end{short}
  Otherwise, @em{false} will be returned.

  Note: This function will yield unexpected results on touchpad gestures. Since
  there is no correlation between physical and pixel distances, these will look
  as if constrained in an infinitely small area, rect width and height will thus
  be 0 regardless of the number of touchpoints.
  @see-class{gtk:gesture}
  @see-class{gdk:rectangle}"
  (let ((rect (gdk:rectangle-new)))
    (when (%gesture-bounding-box gesture rect)
      rect)))

(export 'gesture-bounding-box)

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_get_bounding_box_center
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_gesture_get_bounding_box_center"
               %gesture-bounding-box-center) :boolean
  (gesture (g:object gesture))
  (x (:pointer :double))
  (y (:pointer :double)))

(defun gesture-bounding-box-center (gesture)
 #+liber-documentation
 "@version{#2023-10-21}
  @argument[gesture]{a @class{gtk:gesture} object}
  @begin{return}
    @arg{x} -- a double float with the x coordinate for the bounding box center
    @br{}
    @arg{y} -- a double float with the y coordinate for the bounding box center
  @end{return}
  @begin{short}
    If there are touch sequences being currently handled by @arg{gesture}, this
    function returns in x and y the center of the bounding box containing all
    active touches.
  @end{short}
  Otherwise, @em{false} will be returned.
  @see-class{gtk:gesture}"
  (cffi:with-foreign-objects ((x :double) (y :double))
    (when (%gesture-bounding-box-center gesture x y)
      (values (cffi:mem-ref x :double)
              (cffi:mem-ref y :double)))))

(export 'gesture-bounding-box-center)

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_group
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_gesture_group" gesture-group) :void
 #+liber-documentation
 "@version{#2023-10-21}
  @argument[group]{a @class{gtk:gesture} object to group @arg{gesture} with}
  @argument[gesture]{a @class{gtk:gesture} object}
  @begin{short}
    Adds @arg{gesture} to the same group than @arg{group}.
  @end{short}
  Gestures are by default isolated in their own groups.

  Both gestures must have been added to the same widget before they can be
  grouped.

  When gestures are grouped, the state of the @class{gdk:event-sequence}
  instances is kept in sync for all of those, so calling the
  @fun{gtk:gesture-sequence-state} function, on one will transfer the same value
  to the others.

  Groups also perform an  \"implicit grabbing\" of sequences, if a
  @class{gdk:event-sequence} state is set to @code{:claimed} on one group,
  every other gesture group attached to the same @class{gtk:widget} widget will
  switch the state for that sequence to @code{:denied}.
  @see-class{gtk:gesture}"
  (group (g:object gesture))
  (gesture (g:object gesture)))

(export 'gesture-group)

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_ungroup
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_gesture_ungroup" gesture-ungroup) :void
 #+liber-documentation
 "@version{#2023-10-21}
  @argument[gesture]{a @class{gtk:gesture} object}
  @short{Separates @arg{gesture} into an isolated group.}
  @see-class{gtk:gesture}"
  (gesture (g:object gesture)))

(export 'gesture-ungroup)

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_get_group
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_gesture_get_group" gesture-get-group)
    (g:list-t (g:object gesture))
 #+liber-documentation
 "@version{#2023-10-21}
  @argument[gesture]{a @class{gtk:gesture} object}
  @return{The list of all @class{gtk:gesture} objects in the group of
    @arg{gesture}.}
  @short{Returns all gestures in the group of @arg{gesture}.}
  @see-class{gtk:gesture}"
  (gesture (g:object gesture)))

(export 'gesture-get-group)

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_is_grouped_with
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_gesture_is_grouped_with" gesture-is-grouped-with) :boolean
 #+liber-documentation
 "@version{#2023-10-21}
  @argument[gesture]{a @class{gtk:gesture} object}
  @argument[other]{another @class{gtk:gesture} object}
  @return{The boolean whether the gestures are grouped.}
  @short{Returns @em{true} if both gestures pertain to the same group.}
  @see-class{gtk:gesture}"
  (gesture (g:object gesture))
  (other (g:object gesture)))

(export 'gesture-is-grouped-with)

;;; --- End of file gtk4.gesture.lisp ------------------------------------------

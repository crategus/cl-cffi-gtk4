;;; ----------------------------------------------------------------------------
;;; gtk4.gesture.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.6 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2019 - 2023 Dieter Kaiser
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
;;; enum GtkEventSequenceState
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkEventSequenceState" event-sequence-state
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
  @begin{short}
    Describes the state of a @class{gdk:event-sequence} instance in a
    @class{gtk:gesture} object.
  @end{short}
  @begin{pre}
(define-g-enum \"GtkEventSequenceState\" event-sequence-state
  (:export t
   :type-initializer \"gtk_event_sequence_state_get_type\")
  (:none 0)
  (:claimed 1)
  (:denied 2))
  @end{pre}
  @begin[code]{table}
    @entry[:none]{The sequence is handled, but not grabbed.}
    @entry[:claimed]{The sequence is handled and grabbed.}
    @entry[:denied]{The sequence is denied.}
  @end{table}
  @see-class{gtk:gesture}
  @see-class{gdk:event-sequence}")

;;; ----------------------------------------------------------------------------
;;; struct GtkGesture
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkGesture" gesture
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
    The @sym{gtk:gesture} object is the base object for gesture recognition.
  @end{short}
  Although this object is quite generalized to serve as a base for multi-touch
  gestures, it is suitable to implement single-touch and pointer-based gestures.

  The number of touches that a @sym{gtk:gesture} object need to be recognized
  is controlled by the @code{n-points} property, if a gesture is keeping track
  of less or more than that number of sequences, it will not check whether the
  gesture is recognized.

  As soon as the gesture has the expected number of touches, the gesture will
  run the \"check\" signal regularly on input events until the gesture is
  recognized, the criteria to consider a gesture as recognized is left to
  @sym{gtk:gesture} subclasses.

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
  @fun{gtk:gesture-set-sequence-state} function on one will effectively
  propagate the state throughout the group.

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
  @fun{gtk:gesture-set-sequence-state} function to know about the possible
  lifetimes of a @class{gdk:event-sequence} instance.

  @subheading{Touchpad gestures}
  On the platforms that support it, the @sym{gtk:gesture} object will handle
  transparently touchpad gesture events. The only precautions users of the
  @sym{gtk:gesture} object should do to enable this support are:
  @begin{itemize}
    @item{Enabling GDK_TOUCHPAD_GESTURE_MASK on their @class{gdk-windows}}
    @item{If the gesture has @code{:phase-none}, ensuring events of type
      GDK_TOUCHPAD_SWIPE and GDK_TOUCHPAD_PINCH are handled by the
      @sym{gtk:gesture}}
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
        @entry[gesture]{The @sym{gtk:gesture} object which received the signal.}
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
        @entry[gesture]{The @sym{gtk:gesture} object which received the signal.}
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
        @entry[gesture]{The @sym{gtk:gesture} object which received the signal.}
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
        @entry[gesture]{The @sym{gtk:gesture} object which received the signal.}
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
        @entry[gesture]{The @sym{gtk:gesture} object which received the signal.}
        @entry[sequence]{The @class{gdk:event-sequence} event that was updated.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk:gesture-n-points}
  @see-class{gtk:event-controller}
  @see-class{gtk:gesture-single}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gesture-n-points ---------------------------------------------------

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
  @syntax[]{(gtk:gesture-n-points object) => n-points)}
  @argument[object]{a @class{gtk:gesture} object}
  @argument[n-points]{an integer with the number of touch points}
  @begin{short}
    Accessor of the @slot[gtk:gesture]{n-points} slot of the
    @class{gtk:gesture} class.
  @end{short}

  The number of touch points that trigger recognition on this gesture.
  @see-class{gtk:gesture}")

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_get_device () -> gesture-device
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_gesture_get_device" gesture-device) (g:object gdk-device)
 #+liber-documentation
 "@version{#2022-8-22}
  @argument[gesture]{a @class{gtk:gesture} object}
  @return{A @class{gdk-device} object, or @code{nil}.}
  @begin{short}
    Returns the logical @class{gdk-device} object that is currently operating
    on @arg{gesture}, or @code{nil} if the gesture is not being interacted.
  @end{short}
  @see-class{gtk:gesture}"
  (gesture (g:object gesture)))

(export 'gesture-device)

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_is_active ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_gesture_is_active" gesture-is-active) :boolean
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
;;; gtk_gesture_is_recognized ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_gesture_is_recognized" gesture-is-recognized) :boolean
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
;;; gtk_gesture_get_sequence_state ()
;;; gtk_gesture_set_sequence_state () -> gesture-sequence-state
;;; ----------------------------------------------------------------------------

(defun (setf gesture-sequence-state) (value gesture sequence)
  (when (cffi:foreign-funcall "gtk_gesture_set_sequence_state"
                              (g:object gesture) gesture
                              (g:boxed gdk:event-sequence) sequence
                              event-sequence-state value
                              :boolean)
    value))

(defcfun ("gtk_gesture_get_sequence_state" gesture-sequence-state)
    event-sequence-state
 #+liber-documentation
 "@version{#2022-8-22}
  @syntax[]{(gtk:gesture-sequence-state gesture sequence) => state}
  @syntax[]{(setf (gtk:gesture-sequence-state gesture sequence) state)}
  @argument[gesture]{a @class{gtk:gesture} object}
  @argument[sequence]{a @class{gdk:event-sequence} object}
  @argument[state]{a @symbol{gtk:event-sequence-state} value}
  @begin{short}
    Accessor of the sequence state of the gesture.
  @end{short}
  The @sym{gtk:gesture-sequence-state} function returns the sequence state, as
  seen by the gesture. The @sym{(setf gtk:gesture-sequence-state} function sets
  the state of @arg{sequence} in the gesture.

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
  @see-class{gdk:event-sequence}"
  (gesture (g:object gesture))
  (sequence (g:boxed gdk:event-sequence)))

(export 'gesture-sequence-state)

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_set_state ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_gesture_set_state" gesture-set-state) :boolean
 #+liber-documentation
 "@version{#2022-8-22}
  @argument[gesture]{a @class{gtk:gesture} object}
  @argument[state]{a @symbol{gtk:event-sequence-state} value}
  @return{@em{True} if the state of at least one sequence was changed
    sucessfully.}
  @begin{short}
    Sets the state of all sequences that the gesture is currently interacting
    with.
  @end{short}
  See the @fun{gtk:gesture-sequence-state} function for more details on sequence
  states.
  @see-class{gtk:gesture}
  @see-symbol{gtk:event-sequence-state}
  @see-function{gtk:gesture-sequence-state}"
  (gesture (g:object gesture))
  (state event-sequence-state))

(export 'gesture-set-state)

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_get_sequences ()
;;;
;;; GList *
;;; gtk_gesture_get_sequences (GtkGesture *gesture);
;;;
;;; Returns the list of GdkEventSequences currently being interpreted by
;;; gesture .
;;;
;;; gesture :
;;;     a GtkGesture
;;;
;;; Returns :
;;;     A list of GdkEventSequences, the list elements are owned by GTK and must
;;;     not be freed or modified, the list itself must be deleted through
;;;     g_list_free().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_handles_sequence ()
;;;
;;; gboolean
;;; gtk_gesture_handles_sequence (GtkGesture *gesture,
;;;                               GdkEventSequence *sequence);
;;;
;;; Returns TRUE if gesture is currently handling events corresponding to
;;; sequence .
;;;
;;; gesture :
;;;     a GtkGesture
;;;
;;; sequence :
;;;     a GdkEventSequence or NULL.
;;;
;;; Returns :
;;;     TRUE if gesture is handling sequence , FALSE otherwise
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_get_last_updated_sequence ()
;;;
;;; GdkEventSequence *
;;; gtk_gesture_get_last_updated_sequence (GtkGesture *gesture);
;;;
;;; Returns the GdkEventSequence that was last updated on gesture .
;;;
;;; gesture :
;;;     a GtkGesture
;;;
;;; Returns :
;;;     The last updated sequence.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_get_last_event ()
;;;
;;; GdkEvent *
;;; gtk_gesture_get_last_event (GtkGesture *gesture,
;;;                             GdkEventSequence *sequence);
;;;
;;; Returns the last event that was processed for sequence .
;;;
;;; Note that the returned pointer is only valid as long as the sequence is
;;; still interpreted by the gesture . If in doubt, you should make a copy of
;;; the event.
;;;
;;; gesture :
;;;     a GtkGesture
;;;
;;; sequence :
;;;     a GdkEventSequence.
;;;
;;; Returns :
;;;     The last event from sequence .
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_get_point ()
;;;
;;; gboolean
;;; gtk_gesture_get_point (GtkGesture *gesture,
;;;                        GdkEventSequence *sequence,
;;;                        double *x,
;;;                        double *y);
;;;
;;; If sequence is currently being interpreted by gesture , this function
;;; returns TRUE and fills in x and y with the last coordinates stored for that
;;; event sequence. The coordinates are always relative to the widget
;;; allocation.
;;;
;;; gesture :
;;;     a GtkGesture
;;;
;;; sequence :
;;;     a GdkEventSequence, or NULL for pointer events.
;;;
;;; x :
;;;     return location for X axis of the sequence coordinates.
;;;
;;; y :
;;;     return location for Y axis of the sequence coordinates.
;;;
;;; Returns :
;;;     TRUE if sequence is currently interpreted
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_get_bounding_box ()
;;;
;;; gboolean
;;; gtk_gesture_get_bounding_box (GtkGesture *gesture,
;;;                               GdkRectangle *rect);
;;;
;;; If there are touch sequences being currently handled by gesture , this
;;; function returns TRUE and fills in rect with the bounding box containing all
;;; active touches. Otherwise, FALSE will be returned.
;;;
;;; Note: This function will yield unexpected results on touchpad gestures.
;;; Since there is no correlation between physical and pixel distances, these
;;; will look as if constrained in an infinitely small area, rect width and
;;; height will thus be 0 regardless of the number of touchpoints.
;;;
;;; gesture :
;;;     a GtkGesture
;;;
;;; rect :
;;;     bounding box containing all active touches.
;;;
;;; Returns :
;;;     TRUE if there are active touches, FALSE otherwise
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_get_bounding_box_center ()
;;;
;;; gboolean
;;; gtk_gesture_get_bounding_box_center (GtkGesture *gesture,
;;;                                      double *x,
;;;                                      double *y);
;;;
;;; If there are touch sequences being currently handled by gesture , this
;;; function returns TRUE and fills in x and y with the center of the bounding
;;; box containing all active touches. Otherwise, FALSE will be returned.
;;;
;;; gesture :
;;;     a GtkGesture
;;;
;;; x :
;;;     X coordinate for the bounding box center.
;;;
;;; y :
;;;     Y coordinate for the bounding box center.
;;;
;;; Returns :
;;;     FALSE if no active touches are present, TRUE otherwise
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_group ()
;;;
;;; void
;;; gtk_gesture_group (GtkGesture *group_gesture,
;;;                    GtkGesture *gesture);
;;;
;;; Adds gesture to the same group than group_gesture . Gestures are by default
;;; isolated in their own groups.
;;;
;;; Both gestures must have been added to the same widget before they can be
;;; grouped.
;;;
;;; When gestures are grouped, the state of GdkEventSequences is kept in sync
;;; for all of those, so calling gtk_gesture_set_sequence_state(), on one will
;;; transfer the same value to the others.
;;;
;;; Groups also perform an "implicit grabbing" of sequences, if a
;;; GdkEventSequence state is set to GTK_EVENT_SEQUENCE_CLAIMED on one group,
;;; every other gesture group attached to the same GtkWidget will switch the
;;; state for that sequence to GTK_EVENT_SEQUENCE_DENIED.
;;;
;;; gesture :
;;;     a GtkGesture
;;;
;;; group_gesture :
;;;     GtkGesture to group gesture with
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_ungroup ()
;;;
;;; void
;;; gtk_gesture_ungroup (GtkGesture *gesture);
;;;
;;; Separates gesture into an isolated group.
;;;
;;; gesture :
;;;     a GtkGesture
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_get_group ()
;;;
;;; GList *
;;; gtk_gesture_get_group (GtkGesture *gesture);
;;;
;;; Returns all gestures in the group of gesture
;;;
;;; gesture :
;;;     a GtkGesture
;;;
;;; Returns :
;;;     The list of GtkGestures, free with g_list_free().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_is_grouped_with ()
;;;
;;; gboolean
;;; gtk_gesture_is_grouped_with (GtkGesture *gesture,
;;;                              GtkGesture *other);
;;;
;;; Returns TRUE if both gestures pertain to the same group.
;;;
;;; gesture :
;;;     a GtkGesture
;;;
;;; other :
;;;     another GtkGesture
;;;
;;; Returns :
;;;     whether the gestures are grouped
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk4.gesture.lisp ------------------------------------------

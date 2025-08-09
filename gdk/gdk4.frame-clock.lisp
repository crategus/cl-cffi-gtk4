;;; ----------------------------------------------------------------------------
;;; gdk4.frame-clock.lisp
;;;
;;; The documentation in this file is taken from the GDK 4 Reference Manual
;;; version 4.18 and modified to document the Lisp binding to the GDK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 - 2025 Dieter Kaiser
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
;;; GdkFrameClock
;;;
;;;     Synchronizes painting to a surface
;;;
;;; Types and Values
;;;
;;;     GdkFrameClock
;;;     GdkFrameClockPhase
;;;
;;; Functions
;;;
;;;     gdk_frame_clock_get_frame_time
;;;     gdk_frame_clock_request_phase
;;;     gdk_frame_clock_begin_updating
;;;     gdk_frame_clock_end_updating
;;;     gdk_frame_clock_get_frame_counter
;;;     gdk_frame_clock_get_history_start
;;;     gdk_frame_clock_get_timings
;;;     gdk_frame_clock_get_current_timings
;;;     gdk_frame_clock_get_refresh_info
;;;     gdk_frame_clock_get_fps
;;;
;;; Signals
;;;
;;;     after-paint
;;;     before-paint
;;;     flush-events
;;;     layout
;;;     paint
;;;     resume-events
;;;     update
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GdkFrameClock
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkFrameClockPhase
;;; ----------------------------------------------------------------------------

(gobject:define-gflags "GdkFrameClockPhase" frame-clock-phase
  (:export t
   :type-initializer "gdk_frame_clock_phase_get_type")
  (:none 0)
  (:flush-events #.(ash 1 0))
  (:before-paint #.(ash 1 1))
  (:update #.(ash 1 2))
  (:layout #.(ash 1 3))
  (:paint #.(ash 1 4))
  (:resmue-events #.(ash 1 5))
  (:after-paint #.(ash 1 6)))

#+liber-documentation
(setf (liber:alias-for-symbol 'frame-clock-phase)
      "GFlags"
      (liber:symbol-documentation 'frame-clock-phase)
 "@version{2025-08-02}
  @begin{declaration}
(gobject:define-gflags \"GdkFrameClockPhase\" frame-clock-phase
  (:export t
   :type-initializer \"gdk_frame_clock_phase_get_type\")
  (:none 0)
  (:flush-events #.(ash 1 0))
  (:before-paint #.(ash 1 1))
  (:update #.(ash 1 2))
  (:layout #.(ash 1 3))
  (:paint #.(ash 1 4))
  (:resmue-events #.(ash 1 5))
  (:after-paint #.(ash 1 6)))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:none]{No phase.}
      @entry[:flush-events]{Corresponds to @sig[gdk:frame-clock]{flush-events}
        signals. Should not be handled by applications.}
      @entry[:before-paint]{Corresponds to @sig[gdk:frame-clock]{before-paint}
        signals. Should not be handled by applications.}
      @entry[:update]{Corresponds to the @sig[gdk:frame-clock]{update} signal.}
      @entry[:layout]{Corresponds to the @sig[gdk:frame-clock]{layout} signal.
        Should not be handled by applications.}
      @entry[:paint]{Corresponds to the @sig[gdk:frame-clock]{paint} signal.}
      @entry[:resume-events]{Corresponds to the
        @sig[gdk:frame-clock]{resume-events} signal. Should not be handled by
        applications.}
      @entry[:after-paint]{Corresponds to the @sig[gdk:frame-clock]{after-paint}
        signal. Should not be handled by applications.}
    @end{simple-table}
  @end{values}
  @begin{short}
    The @sym{gdk:frame-clock-phase} enumeration is used to represent the
    different paint clock phases that can be requested.
  @end{short}
  The elements of the enumeration correspond to the signals of the
  @class{gdk:frame-clock} object.
  @see-class{gdk:frame-clock}")

;;; ----------------------------------------------------------------------------
;;; GdkFrameClock
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GdkFrameClock" frame-clock
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gdk_frame_clock_get_type")
  nil)

#+liber-documentation
(setf (documentation 'frame-clock 'type)
 "@version{2025-06-29}
  @begin{short}
    The @class{gdk:frame-clock} object tells the application when to update and
    repaint a surface.
  @end{short}
  This may be synced to the vertical refresh rate of the monitor, for example.
  Even when the frame clock uses a simple timer rather than a hardware-based
  vertical sync, the frame clock helps because it ensures everything paints at
  the same time (reducing the total number of frames). The frame clock can also
  automatically stop painting when it knows the frames will not be visible, or
  scale back animation framerates.

  The @class{gdk:frame-clock} object is designed to be compatible with an OpenGL
  based implementation or with @code{mozRequestAnimationFrame} in Firefox, for
  example.

  A frame clock is idle until someone requests a frame with the
  @fun{gdk:frame-clock-request-phase} function. At some later point that makes
  sense for the synchronization being implemented, the clock will process a
  frame and emit signals for each phase that has been requested. See the
  signals of the @class{gdk:frame-clock} class for documentation of the phases.
  The @val[gdk:frame-clock-phase]{:update} value of the
  @sym{gdk:frame-clock-phase} enumeration and the @sig[gdk:frame-clock]{update}
  signal are most interesting for application writers, and are used to update
  the animations, using the frame time given by the
  @fun{gdk:frame-clock-frame-time} function.

  The frame time is reported in microseconds and generally in the same
  timescale as the system monotonic time. The frame time does not advance
  during the time a frame is being painted, and outside of a frame, an
  attempt is made so that all calls to the @fun{gdk:frame-clock-frame-time}
  function that are called at a \"similar\" time get the same value. This means
  that if different animations are timed by looking at the difference in time
  between an initial value from the @fun{gdk:frame-clock-frame-time} function
  and the value inside the @sig[gdk:frame-clock]{update} signal of the clock,
  they will stay exactly synchronized.
  @begin[Signal Details]{dictionary}
    @begin[frame-clock::after-paint]{signal}
      @begin{pre}
lambda (clock)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[clock]{The @class{gdk:frame-clock} object emitting the signal.}
      @end{simple-table}
      The signal ends processing of the frame. Applications should generally
      not handle this signal.
    @end{signal}
    @begin[frame-clock::before-paint]{signal}
      @begin{pre}
lambda (clock)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[clock]{The @class{gdk:frame-clock} object emitting the signal.}
      @end{simple-table}
      The signal begins processing of the frame. Applications should generally
      not handle this signal.
    @end{signal}
    @begin[frame-clock::flush-events]{signal}
      @begin{pre}
lambda (clock)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[clock]{The @class{gdk:frame-clock} object emitting the signal.}
      @end{simple-table}
      The signal is used to flush pending motion events that are being batched
      up and compressed together. Applications should not handle this signal.
    @end{signal}
    @begin[frame-clock::layout]{signal}
      @begin{pre}
lambda (clock)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[clock]{The @class{gdk:frame-clock} object emitting the signal.}
      @end{simple-table}
      The signal is emitted as the second step of toolkit and application
      processing of the frame. Any work to update sizes and positions of
      application elements should be performed. GTK normally handles this
      internally.
    @end{signal}
    @begin[frame-clock::paint]{signal}
      @begin{pre}
lambda (clock)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[clock]{The @class{gdk:frame-clock} object emitting the signal.}
      @end{simple-table}
      The signal is emitted as the third step of toolkit and application
      processing of the frame. The frame is repainted. GDK normally handles
      this internally and emits @code{GdkSurface::render} signals, which are
      turned into GTK @code{GtkWidget::snapshot} signals.
    @end{signal}
    @begin[frame-clock::resume-events]{signal}
      @begin{pre}
lambda (clock)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[clock]{The @class{gdk:frame-clock} object emitting the signal.}
      @end{simple-table}
      The signal is emitted after processing of the frame is finished, and is
      handled internally by GTK to resume normal event processing. Applications
      should not handle this signal.
    @end{signal}
    @begin[frame-clock::update]{signal}
      @begin{pre}
lambda (clock)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[clock]{The @class{gdk:frame-clock} object emitting the signal.}
      @end{simple-table}
      The signal is emitted as the first step of toolkit and application
      processing of the frame. Animations should be updated using the
      @fun{gdk:frame-clock-frame-time} function. Applications can connect
      directly to this signal, or use the @fun{gtk:widget-add-tick-callback}
      function as a more convenient interface.
    @end{signal}
  @end{dictionary}
  @see-class{gdk:frame-timings}")

;;; ----------------------------------------------------------------------------
;;; gdk_frame_clock_get_frame_time
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_frame_clock_get_frame_time" frame-clock-frame-time) :int64
 #+liber-documentation
 "@version{2025-08-04}
  @argument[clock]{a @class{gdk:frame-clock} object}
  @return{The integer for a timestamp in microseconds.}
  @begin{short}
    Gets the time that should currently be used for animations.
  @end{short}
  Inside the processing of a frame, it is the time used to compute the animation
  position of everything in a frame. Outside of a frame, it is the time of the
  conceptual \"previous frame\", which may be either the actual previous frame
  time, or if that is too old, an updated time.
  @see-class{gdk:frame-clock}"
  (clock (g:object frame-clock)))

(export 'frame-clock-frame-time)

;;; ----------------------------------------------------------------------------
;;; gdk_frame_clock_request_phase
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_frame_clock_request_phase" frame-clock-request-phase) :void
 #+liber-documentation
 "@version{#2025-07-31}
  @argument[clock]{a @class{gdk:frame-clock} object}
  @argument[phase]{a @sym{gdk:frame-clock-phase} value for the phase that is
    requested}
  @begin{short}
    Asks the frame clock to run a particular phase.
  @end{short}
  The signal corresponding to the requested phase will be emitted the next time
  the frame clock processes. Multiple calls to the
  @fun{gdk:frame-clock-request-phase} function will be combined together and
  only one frame processed. If you are displaying animated content and want to
  continually request the @val[gdk:frame-clock-phase]{:update} phase for a
  period of time, you should use the @fun{gdk:frame-clock-begin-updating}
  function instead, since this allows GTK to adjust system parameters to get
  maximally smooth animations.
  @see-class{gdk:frame-clock}
  @see-symbol{gdk:frame-clock-phase}
  @see-function{gdk:frame-clock-begin-updating}"
  (clock (g:object frame-clock))
  (phase frame-clock-phase))

(export 'frame-clock-request-phase)

;;; ----------------------------------------------------------------------------
;;; gdk_frame_clock_begin_updating
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_frame_clock_begin_updating" frame-clock-begin-updating)
    :void
 #+liber-documentation
 "@version{#2025-03-01}
  @argument[clock]{a @class{gdk:frame-clock} object}
  @begin{short}
    Starts updates for an animation.
  @end{short}
  Until a matching call to the @fun{gdk:frame-clock-end-updating} function is
  made, the frame clock will continually request a new frame with the
  @code{:update} phase. This function may be called multiple times and frames
  will be requested until the @fun{gdk:frame-clock-end-updating} function is
  called the same number of times.
  @see-class{gdk:frame-clock}
  @see-function{gdk:frame-clock-end-updating}"
  (clock (g:object frame-clock)))

(export 'frame-clock-begin-updating)

;;; ----------------------------------------------------------------------------
;;; gdk_frame_clock_end_updating
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_frame_clock_end_updating" frame-clock-end-updating) :void
 #+liber-documentation
 "@version{#2025-03-01}
  @argument[clock]{a @class{gdk:frame-clock} object}
  @begin{short}
    Stops updates for an animation.
  @end{short}
  See the documentation for the @fun{gdk:frame-clock-begin-updating} function.
  @see-class{gdk:frame-clock}
  @see-function{gdk:frame-clock-begin-updating}"
  (clock (g:object frame-clock)))

(export 'frame-clock-end-updating)

;;; ----------------------------------------------------------------------------
;;; gdk_frame_clock_get_frame_counter
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_frame_clock_get_frame_counter" frame-clock-frame-counter)
    :int64
 #+liber-documentation
 "@version{#2025-03-01}
  @argument[clock]{a @class{gdk:frame-clock} object}
  @begin{return}
    Inside frame processing, the unsigned integer of the frame counter for the
    current frame. Outside of frame processing, the frame counter for the last
    frame.
  @end{return}
  @begin{short}
    A frame clock maintains a 64-bit counter that increments for each frame
    drawn.
  @end{short}
  @see-class{gdk:frame-clock}"
  (clock (g:object frame-clock)))

(export 'frame-clock-frame-counter)

;;; ----------------------------------------------------------------------------
;;; gdk_frame_clock_get_history_start
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_frame_clock_get_history_start" frame-clock-history-start)
    :int64
 #+liber-documentation
 "@version{#2025-03-01}
  @argument[clock]{a @class{gdk:frame-clock} object}
  @begin{return}
    The unsigned integer for the frame counter value of the oldest frame that
    is available in the internal frame history of the frame clock.
  @end{return}
  @begin{short}
    The frame clock internally keeps a history of @class{gdk:frame-timings}
    objects for recent frames that can be retrieved with the
    @fun{gdk:frame-clock-timings} function.
  @end{short}
  The set of stored frames is the set from the counter values given by the
  @fun{gdk:frame-clock-history-start} function and the
  @fun{gdk:frame-clock-frame-counter} function, inclusive.
  @see-class{gdk:frame-clock}
  @see-class{gdk:frame-timings}
  @see-function{gdk:frame-clock-timings}
  @see-function{gdk:frame-clock-frame-counter}"
  (clock (g:object frame-clock)))

(export 'frame-clock-history-start)

;;; ----------------------------------------------------------------------------
;;; gdk_frame_clock_get_timings
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_frame_clock_get_timings" frame-clock-timings)
    (g:boxed frame-timings)
 #+liber-documentation
 "@version{#2025-03-01}
  @argument[clock]{a @class{gdk:frame-clock} object}
  @argument[counter]{an unsigned integer for the frame counter value identifying
    the frame to be received}
  @begin{return}
    The @class{gdk:frame-timings} instance for the specified frame, or
    @code{nil} if it is not available. See the
    @fun{gdk:frame-clock-history-start} function.
  @end{return}
  @begin{short}
    Retrieves a @fun{gdk:frame-timings} instance holding timing information for
    the current frame or a recent frame.
  @end{short}
  The @class{gdk:frame-timings} instance may not yet be complete. See the
  @fun{gdk:frame-timings-complete} function.
  @see-class{gdk:frame-clock}
  @see-class{gdk:frame-timings}
  @see-function{gdk:frame-clock-history-start}
  @see-function{gdk:frame-timings-complete}"
  (clock (g:object frame-clock))
  (counter :int64))

(export 'frame-clock-timings)

;;; ----------------------------------------------------------------------------
;;; gdk_frame_clock_get_current_timings
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_frame_clock_get_current_timings"
               frame-clock-current-timings) (g:boxed frame-timings)
 #+liber-documentation
 "@version{#2025-03-01}
  @argument[clock]{a @class{gdk:frame-clock} object}
  @begin{return}
    The @class{gdk:frame-timings} instance for the frame currently being
    processed, or even no frame is being processed, for the previous frame.
    Before any frames have been procesed, returns @code{nil}.
  @end{return}
  @begin{short}
    Gets the frame timings for the current frame.
  @end{short}
  @see-class{gdk:frame-clock}
  @see-class{gdk:frame-timings}"
  (clock (g:object frame-clock)))

(export 'frame-clock-current-timings)

;;; ----------------------------------------------------------------------------
;;; gdk_frame_clock_get_refresh_info
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_frame_clock_get_refresh_info" %frame-clock-refresh-info)
    :void
  (clock (g:object frame-clock))
  (time :int64)
  (refresh-interval (:pointer :int64))
  (presentation-time (:pointer :int64)))

(defun frame-clock-refresh-info (clock time)
 #+liber-documentation
 "@version{#2025-08-04}
  @syntax{(gdk:frame-clock-refresh-info clock time) => interval,
    presentation-time}
  @argument[clock]{a @class{gdk:frame-clock} object}
  @argument[time]{an integer for the base time for determining a presentaton
    time}
  @argument[refresh-interval]{an integer for the determined refresh interval,
    or @code{nil}, a default refresh interval of 1/60th of a second will be
    stored if no history is present}
  @argument[presentation-time]{an integer for the next candidate presentation
    time after the given base time, 0 will be will be stored if no history is
    present}
  @begin{short}
    Using the frame history stored in the frame clock, finds the last known
    presentation time and refresh interval, and assuming that presentation times
    are separated by the refresh interval, predicts a presentation time that is
    a multiple of the refresh interval after the last presentation time, and
    later than @arg{time}.
  @end{short}
  @see-class{gdk:frame-clock}"
  (cffi:with-foreign-objects ((refresh-interval :int64)
                              (presentation-time :int64))
    (%frame-clock-refresh-info clock
                               time
                               refresh-interval
                               presentation-time)
    (values (cffi:mem-ref refresh-interval :int64)
            (cffi:mem-ref presentation-time :int64))))

(export 'frame-clock-refresh-info)

;;; ----------------------------------------------------------------------------
;;; gdk_frame_clock_get_fps
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_frame_clock_get_fps" frame-clock-fps) :double
 #+liber-documentation
 "@version{2025-08-04}
  @argument[clock]{a @class{gdk:frame-clock} object}
  @return{The double float for the current fps.}
  @begin{short}
    Calculates the current frames-per-second, based on the frame timings of
    @arg{clock}.
  @end{short}
  @see-class{gdk:frame-clock}"
  (clock (g:object frame-clock)))

(export 'frame-clock-fps)

;;; --- End of file gdk4.frame-clock.lisp --------------------------------------

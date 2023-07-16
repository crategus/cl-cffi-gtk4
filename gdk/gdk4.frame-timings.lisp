;;; ----------------------------------------------------------------------------
;;; gdk4.frame-timings.lisp
;;;
;;; The documentation of this file is taken from the GDK 4 Reference Manual
;;; Version 4.10 and modified to document the Lisp binding to the GDK library.
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
;;; Frame timings
;;;
;;;     Object holding timing information for a single frame
;;;
;;; Types and Values
;;;
;;; 	GdkFrameTimings
;;;
;;; Functions
;;;
;;;     gdk_frame_timings_ref
;;;     gdk_frame_timings_unref
;;;     gdk_frame_timings_get_frame_counter
;;;     gdk_frame_timings_get_complete
;;;     gdk_frame_timings_get_frame_time
;;;     gdk_frame_timings_get_presentation_time
;;;     gdk_frame_timings_get_refresh_interval
;;;     gdk_frame_timings_get_predicted_presentation_time
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; struct GdkFrameTimings
;;; ----------------------------------------------------------------------------

(glib:define-g-boxed-opaque frame-timings "GdkFrameTimings"
  :export t
  :type-initializer "gdk_frame_timings_get_type"
  :alloc (error "GdkFrameTimings cannot be created from the Lisp side."))

#+liber-documentation
(setf (liber:alias-for-class 'frame-timings)
      "GBoxed"
      (documentation 'frame-timings 'type)
 "@version{#2023-3-10}
  @begin{short}
    A @sym{gdk:frame-timings} structure holds timing information for a single
    frame of the application’s displays.
  @end{short}
  The @sym{gdk:frame-timings} structure is opaque, and has no user visible
  fields. An instance cannot be created from the Lisp side. To retrieve a
  @sym{gdk:frame-timings} instance, use the @fun{gdk:frame-clock-timings} or
  @fun{gdk:frame-clock-current-timings} functions. The information in the
  @sym{gdk:frame-timings} instance is useful for precise synchronization of
  video with the event or audio streams, and for measuring quality metrics for
  the application’s display, such as latency and jitter.
  @see-class{gdk:frame-clock}
  @see-function{gdk:frame-clock-timings}
  @see-function{gdk:frame-clock-current-timings}")

;;; ----------------------------------------------------------------------------
;;; gdk_frame_timings_ref ()
;;;
;;; GdkFrameTimings *
;;; gdk_frame_timings_ref (GdkFrameTimings *timings);
;;;
;;; Increases the reference count of timings .
;;;
;;; timings :
;;;     a GdkFrameTimings
;;;
;;; Returns :
;;;     timings
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_frame_timings_unref ()
;;;
;;; void
;;; gdk_frame_timings_unref (GdkFrameTimings *timings);
;;;
;;; Decreases the reference count of timings . If timings is no longer
;;; referenced, it will be freed.
;;;
;;; timings :
;;;     a GdkFrameTimings
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_frame_timings_get_frame_counter () -> frame-timings-frame-counter
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_frame_timings_get_frame_counter"
               frame-timings-frame-counter) :int64
 #+liber-documentation
 "@version{#2023-3-10}
  @argument[timings]{a @class{gdk:frame-timings} instance}
  @return{The integer frame counter value for this frame.}
  @begin{short}
    Gets the frame counter value of the frame clock when this frame was drawn.
  @end{short}
  @see-class{gdk:frame-timings}"
  (timings (g:boxed frame-timings)))

(export 'frame-timings-frame-counter)

;;; ----------------------------------------------------------------------------
;;; gdk_frame_timings_get_complete () -> frame-timings-complete
;;;-----------------------------------------------------------------------------

(cffi:defcfun ("gdk_frame_timings_get_complete" frame-timings-complete) :boolean
 #+liber-documentation
 "@version{#2023-3-10}
  @argument[timings]{a @class{gdk:frame-timings} instance}
  @begin{return}
    @em{True} if all information that will be available for the frame has been
    filled in.
  @end{return}
  @begin{short}
    The timing information in a @class{gdk:frame-timings} instance is filled in
    incrementally as the frame as drawn and passed off to the window system for
    processing and display to the user.
  @end{short}
  The accessor functions for @class{gdk:frame-timings} instances can return 0
  to indicate an unavailable value for two reasons: either because the
  information is not yet available, or because it is not available at all. Once
  the @sym{gdk:frame-timings-complete} function returns @em{true} for a frame,
  you can be certain that no further values will become available and be stored
  in the @class{gdk:frame-timings} instance.
  @see-class{gdk:frame-timings}"
  (timings (g:boxed frame-timings)))

(export 'frame-timings-complete)

;;; ----------------------------------------------------------------------------
;;; gdk_frame_timings_get_frame_time () -> frame-timings-frame-time
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_frame_timings_get_frame_time" frame-timings-frame-time)
    :int64
 #+liber-documentation
 "@version{#2023-3-10}
  @argument[timings]{a @class{gdk:frame-timings} instance}
  @begin{return}
    An integer with the frame time for the frame.
  @end{return}
  @begin{short}
    Returns the frame time for the frame.
  @end{short}
  This is the time value that is typically used to time animations for the
  frame. See the @fun{gdk:frame-clock-frame-time} function.
  @see-class{gdk:frame-timings}
  @see-function{gdk:frame-clock-frame-time}"
  (timings (g:boxed frame-timings)))

(export 'frame-timings-frame-time)

;;; ----------------------------------------------------------------------------
;;; gdk_frame_timings_get_presentation_time ()
;;;     -> frame-timings-presentation-time
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_frame_timings_get_presentation_time"
               frame-timings-presentation-time) :int64
 #+liber-documentation
 "@version{#2023-3-10}
  @argument[timings]{a @class{gdk:frame-timings} instance}
  @begin{return}
    An integer with the time the frame was displayed to the user, or 0 if no
    presentation time is available. See the @fun{gdk:frame-timings-complete}
    function.
  @end{return}
  @begin{short}
    Returns the presentation time.
  @end{short}
  This is the time at which the frame became visible to the user.
  @see-class{gdk:frame-timings}
  @see-function{gdk:frame-timings-complete}"
  (timings (g:boxed frame-timings)))

(export 'frame-timings-presentation-time)

;;; ----------------------------------------------------------------------------
;;; gdk_frame_timings_get_refresh_interval ()
;;;     -> frame-timings-refresh-interval
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_frame_timings_get_refresh_interval"
               frame-timings-refresh-interval) :int64
 #+liber-documentation
 "@version{#2023-3-10}
  @argument[timings]{a @class{gdk:frame-timings} instance}
  @begin{return}
    An integer with the refresh interval of the display, in microseconds, or 0
    if the refresh interval is not available. See the
    @fun{gdk:frame-timings-complete} function.
  @end{return}
  @begin{short}
    Gets the natural interval between presentation times for the display that
    this frame was displayed on.
  @end{short}
  Frame presentation usually happens during the \"vertical blanking interval\".
  @see-class{gdk:frame-timings}
  @see-function{gdk:frame-timings-complete}"
  (timings (g:boxed frame-timings)))

(export 'frame-timings-refresh-interval)

;;; ----------------------------------------------------------------------------
;;; gdk_frame_timings_get_predicted_presentation_time ()
;;;     -> frame-timings-predicted-presentation-time
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_frame_timings_get_predicted_presentation_time"
               frame-timings-predicted-presentation-time) :int64
 #+liber-documentation
 "@version{#2023-3-10}
  @argument[timings]{a @class{gdk:frame-timings} instance}
  @begin{return}
    An integer with the predicted time at which the frame will be presented,
    or 0 if no predicted presentation time is available.
  @end{return}
  @begin{short}
    Gets the predicted time at which this frame will be displayed.
  @end{short}
  Although no predicted time may be available, if one is available, it will be
  available while the frame is being generated, in contrast to the
  @fun{gdk:frame-timings-presentation-time} function, which is only available
  after the frame has been presented. In general, if you are simply animating,
  you should use the @fun{gdk:frame-clock-frame-time} function rather than this
  function, but this function is useful for applications that want exact control
  over latency. For example, a movie player may want this information for
  Audio/Video synchronization.
  @see-class{gdk:frame-timings}
  @see-function{gdk:frame-clock-frame-time}
  @see-function{gdk:frame-timings-presentation-time}"
  (timings (g:boxed frame-timings)))

(export 'frame-timings-predicted-presentation-time)

;;; --- End of file gdk4.frame-timings.lisp ------------------------------------

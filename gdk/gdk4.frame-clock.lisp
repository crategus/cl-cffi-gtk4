;;; ----------------------------------------------------------------------------
;;; gdk.frame-clock.lisp
;;;
;;; The documentation of this file is taken from the GDK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 Dieter Kaiser
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

;;;GdkFrameClock
;;;GdkFrameClock — Synchronizes painting to a surface

;;;Functions
;;;gint64	gdk_frame_clock_get_frame_time ()
;;;void	gdk_frame_clock_request_phase ()
;;;void	gdk_frame_clock_begin_updating ()
;;;void	gdk_frame_clock_end_updating ()
;;;gint64	gdk_frame_clock_get_frame_counter ()
;;;gint64	gdk_frame_clock_get_history_start ()
;;;GdkFrameTimings *	gdk_frame_clock_get_timings ()
;;;GdkFrameTimings *	gdk_frame_clock_get_current_timings ()
;;;void	gdk_frame_clock_get_refresh_info ()
;;;double	gdk_frame_clock_get_fps ()
;;;Signals
;;;void	after-paint	Run Last
;;;void	before-paint	Run Last
;;;void	flush-events	Run Last
;;;void	layout	Run Last
;;;void	paint	Run Last
;;;void	resume-events	Run Last
;;;void	update	Run Last
;;;Types and Values
;;; 	GdkFrameClock
;;;enum	GdkFrameClockPhase
;;;Object Hierarchy
;;;    GObject
;;;    ╰── GdkFrameClock

(in-package :gdk)


;;;Description
;;;A GdkFrameClock tells the application when to update and repaint a surface. This may be synced to the vertical refresh rate of the monitor, for example. Even when the frame clock uses a simple timer rather than a hardware-based vertical sync, the frame clock helps because it ensures everything paints at the same time (reducing the total number of frames). The frame clock can also automatically stop painting when it knows the frames will not be visible, or scale back animation framerates.

;;;GdkFrameClock is designed to be compatible with an OpenGL-based implementation or with mozRequestAnimationFrame in Firefox, for example.

;;;A frame clock is idle until someone requests a frame with gdk_frame_clock_request_phase(). At some later point that makes sense for the synchronization being implemented, the clock will process a frame and emit signals for each phase that has been requested. (See the signals of the GdkFrameClock class for documentation of the phases. GDK_FRAME_CLOCK_PHASE_UPDATE and the “update” signal are most interesting for application writers, and are used to update the animations, using the frame time given by gdk_frame_clock_get_frame_time().

;;;The frame time is reported in microseconds and generally in the same timescale as g_get_monotonic_time(), however, it is not the same as g_get_monotonic_time(). The frame time does not advance during the time a frame is being painted, and outside of a frame, an attempt is made so that all calls to gdk_frame_clock_get_frame_time() that are called at a “similar” time get the same value. This means that if different animations are timed by looking at the difference in time between an initial value from gdk_frame_clock_get_frame_time() and the value inside the “update” signal of the clock, they will stay exactly synchronized.

;;;Functions
;;;gdk_frame_clock_get_frame_time ()
;;;gint64
;;;gdk_frame_clock_get_frame_time (GdkFrameClock *frame_clock);
;;;Gets the time that should currently be used for animations. Inside the processing of a frame, it’s the time used to compute the animation position of everything in a frame. Outside of a frame, it's the time of the conceptual “previous frame,” which may be either the actual previous frame time, or if that’s too old, an updated time.

;;;Parameters
;;;frame_clock

;;;a GdkFrameClock

;;;
;;;Returns
;;;a timestamp in microseconds, in the timescale of of g_get_monotonic_time().

;;;gdk_frame_clock_request_phase ()
;;;void
;;;gdk_frame_clock_request_phase (GdkFrameClock *frame_clock,
;;;                               GdkFrameClockPhase phase);
;;;Asks the frame clock to run a particular phase. The signal corresponding the requested phase will be emitted the next time the frame clock processes. Multiple calls to gdk_frame_clock_request_phase() will be combined together and only one frame processed. If you are displaying animated content and want to continually request the GDK_FRAME_CLOCK_PHASE_UPDATE phase for a period of time, you should use gdk_frame_clock_begin_updating() instead, since this allows GTK to adjust system parameters to get maximally smooth animations.

;;;Parameters
;;;frame_clock

;;;a GdkFrameClock

;;;
;;;phase

;;;the phase that is requested

;;;
;;;gdk_frame_clock_begin_updating ()
;;;void
;;;gdk_frame_clock_begin_updating (GdkFrameClock *frame_clock);
;;;Starts updates for an animation. Until a matching call to gdk_frame_clock_end_updating() is made, the frame clock will continually request a new frame with the GDK_FRAME_CLOCK_PHASE_UPDATE phase. This function may be called multiple times and frames will be requested until gdk_frame_clock_end_updating() is called the same number of times.

;;;Parameters
;;;frame_clock

;;;a GdkFrameClock

;;;
;;;gdk_frame_clock_end_updating ()
;;;void
;;;gdk_frame_clock_end_updating (GdkFrameClock *frame_clock);
;;;Stops updates for an animation. See the documentation for gdk_frame_clock_begin_updating().

;;;Parameters
;;;frame_clock

;;;a GdkFrameClock

;;;
;;;gdk_frame_clock_get_frame_counter ()
;;;gint64
;;;gdk_frame_clock_get_frame_counter (GdkFrameClock *frame_clock);
;;;A GdkFrameClock maintains a 64-bit counter that increments for each frame drawn.

;;;Parameters
;;;frame_clock

;;;a GdkFrameClock

;;;
;;;Returns
;;;inside frame processing, the value of the frame counter for the current frame. Outside of frame processing, the frame counter for the last frame.

;;;gdk_frame_clock_get_history_start ()
;;;gint64
;;;gdk_frame_clock_get_history_start (GdkFrameClock *frame_clock);
;;;GdkFrameClock internally keeps a history of GdkFrameTimings objects for recent frames that can be retrieved with gdk_frame_clock_get_timings(). The set of stored frames is the set from the counter values given by gdk_frame_clock_get_history_start() and gdk_frame_clock_get_frame_counter(), inclusive.

;;;Parameters
;;;frame_clock

;;;a GdkFrameClock

;;;
;;;Returns
;;;the frame counter value for the oldest frame that is available in the internal frame history of the GdkFrameClock.

;;;gdk_frame_clock_get_timings ()
;;;GdkFrameTimings *
;;;gdk_frame_clock_get_timings (GdkFrameClock *frame_clock,
;;;                             gint64 frame_counter);
;;;Retrieves a GdkFrameTimings object holding timing information for the current frame or a recent frame. The GdkFrameTimings object may not yet be complete: see gdk_frame_timings_get_complete().

;;;Parameters
;;;frame_clock

;;;a GdkFrameClock

;;;
;;;frame_counter

;;;the frame counter value identifying the frame to be received.

;;;
;;;Returns
;;;the GdkFrameTimings object for the specified frame, or NULL if it is not available. See gdk_frame_clock_get_history_start().

;;;[nullable][transfer none]

;;;gdk_frame_clock_get_current_timings ()
;;;GdkFrameTimings *
;;;gdk_frame_clock_get_current_timings (GdkFrameClock *frame_clock);
;;;Gets the frame timings for the current frame.

;;;Parameters
;;;frame_clock

;;;a GdkFrameClock

;;;
;;;Returns
;;;the GdkFrameTimings for the frame currently being processed, or even no frame is being processed, for the previous frame. Before any frames have been processed, returns NULL.

;;;[nullable][transfer none]

;;;gdk_frame_clock_get_refresh_info ()
;;;void
;;;gdk_frame_clock_get_refresh_info (GdkFrameClock *frame_clock,
;;;                                  gint64 base_time,
;;;                                  gint64 *refresh_interval_return,
;;;                                  gint64 *presentation_time_return);
;;;Using the frame history stored in the frame clock, finds the last known presentation time and refresh interval, and assuming that presentation times are separated by the refresh interval, predicts a presentation time that is a multiple of the refresh interval after the last presentation time, and later than base_time .

;;;Parameters
;;;frame_clock

;;;a GdkFrameClock

;;;
;;;base_time

;;;base time for determining a presentaton time

;;;
;;;refresh_interval_return

;;;a location to store the determined refresh interval, or NULL. A default refresh interval of 1/60th of a second will be stored if no history is present.

;;;[out][optional]
;;;presentation_time_return

;;;a location to store the next candidate presentation time after the given base time. 0 will be will be stored if no history is present.

;;;[out]
;;;gdk_frame_clock_get_fps ()
;;;double
;;;gdk_frame_clock_get_fps (GdkFrameClock *frame_clock);
;;;Calculates the current frames-per-second, based on the frame timings of frame_clock .

;;;Parameters
;;;frame_clock

;;;a GdkFrameClock

;;;
;;;Returns
;;;the current fps, as a double

;;;Types and Values
;;;GdkFrameClock
;;;typedef struct _GdkFrameClock GdkFrameClock;
;;;The GdkFrameClock struct contains only private fields and should not be accessed directly.

;;;enum GdkFrameClockPhase
;;;GdkFrameClockPhase is used to represent the different paint clock phases that can be requested. The elements of the enumeration correspond to the signals of GdkFrameClock.

;;;Members
;;;GDK_FRAME_CLOCK_PHASE_NONE

;;;no phase

;;;
;;;GDK_FRAME_CLOCK_PHASE_FLUSH_EVENTS

;;;corresponds to GdkFrameClock::flush-events. Should not be handled by applications.

;;;
;;;GDK_FRAME_CLOCK_PHASE_BEFORE_PAINT

;;;corresponds to GdkFrameClock::before-paint. Should not be handled by applications.

;;;
;;;GDK_FRAME_CLOCK_PHASE_UPDATE

;;;corresponds to GdkFrameClock::update.

;;;
;;;GDK_FRAME_CLOCK_PHASE_LAYOUT

;;;corresponds to GdkFrameClock::layout. Should not be handled by applicatiosn.

;;;
;;;GDK_FRAME_CLOCK_PHASE_PAINT

;;;corresponds to GdkFrameClock::paint.

;;;
;;;GDK_FRAME_CLOCK_PHASE_RESUME_EVENTS

;;;corresponds to GdkFrameClock::resume-events. Should not be handled by applications.

;;;
;;;GDK_FRAME_CLOCK_PHASE_AFTER_PAINT

;;;corresponds to GdkFrameClock::after-paint. Should not be handled by applications.

;;;
;;;Signal Details
;;;The “after-paint” signal
;;;void
;;;user_function (GdkFrameClock *clock,
;;;               gpointer       user_data)
;;;This signal ends processing of the frame. Applications should generally not handle this signal.

;;;Parameters
;;;clock

;;;the frame clock emitting the signal

;;;
;;;user_data

;;;user data set when the signal handler was connected.

;;;
;;;Flags: Run Last

;;;The “before-paint” signal
;;;void
;;;user_function (GdkFrameClock *clock,
;;;               gpointer       user_data)
;;;This signal begins processing of the frame. Applications should generally not handle this signal.

;;;Parameters
;;;clock

;;;the frame clock emitting the signal

;;;
;;;user_data

;;;user data set when the signal handler was connected.

;;;
;;;Flags: Run Last

;;;The “flush-events” signal
;;;void
;;;user_function (GdkFrameClock *clock,
;;;               gpointer       user_data)
;;;This signal is used to flush pending motion events that are being batched up and compressed together. Applications should not handle this signal.

;;;Parameters
;;;clock

;;;the frame clock emitting the signal

;;;
;;;user_data

;;;user data set when the signal handler was connected.

;;;
;;;Flags: Run Last

;;;The “layout” signal
;;;void
;;;user_function (GdkFrameClock *clock,
;;;               gpointer       user_data)
;;;This signal is emitted as the second step of toolkit and application processing of the frame. Any work to update sizes and positions of application elements should be performed. GTK normally handles this internally.

;;;Parameters
;;;clock

;;;the frame clock emitting the signal

;;;
;;;user_data

;;;user data set when the signal handler was connected.

;;;
;;;Flags: Run Last

;;;The “paint” signal
;;;void
;;;user_function (GdkFrameClock *clock,
;;;               gpointer       user_data)
;;;This signal is emitted as the third step of toolkit and application processing of the frame. The frame is repainted. GDK normally handles this internally and emits “render” which are turned into “snapshot” signals by GTK.

;;;Parameters
;;;clock

;;;the frame clock emitting the signal

;;;
;;;user_data

;;;user data set when the signal handler was connected.

;;;
;;;Flags: Run Last

;;;The “resume-events” signal
;;;void
;;;user_function (GdkFrameClock *clock,
;;;               gpointer       user_data)
;;;This signal is emitted after processing of the frame is finished, and is handled internally by GTK to resume normal event processing. Applications should not handle this signal.

;;;Parameters
;;;clock

;;;the frame clock emitting the signal

;;;
;;;user_data

;;;user data set when the signal handler was connected.

;;;
;;;Flags: Run Last

;;;The “update” signal
;;;void
;;;user_function (GdkFrameClock *clock,
;;;               gpointer       user_data)
;;;This signal is emitted as the first step of toolkit and application processing of the frame. Animations should be updated using gdk_frame_clock_get_frame_time(). Applications can connect directly to this signal, or use gtk_widget_add_tick_callback() as a more convenient interface.

;;;Parameters
;;;clock

;;;the frame clock emitting the signal

;;;
;;;user_data

;;;user data set when the signal handler was connected.

;;;
;;;Flags: Run Last

;;; --- End of file gdk.frame-clock.lisp ---------------------------------------

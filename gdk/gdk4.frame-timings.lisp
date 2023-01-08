;;; ----------------------------------------------------------------------------
;;; gdk.frame-timings.lisp
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

;;;Frame timings
;;;Frame timings — Object holding timing information for a single frame

;;;Functions
;;;GdkFrameTimings *	gdk_frame_timings_ref ()
;;;void	gdk_frame_timings_unref ()
;;;gint64	gdk_frame_timings_get_frame_counter ()
;;;gboolean	gdk_frame_timings_get_complete ()
;;;gint64	gdk_frame_timings_get_frame_time ()
;;;gint64	gdk_frame_timings_get_presentation_time ()
;;;gint64	gdk_frame_timings_get_refresh_interval ()
;;;gint64	gdk_frame_timings_get_predicted_presentation_time ()
;;;Types and Values
;;; 	GdkFrameTimings

(in-package :gdk)


;;;Description
;;;A GdkFrameTimings object holds timing information for a single frame of the application’s displays. To retrieve GdkFrameTimings objects, use gdk_frame_clock_get_timings() or gdk_frame_clock_get_current_timings(). The information in GdkFrameTimings is useful for precise synchronization of video with the event or audio streams, and for measuring quality metrics for the application’s display, such as latency and jitter.

;;;Functions
;;;gdk_frame_timings_ref ()
;;;GdkFrameTimings *
;;;gdk_frame_timings_ref (GdkFrameTimings *timings);
;;;Increases the reference count of timings .

;;;Parameters
;;;timings

;;;a GdkFrameTimings

;;;
;;;Returns
;;;timings

;;;gdk_frame_timings_unref ()
;;;void
;;;gdk_frame_timings_unref (GdkFrameTimings *timings);
;;;Decreases the reference count of timings . If timings is no longer referenced, it will be freed.

;;;Parameters
;;;timings

;;;a GdkFrameTimings

;;;
;;;gdk_frame_timings_get_frame_counter ()
;;;gint64
;;;gdk_frame_timings_get_frame_counter (GdkFrameTimings *timings);
;;;Gets the frame counter value of the GdkFrameClock when this this frame was drawn.

;;;Parameters
;;;timings

;;;a GdkFrameTimings

;;;
;;;Returns
;;;the frame counter value for this frame

;;;gdk_frame_timings_get_complete ()
;;;gboolean
;;;gdk_frame_timings_get_complete (GdkFrameTimings *timings);
;;;The timing information in a GdkFrameTimings is filled in incrementally as the frame as drawn and passed off to the window system for processing and display to the user. The accessor functions for GdkFrameTimings can return 0 to indicate an unavailable value for two reasons: either because the information is not yet available, or because it isn't available at all. Once gdk_frame_timings_get_complete() returns TRUE for a frame, you can be certain that no further values will become available and be stored in the GdkFrameTimings.

;;;Parameters
;;;timings

;;;a GdkFrameTimings

;;;
;;;Returns
;;;TRUE if all information that will be available for the frame has been filled in.

;;;gdk_frame_timings_get_frame_time ()
;;;gint64
;;;gdk_frame_timings_get_frame_time (GdkFrameTimings *timings);
;;;Returns the frame time for the frame. This is the time value that is typically used to time animations for the frame. See gdk_frame_clock_get_frame_time().

;;;Parameters
;;;timings

;;;A GdkFrameTimings

;;;
;;;Returns
;;;the frame time for the frame, in the timescale of g_get_monotonic_time()

;;;gdk_frame_timings_get_presentation_time ()
;;;gint64
;;;gdk_frame_timings_get_presentation_time
;;;                               (GdkFrameTimings *timings);
;;;Reurns the presentation time. This is the time at which the frame became visible to the user.

;;;Parameters
;;;timings

;;;a GdkFrameTimings

;;;
;;;Returns
;;;the time the frame was displayed to the user, in the timescale of g_get_monotonic_time(), or 0 if no presentation time is available. See gdk_frame_timings_get_complete()

;;;gdk_frame_timings_get_refresh_interval ()
;;;gint64
;;;gdk_frame_timings_get_refresh_interval
;;;                               (GdkFrameTimings *timings);
;;;Gets the natural interval between presentation times for the display that this frame was displayed on. Frame presentation usually happens during the “vertical blanking interval”.

;;;Parameters
;;;timings

;;;a GdkFrameTimings

;;;
;;;Returns
;;;the refresh interval of the display, in microseconds, or 0 if the refresh interval is not available. See gdk_frame_timings_get_complete().

;;;gdk_frame_timings_get_predicted_presentation_time ()
;;;gint64
;;;gdk_frame_timings_get_predicted_presentation_time
;;;                               (GdkFrameTimings *timings);
;;;Gets the predicted time at which this frame will be displayed. Although no predicted time may be available, if one is available, it will be available while the frame is being generated, in contrast to gdk_frame_timings_get_presentation_time(), which is only available after the frame has been presented. In general, if you are simply animating, you should use gdk_frame_clock_get_frame_time() rather than this function, but this function is useful for applications that want exact control over latency. For example, a movie player may want this information for Audio/Video synchronization.

;;;Parameters
;;;timings

;;;a GdkFrameTimings

;;;
;;;Returns
;;;The predicted time at which the frame will be presented, in the timescale of g_get_monotonic_time(), or 0 if no predicted presentation time is available.

;;;Types and Values
;;;GdkFrameTimings
;;;typedef struct _GdkFrameTimings GdkFrameTimings;
;;;The GdkFrameTimings struct contains only private fields and should not be accessed directly.

;;; --- End of file gdk.frame-timings.lisp -------------------------------------

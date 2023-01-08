;;; ----------------------------------------------------------------------------
;;; gtk.media-stream.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GTK library.
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
;;;
;;; GtkMediaStream
;;;
;;;     Display media in GTK
;;;
;;; Types and Values
;;;
;;;     GtkMediaStream
;;;
;;; Accessors
;;;
;;;     gtk_media_stream_get_duration
;;;     gtk_media_stream_get_ended
;;;     gtk_media_stream_get_error
;;;     gtk_media_stream_has_audio
;;;     gtk_media_stream_has_video
;;;     gtk_media_stream_get_loop
;;;     gtk_media_stream_set_loop
;;;     gtk_media_stream_get_muted
;;;     gtk_media_stream_set_muted
;;;     gtk_media_stream_get_playing
;;;     gtk_media_stream_set_playing
;;;     gtk_media_stream_get_timestamp
;;;     gtk_media_stream_get_volume
;;;     gtk_media_stream_set_volume
;;;
;;; Functions
;;;
;;;     gtk_media_stream_is_prepared
;;;     gtk_media_stream_play
;;;     gtk_media_stream_pause
;;;     gtk_media_stream_is_seekable
;;;     gtk_media_stream_is_seeking
;;;     gtk_media_stream_seek
;;;     gtk_media_stream_realize
;;;     gtk_media_stream_unrealize
;;;     gtk_media_stream_prepared
;;;     gtk_media_stream_unprepared
;;;     gtk_media_stream_update
;;;     gtk_media_stream_ended
;;;     gtk_media_stream_seek_success
;;;     gtk_media_stream_seek_failed
;;;     gtk_media_stream_gerror
;;;     gtk_media_stream_error
;;;     gtk_media_stream_error_valist
;;;
;;; Properties
;;;
;;;     duration
;;;     ended
;;;     error
;;;     has-audio
;;;     has-video
;;;     loop
;;;     muted
;;;     playing
;;;     prepared
;;;     seekable
;;;     seeking
;;;     timestamp
;;;     volume
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkMediaStream
;;;         ╰── GtkMediaFile
;;;
;;; Implemented Interfaces
;;;
;;;     GdkPaintable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkMediaStream
;;;
;;; GtkMediaStream is the integration point for media playback inside GTK.
;;;
;;; Apart from application-facing API for stream playback, GtkMediaStream has a
;;; number of APIs that are only useful for implementations and should not be
;;; used in applications: gtk_media_stream_prepared(),
;;; gtk_media_stream_unprepared(), gtk_media_stream_update(),
;;; gtk_media_stream_ended(), gtk_media_stream_seek_success(),
;;; gtk_media_stream_seek_failed(), gtk_media_stream_gerror(),
;;; gtk_media_stream_error(), gtk_media_stream_error_valist().
;;;
;;; See Also
;;;
;;;     GdkPaintable
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkMediaStream" media-stream
  (:superclass g:object
   :export t
   :interfaces ("GdkPaintable")
   :type-initializer "gtk_media_stream_get_type")
  ((duration
    media-stream-duration
    "duration" "gint64" t nil)
   (ended
    media-stream-ended
    "ended" "gboolean" t nil)
   (error
    media-stream-error
    "error" "GError" t nil)
   (has-audio
    media-stream-has-audio
    "has-audio" "gboolean" t nil)
   (has-video
    media-stream-has-video
    "has-video" "gboolean" t nil)
   (loop
    media-stream-loop
    "loop" "gboolean" t t)
   (muted
    media-stream-muted
    "muted" "gboolean" t t)
   (playing
    media-stream-playing
    "playing" "gboolean" t t)
   (prepared
    media-stream-prepared
    "prepared" "gboolean" t t)
   (seekable
    media-stream-seekable
    "seekable" "gboolean" t nil)
   (seeking
    media-stream-seeking
    "seeking" "gboolean" t nil)
   (timestamp
    media-stream-timestamp
    "timestamp" "gint64" t nil)
   (volume
    media-stream-volume
    "volume" "gdouble" t t)))

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;The “duration” property
;;;  “duration”                 gint64
;;;The stream's duration in microseconds or 0 if unknown.

;;;Owner: GtkMediaStream

;;;Flags: Read

;;;Allowed values: >= 0

;;;Default value: 0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;The “ended” property
;;;  “ended”                    gboolean
;;;Set when playback has finished.

;;;Owner: GtkMediaStream

;;;Flags: Read

;;;Default value: FALSE

;;;The “error” property
;;;  “error”                    GError *
;;;NULL for a properly working stream or the GError that the stream is in.

;;;Owner: GtkMediaStream

;;;Flags: Read
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;The “has-audio” property
;;;  “has-audio”                gboolean
;;;Whether the stream contains audio

;;;Owner: GtkMediaStream

;;;Flags: Read

;;;Default value: FALSE
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;The “has-video” property
;;;  “has-video”                gboolean
;;;Whether the stream contains video

;;;Owner: GtkMediaStream

;;;Flags: Read

;;;Default value: FALSE
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;The “loop” property
;;;  “loop”                     gboolean
;;;Try to restart the media from the beginning once it ended.

;;;Owner: GtkMediaStream

;;;Flags: Read / Write

;;;Default value: FALSE
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;The “muted” property
;;;  “muted”                    gboolean
;;;Whether the audio stream should be muted.

;;;Owner: GtkMediaStream

;;;Flags: Read / Write

;;;Default value: FALSE
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;The “playing” property
;;;  “playing”                  gboolean
;;;Whether the stream is currently playing.

;;;Owner: GtkMediaStream

;;;Flags: Read / Write

;;;Default value: FALSE
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;The “prepared” property
;;;  “prepared”                 gboolean
;;;Whether the stream has finished initializing and existence of audio and video is known.

;;;Owner: GtkMediaStream

;;;Flags: Read / Write

;;;Default value: FALSE
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;The “seekable” property
;;;  “seekable”                 gboolean
;;;Set unless the stream is known to not support seeking.

;;;Owner: GtkMediaStream

;;;Flags: Read

;;;Default value: TRUE
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;The “seeking” property
;;;  “seeking”                  gboolean
;;;Set while a seek is in progress.

;;;Owner: GtkMediaStream

;;;Flags: Read

;;;Default value: FALSE
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;The “timestamp” property
;;;  “timestamp”                gint64
;;;The current presentation timestamp in microseconds.

;;;Owner: GtkMediaStream

;;;Flags: Read

;;;Allowed values: >= 0

;;;Default value: 0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;The “volume” property
;;;  “volume”                   double
;;;Volume of the audio stream.

;;;Owner: GtkMediaStream

;;;Flags: Read / Write

;;;Allowed values: [0,1]

;;;Default value: 1
;;; ----------------------------------------------------------------------------





;;;Functions
;;;gtk_media_stream_is_prepared ()
;;;gboolean
;;;gtk_media_stream_is_prepared (GtkMediaStream *self);
;;;Returns whether the stream has finished initializing and existence of audio and video is known.

;;;Parameters
;;;self

;;;a GtkMediaStream

;;;Returns
;;;TRUE if the stream is prepared

;;;gtk_media_stream_get_error ()
;;;const GError *
;;;gtk_media_stream_get_error (GtkMediaStream *self);
;;;If the stream is in an error state, returns the GError explaining that state. Any type of error can be reported here depending on the implementation of the media stream.

;;;A media stream in an error cannot be operated on, calls like gtk_media_stream_play() or gtk_media_stream_seek() will not have any effect.

;;;GtkMediaStream itself does not provide a way to unset an error, but implementations may provide options. For example, a GtkMediaFile will unset errors when a new source is set with ie gtk_media_file_set_file().

;;;Parameters
;;;self

;;;a GtkMediaStream

;;;Returns
;;;NULL if not in an error state or the GError of the stream.

;;;[nullable][transfer none]

;;;gtk_media_stream_has_audio ()
;;;gboolean
;;;gtk_media_stream_has_audio (GtkMediaStream *self);
;;;Returns whether the stream has audio.

;;;Parameters
;;;self

;;;a GtkMediaStream

;;;Returns
;;;TRUE if the stream has audio

;;;gtk_media_stream_has_video ()
;;;gboolean
;;;gtk_media_stream_has_video (GtkMediaStream *self);
;;;Returns whether the stream has video.

;;;Parameters
;;;self

;;;a GtkMediaStream

;;;Returns
;;;TRUE if the stream has video

;;;gtk_media_stream_play ()
;;;void
;;;gtk_media_stream_play (GtkMediaStream *self);
;;;Starts playing the stream. If the stream is in error or already playing, do nothing.

;;;Parameters
;;;self

;;;a GtkMediaStream

;;;gtk_media_stream_pause ()
;;;void
;;;gtk_media_stream_pause (GtkMediaStream *self);
;;;Pauses playback of the stream. If the stream is not playing, do nothing.

;;;Parameters
;;;self

;;;a GtkMediaStream

;;;gtk_media_stream_get_playing ()
;;;gboolean
;;;gtk_media_stream_get_playing (GtkMediaStream *self);
;;;Return whether the stream is currently playing.

;;;Parameters
;;;self

;;;a GtkMediaStream

;;;Returns
;;;TRUE if the stream is playing

;;;gtk_media_stream_set_playing ()
;;;void
;;;gtk_media_stream_set_playing (GtkMediaStream *self,
;;;                              gboolean playing);
;;;Starts or pauses playback of the stream.

;;;Parameters
;;;self

;;;a GtkMediaStream

;;;playing

;;;whether to start or pause playback

;;;gtk_media_stream_get_ended ()
;;;gboolean
;;;gtk_media_stream_get_ended (GtkMediaStream *self);
;;;Returns whether the streams playback is finished.

;;;Parameters
;;;self

;;;a GtkMediaStream

;;;Returns
;;;TRUE if playback is finished

;;;gtk_media_stream_get_timestamp ()
;;;gint64
;;;gtk_media_stream_get_timestamp (GtkMediaStream *self);
;;;Returns the current presentation timestamp in microseconds.

;;;Parameters
;;;self

;;;a GtkMediaStream

;;;Returns
;;;the timestamp in microseconds

;;;gtk_media_stream_get_duration ()
;;;gint64
;;;gtk_media_stream_get_duration (GtkMediaStream *self);
;;;Gets the duration of the stream. If the duration is not known, 0 will be returned.

;;;Parameters
;;;self

;;;a GtkMediaStream

;;;Returns
;;;the duration of the stream or 0 if not known.

;;;gtk_media_stream_is_seekable ()
;;;gboolean
;;;gtk_media_stream_is_seekable (GtkMediaStream *self);
;;;Checks if a stream may be seekable.

;;;This is meant to be a hint. Streams may not allow seeking even if this function returns TRUE. However, if this function returns FALSE, streams are guaranteed to not be seekable and user interfaces may hide controls that allow seeking.

;;;It is allowed to call gtk_media_stream_seek() on a non-seekable stream, though it will not do anything.

;;;Parameters
;;;self

;;;a GtkMediaStream

;;;Returns
;;;TRUE if the stream may support seeking

;;;gtk_media_stream_is_seeking ()
;;;gboolean
;;;gtk_media_stream_is_seeking (GtkMediaStream *self);
;;;Checks if there is currently a seek operation going on.

;;;Parameters
;;;self

;;;a GtkMediaStream

;;;Returns
;;;TRUE if a seek operation is ongoing.

;;;gtk_media_stream_seek ()
;;;void
;;;gtk_media_stream_seek (GtkMediaStream *self,
;;;                       gint64 timestamp);
;;;Start a seek operation on self to timestamp . If timestamp is out of range, it will be clamped.

;;;Seek operations may not finish instantly. While a seek operation is in process, the GtkMediaStream:seeking property will be set.

;;;When calling gtk_media_stream_seek() during an ongoing seek operation, the new seek will override any pending seek.

;;;Parameters
;;;self

;;;a GtkMediaStream

;;;timestamp

;;;timestamp to seek to.

;;;gtk_media_stream_get_loop ()
;;;gboolean
;;;gtk_media_stream_get_loop (GtkMediaStream *self);
;;;Returns whether the stream is set to loop. See gtk_media_stream_set_loop() for details.

;;;Parameters
;;;self

;;;a GtkMediaStream

;;;Returns
;;;TRUE if the stream should loop

;;;gtk_media_stream_set_loop ()
;;;void
;;;gtk_media_stream_set_loop (GtkMediaStream *self,
;;;                           gboolean loop);
;;;Sets whether the stream should loop, ie restart playback from the beginning instead of stopping at the end.

;;;Not all streams may support looping, in particular non-seekable streams. Those streams will ignore the loop setting and just end.

;;;Parameters
;;;self

;;;a GtkMediaStream

;;;loop

;;;TRUE if the stream should loop

;;;gtk_media_stream_get_muted ()
;;;gboolean
;;;gtk_media_stream_get_muted (GtkMediaStream *self);
;;;Returns whether the audio for the stream is muted. See gtk_media_stream_set_muted() for details.

;;;Parameters
;;;self

;;;a GtkMediaStream

;;;Returns
;;;TRUE if the stream is muted

;;;gtk_media_stream_set_muted ()
;;;void
;;;gtk_media_stream_set_muted (GtkMediaStream *self,
;;;                            gboolean muted);
;;;Sets whether the audio stream should be muted. Muting a stream will cause no audio to be played, but it does not modify the volume. This means that muting and then unmuting the stream will restore the volume settings.

;;;If the stream has no audio, calling this function will still work but it will not have an audible effect.

;;;Parameters
;;;self

;;;a GtkMediaStream

;;;muted

;;;TRUE if the stream should be muted

;;;gtk_media_stream_get_volume ()
;;;double
;;;gtk_media_stream_get_volume (GtkMediaStream *self);
;;;Returns the volume of the audio for the stream. See gtk_media_stream_set_volume() for details.

;;;Parameters
;;;self

;;;a GtkMediaStream

;;;Returns
;;;volume of the stream from 0.0 to 1.0

;;;gtk_media_stream_set_volume ()
;;;void
;;;gtk_media_stream_set_volume (GtkMediaStream *self,
;;;                             double volume);
;;;Sets the volume of the audio stream. This function call will work even if the stream is muted.

;;;The given volume should range from 0.0 for silence to 1.0 for as loud as possible. Values outside of this range will be clamped to the nearest value.

;;;If the stream has no audio or is muted, calling this function will still work but it will not have an immediate audible effect. When the stream is unmuted, the new volume setting will take effect.

;;;Parameters
;;;self

;;;a GtkMediaStream

;;;volume

;;;New volume of the stream from 0.0 to 1.0

;;;gtk_media_stream_realize ()
;;;void
;;;gtk_media_stream_realize (GtkMediaStream *self,
;;;                          GdkSurface *surface);
;;;Called by users to attach the media stream to a GdkSurface they manage. The stream can then access the resources of surface for its rendering purposes. In particular, media streams might want to create GdkGLContexts or sync to the GdkFrameClock.

;;;Whoever calls this function is responsible for calling gtk_media_stream_unrealize() before either the stream or surface get destroyed.

;;;Multiple calls to this function may happen from different users of the video, even with the same surface . Each of these calls must be followed by its own call to gtk_media_stream_unrealize().

;;;It is not required to call this function to make a media stream work.

;;;Parameters
;;;self

;;;a GtkMediaStream

;;;surface

;;;a GdkSurface

;;;gtk_media_stream_unrealize ()
;;;void
;;;gtk_media_stream_unrealize (GtkMediaStream *self,
;;;                            GdkSurface *surface);
;;;Undoes a previous call to gtk_media_stream_realize() and causes the stream to release all resources it had allocated from surface .

;;;Parameters
;;;self

;;;a GtkMediaStream previously realized

;;;surface

;;;the GdkSurface the stream was realized with

;;;gtk_media_stream_prepared ()
;;;void
;;;gtk_media_stream_prepared (GtkMediaStream *self,
;;;                           gboolean has_audio,
;;;                           gboolean has_video,
;;;                           gboolean seekable,
;;;                           gint64 duration);
;;;Called by GtkMediaStream implementations to advertise the stream being ready to play and providing details about the stream.

;;;Note that the arguments are hints. If the stream implementation cannot determine the correct values, it is better to err on the side of caution and return TRUE. User interfaces will use those values to determine what controls to show.

;;;This function may not be called again until the stream has been reset via gtk_media_stream_unprepared().

;;;Parameters
;;;self

;;;a GtkMediaStream

;;;has_audio

;;;TRUE if the stream should advertise audio support

;;;has_video

;;;TRUE if the stream should advertise video support

;;;seekable

;;;TRUE if the stream should advertise seekability

;;;duration

;;;The duration of the stream or 0 if unknown

;;;gtk_media_stream_unprepared ()
;;;void
;;;gtk_media_stream_unprepared (GtkMediaStream *self);
;;;Resets a given media stream implementation. gtk_media_stream_prepared() can now be called again.

;;;This function will also reset any error state the stream was in.

;;;Parameters
;;;self

;;;a GtkMediaStream

;;;gtk_media_stream_update ()
;;;void
;;;gtk_media_stream_update (GtkMediaStream *self,
;;;                         gint64 timestamp);
;;;Media stream implementations should regularly call this function to update the timestamp reported by the stream. It is up to implementations to call this at the frequency they deem appropriate.

;;;Parameters
;;;self

;;;a GtkMediaStream

;;;timestamp

;;;the new timestamp

;;;gtk_media_stream_ended ()
;;;void
;;;gtk_media_stream_ended (GtkMediaStream *self);
;;;Pauses the media stream and marks it as ended. This is a hint only, calls to GtkMediaStream.play() may still happen.

;;;Parameters
;;;self

;;;a GtkMediaStream

;;;gtk_media_stream_seek_success ()
;;;void
;;;gtk_media_stream_seek_success (GtkMediaStream *self);
;;;Ends a seek operation started via GtkMediaStream.seek() successfully. This function will unset the GtkMediaStream:ended property if it was set.

;;;See gtk_media_stream_seek_failed() for the other way of ending a seek.

;;;Parameters
;;;self

;;;a GtkMediaStream

;;;gtk_media_stream_seek_failed ()
;;;void
;;;gtk_media_stream_seek_failed (GtkMediaStream *self);
;;;Ends a seek operation started via GtkMediaStream.seek() as a failure. This will not cause an error on the stream and will assume that playback continues as if no seek had happened.

;;;See gtk_media_stream_seek_success() for the other way of ending a seek.

;;;Parameters
;;;self

;;;a GtkMediaStream

;;;gtk_media_stream_gerror ()
;;;void
;;;gtk_media_stream_gerror (GtkMediaStream *self,
;;;                         GError *error);
;;;Sets self into an error state. This will pause the stream (you can check for an error via gtk_media_stream_get_error() in your GtkMediaStream.pause() implementation), abort pending seeks and mark the stream as prepared.

;;;if the stream is already in an error state, this call will be ignored and the existing error will be retained. FIXME: Or do we want to set the new error?

;;;To unset an error, the stream must be reset via a call to gtk_media_stream_unprepared().

;;;Parameters
;;;self

;;;a GtkMediaStream

;;;error

;;;the GError to set.

;;;[transfer full]
;;;gtk_media_stream_error ()
;;;void
;;;gtk_media_stream_error (GtkMediaStream *self,
;;;                        GQuark domain,
;;;                        int code,
;;;                        const char *format,
;;;                        ...);
;;;Sets self into an error state using a printf()-style format string.

;;;This is a utility function that calls gtk_media_stream_gerror(). See that function for details.

;;;Parameters
;;;self

;;;a GtkMediaStream

;;;domain

;;;error domain

;;;code

;;;error code

;;;format

;;;printf()-style format for error message

;;;...

;;;parameters for message format

;;;gtk_media_stream_error_valist ()
;;;void
;;;gtk_media_stream_error_valist (GtkMediaStream *self,
;;;                               GQuark domain,
;;;                               int code,
;;;                               const char *format,
;;;                               va_list args);
;;;Sets self into an error state using a printf()-style format string.

;;;This is a utility function that calls gtk_media_stream_gerror(). See that function for details.

;;;Parameters
;;;self

;;;a GtkMediaStream

;;;domain

;;;error domain

;;;code

;;;error code

;;;format

;;;printf()-style format for error message

;;;args

;;;va_list of parameters for the message format


;;; --- End of file gtk.media-stream.lisp --------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk4.media-stream.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.10 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 - 2023 Dieter Kaiser
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

#+liber-documentation
(setf (documentation 'media-stream 'type)
 "@version{#2023-5-2}
  @begin{short}
    The @sym{gtk:media-stream} object is the integration point for media
    playback inside GTK.
  @end{short}
  GTK provides an implementation of the @sym{gtk:media-stream} interface that
  is called the @class{gtk:media-file} object.

  Apart from application-facing API for stream playback, the
  @sym{gtk:media-stream} object has a number of APIs that are only useful for
  implementations and should not be used in applications:
  @fun{gtk:media-stream-prepared}, @fun{gtk:media-stream-unprepared},
  @fun{gtk.media-stream-update}, @fun{gtk:media-stream-ended},
  @fun{gtk:media-stream-seek-success}, @fun{gtk:media-stream-seek-failed},
  @fun{gtk:media-stream-gerror}, @fun{gtk:media-stream-error},
  @fun{gtk:media-stream-error-valist} functions.
  @see-slot{gtk:media-stream-duration}
  @see-slot{gtk:media-stream-ended}
  @see-slot{gtk:media-stream-error}
  @see-slot{gtk:media-stream-has-audio}
  @see-slot{gtk:media-stream-has-video}
  @see-slot{gtk:media-stream-loop}
  @see-slot{gtk:media-stream-muted}
  @see-slot{gtk:media-stream-playing}
  @see-slot{gtk:media-stream-prepared}
  @see-slot{gtk:media-stream-seekable}
  @see-slot{gtk:media-stream-seeking}
  @see-slot{gtk:media-stream-timestamp}
  @see-slot{gtk:media-stream-volume}
  @see-class{gdk:paintable}
  @see-class{gtk:media-file}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- media-stream-duration --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "duration" 'media-stream) t)
 "The @code{duration} property of type @code{:int64} (Read) @br{}
  The stream's duration in microseconds or 0 if unknown. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'media-stream-duration)
      "Accessor"
      (documentation 'media-stream-duration 'function)
 "@version{#2023-5-2}
  @syntax[]{(gtk:media-stream-duration object) => duration}
  @argument[object]{a @class{gtk:media-stream} object}
  @argument[duration]{an integer with the duration of the stream}
  @begin{short}
    Accessor of the @slot[gtk:media-stream]{duration} slot of the
    @class{gtk:media-stream} class.
  @end{short}
  The @sym{gtk:media-stream-duration} function gets the duration of the stream.
  If the duration is not known, 0 will be returned.
  @see-class{gtk:media-stream}")

;;; --- media-stream-ended -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "ended" 'media-stream) t)
 "The @code{ended} property of type @code{:boolean} (Read) @br{}
  Set when playback has finished. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'media-stream-ended)
      "Accessor"
      (documentation 'media-stream-ended 'function)
 "@version{#2023-5-2}
  @syntax[]{(gtk:media-stream-ended object) => ended}
  @argument[object]{a @class{gtk:media-stream} object}
  @argument[ended]{a boolean whether the playback is finished}
  @begin{short}
    Accessor of the @slot[gtk:media-stream]{ended} slot of the
    @class{gtk:media-stream} class.
  @end{short}
  The @sym{gtk:media-stream-ended} function returns whether the streams
  playback is finished.
  @see-class{gtk:media-stream}")

;;; --- media-stream-error -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "error" 'media-stream) t)
 "The @code{error} property of type @code{GError} (Read) @br{}
  @code{Nil} for a properly working stream or the GError that the stream is
  in.")

#+liber-documentation
(setf (liber:alias-for-function 'media-stream-error)
      "Accessor"
      (documentation 'media-stream-error 'function)
 "@version{#2023-5-2}
  @syntax[]{(gtk:media-stream-error object) => error}
  @argument[object]{a @class{gtk:media-stream} object}
  @argument[error]{@code{nil} if not in an error state or the @code{GError}
    of the stream}
  @begin{short}
    Accessor of the @slot[gtk:media-stream]{error} slot of the
    @class{gtk:media-stream} class.
  @end{short}
  If the stream is in an error state, the @sym{gtk:media-stream-error} function
  returns the @code{GError} explaining that state. Any type of error can be
  reported here depending on the implementation of the media stream.

  A media stream in an error cannot be operated on, calls like the
  @fun{gtk:media-stream-play} or @fun{gtk:media-stream-seek} functions will not
  have any effect.

  The @sym{gtk:media-stream} object itself does not provide a way to unset an
  error, but implementations may provide options. For example, a
  @class{gtk:media-file} object will unset errors when a new source is set with
  i.e. the @fun{gtk:media-file-set-file} function.
  @see-class{gtk:media-stream}
  @see-class{gtk:media-file}")

;;; --- media-stream-has-audio -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "has-audio" 'media-stream) t)
 "The @code{has-audio} property of type @code{:boolean} (Read) @br{}
  Whether the stream contains audio. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'media-stream-has-audio)
      "Accessor"
      (documentation 'media-stream-has-audio 'function)
 "@version{#2023-5-2}
  @syntax[]{(gtk:media-stream-has-audio object) => setting}
  @argument[object]{a @class{gtk:media-stream} object}
  @argument[setting]{@em{true} if the stream has audio}
  @begin{short}
    Accessor of the @slot[gtk:media-stream]{has-audio} slot of the
    @class{gtk:media-stream} class.
  @end{short}
  The @sym{gtk:media-stream-has-audio} function returns whether the stream has
  audio.
  @see-class{gtk:media-stream}")

;;; --- media-stream-has-video -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "has-video" 'media-stream) t)
 "The @code{has-video} property of type @code{:boolean} (Read) @br{}
  Whether the stream contains video. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'media-stream-has-video)
      "Accessor"
      (documentation 'media-stream-has-video 'function)
 "@version{#2023-5-2}
  @syntax[]{(gtk:media-stream-has-video object) => setting}
  @argument[object]{a @class{gtk:media-stream} object}
  @argument[setting]{@em{true} if the stream has video}
  @begin{short}
    Accessor of the @slot[gtk:media-stream]{has-video} slot of the
    @class{gtk:media-stream} class.
  @end{short}
  The @sym{gtk:media-stream-has-video} function returns whether the stream has
  video.
  @see-class{gtk:media-stream}")

;;; --- media-stream-loop ------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "loop" 'media-stream) t)
 "The @code{loop} property of type @code{:boolean} (Read / Write) @br{}
  Try to restart the media from the beginning once it ended. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'media-stream-loop)
      "Accessor"
      (documentation 'media-stream-loop 'function)
 "@version{#2023-5-2}
  @syntax[]{(gtk:media-stream-loop object) => setting}
  @syntax[]{(setf (gtk:media-stream-loop object) setting)}
  @argument[object]{a @class{gtk:media-stream} object}
  @argument[setting]{@em{true} if the stream should loop}
  @begin{short}
    Accessor of the @slot[gtk:media-stream]{loop} slot of the
    @class{gtk:media-stream} class.
  @end{short}
  The @sym{gtk:media-stream-loop} function returns whether the stream is set to
  loop. The @sym{(setf gtk:media-stream-loop)} function sets whether the stream
  should loop, i.e. restart playback from the beginning instead of stopping at
  the end.

  Not all streams may support looping, in particular non-seekable streams.
  Those streams will ignore the loop setting and just end.
  @see-class{gtk:media-stream}")

;;; --- media-stream-muted -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "muted" 'media-stream) t)
 "The @code{muted} property of type @code{:boolean} (Read / Write) @br{}
  Whether the audio stream should be muted. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'media-stream-muted)
      "Accessor"
      (documentation 'media-stream-muted 'function)
 "@version{#2023-5-2}
  @syntax[]{(gtk:media-stream-muted object) => setting}
  @syntax[]{(setf (gtk:media-stream-muted object) setting)}
  @argument[object]{a @class{gtk:media-stream} object}
  @argument[setting]{@em{true} if the stream should be muted}
  @begin{short}
    Accessor of the @slot[gtk:media-stream]{muted} slot of the
    @class{gtk:media-stream} class.
  @end{short}
  The @sym{gtk:media-stream-muted} function returns whether the audio for the
  stream is muted. The @sym{(setf gtk:media-stream-muted)} function sets whether
  the audio stream should be muted. Muting a stream will cause no audio to be
  played, but it does not modify the volume. This means that muting and then
  unmuting the stream will restore the volume settings.

  If the stream has no audio, calling this function will still work but it will
  not have an audible effect.
  @see-class{gtk:media-stream}")

;;; --- media-stream-playing ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "playing" 'media-stream) t)
 "The @code{playing} property of type @code{:boolean} (Read / Write) @br{}
  Whether the audio stream should be muted. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'media-stream-playing)
      "Accessor"
      (documentation 'media-stream-playing 'function)
 "@version{#2023-5-2}
  @syntax[]{(gtk:media-stream-playing object) => setting}
  @syntax[]{(setf (gtk:media-stream-playing object) setting)}
  @argument[object]{a @class{gtk:media-stream} object}
  @argument[setting]{a boolean whether to start or pause playback}
  @begin{short}
    Accessor of the @slot[gtk:media-stream]{playing} slot of the
    @class{gtk:media-stream} class.
  @end{short}
  The @sym{gtk:media-stream-playing} function returns whether the stream is
  currently playing. The @sym{(setf gtk:media-stream-playing)} function
  starts or pauses playback of the stream.
  @see-class{gtk:media-stream}")

;;; --- media-stream-prepared --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "prepared" 'media-stream) t)
 "The @code{prepared} property of type @code{:boolean} (Read / Write) @br{}
  Whether the stream has finished initializing and existence of audio and video
  is known. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'media-stream-prepared)
      "Accessor"
      (documentation 'media-stream-prepared 'function)
 "@version{#2023-5-2}
  @syntax[]{(gtk:media-stream-prepared object) => setting}
  @syntax[]{(setf (gtk:media-stream-prepared object) setting)}
  @argument[object]{a @class{gtk:media-stream} object}
  @argument[setting]{a boolean whether the stream has finished initializing
    and existence of audio and video is known}
  @begin{short}
    Accessor of the @slot[gtk:media-stream]{prepared} slot of the
    @class{gtk:media-stream} class.
  @end{short}
  @see-class{gtk:media-stream}")

;;; --- media-stream-seekable --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "seekable" 'media-stream) t)
 "The @code{seekable} property of type @code{:boolean} (Read) @br{}
  Set unless the stream is known to not support seeking. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'media-stream-seekable)
      "Accessor"
      (documentation 'media-stream-seekable 'function)
 "@version{#2023-5-2}
  @syntax[]{(gtk:media-stream-seekable object) => setting}
  @argument[object]{a @class{gtk:media-stream} object}
  @argument[setting]{a boolean whether the stream is known to support seeking}
  @begin{short}
    Accessor of the @slot[gtk:media-stream]{seekable} slot of the
    @class{gtk:media-stream} class.
  @end{short}
  @see-class{gtk:media-stream}")

;;; --- media-stream-seeking ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "seeking" 'media-stream) t)
 "The @code{seeking} property of type @code{:boolean} (Read) @br{}
  Set while a seek is in progress. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'media-stream-seeking)
      "Accessor"
      (documentation 'media-stream-seeking 'function)
 "@version{#2023-5-2}
  @syntax[]{(gtk:media-stream-seeking object) => setting}
  @argument[object]{a @class{gtk:media-stream} object}
  @argument[setting]{a boolean whether a seek is in progress}
  @begin{short}
    Accessor of the @slot[gtk:media-stream]{seeking} slot of the
    @class{gtk:media-stream} class.
  @end{short}
  @see-class{gtk:media-stream}")

;;; --- media-stream-timestamp -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "timestamp" 'media-stream) t)
 "The @code{timestamp} property of type @code{:int64} (Read) @br{}
  The current presentation timestamp in microseconds. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'media-stream-timestamp)
      "Accessor"
      (documentation 'media-stream-timestamp 'function)
 "@version{#2023-5-2}
  @syntax[]{(gtk:media-stream-timestamp object) => timestamp}
  @argument[object]{a @class{gtk:media-stream} object}
  @argument[timestamp]{an integer with the timestamp in microseconds}
  @begin{short}
    Accessor of the @slot[gtk:media-stream]{timestamp} slot of the
    @class{gtk:media-stream} class.
  @end{short}
  The @sym{gtk:media-stream-timestamp} function returns the current presentation
  timestamp in microseconds.
  @see-class{gtk:media-stream}")

;;; --- media-stream-volume ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "volume" 'media-stream) t)
 "The @code{volume} property of type @code{:double} (Read / Write) @br{}
  Volume of the audio stream. @br{}
  Allowed values: [0,1] @br{}
  Default value: 1")

#+liber-documentation
(setf (liber:alias-for-function 'media-stream-volume)
      "Accessor"
      (documentation 'media-stream-volume 'function)
 "@version{#2023-5-2}
  @syntax[]{(gtk:media-stream-volume object) => volume}
  @syntax[]{(setf (gtk:media-stream-playing object) volume)}
  @argument[object]{a @class{gtk:media-stream} object}
  @argument[volume]{a double float with the volume of the stream from 0.0 to
    1.0}
  @begin{short}
    Accessor of the @slot[gtk:media-stream]{volume} slot of the
    @class{gtk:media-stream} class.
  @end{short}
  The @sym{gtk:media-stream-volume} function returns the volume of the audio for
  the stream. The @sym{(setf gtk:media-stream-volume)} function sets the volume
  of the audio stream. This function call will work even if the stream is muted.

  The given volume should range from 0.0 for silence to 1.0 for as loud as
  possible. Values outside of this range will be clamped to the nearest value.

  If the stream has no audio or is muted, calling this function will still work
  but it will not have an immediate audible effect. When the stream is unmuted,
  the new volume setting will take effect.
  @see-class{gtk:media-stream}")

;;; ----------------------------------------------------------------------------
;;; gtk_media_stream_is_prepared ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_media_stream_is_prepared" media-stream-is-prepared) :boolean
 #+liber-documentation
 "@version{#2023-5-3}
  @argument[stream]{a @class{gtk:media-stream} object}
  @return{@em{True} if the stream is prepared.}
  @begin{short}
    Returns whether the stream has finished initializing and existence of audio
    and video is known.
  @end{short}
  @see-class{gtk:media-stream}"
  (stream (g:object media-stream)))

(export 'media-stream-is-prepared)

;;; ----------------------------------------------------------------------------
;;; gtk_media_stream_play ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_media_stream_play" media-stream-play) :void
 #+liber-documentation
 "@version{#2023-5-3}
  @argument[stream]{a @class{gtk:media-stream} object}
  @begin{short}
    Starts playing the stream.
  @end{short}
  If the stream is in error or already playing, do nothing.
  @see-class{gtk:media-stream}"
  (stream (g:object media-stream)))

(export 'media-stream-play)

;;; ----------------------------------------------------------------------------
;;; gtk_media_stream_pause ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_media_stream_pause" media-stream-pause) :void
 #+liber-documentation
 "@version{#2023-5-3}
  @argument[stream]{a @class{gtk:media-stream} object}
  @begin{short}
    Pauses playback of the stream.
  @end{short}
  If the stream is not playing, do nothing.
  @see-class{gtk:media-stream}"
  (stream (g:object media-stream)))

(export 'media-stream-pause)

;;; ----------------------------------------------------------------------------
;;; gtk_media_stream_is_seekable ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_media_stream_is_seekable" media-stream-is-seekable) :boolean
 #+liber-documentation
 "@version{#2023-5-3}
  @argument[stream]{a @class{gtk:media-stream} object}
  @return{@em{True} if the stream may support seeking.}
  @begin{short}
    Checks if a stream may be seekable.
  @end{short}
  This is meant to be a hint. Streams may not allow seeking even if this
  function returns @em{true} . However, if this function returns @em{false},
  streams are guaranteed to not be seekable and user interfaces may hide
  controls that allow seeking.

  It is allowed to call the @fun{gtk:media-stream-seek} function on a non
  seekable stream, though it will not do anything.
  @see-class{gtk:media-stream}
  @see-function{gtk:media-stream-seek}"
  (stream (g:object media-stream)))

(export 'media-stream-is-seekable)

;;; ----------------------------------------------------------------------------
;;; gtk_media_stream_is_seeking ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_media_stream_is_seeking" media-stream-is-seeking) :boolean
 #+liber-documentation
 "@version{#2023-5-3}
  @argument[stream]{a @class{gtk:media-stream} object}
  @return{@em{True} if a seek is ongoing.}
  @begin{short}
    Checks if there is currently a seek operation going on.
  @end{short}
  @see-class{gtk:media-stream}"
  (stream (g:object media-stream)))

(export 'media-stream-is-seeking)

;;; ----------------------------------------------------------------------------
;;; gtk_media_stream_seek ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_media_stream_seek" media-stream-seek) :void
 #+liber-documentation
 "@version{#2023-5-3}
  @argument[stream]{a @class{gtk:media-stream} object}
  @argument[timestamp]{an integer with the timestamp to seek to}
  @begin{short}
    Start a seek operation on @arg{stream} to @arg{timestamp}.
  @end{short}
  If @arg{timestamp} is out of range, it will be clamped.

  Seek operations may not finish instantly. While a seek operation is in
  process, the @slot[gtk:media-stream]{seeking} property will be set.

  When calling @sym{gtk:media-stream-seek} function during an ongoing seek
  operation, the new seek will override any pending seek.
  @see-class{gtk:media-stream}
  @see-function{gtk:media-stream-seeking}"
  (stream (g:object media-stream))
  (timestamp :int64))

(export 'media-stream-seek)

;;; ----------------------------------------------------------------------------
;;; gtk_media_stream_realize ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_media_stream_realize" media-stream-realize) :void
 #+liber-documentation
 "@version{#2023-5-3}
  @argument[stream]{a @class{gtk:media-stream} object}
  @argument[surface]{a @class{gdk:surface} object}
  @begin{short}
    Called by users to attach the media stream to a @class{gdk:surface} object
    they manage.
  @end{short}
  The stream can then access the resources of @arg{surface} for its rendering
  purposes. In particular, media streams might want to create
  @class{gdk:gl-context} objects or sync to the @class{gdk:frame-clock} object.

  Whoever calls this function is responsible for calling the
  @fun{gtk:media-stream-unrealize} function before either the stream or surface
  get destroyed.

  Multiple calls to this function may happen from different users of the video,
  even with the same surface . Each of these calls must be followed by its own
  call to the @fun{gtk:media-stream-unrealize} function.

  It is not required to call this function to make a media stream work.
  @see-class{gtk:media-stream}
  @see-class{gdk:surface}
  @see-class{gdk:gl-context}
  @see-class{gdk:frame-clock}
  @see-function{gtk:media-stream-unrealize}"
  (stream (g:object media-stream))
  (surface (g:object gdk:surface)))

(export 'media-stream-realize)

;;; ----------------------------------------------------------------------------
;;; gtk_media_stream_unrealize ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_media_stream_unrealize" media-stream-unrealize) :void
 #+liber-documentation
 "@version{#2023-5-3}
  @argument[stream]{a @class{gtk:media-stream} object previously realized}
  @argument[surface]{a @class{gdk:surface} object the stream was realized with}
  @begin{short}
    Undoes a previous call to the @fun{gtk:media-stream-realize} function and
    causes the stream to release all resources it had allocated from
    @arg{surface}.
  @end{short}
  @see-class{gtk:media-stream}
  @see-class{gdk:surface}
  @see-function{gtk:media-stream-realize}"
  (stream (g:object media-stream))
  (surface (g:object gdk:surface)))

(export 'media-stream-unrealize)

;;; ----------------------------------------------------------------------------
;;; gtk_media_stream_prepared ()
;;;
;;; void
;;; gtk_media_stream_prepared (GtkMediaStream *self,
;;;                            gboolean has_audio,
;;;                            gboolean has_video,
;;;                            gboolean seekable,
;;;                            gint64 duration);
;;;
;;; Called by GtkMediaStream implementations to advertise the stream being
;;; ready to play and providing details about the stream.
;;;
;;; Note that the arguments are hints. If the stream implementation cannot
;;; determine the correct values, it is better to err on the side of caution
;;; and return TRUE. User interfaces will use those values to determine what
;;; controls to show.
;;;
;;; This function may not be called again until the stream has been reset via
;;; gtk_media_stream_unprepared().
;;;
;;; self :
;;;     a GtkMediaStream
;;;
;;; has_audio :
;;;     TRUE if the stream should advertise audio support
;;;
;;; has_video :
;;;     TRUE if the stream should advertise video support
;;;
;;; seekable :
;;;     TRUE if the stream should advertise seekability
;;;
;;; duration :
;;;     The duration of the stream or 0 if unknown
;;; ----------------------------------------------------------------------------

;; deprecated since 4.4 and not implemented

;;; ----------------------------------------------------------------------------
;;; gtk_media_stream_unprepared ()
;;;
;;; void
;;; gtk_media_stream_unprepared (GtkMediaStream *self);
;;;
;;; Resets a given media stream implementation. gtk_media_stream_prepared() can
;;; now be called again.
;;;
;;; This function will also reset any error state the stream was in.
;;;
;;; self :
;;;     a GtkMediaStream
;;; ----------------------------------------------------------------------------

;; deprecated since 4.4 and not implemented

;;; ----------------------------------------------------------------------------
;;; gtk_media_stream_update ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_media_stream_update" media-stream-update) :void
 #+liber-documentation
 "@version{#2023-5-3}
  @argument[stream]{a @class{gtk:media-stream} object}
  @argument[timestamp]{an integer with the new timestamp}
  @begin{short}
    Media stream implementations should regularly call this function to update
    the timestamp reported by the stream.
  @end{short}
  It is up to implementations to call this at the frequency they deem
  appropriate.
  @see-class{gtk:media-stream}"
  (stream (g:object media-stream))
  (timestamp :int64))

(export 'media-stream-update)

;;; ----------------------------------------------------------------------------
;;; gtk_media_stream_ended ()
;;;
;;; void
;;; gtk_media_stream_ended (GtkMediaStream *self);
;;;
;;; Pauses the media stream and marks it as ended. This is a hint only, calls
;;; to GtkMediaStream.play() may still happen.
;;;
;;; self :
;;;     a GtkMediaStream
;;; ----------------------------------------------------------------------------

;; deprecated since 4.4 and not implemented

;;; ----------------------------------------------------------------------------
;;; gtk_media_stream_seek_success ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_media_stream_seek_success" media-stream-seek-success) :void
 #+liber-documentation
 "@version{#2023-5-3}
  @argument[stream]{a @class{gtk:media-stream} object}
  @begin{short}
    Ends a seek operation started via the @fun{gtk:media-stream-seek} function
    successfully.
  @end{short}
  This function will unset the @slot[gtk:media-stream]{ended} property if it
  was set.

  See the @fun{gtk:media-stream-seek-failed} function for the other way of
  ending a seek.
  @see-class{gtk:media-stream}
  @see-function{gtk:media-stream-seek}
  @see-function{gtk:media-stream-ended}
  @see-function{gtk:media-stream-seek-failed}"
  (stream (g:object media-stream)))

(export 'media-stream-seek-success)

;;; ----------------------------------------------------------------------------
;;; gtk_media_stream_seek_failed ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_media_stream_seek_failed" media-stream-seek-failed) :void
 #+liber-documentation
 "@version{#2023-5-3}
  @argument[stream]{a @class{gtk:media-stream} object}
  @begin{short}
    Ends a seek operation started via the @fun{gtk:media-stream-seek} function
    as a failure.
  @end{short}
  This will not cause an error on the stream and will assume that playback
  continues as if no seek had happened.

  See the @fun{gtk:media-stream-seek-success} function for the other way of
  ending a seek.
  @see-class{gtk:media-stream}
  @see-function{gtk:media-stream-seek}
  @see-function{gtk:media-stream-seek-success}"
  (stream (g:object media-stream)))

(export 'media-stream-seek-failed)

;;; ----------------------------------------------------------------------------
;;; gtk_media_stream_gerror ()
;;;
;;; void
;;; gtk_media_stream_gerror (GtkMediaStream *self,
;;;                          GError *error);
;;;
;;; Sets self into an error state. This will pause the stream (you can check for
;;; an error via gtk_media_stream_get_error() in your GtkMediaStream.pause()
;;; implementation), abort pending seeks and mark the stream as prepared.
;;;
;;; if the stream is already in an error state, this call will be ignored and
;;; the existing error will be retained. FIXME: Or do we want to set the new
;;; error?
;;;
;;; To unset an error, the stream must be reset via a call to
;;; gtk_media_stream_unprepared().
;;;
;;; self :
;;;     a GtkMediaStream
;;;
;;; error :
;;;     the GError to set.
;;; ----------------------------------------------------------------------------

;; TODO: Consider to implement this function.

;;; ----------------------------------------------------------------------------
;;; gtk_media_stream_error ()
;;;
;;; void
;;; gtk_media_stream_error (GtkMediaStream *self,
;;;                         GQuark domain,
;;;                         int code,
;;;                         const char *format,
;;;                         ...);
;;;
;;; Sets self into an error state using a printf()-style format string.
;;;
;;; This is a utility function that calls gtk_media_stream_gerror(). See that
;;; function for details.
;;;
;;; self :
;;;     a GtkMediaStream
;;;
;;; domain :
;;;     error domain
;;;
;;; code :
;;;     error code
;;;
;;; format :
;;;     printf()-style format for error message
;;;
;;; ... :
;;;     parameters for message format
;;; ----------------------------------------------------------------------------

;; not implemented

;;; ----------------------------------------------------------------------------
;;; gtk_media_stream_error_valist ()
;;;
;;; void
;;; gtk_media_stream_error_valist (GtkMediaStream *self,
;;;                                GQuark domain,
;;;                                int code,
;;;                                const char *format,
;;;                                va_list args);
;;;
;;; Sets self into an error state using a printf()-style format string.
;;;
;;; This is a utility function that calls gtk_media_stream_gerror(). See that
;;; function for details.
;;;
;;; self :
;;;     a GtkMediaStream
;;;
;;; domain :
;;;     error domain
;;;
;;; code :
;;;     error code
;;;
;;; format :
;;;     printf()-style format for error message
;;;
;;; args :
;;;     va_list of parameters for the message format
;;; ----------------------------------------------------------------------------

;; not implemented

;;; --- End of file gtk4.media-stream.lisp -------------------------------------

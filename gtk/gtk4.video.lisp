;;; ----------------------------------------------------------------------------
;;; gtk4.video.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.14 and modified to document the Lisp binding to the GTK library.
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
;;; GtkVideo
;;;
;;;     A widget for displaying video
;;;
;;; Types and Values
;;;
;;;     GtkVideo
;;;
;;; Accessors
;;;
;;;     gtk_video_get_autoplay
;;;     gtk_video_set_autoplay
;;;     gtk_video_get_file
;;;     gtk_video_set_file
;;;     gtk_video_get_graphics_offload                      Since 4.14
;;;     gtk_video_set_graphics_offload                      Since 4.14
;;;     gtk_video_get_loop
;;;     gtk_video_set_loop
;;;     gtk_video_get_media_stream
;;;     gtk_video_set_media_stream
;;;
;;; Functions
;;;
;;;     gtk_video_new
;;;     gtk_video_new_for_media_stream
;;;     gtk_video_new_for_file
;;;     gtk_video_new_for_filename
;;;     gtk_video_new_for_resource
;;;     gtk_video_set_filename
;;;     gtk_video_set_resource
;;;
;;; Properties
;;;
;;;     autoplay
;;;     file
;;;     graphics-offload                                    Since 4.14
;;;     loop
;;;     media-stream
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkVideo
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkGraphicsOffloadEnabled
;;; ----------------------------------------------------------------------------

#+gtk-4-14
(gobject:define-g-enum "GtkGraphicsOffloadEnabled" graphics-offload-enabled
  (:export t
   :type-initializer "gtk_graphics_offload_enabled_get_type")
  (:enabled 0)
  (:disabled 1))

#+(and gtk-4-14 liber-documentation)
(setf (liber:alias-for-symbol 'graphics-offload-enabled)
      "GEnum"
      (liber:symbol-documentation 'graphics-offload-enabled)
 "@version{2024-5-26}
  @begin{declaration}
(gobject:define-g-enum \"GtkGraphicsOffloadEnabled\" graphics-offload-enabled
  (:export t
   :type-initializer \"gtk_graphics_offload_enabled_get_type\")
  (:enabled 0)
  (:disabled 1))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:enabled]{Graphics offloading is enabled.}
      @entry[:disabled]{Graphics offloading is disabled.}
    @end{table}
  @end{values}
  @begin{short}
    Represents the state of graphics offloading.
  @end{short}

  Since 4.14
  @see-class{gtk:video}")

;;; ----------------------------------------------------------------------------
;;; GtkVideo
;;;
;;; GtkVideo is a widget to show a GtkMediaStream.
;;;
;;; It is commonly combined with GtkMediaControls to give the user a way to
;;; control the playback.
;;;
;;; See Also
;;;      GtkMediaControls
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkVideo" video
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget")
   :type-initializer "gtk_video_get_type")
  ((autoplay
    video-autoplay
    "autoplay" "gboolean" t t)
   (file
    video-file
    "file" "GFile" t t)
   #+gtk-4-14
   (graphics-offload
    video-graphics-offload
    "graphics-offload" "GtkGraphicsOffloadEnabled" t t)
   (loop
    video-loop
    "loop" "gboolean" t t)
   (media-stream
    video-media-stream
    "media-stream" "GtkMediaStream" t t)))

#+liber-documentation
(setf (documentation 'video 'type)
 "@version{#2023-4-21}
  @begin{short}
    The @class{gtk:video} widget is a widget to show a @class{gtk:media-stream}
    object with media controls.
  @end{short}

  @image[video]{Figure: GtkVideo}

  The controls are available separately as @class{gtk:media-controls} widgets.
  If you just want to display a video without controls, you can treat it like
  any other paintable and for example put it into a @class{gtk:picture} widget.

  The @class{gtk:video} widget aims to cover use cases such as previews,
  embedded animations, etc. It supports autoplay, looping, and simple media
  controls. It does not have support for video overlays, multichannel audio,
  device selection, or input. If you are writing a full-fledged video player,
  you may want to use the @class{gdk:paintable} API and a media framework such
  as @code{Gstreamer} directly.
  @see-constructor{gtk:video-new}
  @see-constructor{gtk:video-new-for-file}
  @see-constructor{gtk:video-new-for-filename}
  @see-constructor{gtk:video-new-for-media-stream}
  @see-constructor{gtk:video-new-for-resource}
  @see-slot{gtk:video-autoplay}
  @see-slot{gtk:video-file}
  @see-slot{gtk:video-loop}
  @see-slot{gtk:video-media-stream}
  @see-class{gtk:media-stream}
  @see-class{gtk:media-controls}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:video-autoplay -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "autoplay" 'video) t)
 "The @code{autoplay} property of type @code{:boolean} (Read / Write) @br{}
  Whether the video should automatically begin playing. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'video-autoplay)
      "Accessor"
      (documentation 'video-autoplay 'function)
 "@version{#2023-4-29}
  @syntax{(gtk:video-autoplay object) => autoplay}
  @syntax{(setf (gtk:video-autoplay object) autoplay)}
  @argument[object]{a @class{gtk:video} widget}
  @argument[autoplay]{a boolean whether media streams should autoplay}
  @begin{short}
    Accessor of the @slot[gtk:video]{autoplay} slot of the @class{gtk:video}
    class.
  @end{short}
  The @fun{gtk:video-autoplay} function returns @em{true} if videos have been
  set to loop via the @fun{gtk:video-loop} function. The
  @setf{gtk:video-autoplay} function sets whether self automatically starts
  playback when it becomes visible or when a new file gets loaded.
  @see-class{gtk:video}
  @see-function{gtk:video-loop}")

;;; --- gtk:video-file ---------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "file" 'video) t)
 "The @code{file} property of type @class{g:file} (Read / Write) @br{}
  The file played by the video if the video is playing a file.")

#+liber-documentation
(setf (liber:alias-for-function 'video-file)
      "Accessor"
      (documentation 'video-file 'function)
 "@version{#2023-4-29}
  @syntax{(gtk:video-file object) => file}
  @syntax{(setf (gtk:video-file object) file)}
  @argument[object]{a @class{gtk:video} widget}
  @argument[file]{a @class{g:file} object to play}
  @begin{short}
    Accessor of the @slot[gtk:video]{file} slot of the @class{gtk:video} class.
  @end{short}
  The @fun{gtk:video-file} function gets the file played by @arg{object} or
  @code{nil} if not playing back a file. The @setf{gtk:video-file} function
  makes @arg{object} play the given @arg{file}.
  @see-class{gtk:video}
  @see-class{g:file}")

;;; --- gtk:video-graphics-offload ---------------------------------------------

#+(and gtk-4-14 liber-documentation)
(setf (documentation (liber:slot-documentation "graphics-offload" 'video) t)
 "The @code{graphics-offload} property of type
  @symbol{gtk:graphics-offload-enabled} (Read / Write) @br{}
  Whether to enable graphics offload. Since 4.14 @br{}
  Default value: @code{:disabled}")

#+(and gtk-4-14 liber-documentation)
(setf (liber:alias-for-function 'video-graphics-offload)
      "Accessor"
      (documentation 'video-graphics-offload 'function)
 "@version{#2024-5-26}
  @syntax{(gtk:video-graphics-offload object) => setting}
  @syntax{(setf (gtk:video-graphics-offload object) setting)}
  @argument[object]{a @class{gtk:video} widget}
  @argument[setting]{a @symbol{gtk:graphics-offload-enabled} value}
  @begin{short}
    Accessor of the @slot[gtk:video]{graphics-offload} slot of the
    @class{gtk:video} class.
  @end{short}
  The @fun{gtk:video-graphics-offload} function returns whether graphics offload
  is enabled. The @setf{gtk:video-graphics-offload} function sets whether to
  enable graphics offload. See the @symbol{gtk:graphics-offload-enabled}
  documentation for more information on graphics offload.

  Since 4.14
  @see-class{gtk:video}
  @see-symbol{gtk:graphics-offload-enabled}")

;;; --- gtk:video-loop ---------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "loop" 'video) t)
 "The @code{loop} property of type @code{:boolean} (Read / Write) @br{}
  Whether new media files should be set to loop. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'video-loop)
      "Accessor"
      (documentation 'video-loop 'function)
 "@version{#2023-4-29}
  @syntax{(gtk:video-loop object) => loop}
  @syntax{(setf (gtk:video-loop object) loop)}
  @argument[object]{a @class{gtk:video} widget}
  @argument[loop]{a boolean whether media streams should loop}
  @begin{short}
    Accessor of the @slot[gtk:video]{loop} slot of the @class{gtk:video} class.
  @end{short}
  The @fun{gtk:video-loop} function returns @em{true} if videos have been set
  to loop. The @setf{gtk:video-loop} function sets whether new files loaded by
  @arg{object} should be set to loop.
  @see-class{gtk:video}")

;;; --- gtk:video-media-stream -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "media-stream" 'video) t)
 "The @code{media-stream} property of type @class{gtk:media-stream}
  (Read / Write) @br{}
  The media stream played.")

#+liber-documentation
(setf (liber:alias-for-function 'video-media-stream)
      "Accessor"
      (documentation 'video-media-stream 'function)
 "@version{#2023-4-29}
  @syntax{(gtk:video-media-stream object) => stream}
  @syntax{(setf (gtk:video-media-stream object) stream)}
  @argument[object]{a @class{gtk:video} widget}
  @argument[stream]{a @class{gtk:media-stream} object to play or @code{nil}
    to unset}
  @begin{short}
    Accessor of the @slot[gtk:video]{media-stream} slot of the @class{gtk:video}
    class.
  @end{short}
  The @fun{gtk:video-media-stream} function gets the media stream managed by
  @arg{object} or @code{nil} if none. The @setf{gtk:video-media-steam} function
  sets the media stream to be played back. @arg{object} will take full control
  of managing the media stream. If you want to manage a media stream yourself,
  consider using a @class{gtk:image} widget for display.

  If you want to display a file, consider using the @fun{gtk:video-file}
  function instead.
  @see-class{gtk:video}
  @see-class{gtk:media-stream}
  @see-function{gtk:video-file}")

;;; ----------------------------------------------------------------------------
;;; gtk_video_new
;;; ----------------------------------------------------------------------------

(declaim (inline video-new))

(defun video-new ()
 #+liber-documentation
 "@version{#2023-4-29}
  @return{The new @class{gtk:video} widget.}
  @short{Creates a new empty @class{gtk:video} widget.}
  @see-class{gtk:video}"
  (make-instance 'video))

(export 'video-new)

;;; ----------------------------------------------------------------------------
;;; gtk_video_new_for_media_stream
;;; ----------------------------------------------------------------------------

(declaim (inline video-new-for-media-stream))

(defun video-new-for-media-stream (stream)
 #+liber-documentation
 "@version{#2023-4-29}
  @argument[stream]{a @class{gtk:media-stream} object}
  @return{The new @class{gtk:video} widget.}
  @short{Creates a new @class{gtk:video} widget to play the given media stream.}
  @see-class{gtk:video}
  @see-class{gtk:media-stream}"
  (make-instance 'video
                 :media-stream stream))

(export 'video-new-for-media-stream)

;;; ----------------------------------------------------------------------------
;;; gtk_video_new_for_file
;;; ----------------------------------------------------------------------------

(declaim (inline video-new-for-file))

(defun video-new-for-file (file)
 #+liber-documentation
 "@version{#2023-4-29}
  @argument[file]{a @class{g:file} object}
  @return{The new @class{gtk:video} widget.}
  @short{Creates a new @class{gtk:video} widget to play the given @arg{file}.}
  @see-class{gtk:video}
  @see-class{g:file}"
  (make-instance 'video
                 :file file))

(export 'video-new-for-file)

;;; ----------------------------------------------------------------------------
;;; gtk_video_new_for_filename
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_video_new_for_filename" %video-new-for-filename)
    (g:object video)
  (filename :string))

(defun video-new-for-filename (path)
 #+liber-documentation
 "@version{#2023-4-29}
  @argument[path]{a pathname or namestring with the file to play back}
  @return{The new @class{gtk:video} widget.}
  @short{Creates a new @class{gtk:video} widget to play the given @arg{path}.}
  This is a utility function that calls the @fun{gtk:video-new-for-file}
  function.
  @see-class{gtk:video}
  @see-function{gtk:video-new-for-file}"
  (%video-new-for-filename (namestring path)))

(export 'video-new-for-filename)

;;; ----------------------------------------------------------------------------
;;; gtk_video_new_for_resource
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_video_new_for_resource" %video-new-for-resource)
    (g:object video)
  (path :string))

(defun video-new-for-resource (path)
 #+liber-documentation
 "@version{#2023-4-29}
  @argument[path]{a pathname or namestring with the resource path to play back}
  @return{The new @class{gtk:video} widget.}
  @short{Creates a new @class{gtk:video} widget to play the given @arg{path}.}
  This is a utility function that calls the @fun{gtk:video-new-for-file}
  function.
  @see-class{gtk:video}
  @see-function{gtk:video-new-for-file}"
  (%video-new-for-resource (namestring path)))

(export 'video-new-for-resource)

;;; ----------------------------------------------------------------------------
;;; gtk_video_set_filename
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_video_set_filename" %video-set-filename) :void
  (video (g:object video))
  (filename :string))

(defun video-set-filename (video path)
 #+liber-documentation
 "@version{#2023-5-6}
  @argument[video]{a @class{gtk:video} widget}
  @argument[path]{a pathname or namestring with the file to play}
  @begin{short}
    Makes @arg{object} play the given @arg{path}.
  @end{short}
  This is a utility function that calls the @fun{gtk:video-file} function.
  @see-class{gtk:video}
  @see-function{gtk:video-file}"
  (%video-set-filename video (namestring path)))

(export 'video-set-filename)

;;; ----------------------------------------------------------------------------
;;; gtk_video_set_resource
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_video_set_resource" %video-set-resource) :void
  (video (g:object video))
  (path :string))

(defun video-set-resource (video path)
 #+liber-documentation
 "@version{#2023-5-6}
  @argument[video]{a @class{gtk:video} widget}
  @argument[path]{a pathname or namestring with the resource to play}
  @begin{short}
    Makes @arg{object} play the resource at the given @arg{path}.
  @end{short}
  This is a utility function that calls the @fun{gtk:video-file} function.
  @see-class{gtk:video}
  @see-function{gtk:video-file}"
  (%video-set-resource video (namestring path)))

(export 'video-set-resource)

;;; --- End of file gtk4.video.lisp --------------------------------------------

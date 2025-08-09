;;; ----------------------------------------------------------------------------
;;; gtk4.media-file.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.18 and modified to document the Lisp binding to the GTK library,
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
;;; GtkMediaFile
;;;
;;;     Open media files for use in GTK
;;;
;;; Types and Values
;;;
;;;     GtkMediaFile
;;;
;;; Accessors
;;;
;;;     gtk_media_file_set_file
;;;     gtk_media_file_get_file
;;;     gtk_media_file_set_input_stream                     not implemented
;;;     gtk_media_file_get_input_stream                     not implemented
;;;
;;; Functions
;;;
;;;     gtk_media_file_new
;;;     gtk_media_file_new_for_file
;;;     gtk_media_file_new_for_filename
;;;     gtk_media_file_new_for_input_stream                 not implemented
;;;     gtk_media_file_new_for_resource
;;;
;;;     gtk_media_file_clear
;;;     gtk_media_file_set_filename
;;;     gtk_media_file_set_resource
;;;
;;; Properties
;;;
;;;     file
;;;     input-stream                                        not implemented
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
;;; GtkMediaFile
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkMediaFile" media-file
  (:superclass media-stream
   :export t
   :interfaces ("GdkPaintable")
   :type-initializer "gtk_media_file_get_type")
  ((file
    media-file-file
    "file" "GFile" t t)
   #+nil ; TODO: GInputStream is not implemented
   (input-stream
    media-file-input-stream
    "input-stream" "GInputStream" t t)))

#+liber-documentation
(setf (documentation 'media-file 'type)
 "@version{2024-10-31}
  @begin{short}
    The @class{gtk:media-file} class is the implementation for media file usage
    with the @class{gtk:media-stream} class.
  @end{short}
  This provides a simple way to play back video files with GTK.

  GTK provides a GIO extension point for @class{gtk:media-file} implementations
  to allow for external implementations using various media frameworks. GTK
  itself includes implementations using @code{GStreamer} and @code{ffmpeg}.
  @see-constructor{gtk:media-file-new}
  @see-constructor{gtk:media-file-new-for-file}
  @see-constructor{gtk:media-file-new-for-filename}
  @see-constructor{gtk:media-file-new-for-resource}
  @see-slot{gtk:media-file-file}
  @see-class{gtk:media-stream}
  @see-class{gtk:video}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:media-file-file ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "file" 'media-file) t)
 "The @code{file} property of type @class{g:file} (Read / Write) @br{}
  The file being played back or @code{nil} if not playing a file.")

#+liber-documentation
(setf (liber:alias-for-function 'media-file-file)
      "Accessor"
      (documentation 'media-file-file 'function)
 "@version{2025-07-27}
  @syntax{(gtk:media-file-file object) => file}
  @syntax{(setf (gtk:media-file-file object) file)}
  @argument[object]{a @class{gtk:media-file} object}
  @argument[file]{a @class{g:file} object for the file to play}
  @begin{short}
    Accessor of the @slot[gtk:media-file]{file} slot of the
    @class{gtk:media-file} class.
  @end{short}
  The @fun{gtk:media-file-file} function returns the file that @arg{object} is
  currently playing from. When @arg{object} is not playing or not playing from
  a file, @code{nil} is returned.

  The @setf{gtk:media-file-file} function sets the file. If any file is still
  playing, stop playing it. Then start playing the given @arg{file}.
  @see-class{gtk:media-file}
  @see-class{g:file}")

;;; --- gtk:media-file-input-stream --------------------------------------------

#+(and nil liber-documentation)
(setf (documentation (liber:slot-documentation "input-stream" 'media-file) t)
 "The @code{input-stream} property of type @code{GInputStream} (Read / Write)
  @br{}
  The stream being played back or @code{nil} if not playing a stream, like when
  playing a file.")

#+(and nil liber-documentation)
(setf (liber:alias-for-function 'media-file-input-stream)
      "Accessor"
      (documentation 'media-file-input-stream 'function)
 "@version{2025-07-27}
  @syntax{(gtk:media-file-input-stream object) => stream}
  @syntax{(setf (gtk:media-file-input-stream object) stream)}
  @argument[object]{a @class{gtk:media-file} object}
  @argument[stream]{a @code{GInputStream} object for the stream to play from}
  @begin{short}
    Accessor of the @slot[gtk:media-file]{input-stream} slot of the
    @class{gtk:media-file} class.
  @end{short}
  The @fun{gtk:media-file-input-stream} function returns the stream that
  @arg{object} is currently playing from. When @arg{object} is not playing or
  not playing from a stream, @code{nil} is returned.

  The @setf{gtk:media-file-input-stream} function sets the stream. If anything
  is still playing, stop playing it. Then start playing the given @arg{stream}.
  Full control about the stream is assumed for the duration of playback. The
  stream will not bt be closed.
  @see-class{gtk:media-file}")

;;; ----------------------------------------------------------------------------
;;; gtk_media_file_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_media_file_new" media-file-new)
    (g:object media-stream :return)
 #+liber-documentation
 "@version{2024-10-31}
  @return{The new @class{gtk:media-file} object.}
  @short{Creates a new empty media file.}
  @see-class{gtk:media-file}")

(export 'media-file-new)

;;; ----------------------------------------------------------------------------
;;; gtk_media_file_new_for_file
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_media_file_new_for_file" media-file-new-for-file)
    (g:object media-stream :return)
 #+liber-documentation
 "@version{2025-07-27}
  @argument[file]{a @class{g:file} object for the file to play}
  @return{The new @class{gtk:media-file} object playing @arg{file}.}
  @short{Creates a new media file.}
  @see-class{gtk:media-file}
  @see-class{g:file}"
  (file g:object))

(export 'media-file-new-for-file)

;;; ----------------------------------------------------------------------------
;;; gtk_media_file_new_for_filename
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_media_file_new_for_filename" %media-file-new-for-filename)
    (g:object media-stream :return)
  (filename :string))

(defun media-file-new-for-filename (path)
 #+liber-documentation
 "@version{2025-07-27}
  @argument[filename]{a pathname or namestring for the file to open}
  @return{The new @class{gtk:media-file} object playing @arg{filename}.}
  @short{Creates a new media file.}
  This is a utility function that converts the given filename to a
  @class{g:file} object and calls the @fun{gtk:media-file-new-for-file}
  function.
  @see-class{gtk:media-file}
  @see-class{g:file}
  @see-function{gtk:media-file-new-for-file}"
  (%media-file-new-for-filename (namestring path)))

(export 'media-file-new-for-filename)

;;; ----------------------------------------------------------------------------
;;; gtk_media_file_new_for_input_stream
;;; ----------------------------------------------------------------------------

;; TODO: GInputStream is not implemented

;;; ----------------------------------------------------------------------------
;;; gtk_media_file_new_for_resource
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_media_file_new_for_resource" media-file-new-for-resource)
    (g:object media-stream :return)
 #+liber-documentation
 "@version{2025-07-27}
  @argument[path]{a string for the resource path to open}
  @return{The new @class{gtk:media-file} object playing @arg{path}.}
  @short{Creates a new media file.}
  This is a utility function that converts the given resource to a
  @class{g:file} object and calls the @fun{gtk:media-file-new-for-file}
  function.
  @see-class{gtk:media-file}
  @see-class{g:file}
  @see-function{gtk:media-file-new-for-file}"
  (path :string))

(export 'media-file-new-for-resource)

;;; ----------------------------------------------------------------------------
;;;gtk_media_file_clear
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_media_file_clear" media-file-clear) :void
 #+liber-documentation
 "@version{2024-10-31}
  @argument[mediafile]{a @class{gtk:media-file} object}
  @short{Resets the media file to be empty.}
  @see-class{gtk:media-file}"
  (mediafile (g:object media-file)))

(export 'media-file-clear)

;;; ----------------------------------------------------------------------------
;;; gtk_media_file_set_filename
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_media_file_set_filename" %media-file-set-filename) :void
  (mediafile (g:object media-file))
  (filename :string))

(defun media-file-set-filename (mediafile path)
 #+liber-documentation
 "@version{2025-07-27}
  @argument[mediafile]{a @class{gtk:media-file} object}
  @argument[path]{a pathname or namestring for the name of file to play}
  @begin{short}
    This is a utility function that converts the given @arg{filename} to a
    @class{g:file} object and calls the @fun{gtk:media-file-file} function.
  @end{short}
  @see-class{gtk:media-file}
  @see-class{g:file}
  @see-function{gtk:media-file-file}"
  (%media-file-set-filename mediafile (namestring path)))

(export 'media-file-set-filename)

;;; ----------------------------------------------------------------------------
;;; gtk_media_file_set_resource
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_media_file_set_resource" %media-file-set-resource) :void
  (mediafile (g:object media-file))
  (path :string))

(defun media-file-set-resource (mediafile path)
 #+liber-documentation
 "@version{2025-07-27}
  @argument[mediafile]{a @class{gtk:media-file} object}
  @argument[path]{a string for the path to the resource to playback}
  @begin{short}
    This is a utility function that converts the given @arg{path} to a
    @class{g:file} object and calls the @fun{gtk:media-file-file} function.
  @end{short}
  @see-class{gtk:media-file}
  @see-class{g:file}
  @see-function{gtk:media-file-file}"
  (%media-file-set-resource mediafile (namestring path)))

(export 'media-file-set-resource)

;;; --- End of file gtk4.media-file.lisp ---------------------------------------

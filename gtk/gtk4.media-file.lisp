;;; ----------------------------------------------------------------------------
;;; gtk.media-file.lisp
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
;;; GtkMediaFile
;;;
;;;     Open media files for use in GTK
;;;
;;; Types and Values
;;;
;;;     GtkMediaFile
;;;
;;; Functions
;;;
;;;     gtk_media_file_new
;;;     gtk_media_file_new_for_filename
;;;     gtk_media_file_new_for_resource
;;;     gtk_media_file_new_for_file
;;;     gtk_media_file_new_for_input_stream
;;;     gtk_media_file_clear
;;;     gtk_media_file_set_filename
;;;     gtk_media_file_set_resource
;;;     gtk_media_file_set_file
;;;     gtk_media_file_get_file
;;;     gtk_media_file_set_input_stream
;;;     gtk_media_file_get_input_stream
;;;
;;; Properties
;;;
;;;     file
;;;     input-stream
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


;;;Description
;;;GtkMediaFile is the implementation for media file usage with GtkMediaStream.

;;;This provides a simple way to play back video files with GTK.

;;;GTK provides a GIO extension point for GtkMediaFile implementations to allow for external implementations using various media frameworks. GTK itself includes implementations using GStreamer and ffmpeg.

;;;See Also
;;;GtkMediaStream, GtkVideo
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkMediaFile" media-file
  (:superclass media-stream
   :export t
   :interfaces ("GdkPaintable")
   :type-initializer "gtk_media_file_get_type")
  ((file
    media-file-file
    "file" "GFile" t t)
   (input-stream
    media-file-input-stream
    "input-stream" "GInputStream" t t)))



;;;Property Details

;;;The “file” property
;;;  “file”                     GFile *
;;;The file being played back or NULL if not playing a file.

;;;Owner: GtkMediaFile

;;;Flags: Read / Write

;;;The “input-stream” property
;;;  “input-stream”             GInputStream *
;;;The stream being played back or NULL if not playing a stream, like when playing a file.

;;;Owner: GtkMediaFile

;;;Flags: Read / Write





;;;Functions
;;;gtk_media_file_new ()
;;;GtkMediaStream *
;;;gtk_media_file_new (void);
;;;Creates a new empty media file.

;;;Returns
;;;a new GtkMediaFile.

;;;[type Gtk.MediaFile]

;;;gtk_media_file_new_for_filename ()
;;;GtkMediaStream *
;;;gtk_media_file_new_for_filename (const char *filename);
;;;This is a utility function that converts the given filename to a GFile and calls gtk_media_file_new_for_file().

;;;Parameters
;;;filename

;;;filename to open

;;;Returns
;;;a new GtkMediaFile playing filename .

;;;[type Gtk.MediaFile]

;;;gtk_media_file_new_for_resource ()
;;;GtkMediaStream *
;;;gtk_media_file_new_for_resource (const char *resource_path);
;;;This is a utility function that converts the given resource to a GFile and calls gtk_media_file_new_for_file().

;;;Parameters
;;;resource_path

;;;resource path to open

;;;Returns
;;;a new GtkMediaFile playing resource_path .

;;;[type Gtk.MediaFile]

;;;gtk_media_file_new_for_file ()
;;;GtkMediaStream *
;;;gtk_media_file_new_for_file (GFile *file);
;;;Creates a new media file to play file .

;;;Parameters
;;;file

;;;The file to play

;;;Returns
;;;a new GtkMediaFile playing file .

;;;[type Gtk.MediaFile]

;;;gtk_media_file_new_for_input_stream ()
;;;GtkMediaStream *
;;;gtk_media_file_new_for_input_stream (GInputStream *stream);
;;;Creates a new media file to play stream . If you want the resulting media to be seekable, the stream should implement the GSeekable interface.

;;;Parameters
;;;stream

;;;The stream to play

;;;Returns
;;;a new GtkMediaFile.

;;;[type Gtk.MediaFile]

;;;gtk_media_file_clear ()
;;;void
;;;gtk_media_file_clear (GtkMediaFile *self);
;;;Resets the media file to be empty.

;;;Parameters
;;;self

;;;a GtkMediaFile

;;;gtk_media_file_set_filename ()
;;;void
;;;gtk_media_file_set_filename (GtkMediaFile *self,
;;;                             const char *filename);
;;;This is a utility function that converts the given filename to a GFile and calls gtk_media_file_set_file().

;;;Parameters
;;;self

;;;a GtkMediaFile

;;;filename

;;;name of file to play.

;;;[allow-none]
;;;gtk_media_file_set_resource ()
;;;void
;;;gtk_media_file_set_resource (GtkMediaFile *self,
;;;                             const char *resource_path);
;;;This is a utility function that converts the given resource_path to a GFile and calls gtk_media_file_set_file().

;;;Parameters
;;;self

;;;a GtkMediaFile

;;;resource_path

;;;path to resource to play.

;;;[allow-none]
;;;gtk_media_file_set_file ()
;;;void
;;;gtk_media_file_set_file (GtkMediaFile *self,
;;;                         GFile *file);
;;;If any file is still playing, stop playing it.

;;;Then start playing the given file .

;;;Parameters
;;;self

;;;a GtkMediaFile

;;;file

;;;the file to play.

;;;[allow-none]
;;;gtk_media_file_get_file ()
;;;GFile *
;;;gtk_media_file_get_file (GtkMediaFile *self);
;;;Returns the file that self is currently playing from.

;;;When self is not playing or not playing from a file, NULL is returned.

;;;Parameters
;;;self

;;;a GtkMediaFile

;;;Returns
;;;The currently playing file or NULL if not playing from a file.

;;;[nullable][transfer none]

;;;gtk_media_file_set_input_stream ()
;;;void
;;;gtk_media_file_set_input_stream (GtkMediaFile *self,
;;;                                 GInputStream *stream);
;;;If anything is still playing, stop playing it. Then start playing the given stream .

;;;Full control about the stream is assumed for the duration of playback. The stream will not bt be closed.

;;;Parameters
;;;self

;;;a GtkMediaFile

;;;stream

;;;the stream to play from.

;;;[allow-none]
;;;gtk_media_file_get_input_stream ()
;;;GInputStream *
;;;gtk_media_file_get_input_stream (GtkMediaFile *self);
;;;Returns the stream that self is currently playing from.

;;;When self is not playing or not playing from a stream, NULL is returned.

;;;Parameters
;;;self

;;;a GtkMediaFile

;;;Returns
;;;The currently playing stream or NULL if not playing from a stream.

;;;[nullable][transfer none]


;;; --- End of file gtk.media-file.lisp ----------------------------------------

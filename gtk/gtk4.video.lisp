;;; ----------------------------------------------------------------------------
;;; gtk.video.lisp
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

(define-g-object-class "GtkVideo" video
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
   (loop
    video-loop
    "loop" "gboolean" t t)
   (media-stream
    video-media-stream
    "media-stream" "GtkMediaStream" t t)))

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;The “autoplay” property
;;;  “autoplay”                 gboolean
;;;If the video should automatically begin playing.

;;;Owner: GtkVideo

;;;Flags: Read / Write

;;;Default value: FALSE
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;The “file” property
;;;  “file”                     GFile *
;;;The file played by this video if the video is playing a file.

;;;Owner: GtkVideo

;;;Flags: Read / Write
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;The “loop” property
;;;  “loop”                     gboolean
;;;If new media files should be set to loop.

;;;Owner: GtkVideo

;;;Flags: Read / Write

;;;Default value: FALSE
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;The “media-stream” property
;;;  “media-stream”             GtkMediaStream *
;;;The media-stream played

;;;Owner: GtkVideo

;;;Flags: Read / Write
;;; ----------------------------------------------------------------------------


;;; ----------------------------------------------------------------------------
;;; gtk_video_new ()
;;;
;;; GtkWidget *
;;; gtk_video_new (void);
;;;
;;; Creates a new empty GtkVideo.
;;;
;;; Returns :
;;;     a new GtkVideo
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_video_new_for_media_stream ()
;;;
;;; GtkWidget *
;;; gtk_video_new_for_media_stream (GtkMediaStream *stream);
;;;
;;; Creates a GtkVideo to play back the given stream .
;;;
;;; stream :
;;;     a GtkMediaStream.
;;;
;;; Returns :
;;;     a new GtkVideo
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_video_new_for_file ()
;;;
;;; GtkWidget *
;;; gtk_video_new_for_file (GFile *file);
;;;
;;; Creates a GtkVideo to play back the given file .
;;;
;;; file :
;;;     a GFile.
;;;
;;; Returns :
;;;     a new GtkVideo
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;gtk_video_new_for_filename ()
;;;GtkWidget *
;;;gtk_video_new_for_filename (const char *filename);
;;;Creates a GtkVideo to play back the given filename .

;;;This is a utility function that calls gtk_video_new_for_file(), See that function for details.

;;;Parameters
;;;filename

;;;filename to play back.

;;;[allow-none][type filename]
;;;Returns
;;;a new GtkVideo
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_video_new_for_resource ()
;;;
;;; GtkWidget *
;;; gtk_video_new_for_resource (const char *resource_path);
;;;
;;; Creates a GtkVideo to play back the resource at the given resource_path .
;;;
;;; This is a utility function that calls gtk_video_new_for_file(),
;;;
;;; resource_path :
;;;     resource path to play back.
;;;
;;; Returns :
;;;     a new GtkVideo
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_video_get_media_stream ()
;;;
;;; GtkMediaStream *
;;; gtk_video_get_media_stream (GtkVideo *self);
;;;
;;; Gets the media stream managed by self or NULL if none.
;;;
;;; self :
;;;     a GtkVideo
;;;
;;; Returns :
;;;     The media stream managed by self .
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_video_set_media_stream ()
;;;
;;; void
;;; gtk_video_set_media_stream (GtkVideo *self,
;;;                             GtkMediaStream *stream);
;;;
;;; Sets the media stream to be played back. self will take full control of
;;; managing the media stream. If you want to manage a media stream yourself,
;;; consider using a GtkImage for display.
;;;
;;; If you want to display a file, consider using gtk_video_set_file() instead.
;;;
;;; self :
;;;     a GtkVideo
;;;
;;; stream :
;;;     The media stream to play or NULL to unset.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;gtk_video_get_file ()
;;;GFile *
;;;gtk_video_get_file (GtkVideo *self);
;;;Gets the file played by self or NULL if not playing back a file.

;;;Parameters
;;;self

;;;a GtkVideo

;;;Returns
;;;The file played by self .

;;;[nullable][transfer none]
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_video_set_file ()
;;;
;;; void
;;; gtk_video_set_file (GtkVideo *self,
;;;                     GFile *file);
;;;
;;; Makes self play the given file .
;;;
;;; self :
;;;     a GtkVideo
;;;
;;; file :
;;;     the file to play.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_video_set_filename ()
;;;
;;; void
;;; gtk_video_set_filename (GtkVideo *self,
;;;                         const char *filename);
;;;
;;; Makes self play the given filename .
;;;
;;; This is a utility function that calls gtk_video_set_file(),
;;;
;;; self :
;;;     a GtkVideo
;;;
;;; filename :
;;;     the filename to play.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_video_set_filename" video-set-filename) :void
  (video (g:object video))
  (filename :string))

(export 'video-set-filename)

;;; ----------------------------------------------------------------------------
;;; gtk_video_set_resource ()
;;;
;;; void
;;; gtk_video_set_resource (GtkVideo *self,
;;;                         const char *resource_path);
;;;
;;; Makes self play the resource at the given resource_path .
;;;
;;; This is a utility function that calls gtk_video_set_file(),
;;;
;;; self :
;;;     a GtkVideo
;;;
;;; resource_path :
;;;     the resource to set.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_video_get_autoplay ()
;;;
;;; gboolean
;;; gtk_video_get_autoplay (GtkVideo *self);
;;;
;;; Returns TRUE if videos have been set to loop via gtk_video_set_loop().
;;;
;;; self :
;;;     a GtkVideo
;;;
;;; Returns :
;;;     TRUE if streams should autoplay
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_video_set_autoplay ()
;;;
;;; void
;;; gtk_video_set_autoplay (GtkVideo *self,
;;;                         gboolean autoplay);
;;;
;;; Sets whether self automatically starts playback when it becomes visible or
;;; when a new file gets loaded.
;;;
;;; self :
;;;     a GtkVideo
;;;
;;; autoplay :
;;;     whether media streams should autoplay
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_video_get_loop ()
;;;
;;; gboolean
;;; gtk_video_get_loop (GtkVideo *self);
;;;
;;; Returns TRUE if videos have been set to loop via gtk_video_set_loop().
;;;
;;; self :
;;;     a GtkVideo
;;;
;;; Returns :
;;;     TRUE if streams should loop
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_video_set_loop ()
;;;
;;; void
;;; gtk_video_set_loop (GtkVideo *self,
;;;                     gboolean loop);
;;;
;;; Sets whether new files loaded by self should be set to loop.
;;;
;;; self :
;;;     a GtkVideo
;;;
;;; loop :
;;;     whether media streams should loop
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.video.lisp ---------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk.media-controls.lisp
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
;;; GtkMediaControls
;;;
;;;     A widget showing controls for a media stream
;;;
;;; Types and Values
;;;
;;;     GtkMediaControls
;;;
;;; Accessors
;;;
;;;     gtk_media_controls_get_media_stream
;;;     gtk_media_controls_set_media_stream
;;;
;;; Functions
;;;
;;;     gtk_media_controls_new
;;;
;;; Properties
;;;
;;;     media-stream
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkMediaControls
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkMediaControls
;;;
;;; GtkMediaControls is a widget to show controls for a GtkMediaStream and
;;; giving users a way to use it.
;;;
;;; See Also
;;;     GtkVideo
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkMediaControls" media-controls
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget")
   :type-initializer "gtk_media_controls_get_type")
  ((media-stream
    media-controls-media-stream
    "media-stream" "GtkMediaStream" t t)))

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “media-stream” property
;;;
;;;  “media-stream”             GtkMediaStream *
;;;
;;; The media-stream managed by this object or NULL if none.
;;;
;;; Owner: GtkMediaControls
;;;
;;; Flags: Read / Write
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;gtk_media_controls_new ()
;;;GtkWidget *
;;;gtk_media_controls_new (GtkMediaStream *stream);
;;;Creates a new GtkMediaControls managing the stream passed to it.

;;;Parameters
;;;stream

;;;a GtkMediaStream to manage or NULL for none.

;;;[allow-none][transfer none]
;;;Returns
;;;a new GtkMediaControls
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;gtk_media_controls_get_media_stream ()
;;;GtkMediaStream *
;;;gtk_media_controls_get_media_stream (GtkMediaControls *controls);
;;;Gets the media stream managed by controls or NULL if none.

;;;Parameters
;;;controls

;;;a GtkMediaControls

;;;Returns
;;;The media stream managed by controls .
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;gtk_media_controls_set_media_stream ()
;;;void
;;;gtk_media_controls_set_media_stream (GtkMediaControls *controls,
;;;                                     GtkMediaStream *stream);
;;;Sets the stream that is controlled by controls .

;;;Parameters
;;;controls

;;;a GtkMediaControls widget

;;;stream

;;;a GtkMediaStream, or NULL.
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.media-controls.lisp ------------------------------------

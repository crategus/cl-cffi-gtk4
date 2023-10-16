;;; ----------------------------------------------------------------------------
;;; gtk4.media-controls.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.12 and modified to document the Lisp binding to the GTK library.
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
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkMediaControls" media-controls
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget")
   :type-initializer "gtk_media_controls_get_type")
  ((media-stream
    media-controls-media-stream
    "media-stream" "GtkMediaStream" t t)))

#+liber-documentation
(setf (documentation 'media-controls 'type)
 "@version{#2023-4-29}
  @begin{short}
    The @class{gtk:media-controls} widget is a widget to show controls for a
    @class{gtk:media-stream} object and giving users a way to use it.
  @end{short}

  @image[media-controls]{Figure: GtkMediaControls}

  Usually, the @class{gtk:media-controls} widget is used as part of the
  @class{gtk:video} widget.
  @see-class{gtk:media-stream}
  @see-class{gtk:video}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "media-stream"
                                               'media-controls) t)
 "The @code{media-stream} property of type @class{gtk:media-stream}
  (Read / Write) @br{}
  The media stream managed by the media controls.")

#+liber-documentation
(setf (liber:alias-for-function 'media-controls-media-stream)
      "Accessor"
      (documentation 'media-controls-media-stream 'function)
 "@version{#2023-4-29}
  @syntax[]{(gtk:media-controls-media-stream object) => stream}
  @syntax[]{(setf (gtk:media-controls-media-stream object) stream)}
  @argument[object]{a @class{gtk:media-controls} widget}
  @argument[stream]{a @class{gtk:media-stream} object, or @code{nil}}
  @begin{short}
    Accessor of the @slot[gtk:media-controls]{media-stream} slot of the
    @class{gtk:media-controls} class.
  @end{short}
  The @fun{gtk:media-controls-media-stream} function gets the media stream
  managed by the media controls or @code{nil} if none. The
  @setf{gtk:media-controls-media-stream} function sets the media stream.
  @see-class{gtk:media-controls}
  @see-class{gtk:media-stream}")

;;; ----------------------------------------------------------------------------
;;;gtk_media_controls_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline media-controls-new))

(defun media-controls-new (stream)
 #+liber-documentation
 "@version{#2023-4-29}
  @argument[stream]{a @class{gtk:media-stream} object}
  @return{A new @class{gtk:media-controls} widget.}
  @short{Creates the new media controls managing the media stream passed to it.}
  @see-class{gtk:media-controls}
  @see-class{gtk:media-stream}"
  (make-instance 'media-controls
                 :media-stream stream))

(export 'media-controls-new)

;;; --- End of file gtk4.media-controls.lisp -----------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk4.graphics-offload.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.18 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2024 - 2025 Dieter Kaiser
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
;;; Types and values
;;;
;;;     GtkGraphicsOffload
;;;     GtkGraphicsOffloadEnabled
;;;
;;; Accessors
;;;
;;;     gtk_graphics_offload_get_black_background
;;;     gtk_graphics_offload_set_black_background
;;;     gtk_graphics_offload_get_child
;;;     gtk_graphics_offload_set_child
;;;     gtk_graphics_offload_get_enabled
;;;     gtk_graphics_offload_set_enabled
;;;
;;; Functions
;;;
;;;     gtk_graphics_offload_new
;;;
;;; Properties
;;;
;;;     black-background
;;;     child
;;;     enabled
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkGraphicsOffload
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

(gobject:define-genum "GtkGraphicsOffloadEnabled" graphics-offload-enabled
  (:export t
   :type-initializer "gtk_graphics_offload_enabled_get_type")
  (:enabled 0)
  (:disabled 1))

#+liber-documentation
(setf (liber:alias-for-symbol 'graphics-offload-enabled)
      "GEnum"
      (liber:symbol-documentation 'graphics-offload-enabled)
 "@version{2025-07-22}
  @begin{declaration}
(gobject:define-genum \"GtkGraphicsOffloadEnabled\" graphics-offload-enabled
  (:export t
   :type-initializer \"gtk_graphics_offload_enabled_get_type\")
  (:enabled 0)
  (:disabled 1))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:enabled]{Graphics offloading is enabled.}
      @entry[:disabled]{Graphics offloading is disabled.}
    @end{simple-table}
  @end{values}
  @begin{short}
    Represents the state of graphics offloading.
  @end{short}

  Since 4.14
  @see-class{gtk:video}")

;;; ----------------------------------------------------------------------------
;;; GtkGraphicsOffload
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkGraphicsOffload" graphics-offload
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
   :type-initializer "gtk_graphics_offload_get_type")
  ((black-background
    graphics-offload-black-background
    "black-background" "gboolean" t t)
   (child
    graphics-offload-child
    "child" "GktWidget" t t)
   (enabled
    graphics-offload-enabled
    "enabled" "GtkGraphicsOffloadEnabled" t t)))

#+liber-documentation
(setf (documentation 'graphics-offload 'type)
 "@version{2025-07-26}
  @begin{short}
    The @class{gtk:graphics-offload} widget is a widget that allows to bypass
    GSK rendering for its child by passing the content directly to the
    compositor.
  @end{short}
  Graphics offload is an optimization to reduce overhead and battery use that
  is most useful for video content. It only works on some platforms and in
  certain situations. GTK will automatically fall back to normal rendering if
  it does not.

  Graphics offload is most efficient if there are no controls drawn on top of
  the video content. You should consider using graphics offload for your main
  widget if it shows frequently changing content, such as a video, or a VM
  display, and you provide the content in the form of dma-buf textures, see the
  @class{gdk:dmabuf-texture-builder} documentation, in particular if it may be
  fullscreen.

  Numerous factors can prohibit graphics offload:
  @begin{itemize}
    @item{Unsupported platforms. Currently, graphics offload only works on Linux
      with Wayland.}
    @item{Clipping, such as rounded corners that cause the video content to not
      be rectangular.}
    @item{Unsupported dma-buf formats, see the @fun{gdk:display-dmabuf-formats}
      function.}
    @item{Translucent video content, content with an alpha channel, even if it
    is not used.}
    @item{Transforms that are more complex than translations and scales.}
    @item{Filters such as opacity, grayscale or similar.}
  @end{itemize}
  The GTK inspector provides a visual debugging tool for graphics offload.

  Since 4.14
  @see-constructor{gtk:graphics-offload-new}
  @see-slot{gtk:graphics-offload-black-background}
  @see-slot{gtk:graphics-offload-child}
  @see-slot{gtk:graphics-offload-enabled}
  @see-class{gtk:video}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:graphics-offload-black-background ----------------------------------

#+(and gtk-4-16 liber-documentation)
(setf (documentation (liber:slot-documentation "black-background"
                                               'graphics-offload) t)
 "The @code{black-background} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether to draw a black background. Since 4.16 @br{}
  Default value: @em{false}")

#+(and gtk-4-16 liber-documentation)
(setf (liber:alias-for-function 'graphics-offload-black-background)
      "Accessor"
      (documentation 'graphics-offload-black-background 'function)
 "@version{2025-09-27}
  @syntax{(gtk:graphics-offload-black-background object) => setting}
  @syntax{(setf (gtk:graphics-offload-black-background object) setting)}
  @argument[object]{a @class{gtk:graphics-offload} widget}
  @argument[setting]{a boolean whether to draw a black background behind
    the content}
  @begin{short}
    The accessor for the @slot[gtk:graphics-offload]{black-background} slot of
    the @class{gtk:graphics-offload} class gets or sets whether the widget draws
    a black background.
  @end{short}

  A main use case for this is letterboxing where black bars are visible next to
  the content if the aspect ratio of the content does not match the dimensions
  of the monitor. Using this property for letterboxing instead of CSS allows
  compositors to show content with maximum efficiency, using direct scanout to
  avoid extra copies in the compositor.

  On Wayland, this is implemented using the single-pixel buffer protocol.

  Since 4.16
  @see-class{gtk:graphics-offload}")

;;; --- gtk:graphics-offload-child ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "child" 'graphics-offload) t)
 "The @code{child} property of type @class{gtk:widget} (Read / Write) @br{}
  The child widget.")

#+liber-documentation
(setf (liber:alias-for-function 'graphics-offload-child)
      "Accessor"
      (documentation 'graphics-offload-child 'function)
 "@version{2025-09-27}
  @syntax{(gtk:graphics-offload-child object) => child}
  @syntax{(setf (gtk:graphics-offload-child object) child)}
  @argument[object]{a @class{gtk:graphics-offload} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @begin{short}
    The accessor for the @slot[gtk:graphics-offload]{child} slot of the
    @class{gtk:graphics-offload} class gets or sets the child widget.
  @end{short}

  Since 4.14
  @see-class{gtk:graphics-offload}
  @see-class{gtk:widget}")

;;; --- gtk:graphics-offload-enabled -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "enabled" 'graphics-offload) t)
 "The @code{enabled} property of type @sym{gtk:graphics-offload-enabled}
  (Read / Write) @br{}
  Whether graphics offload is enabled. @br{}
  Default value: @val[gtk:graphics-offload-enabled]{:enabled}")

#+liber-documentation
(setf (liber:alias-for-function 'graphics-offload-enabled)
      "Accessor"
      (documentation 'graphics-offload-enabled 'function)
 "@version{2025-09-27}
  @syntax{(gtk:graphics-offload-enabled object) => setting}
  @syntax{(setf (gtk:graphics-offload-enabled object) setting)}
  @argument[object]{a @class{gtk:graphics-offload} widget}
  @argument[setting]{a @sym{gtk:graphics-offload-enabled} value}
  @begin{short}
    The accessor for the @slot[gtk:graphics-offload]{enabled} slot of the
    @class{gtk:graphics-offload} class gets or sets whether the
    @class{gtk:graphics-offload} widget will attempt to offload the content of
    its child widget.
  @end{short}

  Since 4.14
  @see-class{gtk:graphics-offload}
  @see-symbol{gtk:graphics-offload-enabled}")

;;; ----------------------------------------------------------------------------
;;; gtk_graphics_offload_new
;;; ----------------------------------------------------------------------------

(defun graphics-offload-new (&optional child)
 #+liber-documentation
 "@version{2024-11-09}
  @argument[child]{an optional @class{gtk:widget} child widget}
  @return{The new @class{gtk:graphics-offload} widget.}
  @begin{short}
    Creates a new @class{gtk:graphics-offload} widget.
  @end{short}

  Since 4.14
  @see-class{gtk:graphics-offload}
  @see-class{gtk:widget}"
  (make-instance 'graphics-offload
                 :child child))

(export 'graphics-offload-new)

;;; --- End of file gtk4.graphics-offload.lisp ---------------------------------

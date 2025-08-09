;;; ----------------------------------------------------------------------------
;;; gdk4.pixbuf-interaction.lisp
;;;
;;; The documentation in this file is taken from the GDK 4 Reference Manual
;;; version 4.18 and modified to document the Lisp binding to the GDK library,
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
;;; GdkPixbuf Interaction
;;;
;;;     Functions for obtaining pixbufs
;;;
;;; Functions
;;;
;;;     gdk_pixbuf_get_from_surface
;;;     gdk_pixbuf_get_from_texture
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_get_from_surface
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_pixbuf_get_from_surface" %pixbuf-from-surface)
    (g:object gdk-pixbuf:pixbuf)
  (surface (:pointer (:struct cairo:surface-t)))
  (xsrc :int)
  (ysrc :int)
  (width :int)
  (height :int))

(defun pixbuf-from-surface (surface xsrc ysrc width height)
 #+liber-documentation
 "@version{2025-08-02}
  @argument[surface]{a @sym{cairo:surface-t} instance to copy from}
  @argument[xsrc]{an integer for the source x coordinate within @arg{surface}}
  @argument[ysrc]{an integer for the source y coordinate within @arg{surface}}
  @argument[width]{an integer for the width in pixels of region to get}
  @argument[height]{an integer for the height in pixels of region to get}
  @begin{return}
    The newly created @class{gdk-pixbuf:pixbuf} object, or @code{nil} on error.
  @end{return}
  @begin{short}
    Transfers image data from a @sym{cairo:surface-t} instance and converts
    it to an RGB(A) representation inside a @class{gdk-pixbuf:pixbuf} object.
  @end{short}
  This allows you to efficiently read individual pixels from Cairo surfaces.

  This function will create an RGB pixbuf with 8 bits per channel. The pixbuf
  will contain an alpha channel if the surface contains one.
  @begin[Warning]{dictionary}
    This function is deprecated since 4.12. Use the @class{gdk:texture} class
    and subclasses instead of Cairo surfaces and pixbufs.
  @end{dictionary}
  @see-class{gdk-pixbuf:pixbuf}
  @see-class{gdk:texture}
  @see-symbol{cairo:surface-t}"
  #+(and gtk-4-12 gtk-warn-deprecated)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GDK:PIXBUF-FROM-SURFACE is deprecated since 4.12."))
  (%pixbuf-from-surface surface xsrc ysrc width height))

(export 'pixbuf-from-surface)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_get_from_texture
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_pixbuf_get_from_texture" %pixbuf-from-texture)
    (g:object gdk-pixbuf:pixbuf)
  (texture (g:object texture)))

(defun pixbuf-from-texture (texture)
 #+liber-documentation
 "@version{2024-07-16}
  @argument[texture]{a @class{gdk:texture} object}
  @return{The new @class{gdk-pixbuf:pixbuf} object, or @code{nil} on error.}
  @begin{short}
    Creates a new @class{gdk-pixbuf:pixbuf} object from @arg{texture}.
  @end{short}
  This should generally not be used in newly written code as later stages will
  almost certainly convert the pixbuf back into a texture to draw it on screen.
  @begin[Warning]{dictionary}
    This function is deprecated since 4.12. Use the @class{gdk:texture} class
    and subclasses instead of Cairo surfaces and pixbufs.
  @end{dictionary}
  @see-class{gdk:texture}
  @see-class{gdk-pixbuf:pixbuf}"
  #+(and gtk-4-12 gtk-warn-deprecated)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GDK:PIXBUF-FROM-TEXTURE is deprecated since 4.12."))
  (%pixbuf-from-texture texture))

(export 'pixbuf-from-texture)

;;; --- End of file gdk4.pixbuf-interaction.lisp -------------------------------

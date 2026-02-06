;;; ----------------------------------------------------------------------------
;;; gdk4.cairo-interaction.lisp
;;;
;;; The documentation in this file is taken from the GDK 4 Reference Manual
;;; version 4.20 and modified to document the Lisp binding to the GDK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2012 - 2026 Dieter Kaiser
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
;;; Cairo Interaction
;;;
;;;     Functions to support using Cairo
;;;
;;; Functions
;;;
;;;     gdk_cairo_set_source_rgba
;;;     gdk_cairo_set_source_pixbuf                         Deprecated 4.20
;;;     gdk_cairo_rectangle
;;;     gdk_cairo_region
;;;     gdk_cairo_region_create_from_surface
;;;     gdk_cairo_draw_from_gl                              Deprecated 4.6
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; gdk_cairo_set_source_rgba
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_cairo_set_source_rgba" cairo-set-source-rgba) :void
 #+liber-documentation
 "@version{2025-07-30}
  @argument[cr]{a @sym{cairo:context-t} instance}
  @argument[rgba]{a @class{gdk:rgba} color}
  @begin{short}
    Sets the specified @arg{rgba} color as the source color of the Cairo
    context.
  @end{short}
  @begin[Examples]{dictionary}
    This code fragment from the GTK Demo sets a draw function for a
    @class{gtk:drawing-area} widget. The color is parsed to a @struct{gdk:rgba}
    color and then set as the source color for the @fun{cairo:paint} operation
    on the Cairo context.
    @begin{pre}
(gtk:drawing-area-set-draw-func area
        (lambda (widget cr width height)
          (declare (ignore widget width height))
          (let ((rgba (gdk:rgba-parse color)))
            (when rgba
              (gdk:cairo-set-source-rgba cr rgba)
              (cairo:paint cr)))))
    @end{pre}
  @end{dictionary}
  @see-symbol{cairo:context-t}
  @see-class{gdk:rgba}"
  (cr (:pointer (:struct cairo:context-t)))
  (rgba (g:boxed rgba)))

(export 'cairo-set-source-rgba)

;;; ----------------------------------------------------------------------------
;;; gdk_cairo_set_source_pixbuf
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_cairo_set_source_pixbuf" %cairo-set-source-pixbuf) :void
  (cr (:pointer (:struct cairo:context-t)))
  (pixbuf (g:object gdk-pixbuf:pixbuf))
  (x :double)
  (y :double))

(defun cairo-set-source-pixbuf (cr pixbuf x y)
 #+liber-documentation
 "@version{#2025-07-30}
  @argument[cr]{a @sym{cairo:context-t} instance}
  @argument[pixbuf]{a @class{gdk-pixbuf:pixbuf} object}
  @argument[x]{a number coerced to a double float for the x coordinate to
    place upper left corner of @arg{pixbuf}}
  @argument[y]{a number coerced to a double float for the y coordinate to
    place upper left corner of @arg{pixbuf}}
  @begin{short}
    Sets the given @arg{pixbuf} as the source pattern for @arg{cr}.
  @end{short}
  The pattern has a @val[cairo:extend-t]{:none} extend mode of the
  @sym{cairo:extend-t} enumeration and is aligned so that the origin of the
  pixbuf is @code{(x,y)}.
  @begin[Warning]{dictionary}
    This function is deprecated since 4.20.
  @end{dictionary}
  @see-symbol{cairo:context-t}
  @see-symbol{cairo:extend-t}
  @see-class{gdk-pixbuf:pixbuf}"
  #+(and gtk-4-20 gtk-warn-deprecated)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GDK:CAIRO-SET-SOURCE-PIXBUF is deprecated since 4.20."))
  (%cairo-set-source-pixbuf cr
                            pixbuf
                            (coerce x 'double-float)
                            (coerce y 'double-float)))

(export 'cairo-set-source-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gdk_cairo_rectangle
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_cairo_rectangle" cairo-rectangle) :void
 #+liber-documentation
 "@version{#2025-07-30}
  @argument[cr]{a @sym{cairo:context-t} instance}
  @argument[rectangle]{a @class{gdk:rectangle} instance}
  @begin{short}
    Adds the given @arg{rectangle} to the current path of @arg{cr}.
  @end{short}
  @see-symbol{cairo:context-t}
  @see-class{gdk:rectangle}"
  (cr (:pointer (:struct cairo:context-t)))
  (rectangle (g:boxed rectangle)))

(export 'cairo-rectangle)

;;; ----------------------------------------------------------------------------
;;; gdk_cairo_region
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_cairo_region" cairo-region) :void
 #+liber-documentation
 "@version{#2025-07-30}
  @argument[cr]{a @sym{cairo:context-t} instance}
  @argument[region]{a @sym{cairo:region-t} instance}
  @begin{short}
    Adds the given @arg{region} to the current path of @arg{cr}.
  @end{short}
  @see-symbol{cairo:context-t}
  @see-symbol{cairo:region-t}"
  (cr (:pointer (:struct cairo:context-t)))
  (region (:pointer (:struct cairo:region-t))))

(export 'cairo-region)

;;; ----------------------------------------------------------------------------
;;; gdk_cairo_region_create_from_surface
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_cairo_region_create_from_surface"
               cairo-region-create-from-surface)
    (:pointer (:struct cairo:region-t))
 #+liber-documentation
 "@version{#2025-07-30}
  @argument[surface]{a @sym{cairo:surface-t} instance}
  @begin{return}
    The new @sym{cairo:region-t} instance, must be freed with the
    @fun{cairo:region-destroy} function.
  @end{return}
  @begin{short}
    Creates region that describes covers the area where the given surface is
    more than 50% opaque.
  @end{short}
  This function takes into account device offsets that might be set with
  the @setf{cairo:surface-device-offset} function.
  @see-symbol{cairo:surface-t}
  @see-symbol{cairo:region-t}
  @see-function{cairo:region-destroy}
  @see-function{cairo:surface-device-offset}"
  (surface (:pointer (:struct cairo:surface-t))))

(export 'cairo-region-create-from-surface)

;;; ----------------------------------------------------------------------------
;;; gdk_cairo_draw_from_gl                                  Deprecated 4.6
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_cairo_draw_from_gl" %cairo-draw-from-gl) :void
  (cr (:pointer (:struct cairo:context-t)))
  (surface (g:object gdk:surface))
  (source :int)
  (type :int)
  (scale :int)
  (x :int)
  (y :int)
  (width :int)
  (height :int))

(defun cairo-draw-from-gl (cr surface source type scale x y width height)
 #+liber-documentation
 "@version{#2025-07-30}
  @argument[cr]{a @sym{cairo:context-t} instance}
  @argument[surface]{a @class{gdk:surface} object that is being rendered for,
    not necessarily into}
  @argument[source]{an integer for the GL ID of the source buffer}
  @argument[type]{an integer for the type of the source}
  @argument[scale]{an integer for the scale factor that the source buffer is
    allocated for}
  @argument[x]{an integer for the source x position in source to start copying
    from in GL coordinates}
  @argument[y]{an integer for the source y position in source to start copying
    from in GL coordinates}
  @argument[width]{an integer for the width of the region to draw}
  @argument[height]{an integer for the height of the region to draw}
  @begin{short}
    This is the main way to draw GL content in GTK.
  @end{short}
  It takes a render buffer ID @code{(type == GL_RENDERBUFFER)} or a texture ID
  @code{(type == GL_TEXTURE)} and draws it onto @arg{cr} with an
  @code{OVER} operation, respecting the current clip. The top left corner of
  the rectangle specified by @arg{x}, @arg{y}, @arg{width} and @arg{height}
  will be drawn at the current (0,0) position of the Cairo context.

  This will work for all Cairo contexts, as long as @arg{surface} is realized,
  but the fallback implementation that reads back the pixels from the buffer
  may be used in the general case. In the case of direct drawing to a surface
  with no special effects applied to @arg{cr} it will however use a more
  efficient approach.

  For @code{GL_RENDERBUFFER} the code will always fall back to software for
  buffers with alpha components, so make sure you use @code{GL_TEXTURE} if
  using alpha.

  Calling this may change the current GL context.
  @begin[Warning]{dictionary}
    This function is deprecated since 4.6. The function is overly complex and
    produces broken output in various combinations of arguments. If you want to
    draw with GL textures in GTK, use the @fun{gdk:gl-texture-new} function. If
    you want to use that texture in Cairo, use the @fun{gdk:texture-download}
    function to download the data into a Cairo image surface.
  @end{dictionary}
  @see-symbol{cairo:context-t}
  @see-class{gdk:surface}
  @see-function{gdk:gl-texture-new}
  @see-function{gdk:texture-download}"
  #+(and gtk-4-6 gtk-warn-deprecated)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GDK:CAIRO-DRAW-FROM-GL is deprecated since 4.6."))
  (%cairo-draw-from-gl cr surface source type scale x y width height))

(export 'cairo-draw-from-gl)

;;; --- End of file gdk4.cairo-interaction.lisp --------------------------------

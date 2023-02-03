;;; ----------------------------------------------------------------------------
;;; gdk4.cairo-interaction.lisp
;;;
;;; The documentation of this file is taken from the GDK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2012 - 2023 Dieter Kaiser
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
;;; Cairo Interaction
;;;
;;;     Functions to support using Cairo
;;;
;;; Functions
;;;
;;;     gdk_surface_create_similar_surface
;;;     gdk_cairo_set_source_rgba
;;;     gdk_cairo_set_source_pixbuf
;;;     gdk_cairo_rectangle
;;;     gdk_cairo_region
;;;     gdk_cairo_region_create_from_surface
;;;     gdk_cairo_draw_from_gl
;;;
;;; Description
;;;
;;; Cairo is a graphics library that supports vector graphics and image
;;; compositing that can be used with GDK and GTK.
;;;
;;; GDK does not wrap the cairo API, instead it allows to create cairo contexts
;;; which can be used to draw on GdkSurfaces. Additional functions allow use
;;; GdkRectangles with Cairo and to use GdkRGBAs, GdkPixbufs and GdkSurfaces as
;;; sources for drawing operations.
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; gdk_surface_create_similar_surface ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_surface_create_similar_surface" surface-create-similar-surface)
    (:pointer (:struct cairo:surface-t))
 #+liber-documentation
 "@version{#2023-2-3}
  @argument[surface]{a @class{gdk:surface} object to make the new surface
    similar to}
  @argument[content]{a value of the @symbol{cairo:content-t} enumeration for
    the content of the new surface}
  @argument[width]{an integer with the width of the new surface}
  @argument[height]{an integer with the height of the new surface}
  @begin{return}
    A newly allocated @symbol{cairo:surface-t} instance. The caller owns the
    surface and should call the @fun{cairo:surface-destroy} function when done
    with it. This function always returns a valid pointer, but it will return
    a \"nil\" surface if the surface is in an error state.
  @end{return}
  @begin{short}
    Creates a new surface that is as compatible as possible with the given
    @arg{surface}.
  @end{short}
  For example the new surface will have the same fallback resolution and font
  options as @arg{surface}. Generally, the new surface will also use the same
  backend as @arg{surface}, unless that is not possible for some reason. The
  type of the returned surface may be examined with the
  @fun{cairo:surface-type} function.

  Initially the surface contents are all 0, transparent if contents have
  transparency, black otherwise.
  @see-class{gdk:surface}
  @see-symbol{cairo:surface-t}
  @see-symbol{cairo:content-t}
  @see-function{cairo:surface-destroy}
  @see-function{cairo:surface-type}"
  (surface (g:object surface))
  (content cairo:content-t)
  (width :int)
  (height :int))

(export 'surface-create-similar-surface)

;;; ----------------------------------------------------------------------------
;;; gdk_cairo_set_source_rgba ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_cairo_set_source_rgba" cairo-set-source-rgba) :void
 #+liber-documentation
 "@version{#2023-2-3}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[rgba]{a @class{gdk:rgba} color}
  @begin{short}
    Sets the specified @arg{rgba} color as the source color of the Cairo
    context.
  @end{short}
  @see-symbol{cairo:context-t}
  @see-class{gdk:rgba}"
  (cr (:pointer (:struct cairo:context-t)))
  (rgba (g:boxed rgba)))

(export 'cairo-set-source-rgba)

;;; ----------------------------------------------------------------------------
;;; gdk_cairo_set_source_pixbuf ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_cairo_set_source_pixbuf" %cairo-set-source-pixbuf) :void
  (cr (:pointer (:struct cairo:context-t)))
  (pixbuf (g:object gdk-pixbuf:pixbuf))
  (x :double)
  (y :double))

(defun cairo-set-source-pixbuf (cr pixbuf x y)
 #+liber-documentation
 "@version{#2023-2-3}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[pixbuf]{a @class{gdk:pixbuf} object}
  @argument[x]{a number coerced to a double float x coordinate of location to
    place upper left corner of @arg{pixbuf}}
  @argument[y]{a number coerced to a double float y coordinate of location to
    place upper left corner of @arg{pixbuf}}
  @begin{short}
    Sets the given @arg{pixbuf} as the source pattern for @arg{cr}.
  @end{short}
  The pattern has a @code{:none} extend mode of the @symbol{cairo:extend-t}
  enumeration and is aligned so that the origin of the pixbuf is (@arg{x},
  @arg{y}).
  @see-symbol{cairo:context-t}
  @see-symbol{cairo:extend-t}
  @see-class{gdk:pixbuf}"
  (%cairo-set-source-pixbuf cr
                            pixbuf
                            (coerce x 'double-float)
                            (coerce y 'double-float)))

(export 'cairo-set-source-pixbuf)

;;; ----------------------------------------------------------------------------
;;;gdk_cairo_rectangle ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_cairo_rectangle" cairo-rectangle) :void
 #+liber-documentation
 "@version{#2023-2-3}
  @argument[cr]{a @symbol{cairo:context-t} context}
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
;;; gdk_cairo_region ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_cairo_region" cairo-region) :void
 #+liber-documentation
 "@version{#2023-2-3}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[region]{a @symbol{cairo:region-t} instance}
  @begin{short}
    Adds the given @arg{region} to the current path of @arg{cr}.
  @end{short}
  @see-symbol{cairo:context-t}
  @see-symbol{cairo:region-t}"
  (cr (:pointer (:struct cairo:context-t)))
  (region (:pointer (:struct cairo:region-t))))

(export 'cairo-region)

;;; ----------------------------------------------------------------------------
;;; gdk_cairo_region_create_from_surface ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_cairo_region_create_from_surface"
           cairo-region-create-from-surface)
    (:pointer (:struct cairo:region-t))
 #+liber-documentation
 "@version{#2023-2-3}
  @argument[surface]{a @symbol{cairo:surface-t} instance}
  @begin{return}
    A @symbol{cairo:region-t} instance, must be freed with the
    @fun{cairo:region-destroy} function.
  @end{return}
  @begin{short}
    Creates region that describes covers the area where the given surface is
    more than 50% opaque.
  @end{short}
  This function takes into account device offsets that might be set with
  the @fun{cairo:surface-set-device-offset} function.
  @see-symbol{cairo:surface-t}
  @see-symbol{cairo:region-t}
  @see-function{cairo:region-destroy}
  @see-function{cairo:surface-set-device-offset}"
  (surface (:pointer (:struct cairo:surface-t))))

(export 'cairo-region-create-from-surface)

;;; ----------------------------------------------------------------------------
;;; gdk_cairo_draw_from_gl ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_cairo_draw_from_gl" cairo-draw-from-gl) :void
 #+liber-documentation
 "@version{#2023-2-3}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[surface]{a @class{gdk:surface} object we are rendering for,
    not necessarily into}
  @argument[source]{an integer with the GL ID of the source buffer}
  @argument[type]{an integer with the type of the source}
  @argument[scale]{an integer with the scale factor that the source buffer is
    allocated for}
  @argument[x]{an integer with the source x position in source to start copying
    from in GL coordinates}
  @argument[y]{an integer with the source y position in source to start copying
    from in GL coordinates}
  @argument[width]{an integer with the width of the region to draw}
  @argument[height]{an integer with the height of the region to draw}
  @begin{short}
    This is the main way to draw GL content in GTK.
  @end{short}
  It takes a render buffer ID (@arg{type} == @code{GL_RENDERBUFFER}) or a
  texture ID (@arg{type} == @code{GL_TEXTURE}) and draws it onto @arg{cr} with
  an @code{OVER} operation, respecting the current clip. The top left corner of
  the rectangle specified by @arg{x}, @arg{y}, @arg{width} and @arg{height} will
  be drawn at the current (0,0) position of the Cairo context.

  This will work for all Cairo contexts, as long as @arg{surface} is realized,
  but the fallback implementation that reads back the pixels from the buffer
  may be used in the general case. In the case of direct drawing to a surface
  with no special effects applied to @arg{cr} it will however use a more
  efficient approach.

  For @code{GL_RENDERBUFFER} the code will always fall back to software for
  buffers with alpha components, so make sure you use @code{GL_TEXTURE} if
  using alpha.

  Calling this may change the current GL context.
  @see-symbol{cairo:context-t}
  @see-class{gdk:surface}"
  (cr (:pointer (:struct cairo:context-t)))
  (surface (g:object gdk:surface))
  (source :int)
  (type :int)
  (scale :int)
  (x :int)
  (y :int)
  (width :int)
  (height :int))

(export 'cairo-draw-from-gl)

;;; --- End of file gdk4.cairo-interaction.lisp --------------------------------

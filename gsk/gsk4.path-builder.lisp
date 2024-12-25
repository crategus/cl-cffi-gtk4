;;; ----------------------------------------------------------------------------
;;; gsk.path-builder.lisp
;;;
;;; The documentation of this file is taken from the GSK 4 Reference Manual
;;; Version 4.16 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2024 Dieter Kaiser
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
;;; Types and Values
;;;
;;;     GskPathBuilder
;;;
;;; Functions
;;;
;;;     gsk_path_builder_new
;;;
;;;     gsk_path_builder_to_path
;;;     gsk_path_builder_free_to_path
;;;     gsk_path_builder_get_current_point
;;;
;;;     gsk_path_builder_add_rect
;;;     gsk_path_builder_add_circle
;;;     gsk_path_builder_add_layout
;;;     gsk_path_builder_add_path
;;;     gsk_path_builder_add_reverse_path
;;;     gsk_path_builder_add_rounded_rect
;;;     gsk_path_builder_add_segment
;;;     gsk_path_builder_add_cairo_path
;;;
;;;     gsk_path_builder_arc_to
;;;     gsk_path_builder_close
;;;     gsk_path_builder_conic_to
;;;     gsk_path_builder_cubic_to
;;;     gsk_path_builder_html_arc_to
;;;     gsk_path_builder_line_to
;;;     gsk_path_builder_move_to
;;;     gsk_path_builder_quad_to
;;;     gsk_path_builder_rel_arc_to
;;;     gsk_path_builder_rel_conic_to
;;;     gsk_path_builder_rel_cubic_to
;;;     gsk_path_builder_rel_html_arc_to
;;;     gsk_path_builder_rel_line_to
;;;     gsk_path_builder_rel_move_to
;;;     gsk_path_builder_rel_quad_to
;;;     gsk_path_builder_rel_svg_arc_to
;;;     gsk_path_builder_svg_arc_to
;;;
;;;     gsk_path_builder_ref                                not needed
;;;     gsk_path_builder_unref                              not needed
;;; ----------------------------------------------------------------------------

(in-package :gsk)

;;; ----------------------------------------------------------------------------
;;; GskPathBuilder
;;; ----------------------------------------------------------------------------

(glib:define-gboxed-opaque path-builder "GskPathBuilder"
  :export t
  :type-initializer "gsk_path_builder_get_type"
  :alloc (%path-builder-new))

#+liber-documentation
(setf (liber:alias-for-class 'path-builder)
      "GBoxed"
      (documentation 'path-builder 'type)
 "@version{2024-11-12}
  @begin{declaration}
(glib:define-gboxed-opaque path-builder \"GskPathBuilder\"
  :export t
  :type-initializer \"gsk_path_builder_get_type\"
  :alloc (%path-builder-new))
  @end{declaration}
  @begin{short}
    The @class{gsk:path-builder} structure is an auxiliary structure for
    constructing @class{gsk:path} instances.
  @end{short}

  A path is constructed like this:
  @begin{pre}
GskPath *
construct_path (void)
{
  GskPathBuilder *builder;

  builder = gsk_path_builder_new ();

  // add contours to the path here

  return gsk_path_builder_free_to_path (builder);
  @end{pre}
  Adding contours to the path can be done in two ways. The easiest option is to
  use the @sym{gsk:path-builder_add-*} group of functions that add predefined
  contours to the current path, either common shapes like a circle with the
  @fun{gsk:path-builder-add-circle} function or by adding from other paths like
  a path with the @fun{gsk:path-builder-add-path} function. The
  @sym{gsk:path-builder-add-*} methods always add complete contours, and do not
  use or modify the current point.

  The other option is to define each line and curve manually with the
  @fun{gsk:path-builder-*-to} group of functions. You start with a call to the
  @fun{gsk:path-builder-move-to} function to set the starting point and then
  use multiple calls to any of the drawing functions to move the pen along the
  plane. Once you are done, you can call the @fun{gsk:path-builder-close}
  function to close the path by connecting it back with a line to the starting
  point. This is similar to how paths are drawn in Cairo.

  Note that the @class{gsk:path-builder} instance will reduce the degree of
  added Bézier curves as much as possible, to simplify rendering.

  Since 4.14
  @see-constructor{gsk:path-builder-new}
  @see-class{gsk:path}")

;;; ----------------------------------------------------------------------------
;;; gsk_path_builder_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_path_builder_new" %path-builder-new) :pointer)

(cffi:defcfun ("gsk_path_builder_new" path-builder-new)
    (g:boxed path-builder :return)
 #+liber-documentation
 "@version{2024-11-13}
  @return{The new @class{gsk:path-builder} instance.}
  @begin{short}
    Create a new @class{gsk:path-builder} instance.
  @end{short}
  The resulting builder would create an empty @class{gsk:path} instance. Use
  addition functions to add types to it.

  Since 4.14
  @see-class{gsk:path-builder}
  @see-class{gsk:path}")

(export 'path-builder-new)

;;; ----------------------------------------------------------------------------
;;; gsk_path_builder_to_path
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_path_builder_to_path" path-builder-to-path)
    (g:boxed path :return)
 #+liber-documentation
 "@version{2024-11-13}
  @argument[builder]{a @class{gsk:path-builder} instance}
  @return{The newly created @class{gsk:path} instance with all the contours
    added to the path builder.}
  @begin{short}
    Creates a new @class{gsk:path} instance from the given path builder.
  @end{short}
  The given @class{gsk:path-builder} instance is reset once this function
  returns. You cannot call this function multiple times on the same builder
  instance.

  Since 4.14
  @see-class{gsk:path-builder}
  @see-class{gsk:path}"
  (builder (g:boxed path-builder)))

(export 'path-builder-to-path)

;;; ----------------------------------------------------------------------------
;;; gsk_path_builder_free_to_path                           not needed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gsk_path_builder_get_current_point
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_path_builder_get_current_point" %path-builder-current-point)
    (:pointer (:struct graphene:point-t))
  (builder (g:boxed path-builder)))

(defun path-builder-current-point (path point)
 #+liber-documentation
 "@version{#2024-11-13}
  @argument[builder]{a @class{gsk:path-builder} instance}
  @argument[point]{a @symbol{graphene:point-t} instance for the current point}
  @return{The @symbol{graphene:point-t} instance with the current point.}
  @begin{short}
    Gets the current point.
  @end{short}
  The current point is used for relative drawing commands and updated after
  every operation.

  When the builder is created, the default current point is set to (0, 0). Note
  that this is different from Cairo, which starts out without a current point.

  Since 4.14
  @see-class{gsk:path-builder}
  @see-symbol{graphene:point-t}"
  (graphene:point-init-from-point point
                                  (%path-builder-current-point path))
  point)

(export 'path-builder-current-point)

;;; ----------------------------------------------------------------------------
;;; gsk_path_builder_close
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_path_builder_close" path-builder-close) :void
 #+liber-documentation
 "@version{#2024-11-14}
  @argument[builder]{a @class{gsk:path-builder} instance}
  @begin{short}
    Ends the current contour with a line back to the start point.
  @end{short}
  Note that this is different from calling the @fun{gsk:path-builder-line-to}
  function with the start point in that the contour will be closed. A closed
  contour behaves differently from an open one. When stroking, its start and
  end point are considered connected, so they will be joined via the line join,
  and not ended with line caps.

  Since 4.14
  @see-class{gsk:path-builder}
  @see-function{gsk:path-builder-line-to}"
  (builder (g:boxed path-builder)))

(export 'path-builder-close)

;;; ----------------------------------------------------------------------------
;;; gsk_path_builder_add_rect
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_path_builder_add_rect" path-builder-add-rect) :void
 #+liber-documentation
 "@version{2024-11-13}
  @argument[builder]{a @class{gsk:path-builder} instance}
  @argument[rect]{a @symbol{graphene:rect-t} instance with the rectangle
    to create a path for}
  @begin{short}
    Adds @arg{rect} as a new contour to the path built by the path builder.
  @end{short}
  The path is going around the rectangle in clockwise direction.

  If the the width or height are 0, the path will be a closed horizontal or
  vertical line. If both are 0, it will be a closed dot.

  Since 4.14
  @see-class{gsk:path-builder}
  @see-symbol{graphene:rect-t}"
  (builder (g:boxed path-builder))
  (rect (:pointer (:struct graphene:rect-t))))

(export 'path-builder-add-rect)

;;; ----------------------------------------------------------------------------
;;; gsk_path_builder_add_circle
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_path_builder_add_circle" %path-builder-add-circle) :void
  (builder (g:boxed path-builder))
  (center (:pointer (:struct graphene:point-t)))
  (radius :float))

(defun path-builder-add-circle (builder center radius)
 #+liber-documentation
 "@version{2023-11-13}
  @argument[builder]{a @class{gsk:path-builder} instance}
  @argument[center]{a @symbol{graphene:point-t} instance with the center of
    the circle}
  @argument[radius]{a number coerced to a single float with the radius of
    the circle}
  @begin{short}
    Adds a circle with @arg{center} and @arg{radius}.
  @end{short}
  The path is going around the circle in clockwise direction. If @arg{radius}
  is zero, the contour will be a closed point.

  Since 4.14
  @see-class{gsk:path-builder}
  @see-symbol{graphene:point-t}"
  (%path-builder-add-circle builder center (coerce radius 'single-float)))

(export 'path-builder-add-circle)

;;; ----------------------------------------------------------------------------
;;; gsk_path_builder_add_layout
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_path_builder_add_layout" path-builder-add-layout) :void
 #+liber-documentation
 "@version{2024-11-13}
  @argument[builder]{a @class{gsk:path-builder} instance}
  @argument[layout]{a @class{pango:layout} instance with the Pango layout to
    add}
  @begin{short}
    Adds the outlines for the glyphs in @arg{layout} to the path builder.
  @end{short}

  Since 4.14
  @see-class{gsk:path-builder}
  @see-class{pango:layout}"
  (builder (g:boxed path-builder))
  (layout (g:object pango:layout)))

(export 'path-builder-add-layout)

;;; ----------------------------------------------------------------------------
;;; gsk_path_builder_add_path
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_path_builder_add_path" path-builder-add-path) :void
 #+liber-documentation
 "@version{2024-11-13}
  @argument[builder]{a @class{gsk:path-builder} instance}
  @argument[path]{a @class{gsk:path} instance with the path to append}
  @begin{short}
    Appends all of @arg{path} to the path builder.
  @end{short}

  Since 4.14
  @see-class{gsk:path-builder}
  @see-class{gsk:path}"
  (builder (g:boxed path-builder))
  (path (g:boxed path)))

(export 'path-builder-add-path)

;;; ----------------------------------------------------------------------------
;;; gsk_path_builder_add_reverse_path
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_path_builder_add_reverse_path"
               path-builder-add-reverse-path) :void
 #+liber-documentation
 "@version{2024-11-13}
  @argument[builder]{a @class{gsk:path-builder} instance}
  @argument[path]{a @class{gsk:path} instance with the path to append}
  @begin{short}
    Appends all of @arg{path} to the path builder, reverse order.
  @end{short}

  Since 4.14
  @see-class{gsk:path-builder}
  @see-class{gsk:path}"
  (builder (g:boxed path-builder))
  (path (g:boxed path)))

(export 'path-builder-add-reverse-path)

;;; ----------------------------------------------------------------------------
;;; gsk_path_builder_add_rounded_rect
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_path_builder_add_rounded_rect"
               path-builder-add-rounded-rect) :void
 #+liber-documentation
 "@version{2024-11-13}
  @argument[builder]{a @class{gsk:path-builder} instance}
  @argument[rect]{a @symbol{gsk:rounded-rect} instance with the rounded
    rectangle}
  @begin{short}
    Adds @arg{rect} as a new contour to the path built in @arg{builder}.
  @end{short}
  The path is going around the rectangle in clockwise direction.

  Since 4.14
  @see-class{gsk:path-builder}
  @see-symbol{gsk:rounded-rect}"
  (builder (g:boxed path-builder))
  (rect (:pointer (:struct rounded-rect))))

(export 'path-builder-add-rounded-rect)

;;; ----------------------------------------------------------------------------
;;; gsk_path_builder_add_segment
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_path_builder_add_segment" path-builder-add-segment) :void
 #+liber-documentation
 "@version{2024-11-13}
  @argument[builder]{a @class{gsk:path-builder} instance}
  @argument[path]{a @class{gsk:path} instance with the path to take the
    segment to}
  @argument[start]{a @class{gsk:path-point} instance with the point on
    @arg{path} to start at}
  @argument[end]{a @class{gsk:path-point} instance with the point on
    @arg{path} to end at}
  @begin{short}
    Adds to @arg{builder} the segment of @arg{path} from @arg{start} to
    @arg{end}.
  @end{short}
  If @arg{start} is equal to or after @arg{end}, the path will first add the
  segment from @arg{start} to @arg{end} of the path, and then add the segment
  from the beginning to @arg{end}. If the path is closed, these segments will be
  connected.

  Note that this method always adds a path with the given @arg{start} point and
  @arg{end} point. To add a closed path, use the @fun{gsk:path-builder-add-path}
  functin.

  Since 4.14
  @see-class{gsk:path-builder}
  @see-class{gsk:path}
  @see-class{gsk:path-point}
  @see-function{gsk:path-builder-add-path}"
  (builder (g:boxed path-builder))
  (path (g:boxed path))
  (start (g:boxed path-point))
  (end (g:boxed path-point)))

(export 'path-builder-add-segment)

;;; ----------------------------------------------------------------------------
;;; gsk_path_builder_add_cairo_path
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_path_builder_add_cairo_path" path-builder-add-cairo-path)
    :void
 #+liber-documentation
 "@version{2024-11-13}
  @argument[builder]{a @class{gsk:path-builder} instance}
  @argument[context]{a @symbol{cairo:path-t} instance with a path}
  @begin{short}
    Adds a Cairo path to the path builder.
  @end{short}
  You can use the @fun{cairo:copy-path} function to access the path from a
  Cairo context.

  Since 4.14
  @see-class{gsk:path-builder}
  @see-symbol{cairo:path-t}
  @see-function{cairo:copy-path}"
  (builder (g:boxed path-builder))
  (path (:pointer (:struct cairo:path-t))))

(export 'path-builder-add-cairo-path)

;;; ----------------------------------------------------------------------------
;;; gsk_path_builder_move_to
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_path_builder_move_to" %path-builder-move-to) :void
  (builder (g:boxed path-builder))
  (x :float)
  (y :float))

(defun path-builder-move-to (builder x y)
 #+liber-documentation
 "@version{#2024-11-14}
  @argument[builder]{a @class{gsk:path-builder} instance}
  @argument[x]{a number coerced to a single float with the X coordinate}
  @argument[y]{a number coerced to a single float with the Y coordinate}
  @begin{short}
    Starts a new contour by placing the pen at @arg{x, y}.
  @end{short}
  If this function is called twice in succession, the first call will result in
  a contour made up of a single point. The second call will start a new contour.

  Since 4.14
  @see-class{gsk:path-builder}"
  (%path-builder-move-to builder (coerce x 'single-float)
                                 (coerce y 'single-float)))

(export 'path-builder-move-to)

;;; ----------------------------------------------------------------------------
;;; gsk_path_builder_rel_move_to
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_path_builder_rel_move_to" %path-builder-rel-move-to) :void
  (builder (g:boxed path-builder))
  (x :float)
  (y :float))

(defun path-builder-rel-move-to (builder x y)
 #+liber-documentation
 "@version{#2024-11-14}
  @argument[builder]{a @class{gsk:path-builder} instance}
  @argument[x]{a number coerced to a single float with the X offset}
  @argument[y]{a number coerced to a single float with the Y offset}
  @begin{short}
    Starts a new contour by placing the pen at @arg{x, y} relative to the
    current point.
  @end{short}
  This is the relative version of the @fun{gsk:path-builder-move-to} function.

  Since 4.14
  @see-class{gsk:path-builder}
  @see-function{gsk:path-builder-move-to}"
  (%path-builder-rel-move-to builder (coerce x 'single-float)
                                     (coerce y 'single-float)))

(export 'path-builder-rel-move-to)

;;; ----------------------------------------------------------------------------
;;; gsk_path_builder_line_to
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_path_builder_line_to" %path-builder-line-to) :void
  (builder (g:boxed path-builder))
  (x :float)
  (y :float))

(defun path-builder-line-to (builder x y)
 #+liber-documentation
 "@version{#2024-11-14}
  @argument[builder]{a @class{gsk:path-builder} instance}
  @argument[x]{a number coerced to a single float with the X coordinate}
  @argument[y]{a number coerced to a single float with the Y coordinate}
  @begin{short}
    Draws a line from the current point to @arg{x, y} and makes it the new
    current point.
  @end{short}

  @image[line-light]{Figure: Draw line}

  Since 4.14
  @see-class{gsk:path-builder}
  @see-function{gsk:path-builder-move-to}"
  (%path-builder-rel-move-to builder (coerce x 'single-float)
                                     (coerce y 'single-float)))

(export 'path-builder-line-to)

;;; ----------------------------------------------------------------------------
;;; gsk_path_builder_rel_line_to
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_path_builder_rel_line_to" %path-builder-rel-line-to) :void
  (builder (g:boxed path-builder))
  (x :float)
  (y :float))

(defun path-builder-rel-line-to (builder x y)
 #+liber-documentation
 "@version{#2024-11-14}
  @argument[builder]{a @class{gsk:path-builder} instance}
  @argument[x]{a number coerced to a single float with the X offset}
  @argument[y]{a number coerced to a single float with the Y offset}
  @begin{short}
    Draws a line from the current point to a point offset from it by @arg{x, y}
    and makes it the new current point.
  @end{short}
  This is the relative version of @fun{gsk:path-builder-line-to} function.
  Since 4.14
  @see-class{gsk:path-builder}
  @see-function{gsk:path-builder-line-to}"
  (%path-builder-rel-line-to builder (coerce x 'single-float)
                                     (coerce y 'single-float)))

(export 'path-builder-rel-line-to)

;;; ----------------------------------------------------------------------------
;;; gsk_path_builder_arc_to
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_path_builder_arc_to" %path-builder-arc-to) :void
  (builder (g:boxed path-builder))
  (x1 :float)
  (y1 :float)
  (x2 :float)
  (y2 :float))

(defun path-builder-arc-to (builder x1 y1 x2 y2)
 #+liber-documentation
 "@version{#2024-11-14}
  @argument[builder]{a @class{gsk:path-builder} instance}
  @argument[x1]{a number coerced to a single float with the X coordinate of
    the first control point}
  @argument[y1]{a number coerced to a single float with the Y coordinate of
    the first control point}
  @argument[x2]{a number coerced to a single float with the X coordinate of
    the second control point}
  @argument[y2]{a number coerced to a single float with the Y coordinate of
    the second control point}
  @begin{short}
    Adds an elliptical arc from the current point to @arg{x2, y2} with
    @arg{x1, y1} determining the tangent directions.
  @end{short}
  After this, @arg{x2, y2} will be the new current point.

  Note: Two points and their tangents do not determine a unique ellipse, so GSK
  just picks one. If you need more precise control, use the
  @fun{gsk:path-builder-conic-to} or @fun{gsk:path-builder-svg-arc-to}
  functions.

  @image[arc-light]{Figure: Draw arc}

  Since 4.14
  @see-class{gsk:path-builder}
  @see-function{gsk:path-builder-conic-to}
  @see-function{gsk:path-builder-svg-arg-to}"
  (%path-builder-arc-to builder (coerce x1 'single-float)
                                (coerce y1 'single-float)
                                (coerce x2 'single-float)
                                (coerce y2 'single-float)))

(export 'path-builder-arc-to)

;;; ----------------------------------------------------------------------------
;;; gsk_path_builder_rel_arc_to
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_path_builder_rel_arc_to" %path-builder-rel-arc-to) :void
  (builder (g:boxed path-builder))
  (x1 :float)
  (y1 :float)
  (x2 :float)
  (y2 :float))

(defun path-builder-rel-arc-to (builder x1 y1 x2 y2)
 #+liber-documentation
 "@version{#2024-11-14}
  @argument[builder]{a @class{gsk:path-builder} instance}
  @argument[x1]{a number coerced to a single float with the X coordinate of
    the first control point}
  @argument[y1]{a number coerced to a single float with the Y coordinate of
    the first control point}
  @argument[x2]{a number coerced to a single float with the X coordinate of
    the second control point}
  @argument[y2]{a number coerced to a single float with the Y coordinate of
    the second control point}
  @begin{short}
    Adds an elliptical arc from the current point to @arg{x2, y2} with
    @arg{x1, y1} determining the tangent directions.
  @end{short}
  All coordinates are given relative to the current point. This is the
  relative version of the @fun{gsk:path-builder-arc-to} function.

  Since 4.14
  @see-class{gsk:path-builder}
  @see-function{gsk:path-builder-conic-to}
  @see-function{gsk:path-builder-svg-arg-to}"
  (%path-builder-rel-arc-to builder (coerce x1 'single-float)
                                    (coerce y1 'single-float)
                                    (coerce x2 'single-float)
                                    (coerce y2 'single-float)))

(export 'path-builder-rel-arc-to)

;;; ----------------------------------------------------------------------------
;;; gsk_path_builder_conic_to
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_path_builder_conic_to" %path-builder-conic-to) :void
  (builder (g:boxed path-builder))
  (x1 :float)
  (y1 :float)
  (x2 :float)
  (y2 :float)
  (weight :float))

(defun path-builder-conic-to (builder x1 y1 x2 y2 weight)
 #+liber-documentation
 "@version{#2024-11-14}
  @argument[builder]{a @class{gsk:path-builder} instance}
  @argument[x1]{a number coerced to a single float with the X coordinate of
    the first control point}
  @argument[y1]{a number coerced to a single float with the Y coordinate of
    the first control point}
  @argument[x2]{a number coerced to a single float with the X coordinate of
    the second control point}
  @argument[y2]{a number coerced to a single float with the Y coordinate of
    the second control point}
  @argument[weight]{a number coerced to a single float with the weight of the
    control point, must be greater than zero}
  @begin{short}
    Adds a conic curve from the current point to @arg{x2, y2} with the given
    @arg{weight} and @arg{x1, y1} as the control point.
  @end{short}
  The weight determines how strongly the curve is pulled towards the control
  point. A conic with weight 1 is identical to a quadratic Bézier curve with
  the same points.

  Conic curves can be used to draw ellipses and circles. They are also known as
  rational quadratic Bézier curves.

  After this, @arg{x2, y2} will be the new current point.

  @image[conic-light]{Figure: Draw conic curve}

  Since 4.14
  @see-class{gsk:path-builder}"
  (%path-builder-conic-to builder (coerce x1 'single-float)
                                  (coerce y1 'single-float)
                                  (coerce x2 'single-float)
                                  (coerce y2 'single-float)
                                  (coerce weight 'single-float)))

(export 'path-builder-conic-to)

;;; ----------------------------------------------------------------------------
;;; gsk_path_builder_rel_conic_to
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_path_builder_rel_conic_to" %path-builder-rel-conic-to) :void
  (builder (g:boxed path-builder))
  (x1 :float)
  (y1 :float)
  (x2 :float)
  (y2 :float)
  (weight :float))

(defun path-builder-rel-conic-to (builder x1 y1 x2 y2 weight)
 #+liber-documentation
 "@version{#2024-11-14}
  @argument[builder]{a @class{gsk:path-builder} instance}
  @argument[x1]{a number coerced to a single float with the X coordinate of
    the first control point}
  @argument[y1]{a number coerced to a single float with the Y coordinate of
    the first control point}
  @argument[x2]{a number coerced to a single float with the X coordinate of
    the second control point}
  @argument[y2]{a number coerced to a single float with the Y coordinate of
    the second control point}
  @argument[weight]{a number coerced to a single float with the weight of the
    control point, must be greater than zero}
  @begin{short}
    Adds a conic curve from the current point to @arg{x2, y2} with the given
    @arg{weight} and @arg{x1, y1} as the control point.
  @end{short}
  All coordinates are given relative to the current point. This is the relative
  version of the @fun{gsk:path-builder-conic-to} function.

  Since 4.14
  @see-class{gsk:path-builder}
  @see-function{gsk:path-builder-conic-to}"
  (%path-builder-rel-conic-to builder (coerce x1 'single-float)
                                      (coerce y1 'single-float)
                                      (coerce x2 'single-float)
                                      (coerce y2 'single-float)
                                      (coerce weight 'single-float)))

(export 'path-builder-rel-conic-to)

;;; ----------------------------------------------------------------------------
;;; gsk_path_builder_cubic_to
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_path_builder_cubic_to" %path-builder-cubic-to) :void
  (builder (g:boxed path-builder))
  (x1 :float)
  (y1 :float)
  (x2 :float)
  (y2 :float)
  (x3 :float)
  (y3 :float))

(defun path-builder-cubic-to (builder x1 y1 x2 y2 x3 y3)
 #+liber-documentation
 "@version{#2024-11-14}
  @argument[builder]{a @class{gsk:path-builder} instance}
  @argument[x1]{a number coerced to a single float with the X coordinate of
    the first control point}
  @argument[y1]{a number coerced to a single float with the Y coordinate of
    the first control point}
  @argument[x2]{a number coerced to a single float with the X coordinate of
    the second control point}
  @argument[y2]{a number coerced to a single float with the Y coordinate of
    the second control point}
  @argument[x3]{a number coerced to a single float with the X coordinate of
    the end of the curve}
  @argument[y2]{a number coerced to a single float with the Y coordinate of
    the end of the curve}
  @begin{short}
    Adds a cubic Bézier curve from the current point to @arg{x3, y3} with
    @arg{x1, y1} and @arg{x2, y2} as the control points.
  @end{short}
  After this, @arg{x3, y3} will be the new current point.

  @imgage[cubic-light]{Figure: Draw cubic}

  @see-class{gsk:path-builder}"
  (%path-builder-cubic-to builder (coerce x1 'single-float)
                                  (coerce y1 'single-float)
                                  (coerce x2 'single-float)
                                  (coerce y2 'single-float)
                                  (coerce x3 'single-float)
                                  (coerce y3 'single-float)))

(export 'path-builder-cubic-to)

;;; ----------------------------------------------------------------------------
;;; gsk_path_builder_rel_cubic_to
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_path_builder_rel_cubic_to" %path-builder-rel-cubic-to) :void
  (builder (g:boxed path-builder))
  (x1 :float)
  (y1 :float)
  (x2 :float)
  (y2 :float)
  (x3 :float)
  (y3 :float))

(defun path-builder-rel-cubic-to (builder x1 y1 x2 y2 x3 y3)
 #+liber-documentation
 "@version{#2024-11-14}
  @argument[builder]{a @class{gsk:path-builder} instance}
  @argument[x1]{a number coerced to a single float with the X coordinate of
    the first control point}
  @argument[y1]{a number coerced to a single float with the Y coordinate of
    the first control point}
  @argument[x2]{a number coerced to a single float with the X coordinate of
    the second control point}
  @argument[y2]{a number coerced to a single float with the Y coordinate of
    the second control point}
  @argument[x3]{a number coerced to a single float with the X coordinate of
    the end of the curve}
  @argument[y3]{a number coerced to a single float with the Y coordinate of
    the end of the curve}
  @begin{short}
    Adds a cubic Bézier curve from the current point to @arg{x3, y3} with
    @arg{x1, y1} and @arg{x2, y2} as the control points.
  @end{short}
  All coordinates are given relative to the current point. This is the relative
  version of the @fun{gsk:path-builder-cubic-to} function.
  @see-class{gsk:path-builder}
  @see-function{gsk:path-builder-cubic-to}"
  (%path-builder-rel-cubic-to builder (coerce x1 'single-float)
                                      (coerce y1 'single-float)
                                      (coerce x2 'single-float)
                                      (coerce y2 'single-float)
                                      (coerce x3 'single-float)
                                      (coerce y3 'single-float)))

(export 'path-builder-rel-cubic-to)

;;; ----------------------------------------------------------------------------
;;; gsk_path_builder_quad_to
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_path_builder_quad_to" %path-builder-quad-to) :void
  (builder (g:boxed path-builder))
  (x1 :float)
  (y1 :float)
  (x2 :float)
  (y2 :float))

(defun path-builder-quad-to (builder x1 y1 x2 y2)
 #+liber-documentation
 "@version{#2024-11-14}
  @argument[builder]{a @class{gsk:path-builder} instance}
  @argument[x1]{a number coerced to a single float with the X coordinate of
    the first control point}
  @argument[y1]{a number coerced to a single float with the Y coordinate of
    the first control point}
  @argument[x2]{a number coerced to a single float with the X coordinate of
    the end of the curve}
  @argument[y2]{a number coerced to a single float with the Y coordinate of
    the end of the curve}
  @begin{short}
    Adds a quadratic Bézier curve from the current point to @arg{x2, y2} with
    @arg{x1, y1} as the control point.
  @end{short}
  After this, @arg{x2, y2} will be the new current point.

  @imgage[quad-light]{Figure: Draw quad}

  @see-class{gsk:path-builder}"
  (%path-builder-quad-to builder (coerce x1 'single-float)
                                 (coerce y1 'single-float)
                                 (coerce x2 'single-float)
                                 (coerce y2 'single-float)))

(export 'path-builder-quad-to)

;;; ----------------------------------------------------------------------------
;;; gsk_path_builder_rel_quad_to
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_path_builder_rel_quad_to" %path-builder-rel-quad-to) :void
  (builder (g:boxed path-builder))
  (x1 :float)
  (y1 :float)
  (x2 :float)
  (y2 :float))

(defun path-builder-rel-quad-to (builder x1 y1 x2 y2)
 #+liber-documentation
 "@version{#2024-11-14}
  @argument[builder]{a @class{gsk:path-builder} instance}
  @argument[x1]{a number coerced to a single float with the X coordinate of
    the first control point}
  @argument[y1]{a number coerced to a single float with the Y coordinate of
    the first control point}
  @argument[x2]{a number coerced to a single float with the X coordinate of
    the end of the curve}
  @argument[y2]{a number coerced to a single float with the Y coordinate of
    the end of the curve}
  @begin{short}
    Adds a quadratic Bézier curve from the current point to @arg{x2, y2} with
    @arg{x1, y1} as the control point.
  @end{short}
  All coordinates are given relative to the current point. This is the relative
  version of the @fun{gsk:path-builder-quad-to} function.
  @see-class{gsk:path-builder}
  @see-function{gsk:path-builder-quad-to}"
  (%path-builder-quad-to builder (coerce x1 'single-float)
                                 (coerce y1 'single-float)
                                 (coerce x2 'single-float)
                                 (coerce y2 'single-float)))

(export 'path-builder-rel-quad-to)

;;; ----------------------------------------------------------------------------
;;; gsk_path_builder_html_arc_to
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_path_builder_html_arc_to" %path-builder-html-arc-to) :void
  (builder (g:boxed path-builder))
  (x1 :float)
  (y1 :float)
  (x2 :float)
  (y2 :float)
  (radius :float))

(defun path-builder-html-arc-to (builder x1 y1 x2 y2 radius)
 #+liber-documentation
 "@version{#2024-11-14}
  @argument[builder]{a @class{gsk:path-builder} instance}
  @argument[x1]{a number coerced to a single float with the X coordinate of
    the first control point}
  @argument[y1]{a number coerced to a single float with the Y coordinate of
    the first control point}
  @argument[x2]{a number coerced to a single float with the X coordinate of
    the second control point}
  @argument[y2]{a number coerced to a single float with the Y coordinate of
    the second control point}
  @argument[radius]{a number coerced to a single float with the radius of
    the circle}
  @begin{short}
    Implements arc-to according to the HTML Canvas spec.
  @end{short}
  A convenience function that implements the HTML arc-to functionality.

  After this, the current point will be the point where the circle with the
  given @arg{radius} touches the line from @arg{x1, y1 to x2, y2}.

  Since 4.14
  @see-class{gsk:path-builder}
  @see-function{gsk:path-builder-arc-to}"
 (%path-builder-html-arc-to builder x1 y1 x2 y2 radius))

(export 'path-builder-html-arc-to)

;;; ----------------------------------------------------------------------------
;;; gsk_path_builder_rel_html_arc_to
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_path_builder_rel_html_arc_to" %path-builder-rel-html-arc-to)
    :void
  (builder (g:boxed path-builder))
  (x1 :float)
  (y1 :float)
  (x2 :float)
  (y2 :float)
  (radius :float))

(defun path-builder-rel-html-arc-to (builder x1 y1 x2 y2 radius)
 #+liber-documentation
 "@version{#2024-11-14}
  @argument[builder]{a @class{gsk:path-builder} instance}
  @argument[x1]{a number coerced to a single float with the X coordinate of
    the first control point}
  @argument[y1]{a number coerced to a single float with the Y coordinate of
    the first control point}
  @argument[x2]{a number coerced to a single float with the X coordinate of
    the second control point}
  @argument[y2]{a number coerced to a single float with the Y coordinate of
    the second control point}
  @argument[radius]{a number coerced to a single float with the radius of
    the circle}
  @begin{short}
    Implements arc-to according to the HTML Canvas spec.
  @end{short}
  All coordinates are given relative to the current point. This is the relative
  version of the @fun{gsk:path-builder-html-arc-to} function.

  Since 4.14
  @see-class{gsk:path-builder}
  @see-function{gsk:path-builder-html-arc-to}"
 (%path-builder-rel-html-arc-to builder x1 y1 x2 y2 radius))

(export 'path-builder-rel-html-arc-to)

;;; ----------------------------------------------------------------------------
;;; gsk_path_builder_svg_arc_to
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_path_builder_svg_arc_to" %path-builder-svg-arc-to) :void
  (builder (g:boxed path-builder))
  (rx :float)
  (ry :float)
  (x-axis-rotation :float)
  (large-arc :boolean)
  (positive-sweep :boolean)
  (x :float)
  (y :float))

(defun path-builder-svg-arc-to (builder rx ry
                                        x-axis-rotation
                                        large-arc
                                        positive-sweep
                                        x y)
 #+liber-documentation
 "@version{#2024-11-14}
  @argument[rx]{a number coerced to a single float with the X radius}
  @argument[ry]{a number coerced to a single float with the Y radius}
  @argument[x-axis-rotation]{a number coerced to a single float with the
    rotation of the ellipsis}
  @argument[large-arc]{whether to add the large arc}
  @argument[positive-sweep]{whether to sweep in the positive direction}
  @argument[x]{a number coerced to a single float with the X coordinate
    of the end point}
  @argument[y]{a number coerced to a single float with the Y coordinate
    of the end point}
  @begin{short}
    Implements arc-to according to the SVG spec.
  @end{short}
  A convenience function that implements the SVG arc-to functionality.
  After this, @arg{x, y} will be the new current point.

  Since 4.14
  @see-class{gsk:path-builder}
  @see-function{gsk:path-builder-arc-to}"
  (%path-builder-svg-arc-to builder (coerce rx 'single-float)
                                    (coerce ry 'single-float)
                                    (coerce x-axis-rotation 'single-float)
                                    large-arc
                                    positive-sweep
                                    (coerce x 'single-float)
                                    (coerce y 'single-float)))

(export 'path-builder-svg-arc-to)

;;; ----------------------------------------------------------------------------
;;; gsk_path_builder_rel_svg_arc_to
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_path_builder_rel_svg_arc_to" %path-builder-rel-svg-arc-to)
    :void
  (builder (g:boxed path-builder))
  (rx :float)
  (ry :float)
  (x-axis-rotation :float)
  (large-arc :boolean)
  (positive-sweep :boolean)
  (x :float)
  (y :float))

(defun path-builder-rel-svg-arc-to (builder rx ry
                                            x-axis-rotation
                                            large-arc
                                            positive-sweep
                                            x y)
 #+liber-documentation
 "@version{#2024-11-14}
  @argument[rx]{a number coerced to a single float with the X radius}
  @argument[ry]{a number coerced to a single float with the Y radius}
  @argument[x-axis-rotation]{a number coerced to a single float with the
    rotation of the ellipsis}
  @argument[large-arc]{whether to add the large arc}
  @argument[positive-sweep]{whether to sweep in the positive direction}
  @argument[x]{a number coerced to a single float with the X coordinate
    of the end point}
  @argument[y]{a number coerced to a single float with the Y coordinate
    of the end point}
  @begin{short}
    Implements arc-to according to the SVG spec.
  @end{short}
  All coordinates are given relative to the current point. This is the relative
  version of the @fun{gsk:path-builder-svg-arc-to} function.

  Since 4.14
  @see-class{gsk:path-builder}
  @see-function{gsk:path-builder-svg-arc-to}"
  (%path-builder-rel-svg-arc-to builder (coerce rx 'single-float)
                                        (coerce ry 'single-float)
                                        (coerce x-axis-rotation 'single-float)
                                        large-arc
                                        positive-sweep
                                        (coerce x 'single-float)
                                        (coerce y 'single-float)))

(export 'path-builder-rel-svg-arc-to)

;;; ----------------------------------------------------------------------------
;;; gsk_path_builder_ref                                    not needed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gsk_path_builder_unref                                  not needed
;;; ----------------------------------------------------------------------------

;;; --- End of file gsk4.path-builder.lisp -------------------------------------

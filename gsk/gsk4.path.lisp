;;; ----------------------------------------------------------------------------
;;; gsk.path.lisp
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
;;;     GskPathMeasure
;;;     GskPathPoint
;;;     GskStroke
;;;     GskPath
;;;
;;;     GskPathForeachFlags
;;;     GskPathOperation
;;;     GskPathDirection
;;;     GskLineCap
;;;     GskLineJoin
;;;     GskFillRule
;;;
;;; Functions
;;;
;;;     gsk_path_measure_new
;;;     gsk_path_measure_new_with_tolerance
;;;     gsk_path_measure_get_length
;;;     gsk_path_measure_get_path
;;;     gsk_path_measure_get_point
;;;     gsk_path_measure_get_tolerance
;;;     gsk_path_measure_ref
;;;     gsk_path_measure_unref
;;;
;;;     gsk_path_point_get_curvature
;;;     gsk_path_point_get_distance
;;;     gsk_path_point_get_position
;;;     gsk_path_point_get_rotation
;;;     gsk_path_point_get_tangent
;;;     gsk_path_point_compare
;;;     gsk_path_point_copy
;;;     gsk_path_point_equal
;;;     gsk_path_point_free
;;;
;;;     gsk_stroke_new
;;;     gsk_stroke_copy
;;;     gsk_stroke_free
;;;     gsk_stroke_equal
;;;     gsk_stroke_get_dash
;;;     gsk_stroke_set_dash
;;;     gsk_stroke_get_dash_offset
;;;     gsk_stroke_set_dash_offset
;;;     gsk_stroke_get_line_cap
;;;     gsk_stroke_set_line_cap
;;;     gsk_stroke_get_line_join
;;;     gsk_stroke_set_line_join
;;;     gsk_stroke_get_line_width
;;;     gsk_stroke_set_line_width
;;;     gsk_stroke_get_miter_limit
;;;     gsk_stroke_set_miter_limit
;;;     gsk_stroke_to_cairo
;;;
;;;     gsk_path_parse
;;;
;;;     GskPathForeachFunc
;;;     gsk_path_foreach
;;;
;;;     gsk_path_get_bounds
;;;     gsk_path_get_closest_point
;;;     gsk_path_get_start_point
;;;     gsk_path_get_end_point
;;;     gsk_path_get_stroke_bounds
;;;     gsk_path_in_fill
;;;     gsk_path_is_closed
;;;     gsk_path_is_empty
;;;     gsk_path_print
;;;     gsk_path_ref
;;;     gsk_path_unref
;;;     gsk_path_to_cairo
;;;     gsk_path_to_string

;;; ----------------------------------------------------------------------------

(in-package :gsk)

;;; ----------------------------------------------------------------------------
;;; GskPathForeachFlags
;;; ----------------------------------------------------------------------------

(gobject:define-gflags "GskPathForeachFlags" path-foreach-flags
  (:export t
   :type-initializer "gsk_path_foreach_flags_get_type")
  (:allow-only-lines 0)
  (:allow-quad #.(ash 1 0))
  (:allow-cubic #.(ash 1 1))
  (:allow-conic #.(ash 1 2)))

#+liber-documentation
(setf (liber:alias-for-symbol 'path-foreach-flags)
      "GFlags"
      (liber:symbol-documentation 'path-foreach-flags)
 "@version{2024-11-9}
  @begin{declaration}
(gobject:define-gflags \"GskPathForeachFlags\" path-foreach-flags
  (:export t
   :type-initializer \"gsk_path_foreach_flags_get_type\")
  (:allow-only-lines 0)
  (:allow-quad #.(ash 1 0))
  (:allow-cubic #.(ash 1 1))
  (:allow-conic #.(ash 1 2)))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:allow-only-lines]{The default behavior, only allow lines.}
      @entry[:allow-quad]{Allow emission of @code{:quad} operations.}
      @entry[:allow-cubic]{Allow emission of @code{:cubic} operations.}
      @entry[:allow-conic]{Allow emission of @code{:conic} operations.}
    @end{table}
  @end{values}
  @begin{short}
    Flags that can be passed to the @fun{gsk:path-foreach} function to influence
    what kinds of operations the path is decomposed into.
  @end{short}
  By default, the @fun{gsk:path-foreach} function will only emit a path with all
  operations flattened to straight lines to allow for maximum compatibility. The
  only operations emitted will be @code{:move}, @code{:line} and @code{:close}.

  Since 4.14
  @see-class{gsk:path}
  @see-symbol{gsk:path-operation}
  @see-function{gsk:path-foreach}")

;;; ----------------------------------------------------------------------------
;;; GskPathOperation
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GskPathOperation" path-operation
  (:export t
   :type-initializer "gsk_path_operation_get_type")
  :move
  :close
  :line
  :quad
  :cubic
  :conic)

#+liber-documentation
(setf (liber:alias-for-symbol 'path-operation)
      "GEnum"
      (liber:symbol-documentation 'path-operation)
 "@version{2024-11-9}
  @begin{declaration}
(gobject:define-genum \"GskPathOperation\" path-operation
  (:export t
   :type-initializer \"gsk_path_operation_get_type\")
  :move
  :close
  :line
  :quad
  :cubic
  :conic)
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:move]{A move-to operation, with 1 point describing the target
        point.}
      @entry[:close]{A close operation ending the current contour with a line
        back to the starting point. Two points describe the start and end of
        the line.}
      @entry[:line]{A line-to operation, with 2 points describing the start and
        end point of a straight line.}
      @entry[:quad]{A curve-to operation describing a quadratic Bézier curve
        with 3 points describing the start point, the control point and the end
          point of the curve.}
      @entry[:cubic]{A curve-to operation describing a cubic Bézier curve with
        4 points describing the start point, the two control points and the end
        point of the curve.}
      @entry[:conic]{A rational quadratic Bézier curve with 3 points describing
        the start point, control point and end point of the curve. A weight for
        the curve will be passed, too.}
    @end{table}
  @end{values}
  @begin{short}
    Path operations are used to describe the segments of a @class{gsk:path}
    instance.
  @end{short}

  Since 4.14
  @see-class{gsk:path}")

;;; ----------------------------------------------------------------------------
;;; GskPathDirection
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GskPathDirection" path-direction
  (:export t
   :type-initializer "gsk_path_direction_get_type")
  :from-start
  :to-start
  :to-end
  :from-end)

#+liber-documentation
(setf (liber:alias-for-symbol 'path-direction)
      "GEnum"
      (liber:symbol-documentation 'path-direction)
 "@version{2024-11-9}
  @begin{declaration}
(gobject:define-genum \"GskPathDirection\" path-direction
  (:export t
   :type-initializer \"gsk_path_direction_get_type\")
  :from-start
  :to-start
  :to-end
  :from-end)
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:from-start]{The tangent in path direction of the incoming side of
        the path.}
      @entry[:to-start]{The tangent against path direction of the incoming side
        of the path.}
      @entry[:to-end]{The tangent in path direction of the outgoing side of
        the path.}
      @entry[:from-end]{The tangent against path direction of the outgoing side
        of the path.}
    @end{table}
  @end{values}
  @begin{short}
    The values of the @symbol{gsk:path-direction} enumeration are used to pick
    one of the four tangents at a given point on the path.
  @end{short}
  Note that the directions for @code{:from-start}/@code{:to-end} and
  @code{:to-start}/@code{:from-end} will coincide for smooth points. Only sharp
  turns will exhibit four different directions.

  @image[directions-light]{Figure: GtkPathDirection}

  Since 4.14
  @see-class{gsk:path}")

;;; ----------------------------------------------------------------------------
;;; GskLineCap
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GskLineCap" line-cap
  (:export t
   :type-initializer "gsk_line_cap_get_type")
  :butt
  :round
  :square)

#+liber-documentation
(setf (liber:alias-for-symbol 'line-cap)
      "GEnum"
      (liber:symbol-documentation 'line-cap)
 "@version{2024-11-9}
  @begin{declaration}
(gobject:define-genum \"GskLineCap\" line-cap
  (:export t
   :type-initializer \"gsk_line_cap_get_type\")
  :butt
  :round
  :square)
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:butt]{Start and stop the line exactly at the start and end point.}
      @entry[:round]{Use a round ending, the center of the circle is the start
        or end point.}
      @entry[:square]{Use squared ending, the center of the square is the start
        or end point.}
    @end{table}
  @end{values}
  @begin{short}
    Specifies how to render the start and end points of contours or dashes when
    stroking.
  @end{short}
  The default line cap style is @code{:butt}.

  @image[caps-light]{Figure: GskLineCap}

  Since 4.16
  @see-class{gsk:path}
  @see-class{gsk:stroke}")

;;; ----------------------------------------------------------------------------
;;; GskLineJoin
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GskLineJoin" line-join
  (:export t
   :type-initializer "gsk_line_join_get_type")
  :miter
  :round
  :bevel)

#+liber-documentation
(setf (liber:alias-for-symbol 'line-join)
      "GEnum"
      (liber:symbol-documentation 'line-join)
 "@version{2024-11-9}
  @begin{declaration}
(gobject:define-genum \"GskLineJoin\" line-join
  (:export t
   :type-initializer \"gsk_line_join_get_type\")
  :miter
  :round
  :bevel)
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:miter]{Use a sharp angled corner.}
      @entry[:round]{Use a round join, the center of the circle is the join
        point.}
      @entry[:bevel]{Use a cut-off join, the join is cut off at half the line
        width from the joint point.}
    @end{table}
  @end{values}
  @begin{short}
    Specifies how to render the junction of two lines when stroking.
  @end{short}
  The default line join style is @code{:miter}.

  @image[join-light]{Figure: GskLineJoin}

  Since 4.14
  @see-class{gsk:path}
  @see-class{gsk:stroke}")

;;; ----------------------------------------------------------------------------
;;; GskFillrule
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GskFillRule" fill-rule
  (:export t
   :type-initializer "gsk_fill_rule_get_type")
  :winding
  :even-odd)

#+liber-documentation
(setf (liber:alias-for-symbol 'fill-rule)
      "GEnum"
      (liber:symbol-documentation 'fill-rule)
 "@version{2024-11-9}
  @begin{declaration}
(gobject:define-genum \"GskFillRule\" fill-rule
  (:export t
   :type-initializer \"gsk_fill_rule_get_type\")
  :winding
  :even-odd)
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:winding]{If the path crosses the ray from left-to-right, counts
        +1. If the path crosses the ray from right to left, counts -1. (Left
        and right are determined from the perspective of looking along the ray
        from the starting point.) If the total count is non-zero, the point
        will be filled.}
      @entry[:even-odd]{Counts the total number of intersections, without
        regard to the orientation of the contour. If the total number of
        intersections is odd, the point will be filled.}
    @end{table}
  @end{values}
  @begin{short}
    The @symbol{gsk:fill-rule} enumeration is used to select how paths are
    filled.
  @end{short}
  Whether or not a point is included in the fill is determined by taking a ray
  from that point to infinity and looking at intersections with the path. The
  ray can be in any direction, as long as it does not pass through the end
  point of a segment or have a tricky intersection such as intersecting tangent
  to the path.

  Note that filling is not actually implemented in this way. This is just a
  description of the rule that is applied.

  Since 4.14
  @see-class{gsk:path}")

;;; ----------------------------------------------------------------------------
;;; GskPathMeasure
;;; ----------------------------------------------------------------------------

(glib:define-gboxed-opaque path-measure "GskPathMeasure"
  :export t
  :type-initializer "gsk_path_measure_get_type"
  :alloc (error "GSK:PATH-MEASURE cannot be created from the Lisp side"))

#+liber-documentation
(setf (liber:alias-for-class 'path-measure)
      "GBoxed"
      (documentation 'path-measure 'type)
 "@version{2024-11-8}
  @begin{declaration}
(glib:define-gboxed-opaque path-measure \"GskPathMeasure\"
  :export t
  :type-initializer \"gsk_path_measure_get_type\"
  :alloc (error \"GSK:PATH-MEASURE cannot be created from the Lisp side\"))
  @end{declaration}
  @begin{short}
    The @class{gsk:path-measure} structure is a structure that allows
    measurements on @class{gsk:path} intances such as determining the length of
    the path.
  @end{short}
  Many measuring operations require sampling the path length at intermediate
  points. Therefore, a @class{gsk:path-measure} instance has a tolerance that
  determines what precision is required for such approximations.

  Since 4.14
  @see-constructor{gsk:path-measure-new}
  @see-constructor{gsk:path-measure-new-with-tolerance}
  @see-class{gsk:path}")

;;; ----------------------------------------------------------------------------
;;; GskPathPoint
;;; ----------------------------------------------------------------------------

;; We have no method in the C library to allocate a GskPathPoint instance.
;; This implementation allows the allocation of the GskPathPoint instance from
;; the Lisp side.

(cffi:defcstruct %path-point
  (contour :size)
  (idx :size)
  (t :float))

(cffi:defcfun ("gsk_path_point_copy" %path-point-copy) :pointer
  (point :pointer))

(defun %path-point-alloc ()
  (cffi:with-foreign-object (point '(:struct %path-point))
    (%path-point-copy point)))

;;; ----------------------------------------------------------------------------

(glib:define-gboxed-opaque path-point "GskPathPoint"
  :export t
  :type-initializer "gsk_path_point_get_type"
  :alloc (%path-point-alloc))

#+liber-documentation
(setf (liber:alias-for-class 'path-point)
      "GBoxed"
      (documentation 'path-point 'type)
 "@version{2024-11-7}
  @begin{declaration}
(glib:define-gboxed-opaque path-point \"GskPathPoint\"
  :export t
  :type-initializer \"gsk_path_point_get_type\"
  :alloc (error \"GSK:PATH-POINT cannot be created from the Lisp side\"))
  @end{declaration}
  @begin{short}
    The @class{gsk:path-point} structure is an opaque type representing a point
    on a path.
  @end{short}
  It can be queried for properties of the path at that point, such as its
  tangent or its curvature.

  To obtain a @class{gsk:path-point} instance, use the
  @fun{gsk:path-closest-point}, @fun{gsk:path-start-point},
  @fun{gsk:path-end-point} or @fun{gsk:path-measure-point} functions.

  Note that @class{gsk:path-point} instances are meant to be stack-allocated,
  and do not hold a reference to the path object they are obtained from. It is
  the callers responsibility to keep a reference to the path as long as the
  @class{gsk:path-point} instance is used.

  Since 4.14
  @see-constructor{gsk:path-point-copy}
  @see-class{gsk:path}")

;;; ----------------------------------------------------------------------------
;;; GskStroke
;;; ----------------------------------------------------------------------------

(glib:define-gboxed-opaque stroke "GskStroke"
  :export t
  :type-initializer "gsk_stroke_get_type"
  :alloc (error "GSK:STROKE cannot be created from the Lisp side"))

#+liber-documentation
(setf (liber:alias-for-class 'stroke)
      "GBoxed"
      (documentation 'stroke 'type)
 "@version{2024-11-9}
  @begin{declaration}
(glib:define-gboxed-opaque stroke \"GskStroke\"
  :export t
  :type-initializer \"gsk_stroke_get_type\"
  :alloc (error \"GSK:STROKE cannot be created from the Lisp side\"))
  @end{declaration}
  @begin{short}
    The @class{gsk:stroke} structure collects the parameters that influence the
    operation of stroking a path.
  @end{short}

  Since 4.14
  @see-constructor{gsk:stroke-new}
  @see-constructor{gsk:stroke-copy}
  @see-class{gsk:path}")

;;; ----------------------------------------------------------------------------
;;; GskPath
;;; ----------------------------------------------------------------------------

(glib:define-gboxed-opaque path "GskPath"
  :export t
  :type-initializer "gsk_path_get_type"
  :alloc (error "GSK:PATH cannot be created from the Lisp side"))

#+liber-documentation
(setf (liber:alias-for-class 'path)
      "GBoxed"
      (documentation 'path 'type)
 "@version{2024-11-9}
  @begin{declaration}
(glib:define-gboxed-opaque path \"GskPath\"
  :export t
  :type-initializer \"gsk_path_get_type\"
  :alloc (error \"GSK:PATH cannot be created from the Lisp side\"))
  @end{declaration}
  @begin{short}
    A @class{gsk:path} structure describes lines and curves that are more
    complex than simple rectangles.
  @end{short}
  Paths can used for rendering (filling or stroking) and for animations, for
  eample as trajectories.

  The @class{gsk:path} structure is an immutable, opaque, reference-counted
  structure. After creation, you cannot change the types it represents. Instead,
  new @class{gsk:path} instances have to be created. The
  @class{gsk:path-builder} structure is meant to help in this endeavor.

  Conceptually, a path consists of zero or more contours (continuous, connected
  curves), each of which may or may not be closed. Contours are typically
  constructed from Bézier segments.

  @image[path-light]{Figure: GskPath}

  Since 4.14
  @see-class{gsk:path-builder}")

;;; ----------------------------------------------------------------------------
;;; gsk_path_measure_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_path_measure_new" path-measure-new)
    (g:boxed path-measure :return)
 #+liber-documentation
 "@version{2024-11-8}
  @argument[path]{a @class{gsk:path} instance with the path to measure}
  @return{The new @class{gsk:path-measure} instance representing @arg{path}.}
  @begin{short}
    Creates a measure object for the given @arg{path} with the default
    tolerance.
  @end{short}

  Since 4.14
  @see-class{gsk:path-measure}
  @see-class{gsk:path}"
  (path (g:boxed path)))

(export 'path-measure-new)

;;; ----------------------------------------------------------------------------
;;; gsk_path_measure_new_with_tolerance
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_path_measure_new_with_tolerance"
               %path-measure-new-with-tolerance) (g:boxed path-measure :return)
  (path (g:boxed path))
  (tolerance :float))

(defun path-measure-new-with-tolerance (path tolerance)
 #+liber-documentation
 "@version{2024-11-8}
  @argument[path]{a @class{gsk:path} instance with the path to measure}
  @argument[tolerance]{a number coerced to a float with the tolerance for
    measuring operations}
  @return{The new @class{gsk:path-measure} instance representing @arg{path}.}
  @begin{short}
    Creates a measure object for the given @arg{path} and @arg{tolerance}.
  @end{short}

  Since 4.14
  @see-class{gsk:path-measure}
  @see-class{gsk:path}"
  (%path-measure-new-with-tolerance path (coerce tolerance 'single-float)))

(export 'path-measure-new-with-tolerance)

;;; ----------------------------------------------------------------------------
;;; gsk_path_measure_get_length
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_path_measure_get_length" path-measure-length) :float
 #+liber-documentation
 "@version{2024-11-8}
  @argument[measure]{a @class{gsk:path-measure} instance}
  @return{The float with the length of the path measured by @arg{measure.}}
  @begin{short}
    Gets the length of the path being measured.
  @end{short}
  The length is cached, so this function does not do any work.

  Since 4.14
  @see-class{gsk:path-measure}"
  (measure (g:boxed path-measure)))

(export 'path-measure-length)

;;; ----------------------------------------------------------------------------
;;; gsk_path_measure_get_path
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_path_measure_get_path" path-measure-path) (g:boxed path)
 #+liber-documentation
 "@version{2024-11-8}
  @argument[measure]{a @class{gsk:path-measure} instance}
  @return{The @class{gsk:path} instance of @arg{measure}.}
  @begin{short}
    Returns the path that the measure was created for.
  @end{short}

  Since 4.14
  @see-class{gsk:path-measure}"
  (measure (g:boxed path-measure)))

(export 'path-measure-path)

;;; ----------------------------------------------------------------------------
;;; gsk_path_measure_get_point
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_path_measure_get_point" %path-measure-point) :boolean
  (measure (g:boxed path-measure))
  (distance :float)
  (result (g:boxed path-point)))

(defun path-measure-point (measure distance)
 #+liber-documentation
 "@version{2024-11-9}
  @argument[measure]{a @class{gsk:path-measure} instance}
  @argument[distance]{a number coerced to a float with the distance}
  @begin{short}
    Returns the point at the given distance into the path.
  @end{short}
  An empty path has no points, so @code{nil} is returned in that case.

  Since 4.14
  @see-class{gsk:path-measure}"
  (let ((result (make-instance 'path-point))
        (distance (coerce distance 'single-float)))
    (when (%path-measure-point measure distance result)
      result)))

(export 'path-measure-point)

;;; ----------------------------------------------------------------------------
;;; gsk_path_measure_get_tolerance
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_path_measure_get_tolerance" path-measure-tolerance) :float
 #+liber-documentation
 "@version{2024-11-9}
  @argument[measure]{a @class{gsk:path-measure} instance}
  @return{The float with the tolerance of @arg{measure}}
  @begin{short}
    Returns the tolerance that the measure was created with.
  @end{short}

  Since 4.14
  @see-class{gsk:path-measure}"
  (measure (g:boxed path-measure)))

(export 'path-measure-tolerance)

;;; ----------------------------------------------------------------------------
;;; gsk_path_measure_ref                                    not needed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gsk_path_measure_unref                                  not needed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gsk_path_point_get_curvature
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_path_point_get_curvature" path-point-curvature) :float
 #+liber-documentation
 "@version{2024-11-9}
  @argument[point]{a @class{gsk:path-point} instance}
  @argument[path]{a @class{gsk:path} instance with the path that @arg{point}
    is on}
  @argument[direction]{a @symbol{gsk:path-direction} value with the direction
    for which to return the curvature}
  @argument[center]{a @symbol{graphene:point-t} instance which will be set
    with the center of the osculating circle}
  @return{The float with the curvature of the path at the given @arg{point}.}
  @begin{short}
    Calculates the curvature of the path at the point.
  @end{short}
  Returns the center of the osculating circle as well. The curvature is the
  inverse of the radius of the osculating circle.

  Lines have a curvature of zero, indicating an osculating circle of infinite
  radius. In this case, the center is not modified. Circles with a radius of
  zero have @code{INFINITY} as curvature.

  Note that certain points on a path may not have a single curvature, such as
  sharp turns. At such points, there are two curvatures - the limit of the
  curvature of the path going into the point, and the limit of the curvature
  of the path coming out of it. The direction argument lets you choose which
  one to get.

  @image[curvature-light]{Figure: Curvature at point}
  @see-class{gsk:path-point}
  @see-class{gsk:path}
  @see-symbol{graphene:point-t}"
  (point (g:boxed path-point))
  (path (g:boxed path))
  (direction path-direction)
  (center (:pointer (:struct graphene:point-t))))

(export 'path-point-curvature)

;;; ----------------------------------------------------------------------------
;;; gsk_path_point_get_distance
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_path_point_get_distance" path-point-distance) :float
 #+liber-documentation
 "@version{#2024-11-9}
  @argument[point]{a @class{gsk:path-point} instance}
  @argument[measure]{a @class{gsk:path-measure} instance for the path}
  @return{The float with the distance of @arg{point}.}
  @begin{short}
    Returns the distance from the beginning of the path to point.
  @end{short}

  Since 4.14
  @see-class{gsk:path-point}
  @see-class{gsk:path-measure}"
  (point (g:boxed path-point))
  (measure (g:boxed path-measure)))

(export 'path-point-distance)

;;; ----------------------------------------------------------------------------
;;; gsk_path_point_get_position
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_path_point_position" %path-point-position) :void
  (point (g:boxed path-point))
  (path (g:boxed path))
  (pos (:pointer (:struct graphene:point-t))))

(defun path-point-position (point path pos)
 #+liber-documentation
 "@version{#2024-11-9}
  @argument[point]{a @class{gsk:path-point} instance}
  @argument[path]{a @class{gsk:path} instance with the path @arg{point} is on}
  @argument[pos]{a @symbol{graphene:point-t} instance for the coordinates of
    the point}
  @return{The @symbol{graphene:point-t} instance.}
  @begin{short}
    Gets the position of the point.
  @end{short}

  Since 4.14
  @see-class{gsk:path-point}
  @see-class{gsk:path}
  @see-symbol{graphene:point-t}"
  (%path-point-position point path pos)
  pos)

(export 'path-point-position)

;;; ----------------------------------------------------------------------------
;;; gsk_path_point_get_rotation
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_path_point_get_rotation" path-point-rotation) :float
 #+liber-documentation
 "@version{#2024-11-9}
  @argument[point]{a @class{gsk:path-point} instance}
  @argument[path]{a @class{gsk:path} instance with the path that @arg{point}
    is on}
  @argument[direction]{a @symbol{gsk:path-direction} value with the direction
    for which to return the rotation}
  @return{The float with the angle between the tangent and the X axis,
    in degrees.}
  @begin{short}
    Gets the direction of the tangent at a given point.
  @end{short}
  This is a convenience variant of the @fun{gsk:path-point-tangent} function
  that returns the angle between the tangent and the X axis. The angle can,
  for example, be used in the @fun{gtk:snapshot-rotate} function.

  Since 4.14
  @see-class{gsk:path-point}
  @see-class{gsk:path}
  @see-symbol{gsk:path-direction}
  @see-function{gsk:path-point-tangent}
  @see-function{gtk:snapshot-rotate}"
  (point (g:boxed path-point))
  (path (g:boxed path))
  (direction path-direction))

(export 'path-point-rotation)

;;; ----------------------------------------------------------------------------
;;; gsk_path_point_get_tangent
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_path_point_get_tangent" %path-point-tangent) :void
  (point (g:boxed path-point))
  (path (g:boxed path))
  (direction path-direction)
  (tangent (:pointer (:struct graphene:vec2-t))))

(defun path-point-tangent (point path direction tangent)
 #+liber-documentation
 "@version{#2024-11-9}
  @argument[point]{a @class{gsk:path-point} instance}
  @argument[path]{a @class{gsk:path} instance with the path that @arg{point}
    is on}
  @argument[direction]{a @symbol{gsk:path-direction} value with the direction
    for which to return the tangent}
  @argument[tangent]{a @symbol{graphene:vec2-t} instance for the tangent at
    the point}
  @return{The @symbol{graphene:vec2-t} instance.}
  @begin{short}
    Gets the tangent of the path at the point.
  @end{short}
  Note that certain points on a path may not have a single tangent, such as
  sharp turns. At such points, there are two tangents - the direction of the
  path going into the point, and the direction coming out of it. The direction
  argument lets you choose which one to get.

  If the path is just a single point, for example, a circle with radius zero,
  then tangent is set to @code{0, 0}.

  If you want to orient something in the direction of the path, the
  @fun{gsk:path-point-rotation} function may be more convenient to use.

  Since 4.14
  @see-class{gsk:path-point}
  @see-class{gsk:path}
  @see-symbol{gsk:path-direction}
  @see-symbol{graphene:vec2-t}
  @see-function{gsk:path-point-rotation}"
  (%path-point-tangent point path direction tangent)
  tangent)

(export 'path-point-tangent)

;;; ----------------------------------------------------------------------------
;;; gsk_path_point_compare
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_path_point_compare" path-point-compare) :int
 #+liber-documentation
 "@version{#2024-11-9}
  @argument[point1]{a @class{gsk:path-point} instance}
  @argument[point2]{another @class{gsk:path-point} instance}
  @return{The integer which is -1 if @arg{point1} is before @arg{point2},
    1 if @arg{point1} is after @arg{point2}, 0 if they are equal.}
  @begin{short}
    Returns whether @arg{point1} is before or after @arg{point2}.
  @end{short}

  Since 4.14
  @see-class{gsk:path-point}"
  (point1 (g:boxed path-point))
  (point2 (g:boxed path-point)))

(export 'path-point-compare)

;;; ----------------------------------------------------------------------------
;;; gsk_path_point_copy
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_path_point_copy" path-point-copy)
    (g:boxed path-point :return)
 #+liber-documentation
 "@version{#2024-11-9}
  @argument[point]{a @class{gsk:path-point} instance}
  @return{The copied @class{gsk:path-point} instance.}
  @short{Copies a path point.}
  @see-class{gsk:path-point-copy}"
  (point (g:boxed path-point)))

(export 'path-point-copy)

;;; ----------------------------------------------------------------------------
;;; gsk_path_point_equal
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_path_point_equal" path-point-equal) :boolean
 #+liber-documentation
 "@version{#2024-11-9}
  @argument[point1]{a @class{gsk:path-point} instance}
  @argument[point2]{another @class{gsk:path-point} instance}
  @return{@em{True} if @arg{point1} and @arg{point2} are equal.}
  @begin{short}
    Returns whether the two path points refer to the same location on all paths.
  @end{short}
  Note that the start and end point of a closed contour will compare nonequal
  according to this definition. Use the @fun{gsk:path-is-closed} function to
  find out if the start and end point of a concrete path refer to the same
  location.

  Since 4.14
  @see-class{gsk:path-point}"
  (point1 (g:boxed path-point))
  (point2 (g:boxed path-point)))

(export 'path-point-equal)

;;; ----------------------------------------------------------------------------
;;; gsk_path_point_free                                     not needed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gsk_stroke_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_stroke_new" %stroke-new) (g:boxed stroke :return)
  (width :float))

(defun stroke-new (width)
 #+liber-documentation
 "@version{#2024-11-11}
  @argument[width]{a number coerced to a float with the line width of the
    stroke, must be > 0}
  @return{The new @class{gsk:stroke} instance.}
  @short{Creates a new @class{gsk:stroke} instance with the given @arg{width}.}

  Since 4.14
  @see-class{gsk:stroke}"
  (%stroke-new (coerce width 'single-float)))

(export 'stroke-new)

;;; ----------------------------------------------------------------------------
;;; gsk_stroke_copy
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_stroke_copy" stroke-copy) (g:boxed stroke :return)
 #+liber-documentation
 "@version{#2024-11-11}
  @argument[stroke]{a @class{gsk:stroke} instance}
  @return{The new @class{gsk:stroke} instance.}
  @short{Creates a copy of the given @arg{stroke}.}
  @see-class{gsk:stroke}"
  (stroke (g:boxed stroke)))

(export 'stroke-copy)

;;; ----------------------------------------------------------------------------
;;; gsk_stroke_free                                         not needed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gsk_stroke_equal
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_stroke_equal" stroke-equal) :boolean
 #+liber-documentation
 "@version{#2024-11-11}
  @argument[stoke1]{a @class{gsk:stroke} instance}
  @argument[stoke2]{another @class{gsk:stroke} instance}
  @return{@em{True} if the two strokes are equal, @em{false} otherwise.}
  @short{Checks if two strokes are identical.}
  @see-class{gsk:stroke}"
  (stroke1 (g:boxed stroke))
  (stroke2 (g:boxed stroke)))

(export 'stroke-equal)

;;; ----------------------------------------------------------------------------
;;; gsk_stroke_get_dash
;;; gsk_stroke_set_dash
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_stroke_set_dash" %stroke-set-dash) :void
  (stroke (g:boxed stroke))
  (dash (:pointer :float))
  (n :size))

(defun (setf stroke-dash) (dash stroke)
  (let ((n (length dash)))
    (cffi:with-foreign-object (ptr :float n)
      (iter (for i from 0)
            (for val in dash)
            (setf (cffi:mem-aref ptr :float i)
                  (coerce val 'single-float)))
      (%stroke-set-dash stroke ptr n)))
  dash)

(cffi:defcfun ("gsk_stroke_get_dash" %stroke-dash) (:pointer :float)
  (stroke (g:boxed stroke))
  (n-dash :size))

(defun stroke-dash (stroke)
 #+liber-documentation
 "@version{#2024-11-11}
  @syntax{(gsk:stroke-dash stroke) => dash}
  @syntax{(setf (gsk:stroke-dash stroke) dash)}
  @argument[stroke]{a @class{gsk:stroke} instance}
  @argument[dash]{a list of numbers coerced to floats with the dashes}
  @begin{short}
    The @fun{gsk:stroke-dash} function gets the list of dashes in use or
    @code{nil} if dashing is disabled.
  @end{short}
  The @setf{gsk:stroke-dash} funtion sets the dash pattern to use by the stroke.

  A dash pattern is specified by a list of alternating non-negative values.
  Each value provides the length of alternate 'on' and 'off' portions of the
  stroke. Each 'on' segment will have caps applied as if the segment were a
  separate contour. In particular, it is valid to use an 'on' length of 0 with
  @code{:round} or @code{:square} to draw dots or squares along a path.

  If @arg{dash} is empty, if all elements in @arg{dash} are 0, or if there are
  negative values in @arg{dash}, then dashing is disabled. If @arg{dash} has one
  element, an alternating 'on' and 'off' pattern with the single dash length
  provided is assumed. If @arg{dash} has uneven elements, the dash list will be
  used with the first element in dash defining an 'on' or 'off' in alternating
  passes through the list.

  You can specify a starting offset into the dash with the
  @fun{gsk:stroke-dash-offset} function.

  Since 4.14
  @see-class{gsk:stroke}
  @see-function{gsk:stroke-dash-offset}"
  (cffi:with-foreign-objects ((ptr :pointer) (n :size))
    (setf ptr (%stroke-dash stroke n))
    (iter (for i from 0 below (cffi:mem-ref n :size))
          (collect (cffi:mem-aref ptr :float i)))))

(export 'stroke-dash)

;;; ----------------------------------------------------------------------------
;;; gsk_stroke_get_dash_offset
;;; gsk_stroke_set_dash_offset
;;; ----------------------------------------------------------------------------

(defun (setf stroke-dash-offset) (offset stroke)
  (let ((offset (coerce offset 'single-float)))
    (cffi:foreign-funcall "gsk_stroke_set_dash_offset"
                          (g:boxed stroke) stroke
                          :float offset
                          :void)
    offset))

(cffi:defcfun ("gsk_stroke_get_dash_offset" stroke-dash-offset) :float
 #+liber-documentation
 "@version{#2024-11-11}
  @syntax{(gsk:stroke-dash-offset stroke) => offset}
  @syntax{(setf (gsk:stroke-dash-offset stroke) offset)}
  @argument[stroke]{a @class{gsk:stroke} instance}
  @argument[offset]{a number coerced to a float with the offset into the
    dash pattern}
  @begin{short}
    The @fun{gsk:stroke-dash-offset} function returns the dash offset of a
    @class{gsk:stroke} instance.
  @end{short}
  The @setf{gsk:stroke-dash-offset} function sets the offset into the dash
  pattern where dashing should begin.

  This is an offset into the length of the path, not an index into the list
  values of the dash list. See the @fun{gsk:stroke-dash} function for more
  details on dashing.

  Since 4.14
  @see-class{gsk:stroke}
  @see-function{gsk:stroke-dash}"
  (stroke (g:boxed stroke)))

(export 'stroke-dash-offset)

;;; ----------------------------------------------------------------------------
;;; gsk_stroke_get_line_cap
;;; gsk_stroke_set_line_cap
;;; ----------------------------------------------------------------------------

(defun (setf stroke-line-cap) (cap stroke)
  (cffi:foreign-funcall "gsk_stroke_set_line_cap"
                        (g:boxed stroke) stroke
                        line-cap cap
                        :void)
  cap)

(cffi:defcfun ("gsk_stroke_get_line_cap" stroke-line-cap) line-cap
 #+liber-documentation
 "@version{#2024-11-11}
  @syntax{(gsk:stroke-line-cap stroke) => cap}
  @syntax{(setf (gsk:stroke-line-cap stroke) cap)}
  @argument[stroke]{a @class{gsk:stroke} instance}
  @argument[cap]{a @symbol{gsk:line-cap} value}
  @begin{short}
    The @fun{gsk:stroke-line-cap} function gets the line cap used.
  @end{short}
  The @setf{gsk:stroke-line-cap} functionsets the line cap to be used when
  stroking. See the @symbol{gsk:line-cap} documentation for details.

  Since 4.14
  @see-class{gsk:stroke}
  @see-symbol{gsk:line-cap}"
  (stroke (g:boxed stroke)))

(export 'stroke-line-cap)

;;; ----------------------------------------------------------------------------
;;; gsk_stroke_get_line_join
;;; gsk_stroke_set_line_join
;;; ----------------------------------------------------------------------------

(defun (setf stroke-line-join) (join stroke)
  (cffi:foreign-funcall "gsk_stroke_set_line_join"
                        (g:boxed stroke) stroke
                        line-join join
                        :void)
  join)

(cffi:defcfun ("gsk_stroke_get_line_join" stroke-line-join) line-join
 #+liber-documentation
 "@version{#2024-11-11}
  @syntax{(gsk:stroke-line-join stroke) => join}
  @syntax{(setf (gsk:stroke-line-join stroke) join)}
  @argument[stroke]{a @class{gsk:stroke} instance}
  @argument[join]{a @symbol{gsk:line-join} value}
  @begin{short}
    The @fun{gsk:stroke-line-join} function gets the line join used.
  @end{short}
  The @setf{gsk:stroke-line-join} function sets the line join to be used when
  stroking. See the @symbol{gsk:line-join} documentation for details.

  Since 4.14
  @see-class{gsk:stroke}
  @see-symbol{gsk:line-join}"
  (stroke (g:boxed stroke)))

(export 'stroke-line-join)

;;; ----------------------------------------------------------------------------
;;; gsk_stroke_get_line_width
;;; gsk_stroke_set_line_width
;;; ----------------------------------------------------------------------------

(defun (setf stroke-line-width) (width stroke)
  (let ((width (coerce width 'single-float)))
    (cffi:foreign-funcall "gsk_stroke_set_line_width"
                          (g:boxed stroke) stroke
                          :float width
                          :void)
    width))

(cffi:defcfun ("gsk_stroke_get_line_width" stroke-line-width) :float
 #+liber-documentation
 "@version{#2024-11-11}
  @syntax{(gsk:stroke-line-width stroke) => width}
  @syntax{(setf (gsk:stroke-line-width stroke) width)}
  @argument[stroke]{a @class{gsk:stroke} instance}
  @argument[width]{a number coerced to a float with the width of the line
    in pixels}
  @begin{short}
    The @fun{gsk:stroke-line-width} function gets the line width used.
  @end{short}
  The @setf{gsk:stroke-line-width} function sets the line width to be used when
  stroking. The line width must be > 0.

  Since 4.14
  @see-class{gsk:stroke}"
  (stroke (g:boxed stroke)))

(export 'stroke-line-width)

;;; ----------------------------------------------------------------------------
;;; gsk_stroke_get_miter_limit
;;; gsk_stroke_set_miter_limit
;;; ----------------------------------------------------------------------------

(defun (setf stroke-miter-limit) (limit stroke)
  (let ((limit (coerce limit 'single-float)))
    (cffi:foreign-funcall "gsk_stroke_set_miter_limit"
                          (g:boxed stroke) stroke
                          :float limit
                          :void)
    limit))

(cffi:defcfun ("gsk_stroke_get_miter_limit" stroke-miter-limit) :float
 #+liber-documentation
 "@version{#2024-11-11}
  @syntax{(gsk:stroke-miter-limit stroke) => limit}
  @syntax{(setf (gsk:stroke-miter-limit stroke) limit)}
  @argument[stroke]{a @class{gsk:stroke} instance}
  @argument[limit]{a number coerced to a float with the miter limit}
  @begin{short}
    The @fun{gsk:stroke-miter-limit} function returns the miter limit of a
    @class{gsk:stroke} instance.
  @end{short}
  The @setf{gsk:stroke-miter-limit} function sets the limit for the distance
  from the corner where sharp turns of joins get cut off.

  The miter limit is in units of line width and must be non-negative. For joins
  of type @code{:miter} that exceed the miter limit, the join gets rendered as
  if it was of the @code{:bevel} type.

  Since 4.14
  @see-class{gsk:stroke}
  @see-symbol{gsk:line-join}"
  (stroke (g:boxed stroke)))

(export 'stroke-miter-limit)

;;; ----------------------------------------------------------------------------
;;; gsk_stroke_to_cairo
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_stroke_to_cairo" stroke-to-cairo) :void
 #+liber-documentation
 "@version{#2024-11-11}
  @argument[stroke]{a @class{gsk:stroke} instance}
  @argument[context]{a @symbol{cairo:context-t} instance to configure}
  @begin{short}
    A helper function that sets the stroke parameters of @arg{context} from the
    values found in @arg{stroke}.
  @end{short}

  Since 4.14
  @see-class{gsk:stroke}"
  (stroke (g:boxed stroke))
  (context (:pointer (:struct cairo:context-t))))

(export 'stroke-to-cairo)

;;; ----------------------------------------------------------------------------
;;; gsk_path_parse
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_path_parse" path-parse) (g:boxed path :return)
 #+liber-documentation
 "@version{#2024-11-11}
  @argument[str]{a string}
  @begin{short}
    This is a convenience function that constructs a @class{gsk:path} instance
    from a serialized form.
  @end{short}
  The string is expected to be in (a superset of) SVG path syntax, as,
  for example, produced by the @fun{gsk:path-to-string} function.

  A high-level summary of the syntax:
  @begin[code]{table}
    @entry[M x y]{Move to @code{(x,y)}}
    @entry[L x y]{Add a line from the current point to @code{(x,y)}}
    @entry[Q x1 y1 x2 y2]{Add a quadratic Bézier from the current point to
      @code{(x2,y2)}, with control point @code{(x1,y1)}}
    @entry[C x1 y1 x2 y2 x3 y3]{Add a cubic Bézier from the current point to
      @code{(x3,y3)}, with control points @code{(x1,y1)} and @code{(x2,y2)}}
    @entry[Z]{Close the contour by drawing a line back to the start point}
    @entry[H x]{Add a horizontal line from the current point to the given x
      value}
    @entry[V y]{Add a vertical line from the current point to the given y value}
    @entry[T x2 y2]{Add a quadratic Bézier, using the reflection of the
      previous segments’ control point as control point}
    @entry[S x2 y2 x3 y3]{Add a cubic Bézier, using the reflection of the
      previous segments’ second control point as first control point}
    @entry[A rx ry r l s x y]{Add an elliptical arc from the current point to
      @code{(x,y)} with radii @code{rx} and @code{ry}. See the SVG
      documentation for how the other parameters influence the arc.}
    @entry[O x1 y1 x2 y2 w]{Add a rational quadratic Bézier from the current
      point to @code{(x2,y2)} with control point @code{(x1,y1)} and weight
      @code{w}.}
  @end{table}
  All the commands have lowercase variants that interpret coordinates relative
  to the current point. The O command is an extension that is not supported in
  SVG.

  Since 4.14
  @see-class{gsk:path}
  @see-function{gsk:path-to-string}"
  (str :string))

(export 'path-parse)

;;; ----------------------------------------------------------------------------
;;; GskPathForeachFunc
;;;
;;; gboolean
;;; (* GskPathForeachFunc) (
;;;   GskPathOperation op,
;;;   const graphene_point_t* pts,
;;;   gsize n_pts,
;;;   float weight,
;;;   gpointer user_data
;;; )
;;; ----------------------------------------------------------------------------

;; TODO: Do a Lisp implementation for the array of points

(cffi:defcallback path-foreach-func :boolean
    ((operation path-operation)
     (points :pointer)
     (n-points :size)
     (weight :float)
     (data :pointer))
  (let ((func (glib:get-stable-pointer-value data)))
    (funcall func operation points n-points weight)))

#+liber-documentation
(setf (liber:alias-for-symbol 'path-foreach-func)
      "Callback"
      (liber:symbol-documentation 'path-foreach-func)
 "@version{#2024-11-12}
  @syntax{lambda (operation points n-points weight)}
  @argument[operation]{a @symbol{gsk:path-operation} value}
  @argument[points]{an array of @symbol{graphene:point-t} instances}
  @argument[n-points]{an integer with the points of the operation}
  @argument[weight]{a float with the weight for conic curves, or unused if not
    a conic curve}
  @return{@em{True} to continue iterating the path, @em{false} to immediately
  abort and not call the function again}
  @begin{short}
    Prototype of the callback function to iterate through the operations of a
    path.
  @end{short}
  For each operation, the callback function is given @arg{operation} itself,
  the points that the operation is applied to in @arg{points}, and a weight for
  conic curves. The @arg{n-points} argument is somewhat redundant, since the
  number of points can be inferred from the operation.

  Each contour of the path starts with a @code{:move} operation. Closed contours
  end with a @code{:close} operation.

  Since 4.14
  @see-function{gsk:path-foreach}
  @see-symbol{graphene:point-t}")

(export 'path-foreach-func)

;;; ----------------------------------------------------------------------------
;;; gsk_path_foreach
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_path_foreach" %path-foreach) :boolean
  (path (g:boxed path))
  (flags path-foreach-flags)
  (func :pointer)
  (data :pointer))

(defun path-foreach (path flags func)
 #+liber-documentation
 "@version{#2024-11-12}
  @argument[path]{a @class{gsk:path} instance}
  @argument[flags]{a @symbol{gsk:path-foreach-flags} value with the flags to
    pass to @arg{func}}
  @argument[func]{a @symbol{gsk:path-foreach-func} callback function}
  @return{@em{False} if @arg{func} returned @em{false}, @em{true} otherwise}
  @begin{short}
    Calls @arg{func} for every operation of the path.
  @end{short}
  Note that this may only approximate @arg{path}, because paths can contain
  optimizations for various specialized contours, and depending on the flags,
  the path may be decomposed into simpler curves than the ones that it
  contained originally.

  This function serves two purposes:
  @begin{itemize}
    @begin{item}
      When the flags allow everything, it provides access to the raw,
      unmodified data of the path.
    @end{item}
    @begin{item}
      When the flags disallow certain operations, it provides an
      approximation of the path using just the allowed operations.
    @end{item}
  @end{itemize}
  Since 4.14
  @see-class{gsk:path}
  @see-symbol{gsk:path-foreach-flags}
  @see-symbol{gsk:path-foreach-func}"
  (glib:with-stable-pointer (ptr func)
    (%path-foreach path
                   flags
                   (cffi:callback path-foreach-func)
                   ptr)))

(export 'path-foreach)

;;; ----------------------------------------------------------------------------
;;; gsk_path_get_bounds
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_path_get_bounds" %path-bounds) :boolean
  (path (g:boxed path))
  (bounds (:pointer (:struct graphene:rect-t))))

(defun path-bounds (path bounds)
 #+liber-documentation
 "@version{#2024-11-12}
  @argument[path]{a @class{gsk:path} instance}
  @argument[bounds]{a @symbol{graphene:rect-t} instance for the bounds of the
    given @arg{path}}
  @return{The @symbol{graphene:rect-t} instance or @code{nil}.}
  @begin{short}
    Computes the bounds of the given @arg{path}.
  @end{short}
  The returned bounds may be larger than necessary, because this function aims
  to be fast, not accurate. The bounds are guaranteed to contain the path.

  It is possible that the returned rectangle has 0 width and/or height. This
  can happen when the path only describes a point or an axis-aligned line.

  If the path is empty, @em{false} is returned and @arg{bounds} are set to the
  @fun{graphene:rect-zero} value. This is different from the case where the
  path is a single point at the origin, where the bounds will also be set to
  the zero rectangle but @em{true} will be returned.

  Since 4.14
  @see-class{gsk:path}
  @see-symbol{graphene:rect-t}
  @see-function{graphene:rect-zero}"
  (when (%path-bounds path bounds)
    bounds))

(export 'path-bounds)

;;; ----------------------------------------------------------------------------
;;; gsk_path_get_closest_point
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_path_get_closest_point" %path-closest-point) :boolean
  (path (g:boxed path))
  (point (:pointer (:struct graphene:point-t)))
  (threshold :float)
  (result (g:boxed path-point))
  (distance (:pointer :float)))

(defun path-closest-point (path point threshold)
 #+liber-documentation
 "@version{2024-11-12}
  @syntax{(gsk:path-closest-point path point threshold) => result, distance}
  @argument[path]{a @class{gsk:path} instance}
  @argument[point]{a @symbol{graphene:point-t} instance}
  @argument[threshold]{a float with the maximum allowed distance}
  @begin{return}
    @arg{result} -- a @class{gsk:path-point} instance with the closest point
    @arg{distance} -- a float with the distance
  @end{return}
  @begin{short}
    Computes the closest point on the path to the given @arg{point} and sets
    the result to it.
  @end{short}
  If there is no point closer than the given @arg{threshold}, @code{nil}
  is returned.

  Since 4.14
  @see-class{gsk:path}"
  (let ((result (make-instance 'path-point)))
    (cffi:with-foreign-object (distance :float)
      (when (%path-closest-point path point threshold result distance)
        (values result
                (cffi:mem-ref distance :float))))))

(export 'path-closest-point)

;;; ----------------------------------------------------------------------------
;;; gsk_path_get_start_point
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_path_get_start_point" %path-start-point) :boolean
  (path (g:boxed path))
  (result (g:boxed path-point)))

(defun path-start-point (path)
 #+liber-documentation
 "@version{2024-11-12}
  @argument[path]{a @class{gsk:path} instance}
  @return{The @class{gsk:path-point} instance with the point.}
  @begin{short}
    Gets the start point of the path.
  @end{short}
  An empty path has no points, so @code{nil} is returned in this case.

  Since 4.14
  @see-class{gsk:path}
  @see-class{gsk:path-pointer}"
  (let ((result (make-instance 'path-point)))
    (when (%path-start-point path result)
      result)))

(export 'path-start-point)

;;; ----------------------------------------------------------------------------
;;; gsk_path_get_end_point
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_path_get_end_point" %path-end-point) :boolean
  (path (g:boxed path))
  (result (g:boxed path-point)))

(defun path-end-point (path)
 #+liber-documentation
 "@version{2024-11-12}
  @argument[path]{a @class{gsk:path} instance}
  @return{The @class{gsk:path-point} instance with the point.}
  @begin{short}
    Gets the end point of the path.
  @end{short}
  An empty path has no points, so @code{nil} is returned in this case.

  Since 4.14
  @see-class{gsk:path}
  @see-class{gsk:path-pointer}"
  (let ((result (make-instance 'path-point)))
    (when (%path-end-point path result)
      result)))

(export 'path-end-point)

;;; ----------------------------------------------------------------------------
;;; gsk_path_get_stroke_bounds
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_path_get_stroke_bounds" %path-stroke-bounds) :boolean
  (path (g:boxed path))
  (stroke (g:boxed stroke))
  (bounds (:pointer (:struct graphene:rect-t))))

(defun path-stroke-bounds (path stroke bounds)
 #+liber-documentation
 "@version{#2024-11-12}
  @argument[path]{a @class{gsk:path} instance}
  @argument[stroke]{a @class{gsk:stroke} instance with the stroke parameters}
  @argument[bounds]{a @symbol{graphene:rect-t} instance with the bounds to
    fill in}
  @return{The @symbol{graphene:rect-t} instance with the bounds, or @code{nil}.}
  @begin{short}
    Computes the bounds for stroking the given @arg{path} with the parameters
    in stroke.
  @end{short}
  The returned bounds may be larger than necessary, because this function aims
  to be fast, not accurate. The bounds are guaranteed to contain the area
  affected by the stroke, including protrusions like miters.

  Since 4.14
  @see-class{gsk:path}
  @see-class{gsk:stroke}
  @see-symbol{graphene:rect-t}"
  (when (%path-stroke-bounds path stroke bounds)
    bounds))

(export 'path-stroke-bounds)

;;; ----------------------------------------------------------------------------
;;; gsk_path_in_fill
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_path_in_fill" path-in-fill) :boolean
 #+liber-documentation
 "@version{#2024-11-12}
  @argument[path]{a @class{gsk:path} instance}
  @argument[point]{a @symbol{graphene:point-t} instance with the point to test}
  @argument[rule]{a @symbol{gsk:fill-rule} value}
  @return{@em{True} if @arg{point} is inside.}
  @begin{short}
    Returns whether the given @arg{point} is inside the area that would be
    affected if the path was filled according to @arg{rule}.
  @end{short}
  Note that this function assumes that filling a contour implicitly closes it.

  Since 4.14
  @see-class{gsk:path}
  @see-symbol{graphene:point-t}
  @see-symbol{gsk:fill-rule}"
  (path (g:boxed path))
  (point (:pointer (:struct graphene:point-t)))
  (rule fill-rule))

(export 'path-in-fill)

;;; ----------------------------------------------------------------------------
;;; gsk_path_is_closed
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_path_is_closed" path-is-closed) :boolean
 #+liber-documentation
 "@version{#2024-11-12}
  @argument[path]{a @class{gsk:path} instance}
  @return{@em{True} if the path is closed.}
  @begin{short}
    Returns if the path represents a single closed contour.
  @end{short}

  Since 4.14
  @see-class{gsk:path}"
  (path (g:boxed path)))

(export 'path-is-closed)

;;; ----------------------------------------------------------------------------
;;; gsk_path_is_empty
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_path_is_empty" path-is-empty) :boolean
 #+liber-documentation
 "@version{#2024-11-12}
  @argument[path]{a @class{gsk:path} instance}
  @return{@em{True} if the path is empty.}
  @begin{short}
    Checks if the path is empty, that is, contains no lines or curves.
  @end{short}

  Since 4.14
  @see-class{gsk:path}"
  (path (g:boxed path)))

(export 'path-is-empty)

;;; ----------------------------------------------------------------------------
;;; gsk_path_print                                          not needed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gsk_path_ref                                            not needed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gsk_path_unref                                          not needed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gsk_path_to_cairo
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_path_to_cairo" path-to-cairo) :void
 #+liber-documentation
 "@version{#2024-11-12}
  @argument[path]{a @class{gsk:path} instance}
  @argument[context]{a @symbol{cairo:context-t} instance}
  @begin{short}
    Appends the given @arg{path} to the given Cairo @arg{context} for drawing
    with Cairo.
  @end{short}
  This may cause some suboptimal conversions to be performed as Cairo does not
  support all features of the @class{gsk:path} instance.

  This function does not clear the existing Cairo path. Call the
  @fun{cairo:new-path} function if you want this.

  Since 4.14
  @see-class{gsk:path}
  @see-symbol{cairo:context-t}
  @see-function{cairo:new-path}"
  (path (g:boxed path))
  (context (:pointer (:struct cairo:context-t))))

(export 'path-to-cairo)

;;; ----------------------------------------------------------------------------
;;; gsk_path_to_string
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_path_to_string" path-to-string) :string
 #+liber-documentation
 "@version{#2024-11-12}
  @argument[path]{a @class{gsk:path} instance}
  @return{The string for @arg{path}.}
  @begin{short}
    Converts the path into a human-readable string representation suitable for
    printing.
  @end{short}
  The string is compatible with a superset of the
  @url[https://www.w3.org/TR/SVG11/paths.html#PathData]{SVG path syntax}.
  See the @fun{gsk:path-parse} function for a summary of the syntax.

  Since 4.14
  @see-class{gsk:path}
  @see-function{gsk:path-parse}"
  (path (g:boxed path)))

(export 'path-to-string)

;;; --- End of file gsk4.path.lisp ---------------------------------------------

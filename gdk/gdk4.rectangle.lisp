;;; ----------------------------------------------------------------------------
;;; gdk4.rectangle.lisp
;;;
;;; The documentation in this file is taken from the GDK 4 Reference Manual
;;; version 4.18 and modified to document the Lisp binding to the GDK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2025 Dieter Kaiser
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
;;; Rectangles
;;;
;;;     Data type for representing rectangles
;;;
;;; Types and Values
;;;
;;;     GdkRectangle
;;;
;;; Functions
;;;
;;;     gdk_rectangle_contains_point
;;;     gdk_rectangle_equal
;;;     gdk_rectangle_intersect
;;;     gdk_rectangle_union
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkRectangle
;;; ----------------------------------------------------------------------------

;; TODO: cairo:rectangle-t is no longer exported from Cairo

(glib:define-gboxed-cstruct rectangle "GdkRectangle"
  (:export t
   :type-initializer "gdk_rectangle_get_type")
  (x :int :initform 0)
  (y :int :initform 0)
  (width :int :initform 0)
  (height :int :initform 0))

#+liber-documentation
(setf (liber:alias-for-class 'rectangle)
      "GBoxed"
      (documentation 'rectangle 'type)
 "@version{2025-06-30}
  @begin{declaration}
(glib:define-gboxed-cstruct rectangle \"GdkRectangle\"
  (:export t
   :type-initializer \"gdk_rectangle_get_type\")
  (x :int :initform 0)
  (y :int :initform 0)
  (width :int :initform 0)
  (height :int :initform 0))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[x]{The x coordinate of the top left corner.}
      @entry[y]{The y coordinate of the top left corner.}
      @entry[width]{The width of the rectangle.}
      @entry[height]{The height of the rectangle.}
    @end{simple-table}
  @end{values}
  @begin{short}
    The @class{gdk:rectangle} structure is a data type for representing
    rectangles.
  @end{short}
  The @class{gdk:rectangle} structure is identical to the
  @sym{cairo:rectangle-t} structure. Together with the @sym{cairo:region-t} data
  type of Cairo, these are the central types for representing sets of pixels.

  The intersection of two rectangles can be computed with the
  @fun{gdk:rectangle-intersect} function. To find the union of two rectangles
  use the @fun{gdk:rectangle-union} function.

  The @sym{cairo:region-t} type provided by Cairo is usually used for managing
  non-rectangular clipping of graphical operations.

  The Graphene library has a number of other data types for regions and volumes
  in 2D and 3D.
  @see-constructor{gdk:rectangle-new}
  @see-constructor{gdk:rectangle-copy}
  @see-slot{gdk:rectangle-x}
  @see-slot{gdk:rectangle-y}
  @see-slot{gdk:rectangle-width}
  @see-slot{gdk:rectangle-height}
  @see-symbol{cairo:region-t}")

;;; --- gdk:rectangle-x --------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'rectangle-x)
      "Accessor"
      (documentation 'rectangle-x 'function)
 "@version{2025-06-30}
  @syntax{(gdk:rectangle-x instance) => x}
  @syntax{(setf (gdk:rectangle-x instance) x)}
  @argument[instance]{a @class{gdk:rectangle} instance}
  @argument[x]{an integer for the x coordinate of the rectangle}
  @begin{short}
    Accessor of the @code{x} slot of the @class{gdk:rectangle} structure.
  @end{short}
  @see-class{gdk:rectangle}")

;;; --- gdk:rectangle-y --------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'rectangle-y)
      "Accessor"
      (documentation 'rectangle-y 'function)
 "@version{2025-06-30}
  @syntax{(gdk:rectangle-y instance) => y}
  @syntax{(setf (gdk:rectangle-y instance) y)}
  @argument[instance]{a @class{gdk:rectangle} instance}
  @argument[y]{an integer for the y coordinate of the rectangle}
  @begin{short}
    Accessor of the @code{y} slot of the @class{gdk:rectangle} structure.
  @end{short}
  @see-class{gdk:rectangle}")

;;; --- gtk:rectangle-width ----------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'rectangle-width)
      "Accessor"
      (documentation 'rectangle-width 'function)
 "@version{2025-06-30}
  @syntax{(gdk:rectangle-width instance) => width}
  @syntax{(setf (gdk:rectangle-width instance) width)}
  @argument[instance]{a @class{gdk:rectangle} instance}
  @argument[width]{an integer for the width of the rectangle}
  @begin{short}
    Accessor of the @code{width} slot of the @class{gdk:rectangle} structure.
  @end{short}
  @see-class{gdk:rectangle}")

;;; --- gdk:rectangle-height ---------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'rectangle-height)
      "Accessor"
      (documentation 'rectangle-height 'function)
 "@version{2025-06-30}
  @syntax{(gdk:rectangle-height instance) => height}
  @syntax{(setf (gdk:rectangle-height instance) height)}
  @argument[instance]{a @class{gdk:rectangle} instance}
  @argument[height]{an integer for the height of the rectangle}
  @begin{short}
    Accessor of the @code{height} slot of the @class{gdk:rectangle} structure.
  @end{short}
  @see-class{gdk:rectangle}")

;;; --- gdk:rectangle-new ------------------------------------------------------

(declaim (inline rectangle-new))

(defun rectangle-new (&key (x 0) (y 0) (width 0) (height 0))
 #+liber-documentation
 "@version{2025-08-04}
  @argument[x]{an integer for the x coordinate of the top left corner of
    the rectangle}
  @argument[y]{an integer for the y coordinate of the top left corner of
    the rectangle}
  @argument[width]{an integer for the width of the rectangle}
  @argument[height]{an integer for the height of the rectangle}
  @begin{short}
    Returns a @class{gdk:rectangle} instance for the initial values given to
    @arg{x}, @arg{y}, @arg{width}, and @arg{height}.
  @end{short}
  @see-class{gdk:rectangle}
  @see-function{gdk:rectangle-copy}"
  (make-rectangle :x x :y y :width width :height height))

(export 'rectangle-new)

;;; --- gdk:rectangle-copy -----------------------------------------------------

(declaim (inline rectangle-copy))

(defun rectangle-copy (instance)
 #+liber-documentation
 "@version{2024-07-09}
  @argument[instance]{a @class{gdk:rectangle} instance}
  @begin{short}
    Copy constructor of a @class{gdk:rectangle} structure.
  @end{short}
  @see-class{gdk:rectangle}
  @see-function{gdk:rectangle-new}"
  (copy-rectangle instance))

(export 'rectangle-copy)

;;; ----------------------------------------------------------------------------
;;; gdk_rectangle_contains_point
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_rectangle_contains_point" rectangle-contains-point) :boolean
 #+liber-documentation
 "@version{2025-06-30}
  @argument[rect]{a @class{gdk:rectangle} instance}
  @argument[x]{an integer for the x coordinate}
  @argument[y]{an integer for the y coordinate}
  @return{@em{True} if @arg{rect} contains the point.}
  @begin{short}
    Returns @em{true} if @arg{rect} contains the point described by
    @arg{x} and @arg{y}.
  @end{short}
  @see-class{gdk:rectangle}"
  (rect (g:boxed rectangle))
  (x :int)
  (y :int))

(export 'rectangle-contains-point)

;;; ----------------------------------------------------------------------------
;;; gdk_rectangle_equal
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_rectangle_equal" rectangle-equal) :boolean
 #+liber-documentation
 "@version{2024-07-09}
  @argument[rect1]{a @class{gdk:rectangle} instance}
  @argument[rect2]{a @class{gdk:rectangle} instance}
  @return{@em{True} if the rectangles are equal.}
  @begin{short}
    Checks if the two given rectangles are equal.
  @end{short}
  @see-class{gdk:rectangle}"
  (rect1 (g:boxed rectangle))
  (rect2 (g:boxed rectangle)))

(export 'rectangle-equal)

;;; ----------------------------------------------------------------------------
;;; gdk_rectangle_intersect
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_rectangle_intersect" %rectangle-intersect) :boolean
  (rect1 (g:boxed rectangle))
  (rect2 (g:boxed rectangle))
  (dest  (g:boxed rectangle)))

(defun rectangle-intersect (rect1 rect2)
 #+liber-documentation
 "@version{2024-08-04}
  @argument[rect1]{a @class{gdk:rectangle} instance}
  @argument[rect2]{a @class{gdk:rectangle} instance}
  @begin{return}
    The @class{gdk:rectangle} instance for the intersection of @arg{rect1}
    and @arg{rect2}, or @code{nil}.
  @end{return}
  @begin{short}
    Calculates the intersection of two rectangles.
  @end{short}
  If the rectangles do not intersect @code{nil} is returned.
  @see-class{gdk:rectangle}
  @see-function{gdk:rectangle-union}"
  (let ((dest (make-rectangle)))
    (when (%rectangle-intersect rect1 rect2 dest)
      dest)))

(export 'rectangle-intersect)

;;; ----------------------------------------------------------------------------
;;; gdk_rectangle_union
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_rectangle_union" %rectangle-union) :void
  (rect1 (g:boxed rectangle))
  (rect2 (g:boxed rectangle))
  (dest  (g:boxed rectangle)))

(defun rectangle-union (rect1 rect2)
 #+liber-documentation
 "@version{2025-08-04}
  @argument[rect1]{a @class{gdk:rectangle} instance}
  @argument[rect2]{a @class{gdk:rectangle} instance}
  @begin{return}
    The @class{gdk:rectangle} instance for the union of @arg{rect1} and
    @arg{rect2}.
  @end{return}
  @begin{short}
    Calculates the union of two rectangles.
  @end{short}
  The union of rectangles @arg{rect1} and @arg{rect2} is the smallest rectangle
  which includes both rectangles within it.
  @see-class{gdk:rectangle}
  @see-function{gtk-rectangle-intersect}"
  (let ((dest (make-rectangle)))
    (%rectangle-union rect1 rect2 dest)
    dest))

(export 'rectangle-union)

;;; --- End of file gdk4.rectangle.lisp -----------------------------------------

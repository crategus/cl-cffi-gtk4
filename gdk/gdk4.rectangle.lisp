;;; ----------------------------------------------------------------------------
;;; gdk4.rectangle.lisp
;;;
;;; The documentation of this file is taken from the GDK 4 Reference Manual
;;; Version 4.9 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2023 Dieter Kaiser
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

(define-g-boxed-cstruct rectangle "GdkRectangle"
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
 "@version{#2022-1-21}
  @begin{short}
    A @sym{gdk:rectangle} data type for representing rectangles.
  @end{short}

  The @sym{gdk:rectangle} structure is identical to the
  @symbol{cairo-rectangle-t} structure. Together with the
  @symbol{cairo-region-t} data type of Cairo, these are the central types for
  representing sets of pixels.

  The intersection of two rectangles can be computed with the
  @fun{gdk:rectangle-intersect}. To find the union of two rectangles use the
  @fun{gdk:rectangle-union} function.

  The @symbol{cairo-region-t} type provided by Cairo is usually used for
  managing non-rectangular clipping of graphical operations.

  The Graphene library has a number of other data types for regions and volumes
  in 2D and 3D.
  @begin{pre}
(define-g-boxed-cstruct gdk:rectangle \"GdkRectangle\"
  (:export t
   :type-initializer \"gdk_rectangle_get_type\")
  (x :int :initform 0)
  (y :int :initform 0)
  (width :int :initform 0)
  (height :int :initform 0))
  @end{pre}
  @see-slot{gdk:rectangle-x}
  @see-slot{gdk:rectangle-y}
  @see-slot{gdk:rectangle-width}
  @see-slot{gdk:rectangle-height}
  @see-symbol{cairo-region-t}
  @see-symbol{cairo-rectangle-t}")

;;; --- rectangle-x --------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'rectangle-x)
      "Accessor"
      (documentation 'rectangle-x 'function)
 "@version{#2022-1-21}
  @syntax[]{(gdk:rectangle-x instance) => x}
  @syntax[]{(setf (gdk:rectangle-x instance) x)}
  @argument[instance]{a @class{gdk:rectangle} instance}
  @argument[x]{an integer with the x coordinate of the rectangle}
  @begin{short}
    Accessor of the @code{x} slot of the @class{gdk:rectangle} structure.
  @end{short}
  @see-class{gdk:rectangle}")

;;; --- rectangle-y --------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'rectangle-y)
      "Accessor"
      (documentation 'rectangle-y 'function)
 "@version{#2022-1-21}
  @syntax[]{(gdk:rectangle-y instance) => y}
  @syntax[]{(setf (gdk:rectangle-y instance) y)}
  @argument[instance]{a @class{gdk:rectangle} instance}
  @argument[y]{an integer with the y coordinate of the rectangle}
  @begin{short}
    Accessor of the @code{y} slot of the @class{gdk:rectangle} structure.
  @end{short}
  @see-class{gdk:rectangle}")

;;; --- gtk-rectangle-width ----------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'rectangle-width)
      "Accessor"
      (documentation 'rectangle-width 'function)
 "@version{#2022-1-21}
  @syntax[]{(gdk:rectangle-width instance) => width}
  @syntax[]{(setf (gdk:rectangle-width instance) width)}
  @argument[instance]{a @class{gdk:rectangle} instance}
  @argument[width]{an integer with the width of the rectangle}
  @begin{short}
    Accessor of the @code{width} slot of the @class{gdk:rectangle} structure.
  @end{short}
  @see-class{gdk:rectangle}")

;;; --- rectangle-height ---------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'rectangle-height)
      "Accessor"
      (documentation 'rectangle-height 'function)
 "@version{#2022-1-21}
  @syntax[]{(gdk:rectangle-height instance) => height}
  @syntax[]{(setf (gdk:rectangle-height instance) height)}
  @argument[instance]{a @class{gdk:rectangle} instance}
  @argument[height]{an integer with the height of the rectangle}
  @begin{short}
    Accessor of the @code{height} slot of the @class{gdk:rectangle} structure.
  @end{short}
  @see-class{gdk:rectangle}")

;;; --- rectangle-new ------------------------------------------------------

(declaim (inline rectangle-new))

(defun rectangle-new (&key (x 0) (y 0) (width 0) (height 0))
 #+liber-documentation
 "@version{#2022-1-21}
  @argument[x]{an integer with the value for the @code{x} slot}
  @argument[y]{an integer with the value for the @code{y} slot}
  @argument[width]{an integer with the value for the @code{width} slot}
  @argument[height]{an integer with the value for the @code{height} slot}
  @begin{short}
    Returns a @class{gdk:rectangle} instance with the initial values given to
    @arg{x}, @arg{y}, @arg{width}, and @arg{height}.
  @end{short}
  @see-class{gdk:rectangle}
  @see-function{gdk:rectangle-copy}"
  (make-rectangle :x x :y y :width width :height height))

(export 'rectangle-new)

;;; --- rectangle-copy -----------------------------------------------------

(declaim (inline rectangle-copy))

(defun rectangle-copy (instance)
 #+liber-documentation
 "@version{#2022-1-21}
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

(defcfun ("gdk_rectangle_contains_point" rectangle-contains-point) :boolean
 #+liber-documentation
 "@version{#2022-1-21}
  @argument[rect]{a @class{gdk:rectangle} instance}
  @argument[x]{an integer with the x coordinate}
  @argument[y]{an integer with the y coordinate}
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

(defcfun ("gdk_rectangle_equal" rectangle-equal) :boolean
 #+liber-documentation
 "@version{#2022-1-21}
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

(defcfun ("gdk_rectangle_intersect" %rectangle-intersect) :boolean
  (rect1 (g:boxed rectangle))
  (rect2 (g:boxed rectangle))
  (dest  (g:boxed rectangle)))

(defun rectangle-intersect (rect1 rect2)
 #+liber-documentation
 "@version{#2022-1-21}
  @argument[rect1]{a @class{gdk:rectangle} instance}
  @argument[rect2]{a @class{gdk:rectangle} instance}
  @return{A @class{gdk:rectangle} instance with the intersection of @arg{rect1}
    and @arg{rect2}, or @code{nil}.}
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

(defcfun ("gdk_rectangle_union" %rectangle-union) :void
  (rect1 (g:boxed rectangle))
  (rect2 (g:boxed rectangle))
  (dest  (g:boxed rectangle)))

(defun rectangle-union (rect1 rect2)
 #+liber-documentation
 "@version{#2022-1-21}
  @argument[rect1]{a @class{gdk:rectangle} instance}
  @argument[rect2]{a @class{gdk:rectangle} instance}
  @return{A @class{gdk:rectangle} instance with the union of @arg{rect1} and
    @arg{rect2}.}
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

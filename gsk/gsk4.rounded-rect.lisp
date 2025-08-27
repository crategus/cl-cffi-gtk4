;;; ----------------------------------------------------------------------------
;;; gsk.rounded-rect.lisp
;;;
;;; The documentation in this file is taken from the GSK 4 Reference Manual
;;; version 4.18 and modified to document the Lisp binding to the GTK library,
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
;;; GskRoundedRect
;;;
;;;     A rounded rectangle
;;;
;;; Types and Values
;;;
;;;     GskCorner
;;;     GskRoundedRect
;;;
;;; Functions
;;;
;;;     gsk_rounded_rect_init
;;;     gsk_rounded_rect_init_from_rect
;;;     gsk_rounded_rect_init_copy
;;;     gsk_rounded_rect_normalize
;;;     gsk_rounded_rect_offset
;;;     gsk_rounded_rect_shrink
;;;     gsk_rounded_rect_is_rectilinear
;;;     gsk_rounded_rect_contains_point
;;;     gsk_rounded_rect_contains_rect
;;;     gsk_rounded_rect_intersects_rect
;;; ----------------------------------------------------------------------------

(in-package :gsk)

;; TODO: Implement a gsk:with-rounded-rect macro

;;; ----------------------------------------------------------------------------
;;; GskCorner
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GskCorner" corner
  (:export t
   :type-initializer "gsk_corner_get_type")
  (:top-left 0)
  (:top-right 1)
  (:bottom-right 2)
  (:bottom-left 3))

#+liber-documentation
(setf (liber:alias-for-symbol 'corner)
      "GEnum"
      (liber:symbol-documentation 'corner)
 "@version{2025-08-23}
  @begin{declaration}
(gobject:define-genum \"GskCorner\" corner
  (:export t
   :type-initializer \"gsk_corner_get_type\")
  (:top-left 0)
  (:top-right 1)
  (:bottom-right 2)
  (:bottom-left 3))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:top-left]{The top left corner.}
      @entry[:top-right]{The top right corner.}
      @entry[:bottom-right]{The bottom right corner.}
      @entry[:bottom-left]{The bottom left corner.}
    @end{simple-table}
  @end{values}
  @begin{short}
    The corner indices used by the @sym{gsk:rounded-rect} instance.
  @end{short}
  @see-symbol{gsk:rounded-rect}")

(export 'corner)

;;; ----------------------------------------------------------------------------
;;; GskRoundedRect
;;; ----------------------------------------------------------------------------

(cffi:defcstruct rounded-rect
  (bounds (:struct graphene:rect-t))
  (corner (:struct graphene:size-t) :count 4))

#+liber-documentation
(setf (liber:alias-for-symbol 'rounded-rect)
      "CStruct"
      (liber:symbol-documentation 'rounded-rect)
 "@version{2025-08-23}
  @begin{declaration}
(cffi:defcstruct rounded-rect
  (bounds (:struct graphene:rect-t))
  (corner (:struct graphene:size-t) :count 4))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[bounds]{The @sym{graphene:rect-t} instance for the bounds of the
        rounded rectangle.}
      @entry[corner]{The array of @sym{graphene:size-t} instances for the size
        of the 4 rounded corners.}
    @end{simple-table}
  @end{values}
  @begin{short}
    A rectangular region with rounded corners.
  @end{short}
  Application code should normalize rounded rectangles using the
  @fun{gsk:rounded-rect-normalize} function. This function will ensure that the
  bounds of the rounded rectangle are normalized and ensure that the corner
  values are positive and the corners do not overlap.

  All functions taking a @sym{gsk:rounded-rect} instance as an argument will
  internally operate on a normalized copy. All functions returning a
  @sym{gsk:rounded-rect} instance will always return a normalized one.

  The algorithm used for normalizing corner sizes is described in the
  @url[https://drafts.csswg.org/css-backgrounds-3/#border-radius]{CSS specification}.
  @see-slot{gsk:rounded-rect-bounds}
  @see-slot{gsk:rounded-rect-corner}
  @see-function{gsk:rounded-rect-normalize}")

(export 'rounded-rect)

;;; ----------------------------------------------------------------------------

(defun rounded-rect-bounds (rect)
 #+liber-documentation
 "@version{2025-08-02}
  @syntax{(gsk:rounded-rect-bounds rect) => bounds}
  @argument[rect]{a @sym{gsk:rounded-rect} instance}
  @argument[bounds]{a @sym{graphene:rect-t} instance}
  @begin{short}
    The accessor for the @arg{bounds} slot of the rounded rectangle returns
    the bounds of the rounded rectangle.
  @end{short}
  @see-symbol{gsk:rounded-rect}
  @see-symbol{graphene:rect-t}"
  (cffi:foreign-slot-pointer rect '(:struct rounded-rect) 'bounds))

(export 'rounded-rect-bounds)

(defun rounded-rect-corner (rect nth)
 #+liber-documentation
 "@version{2025-08-02}
  @syntax{(gsk:rounded-rect-corner rect nth) => corner}
  @argument[rect]{a @sym{gsk:rounded-rect} instance}
  @argument[nth]{an integer for the number of the corner to retrieve}
  @argument[corner]{a @sym{graphene:size-t} instance for the size of the
    @arg{nth} corner}
  @begin{short}
    The accessor for the @arg{nth} corner of the rounded rectangle returns
    the size of the 4 rounded corners.
  @end{short}
  @see-symbol{gsk:rounded-rect}
  @see-symbol{graphene:size-t}"
  (let ((ptr (cffi:foreign-slot-pointer rect '(:struct rounded-rect) 'corner)))
    (cffi:mem-aptr ptr '(:struct graphene:size-t) nth)))

(export 'rounded-rect-corner)

;;; ----------------------------------------------------------------------------

(defmacro with-rounded-rect ((var &rest args) &body body)
 #+liber-documentation
 "@version{2025-08-02}
  @syntax{(gsk:with-rounded-rect (rounded) body) => result}
  @syntax{(gsk:with-rounded-rect (rounded source) body) => result}
  @syntax{(gsk:with-rounded-rect (rounded rect radius) body) => result}
  @syntax{(gsk:with-rounded-rect (rounded rect ltop rtop lbottom rbottom) body)
    => result}
  @argument[rounded]{a @sym{gsk:rounded-rect} instance to create and initialize}
  @argument[source]{a @sym{gsk:rounded-rect} instance to use for initialization}
  @argument[rect]{a @sym{graphene:rect-t} instance describing the bounds}
  @argument[radius]{a number coerced to a single float for the border radius}
  @argument[ltop]{a @sym{graphene:size-t} instance for the rounding radius of
    the top left corner}
  @argument[rtop]{a @sym{graphene:size-t} instance for the rounding radius of
    the top rigth corner}
  @argument[lbottom]{a @sym{graphene:size-t} instance for the rounding radius
    of the bottom left corner}
  @argument[rbottom]{a @sym{graphene:size-t} instance for the rounding radius
    of the bottom rigth corner}
  @begin{short}
    The @fun{gsk:with-rounded-rect} macro allocates a new @sym{gsk:rounded-rect}
    instance, initializes the rounded rect with the given values and executes
    the body that uses the rounded rect.
  @end{short}
  After execution of the body the allocated memory for the rounded rect is
  released.

  If no argument is given the components of the rounded rectangle are
  uninitialized. The initialization with one argument uses the
  @fun{gsk:rounded-rect-init-copy} function, which initializes the rounded from
  another rounded rect. The initialization from a @sym{graphene:rect-t} instance
  and a number uses the @fun{gsk:rounded-rect-init-from-rect} function. If six
  arguments are given, the rounded rectangle is initialized with the
  @fun{gsk:rounded-rect-init} function.
  @see-symbol{gsk:rounded-rect}
  @see-symbol{graphene:rect-t}
  @see-symbol{graphene:size-t}
  @see-function{gsk:rounded-rect-init}
  @see-function{gsk:rounded-rect-init-from-rect}
  @see-function{gsk:rounded-rect-init-copy}"
  (cond ((null args)
         ;; No arguments, the default is no initialization
         `(cffi:with-foreign-object (,var '(:struct rounded-rect))
            (progn ,@body)))
        ((null (second args))
         ;; One argument, the argument must be of type rounded-rect
         `(cffi:with-foreign-object (,var '(:struct rounded-rect))
            (rounded-rect-init-copy ,var ,@args)
            (progn ,@body)))
        ((null (third args))
        ;; Two arguments
        `(cffi:with-foreign-object (,var '(:struct rounded-rect))
           (rounded-rect-init-from-rect ,var ,@args)
           (progn ,@body)))
        ((null (sixth args))
         ;; Five arguments
         `(cffi:with-foreign-object (,var '(:struct rounded-rect))
            (rounded-rect-init ,var ,@args)
            (progn ,@body)))
        (t
         (error "Syntax error in GSK:WITH-ROUNDED-RECT"))))

(export 'with-rounded-rect)

(defmacro with-rounded-rects (vars &body body)
 #+liber-documentation
 "@version{2025-08-02}
  @syntax{(gsk:with-rounded-rects (rounded1 ... roundedn) body) => result}
  @argument[rounded1 ... roundedn]{newly created @sym{gsk:rounded-rect}
    instances}
  @argument[body]{a body that uses the bindings @arg{rounded1 ... roundedn}}
  @begin{short}
    The @fun{gsk:with-rounded-rects} macro creates new variable bindings and
    executes the body that use these bindings.
  @end{short}
  The macro performs the bindings sequentially, like the @sym{let*} macro.

  Each box can be initialized with values using the syntax for the
  @fun{gsk:with-rounded-rect} macro. See also the @fun{gsk:rounded-rect}
  documentation.
  @see-symbol{gsk:rounded-rect}
  @see-macro{gsk:with-rounded-rect}"
  (if vars
      (let ((var (glib-sys:mklist (first vars))))
        `(with-rounded-rect ,var
           (with-rounded-rects ,(rest vars)
             ,@body)))
      `(progn ,@body)))

(export 'with-rounded-rects)

;;; ----------------------------------------------------------------------------
;;; gsk_rounded_rect_init
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_rounded_rect_init" rounded-rect-init)
    (:pointer (:struct rounded-rect))
 #+liber-documentation
 "@version{2025-08-02}
  @argument[rect]{a @sym{gsk:rounded-rect} instance to initialize}
  @argument[bounds]{a @sym{graphene:rect-t} instance describing the bounds}
  @argument[top-left]{a @sym{graphene:size-t} instance for the rounding radius
    of the top left corner}
  @argument[top-right]{a @sym{graphene:size-t} instance for the rounding radius
    of the top right corner}
  @argument[bottom-right]{a @sym{graphene:size-t} instance for the rounding
    radius of the bottom right corner}
  @argument[bottom-left]{a @sym{graphene:size-t} instance for the rounding
    radius of the bottom left corner}
  @return{The initialized @sym{gsk:rounded-rect} instance.}
  @begin{short}
    Initializes the given @arg{rect} with the given values.
  @end{short}
  This function will implicitly normalize the rounded rectangle before
  returning.
  @see-symbol{gsk:rounded-rect}
  @see-symbol{graphene:rect-t}
  @see-symbol{graphene:size-t}"
  (rect (:pointer (:struct rounded-rect)))
  (bounds (:pointer (:struct graphene:rect-t)))
  (top-left (:pointer (:struct graphene:size-t)))
  (top-right (:pointer (:struct graphene:size-t)))
  (bottom-right (:pointer (:struct graphene:size-t)))
  (bottom-left (:pointer (:struct graphene:size-t))))

(export 'rounded-rect-init)

;;; ----------------------------------------------------------------------------
;;; gsk_rounded_rect_init_from_rect
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_rounded_rect_init_from_rect" %rounded-rect-init-from-rect)
    (:pointer (:struct rounded-rect))
  (rect (:pointer (:struct rounded-rect)))
  (bounds (:pointer (:struct graphene:rect-t)))
  (radius :float))

(defun rounded-rect-init-from-rect (rect bounds radius)
 #+liber-documentation
 "@version{2025-08-02}
  @argument[rect]{a @sym{gsk:rounded-rect} instance}
  @argument[bounds]{a @sym{graphene:rect-t} instance}
  @argument[radius]{a number coerced to a single float for the border radius}
  @return{The initialized @sym{gsk:rounded-rect} instance.}
  @begin{short}
    Initializes @arg{rect} to the given @arg{bounds} and sets the radius of all
    four corners to @arg{radius}.
  @end{short}
  @see-symbol{gsk:rounded-rect}
  @see-symbol{graphene:rect-t}"
  (let ((radius (coerce radius 'single-float)))
    (%rounded-rect-init-from-rect rect bounds radius)))

(export 'rounded-rect-init-from-rect)

;;; ----------------------------------------------------------------------------
;;; gsk_rounded_rect_init_copy
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_rounded_rect_init_copy" rounded-rect-init-copy)
    (:pointer (:struct rounded-rect))
 #+liber-documentation
 "@version{2025-08-02}
  @argument[rect]{a @sym{gsk:rounded-rect} instance}
  @argument[src]{a @sym{gsk:rounded-rect} instance}
  @return{The initialized @sym{gsk:rounden-rect} instance.}
  @begin{short}
    Initializes @arg{rect} using the given @arg{src} rounded rectangle.
  @end{short}
  This function will not normalize the rounded rectangle, so make sure the
  source rounded rectangle is normalized.
  @see-symbol{gsk:rounded-rect}"
  (rect (:pointer (:struct rounded-rect)))
  (src (:pointer (:struct rounded-rect))))

(export 'rounded-rect-init-copy)

;;; ----------------------------------------------------------------------------
;;; gsk_rounded_rect_normalize
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_rounded_rect_normalize" rounded-rect-normalize)
    (:pointer (:struct rounded-rect))
 #+liber-documentation
 "@version{#2025-08-02}
  @argument[rect]{a @sym{gsk:rounded-rect} instance}
  @return{The normalized @sym{gsk:rounded-rect} instance.}
  @begin{short}
    Normalizes the passed rounded rectangle.
  @end{short}
  This function will ensure that the bounds of the rounded rectangle are
  normalized and ensure that the corner values are positive and the corners do
  not overlap.
  @see-class{gsk:rounded-rect}"
  (rect (:pointer (:struct rounded-rect))))

(export 'rounded-rect-normalize)

;;; ----------------------------------------------------------------------------
;;; gsk_rounded_rect_offset
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_rounded_rect_offset" %rounded-rect-offset)
    (:pointer (:struct rounded-rect))
  (rect (:pointer (:struct rounded-rect)))
  (dx :float)
  (dy :float))

(defun rounded-rect-offset (rect dx dy)
 #+liber-documentation
 "@version{#2025-08-02}
  @argument[rect]{a @sym{gsk:rounded-rect} instance}
  @return{The offset @sym{gsk:rounded-rect} instance.}
  @begin{short}
    Offsets the origin of the bound by @arg{dx} and @arg{dy}.
  @end{short}
  The size and corners of the rounded rectangle are unchanged.
  @see-symbol{gsk:rounded-rect}"
  (let ((dx (coerce dx 'single-float))
        (dy (coerce dy 'single-float)))
    (%rounded-rect-offset rect dx dy)))

(export 'rounded-rect-offset)

;;; ----------------------------------------------------------------------------
;;; gsk_rounded_rect_shrink
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_rounded_rect_shrink" %rounded-rect-shrink)
    (:pointer (:struct rounded-rect))
  (rect (:pointer (:struct rounded-rect)))
  (top :float)
  (right :float)
  (bottom :float)
  (left :float))

(defun rounded-rect-shrink (rect top right bottom left)
 #+liber-documentation
 "@version{#2025-08-02}
  @argument[rect]{a @sym{gsk:rounded-rect} instance}
  @argument[top]{a number coerced to a single float how far to move the top
    side downwards}
  @argument[right]{a number coerced to a single float how far to move the right
    side to the left}
  @argument[bottom]{a number coerced to a single float how far to move the
    bottom side upwards}
  @argument[left]{a number coerced to a single float how far to move the left
    side to the right}
  @return{The resized @sym{gsk:rounded-rect} instance.}
  @begin{short}
    Shrinks (or grows) the given rounded rectangle by moving the 4 sides
    according to the offsets given.
  @end{short}
  The corner radii will be changed in a way that tries to keep the center of
  the corner circle intact. This emulates CSS behavior.

  This function also works for growing rounded rectangles if you pass negative
  values for the arguments.
  @see-symbol{gsk:rounded-rect}"
  (%rounded-rect-shrink rect
                        (coerce top 'single-float)
                        (coerce right 'single-float)
                        (coerce bottom 'single-float)
                        (coerce left 'single-float)))

(export 'rounded-rect-shrink)

;;; ----------------------------------------------------------------------------
;;; gsk_rounded_rect_is_rectilinear
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_rounded_rect_is_rectilinear" rounded-rect-is-rectilinear)
    :boolean
 #+liber-documentation
 "@version{#2025-08-02}
  @argument[rect]{a @sym{gsk:rounded-rect} instance to check}
  @return{@em{True} if the rounded rectangle is rectilinear.}
  @begin{short}
    Checks if all corners of @arg{rect} are right angles and the rounded
    rectangle covers all of its bounds.
  @end{short}
  This information can be used to decide if the @fun{gsk:clip-node-new}
  or the @fun{gsk:rounded-clip-node-new} function should be called.
  @see-symbol{gsk:rounded-rect}"
  (rect (:pointer (:struct rounded-rect))))

(export 'rounded-rect-is-rectilinear)

;;; ----------------------------------------------------------------------------
;;;gsk_rounded_rect_contains_point
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_rounded_rect_contains_point" rounded-rect-contains-point)
    :boolean
 #+liber-documentation
 "@version{#2025-08-02}
  @argument[rect]{a @sym{gsk:rounded-rect} instance}
  @argument[point]{a @sym{graphene:point-t} instance for the point to check}
  @return{@em{True} if the point is inside the rounded rectangle.}
  @begin{short}
    Checks if the given point is inside the rounded rectangle.
  @end{short}
  This function returns @em{false} if the point is in the rounded corner areas.
  @see-symbol{gsk:rounded-rect}
  @see-symbol{graphene:point-t}"
  (rect (:pointer (:struct rounded-rect)))
  (point (:pointer (:struct graphene:point-t))))

(export 'rounded-rect-contains-point)

;;; ----------------------------------------------------------------------------
;;; gsk_rounded_rect_contains_rect
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_rounded_rect_contains_rect" rounded-rect-contains-rect)
    :boolean
 #+liber-documentation
 "@version{#2025-08-03}
  @argument[rect]{a @sym{gsk:rounded-rect} instance}
  @argument[other]{a @sym{graphene:rect-t} instance for the rectangle to check}
  @begin{return}
    @em{True} if @arg{other} is fully contained inside the rounded rectangle.
  @end{return}
  @begin{short}
    Checks if the given @arg{other} is contained inside the rounded rectangle.
  @end{short}
  This function returns @em{false} if @arg{other} extends into one of the
  rounded corner areas.
  @see-symbol{gsk:rounded-rect}
  @see-symbol{graphene:rect-t}"
  (rect (:pointer (:struct rounded-rect)))
  (other (:pointer (:struct graphene:rect-t))))

(export 'rounded-rect-contains-rect)

;;; ----------------------------------------------------------------------------
;;; gsk_rounded_rect_intersects_rect
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_rounded_rect_intersects_rect" rounded-rect-intersects-rect)
    :boolean
 #+liber-documentation
 "@version{#2025-08-02}
  @argument[rect]{a @sym{gsk:rounded-rect} instance}
  @argument[other]{a @sym{graphene:rect-t} instance to check}
  @return{@em{True} if @arg{other} intersects with the rounded rectangle.}
  @begin{short}
    Checks if part of the given @arg{other} is contained inside the rounded
    rectangle.
  @end{short}
  This function returns @em{false} if @arg{other} only extends into one of the
  rounded corner areas but not into the rounded rectangle itself.
  @see-symbol{gsk:rounded-rect}
  @see-symbol{graphene:rect-t}"
  (rect (:pointer (:struct rounded-rect)))
  (other (:pointer (:struct graphene:rect-t))))

(export 'rounded-rect-intersects-rect)

;;; --- End of file gsk.rounded-rect.lisp --------------------------------------

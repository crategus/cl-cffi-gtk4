;;; ----------------------------------------------------------------------------
;;; gsk.rounded-rect.lisp
;;;
;;; The documentation of this file is taken from the GSK 4 Reference Manual
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
;;;     GSK_ROUNDED_RECT_INIT()
;;;
;;;     gsk_rounded_rect_init
;;;     gsk_rounded_rect_init_copy
;;;     gsk_rounded_rect_init_from_rect
;;;     gsk_rounded_rect_normalize
;;;     gsk_rounded_rect_offset
;;;     gsk_rounded_rect_shrink
;;;     gsk_rounded_rect_is_rectilinear
;;;     gsk_rounded_rect_contains_point
;;;     gsk_rounded_rect_contains_rect
;;;     gsk_rounded_rect_intersects_rect
;;;
;;; Description
;;;
;;; GskRoundedRect defines a rectangle with rounded corners, as is commonly
;;; used in drawing.
;;;
;;; Operations on a GskRoundedRect will normalize the rectangle, to ensure that
;;; the bounds are normalized and that the corner sizes don't exceed the size of
;;; the rectangle. The algorithm used for normalizing corner sizes is described
;;; in the CSS specification.
;;; ----------------------------------------------------------------------------

(in-package :gsk)

;;; ----------------------------------------------------------------------------
;;; enum GskCorner
;;; ----------------------------------------------------------------------------

(gobject:define-g-enum "GskCorner" corner
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
 "@version{2023-10-27}
  @begin{short}
    The corner indices used by the @symbol{gsk:rounded-rect} instance.
  @end{short}
  @begin{pre}
(gobject:define-g-enum \"GskCorner\" corner
  (:export t
   :type-initializer \"gsk_corner_get_type\")
  (:top-left 0)
  (:top-right 1)
  (:bottom-right 2)
  (:bottom-left 3))
  @end{pre}
  @begin[code]{table}
    @entry[:top-left]{The top left corner.}
    @entry[:top-right]{The top right corner.}
    @entry[:bottom-right]{The bottom right corner.}
    @entry[:bottom-left]{The bottom left corner.}
  @end{table}
  @see-symbol{gsk:rounded-rect}")

(export 'corner)

;;; ----------------------------------------------------------------------------
;;; struct GskRoundedRect
;;;
;;; struct GskRoundedRect {
;;;   graphene_rect_t bounds;
;;;
;;;   graphene_size_t corner[4];
;;; };
;;;
;;; A rectangular region with rounded corners.
;;;
;;; Application code should normalize rectangles using
;;; gsk_rounded_rect_normalize(); this function will ensure that the bounds of
;;; the rectangle are normalized and ensure that the corner values are positive
;;; and the corners do not overlap. All functions taking a GskRoundedRect as an
;;; argument will internally operate on a normalized copy; all functions
;;; returning a GskRoundedRect will always return a normalized one.
;;;
;;; graphene_rect_t bounds;
;;;
;;;
;;; graphene_size_t corner[4];
;;;
;;; ----------------------------------------------------------------------------

(cffi:defcstruct rounded-rect
  (bounds (:struct graphene:rect-t))
  (corner (:struct graphene:size-t) :count 4))

#+liber-documentation
(setf (liber:alias-for-symbol 'rounded-rect)
      "CStruct"
      (liber:symbol-documentation 'rounded-rect)
 "@version{2023-10-27}
  @begin{short}
    A rectangular region with rounded corners.
  @end{short}
  Application code should normalize rectangles using the
  @fun{gsk:rounded-rect-normalize} function. This function will ensure that the
  bounds of the rectangle are normalized and ensure that the corner values are
  positive and the corners do not overlap.

  All functions taking a @symbol{gsk:rounded-rect} instance as an argument will
  internally operate on a normalized copy. All functions returning a
  @symbol{gsk:rounded-rect} instance will always return a normalized one.

  The algorithm used for normalizing corner sizes is described in the
  @url[https://drafts.csswg.org/css-backgrounds-3/#border-radius]{CSS specification}.
  @begin{pre}
(cffi:defcstruct rounded-rect
  (bounds (:struct graphene:rect-t))
  (corner (:struct graphene:size-t) :count 4))
  @end{pre}
  @begin[code]{table}
    @entry[bounds]{A @symbol{graphene:rect-t} instance with the bounds of the
      rectangle.}
    @entry[corner]{An array of @symbol{graphene:size-t} instances with the size
      of the 4 rounded corners.}
  @end{table}
  @see-slot{gsk:rounded-rect-bounds}
  @see-slot{gsk:rounded-rect-corner}
  @see-function{gsk:rounded-rect-normalize}")

(export 'rounded-rect)

;;; ----------------------------------------------------------------------------

(defun rounded-rect-bounds (rect)
  (cffi:foreign-slot-pointer rect '(:struct rounded-rect) 'bounds))

(export 'rounded-rect-bounds)

(defun rounded-rect-corner (rect nth)
  (let ((ptr (cffi:foreign-slot-pointer rect '(:struct rounded-rect) 'corner)))
    (cffi:mem-aptr ptr '(:struct graphene:size-t) nth)))

(export 'rounded-rect-corner)

;;; ----------------------------------------------------------------------------
;;; GSK_ROUNDED_RECT_INIT()
;;;
;;; #define             GSK_ROUNDED_RECT_INIT(_x,_y,_w,_h)
;;;
;;; Initializes a GskRoundedRect when declaring it. All corner sizes will be
;;; initialized to 0.
;;;
;;; _x :
;;;     the X coordinate of the origin
;;;
;;; _y :
;;;     the Y coordinate of the origin
;;;
;;; _w :
;;;     the width
;;;
;;; _h :
;;;     the height
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gsk_rounded_rect_init ()
;;;
;;; GskRoundedRect *
;;; gsk_rounded_rect_init (GskRoundedRect *self,
;;;                        const graphene_rect_t *bounds,
;;;                        const graphene_size_t *top_left,
;;;                        const graphene_size_t *top_right,
;;;                        const graphene_size_t *bottom_right,
;;;                        const graphene_size_t *bottom_left);
;;;
;;; Initializes the given GskRoundedRect with the given values.
;;;
;;; This function will implicitly normalize the GskRoundedRect before returning.
;;;
;;; self :
;;;     The GskRoundedRect to initialize
;;;
;;; bounds :
;;;     a graphene_rect_t describing the bounds
;;;
;;; top_left :
;;;     the rounding radius of the top left corner
;;;
;;; top_right :
;;;     the rounding radius of the top right corner
;;;
;;; bottom_right :
;;;     the rounding radius of the bottom right corner
;;;
;;; bottom_left :
;;;     the rounding radius of the bottom left corner
;;;
;;; Returns :
;;;     the initialized rectangle.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_rounded_rect_init" rounded-rect-init)
    (:pointer (:struct rounded-rect))
  (rect (:pointer (:struct rounded-rect)))
  (bounds (:pointer (:struct graphene:rect-t)))
  (top-left (:pointer (:struct graphene:size-t)))
  (top-right (:pointer (:struct graphene:size-t)))
  (bottom-right (:pointer (:struct graphene:size-t)))
  (bottom-left (:pointer (:struct graphene:size-t))))

(export 'rounded-rect-init)

;;; ----------------------------------------------------------------------------
;;; gsk_rounded_rect_init_copy ()
;;;
;;; GskRoundedRect *
;;; gsk_rounded_rect_init_copy (GskRoundedRect *self,
;;;                             const GskRoundedRect *src);
;;;
;;; Initializes self using the given src rectangle.
;;;
;;; This function will not normalize the GskRoundedRect, so make sure the source
;;; is normalized.
;;;
;;; self :
;;;     a GskRoundedRect
;;;
;;; src :
;;;     a GskRoundedRect
;;;
;;; Returns :
;;;     the initialized rectangle.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_rounded_rect_init_copy" rounded-rect-init-copy)
    (:pointer (:struct rounded-rect))
  (rect (:pointer (:struct rounded-rect)))
  (src (:pointer (:struct rounded-rect))))

(export 'rounded-rect-init-copy)

;;; ----------------------------------------------------------------------------
;;; gsk_rounded_rect_init_from_rect ()
;;;
;;; GskRoundedRect *
;;; gsk_rounded_rect_init_from_rect (GskRoundedRect *self,
;;;                                  const graphene_rect_t *bounds,
;;;                                  float radius);
;;;
;;; Initializes self to the given bounds and sets the radius of all four corners
;;; to radius .
;;;
;;; self :
;;;     a GskRoundedRect
;;;
;;; bounds :
;;;     a graphene_rect_t
;;;
;;; radius :
;;;     the border radius
;;;
;;; Returns :
;;;     the initialized rectangle.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_rounded_rect_init_from_rect" %rounded-rect-init-from-rect)
    (:pointer (:struct rounded-rect))
  (rect (:pointer (:struct rounded-rect)))
  (bounds (:pointer (:struct graphene:rect-t)))
  (radius :float))

(defun rounded-rect-init-from-rect (rect bounds radius)
  (let ((radius (coerce radius 'single-float)))
    (%rounded-rect-init-from-rect rect bounds radius)))

(export 'rounded-rect-init-from-rect)

;;; ----------------------------------------------------------------------------
;;; gsk_rounded_rect_normalize ()
;;;
;;; GskRoundedRect *
;;; gsk_rounded_rect_normalize (GskRoundedRect *self);
;;;
;;; Normalizes the passed rectangle.
;;;
;;; this function will ensure that the bounds of the rectangle are normalized
;;; and ensure that the corner values are positive and the corners do not
;;; overlap.
;;;
;;; self :
;;;     a GskRoundedRect
;;;
;;; Returns :
;;;     the normalized rectangle.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_rounded_rect_normalize" rounded-rect-normalize)
    (:pointer (:struct rounded-rect))
  (rect (:pointer (:struct rounded-rect))))

(export 'rounded-rect-normalize)

;;; ----------------------------------------------------------------------------
;;; gsk_rounded_rect_offset ()
;;;
;;; GskRoundedRect *
;;; gsk_rounded_rect_offset (GskRoundedRect *self,
;;;                          float dx,
;;;                          float dy);
;;;
;;; Offsets the bound's origin by dx and dy .
;;;
;;; The size and corners of the rectangle are unchanged.
;;;
;;; self :
;;;     a GskRoundedRect
;;;
;;; dx :
;;;     the horizontal offset
;;;
;;; dy :
;;;     the vertical offset
;;;
;;; Returns :
;;;     the offset rectangle.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_rounded_rect_offset" %rounded-rect-offset)
    (:pointer (:struct rounded-rect))
  (rect (:pointer (:struct rounded-rect)))
  (dx :float)
  (dy :float))

(defun rounded-rect-offset (rect dx dy)
  (let ((dx (coerce dx 'single-float))
        (dy (coerce dy 'single-float)))
    (%rounded-rect-offset rect dx dy)))

(export 'rounded-rect-offset)

;;; ----------------------------------------------------------------------------
;;; gsk_rounded_rect_shrink ()
;;;
;;;GskRoundedRect *
;;;gsk_rounded_rect_shrink (GskRoundedRect *self,
;;;                         float top,
;;;                         float right,
;;;                         float bottom,
;;;                         float left);
;;;Shrinks (or grows) the given rectangle by moving the 4 sides according to the offsets given. The corner radii will be changed in a way that tries to keep the center of the corner circle intact. This emulates CSS behavior.

;;;This function also works for growing rectangles if you pass negative values for the top , right , bottom or left .

;;;self
;;;The GskRoundedRect to shrink or grow

;;;top
;;;How far to move the top side downwards

;;;right
;;;How far to move the right side to the left

;;;bottom
;;;How far to move the bottom side upwards

;;;left
;;;How far to move the left side to the right

;;;Returns
;;;the resized GskRoundedRect.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_rounded_rect_shrink" %rounded-rect-shrink)
    (:pointer (:struct rounded-rect))
  (rect (:pointer (:struct rounded-rect)))
  (top :float)
  (right :float)
  (bottom :float)
  (left :float))

(defun rounded-rect-shrink (rect top right bottom left)
  (%rounded-rect-shrink rect
                        (coerce top 'single-float)
                        (coerce right 'single-float)
                        (coerce bottom 'single-float)
                        (coerce left 'single-float)))

(export 'rounded-rect-shrink)

;;; ----------------------------------------------------------------------------
;;; gsk_rounded_rect_is_rectilinear ()
;;;
;;;gboolean
;;;gsk_rounded_rect_is_rectilinear (const GskRoundedRect *self);
;;;Checks if all corners of self are right angles and the rectangle covers all of its bounds.

;;;This information can be used to decide if gsk_clip_node_new() or gsk_rounded_clip_node_new() should be called.

;;;self
;;;the GskRoundedRect to check

;;;Returns
;;;TRUE if the rectangle is rectilinear
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_rounded_rect_is_rectilinear" rounded-rect-ist-rectilinear)
    :boolean
  (rect (:pointer (:struct rounded-rect))))

(export 'rounded-rect-is-rectilinear)

;;; ----------------------------------------------------------------------------
;;;gsk_rounded_rect_contains_point ()
;;;gboolean
;;;gsk_rounded_rect_contains_point (const GskRoundedRect *self,
;;;                                 const graphene_point_t *point);
;;;Checks if the given point is inside the rounded rectangle. This function returns FALSE if the point is in the rounded corner areas.

;;;self
;;;a GskRoundedRect

;;;point
;;;the point to check

;;;Returns
;;;TRUE if the point is inside the rounded rectangle
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_rounded_rect_contains_point" rounded-rect-contains-point)
    :boolean
  (rect (:pointer (:struct rounded-rect)))
  (point (:pointer (:struct graphene:point-t))))

(export 'rounded-rect-contains-point)

;;; ----------------------------------------------------------------------------
;;; gsk_rounded_rect_contains_rect ()
;;;
;;;gboolean
;;;gsk_rounded_rect_contains_rect (const GskRoundedRect *self,
;;;                                const graphene_rect_t *rect);
;;;Checks if the given rect is contained inside the rounded rectangle. This function returns FALSE if rect extends into one of the rounded corner areas.

;;;self
;;;a GskRoundedRect

;;;rect
;;;the rectangle to check

;;;Returns
;;;TRUE if the rect is fully contained inside the rounded rectangle
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_rounded_rect_contains_rect" rounded-rect-contains-rect)
    :boolean
  (rect (:pointer (:struct rounded-rect)))
  (other (:pointer (:struct graphene:rect-t))))

(export 'rounded-rect-contains-rect)

;;; ----------------------------------------------------------------------------
;;; gsk_rounded_rect_intersects_rect ()
;;;
;;;gboolean
;;;gsk_rounded_rect_intersects_rect (const GskRoundedRect *self,
;;;                                  const graphene_rect_t *rect);
;;;Checks if part of the given rect is contained inside the rounded rectangle. This function returns FALSE if rect only extends into one of the rounded corner areas but not into the rounded rectangle itself.

;;;self
;;;a GskRoundedRect

;;;rect
;;;the rectangle to check

;;;Returns
;;;TRUE if the rect intersects with the rounded rectangle
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_rounded_rect_intersects_rect" rounded-rect-intersects-rect)
    :boolean
  (rect (:pointer (:struct rounded-rect)))
  (other (:pointer (:struct graphene:rect-t))))

(export 'rounded-rect-intersects-rect)

;;; --- End of file gsk.rounded-rect.lisp --------------------------------------

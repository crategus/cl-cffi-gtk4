;;; ----------------------------------------------------------------------------
;;; gsk.rounded-rect.lisp
;;;
;;; The documentation of this file is taken from the GSK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 Dieter Kaiser
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




;;;Functions
;;;GSK_ROUNDED_RECT_INIT()
;;;#define             GSK_ROUNDED_RECT_INIT(_x,_y,_w,_h)
;;;Initializes a GskRoundedRect when declaring it. All corner sizes will be initialized to 0.

;;;Parameters
;;;_x

;;;the X coordinate of the origin


;;;_y

;;;the Y coordinate of the origin


;;;_w

;;;the width


;;;_h

;;;the height


;;;gsk_rounded_rect_init ()
;;;GskRoundedRect *
;;;gsk_rounded_rect_init (GskRoundedRect *self,
;;;                       const graphene_rect_t *bounds,
;;;                       const graphene_size_t *top_left,
;;;                       const graphene_size_t *top_right,
;;;                       const graphene_size_t *bottom_right,
;;;                       const graphene_size_t *bottom_left);
;;;Initializes the given GskRoundedRect with the given values.

;;;This function will implicitly normalize the GskRoundedRect before returning.

;;;Parameters
;;;self

;;;The GskRoundedRect to initialize


;;;bounds

;;;a graphene_rect_t describing the bounds


;;;top_left

;;;the rounding radius of the top left corner


;;;top_right

;;;the rounding radius of the top right corner


;;;bottom_right

;;;the rounding radius of the bottom right corner


;;;bottom_left

;;;the rounding radius of the bottom left corner


;;;Returns
;;;the initialized rectangle.

;;;[transfer none]

;;;gsk_rounded_rect_init_copy ()
;;;GskRoundedRect *
;;;gsk_rounded_rect_init_copy (GskRoundedRect *self,
;;;                            const GskRoundedRect *src);
;;;Initializes self using the given src rectangle.

;;;This function will not normalize the GskRoundedRect, so make sure the source is normalized.

;;;Parameters
;;;self

;;;a GskRoundedRect


;;;src

;;;a GskRoundedRect


;;;Returns
;;;the initialized rectangle.

;;;[transfer none]

;;;gsk_rounded_rect_init_from_rect ()
;;;GskRoundedRect *
;;;gsk_rounded_rect_init_from_rect (GskRoundedRect *self,
;;;                                 const graphene_rect_t *bounds,
;;;                                 float radius);
;;;Initializes self to the given bounds and sets the radius of all four corners to radius .

;;;Parameters
;;;self

;;;a GskRoundedRect


;;;bounds

;;;a graphene_rect_t


;;;radius

;;;the border radius


;;;Returns
;;;the initialized rectangle.

;;;[transfer none]

;;;gsk_rounded_rect_normalize ()
;;;GskRoundedRect *
;;;gsk_rounded_rect_normalize (GskRoundedRect *self);
;;;Normalizes the passed rectangle.

;;;this function will ensure that the bounds of the rectangle are normalized and ensure that the corner values are positive and the corners do not overlap.

;;;Parameters
;;;self

;;;a GskRoundedRect


;;;Returns
;;;the normalized rectangle.

;;;[transfer none]

;;;gsk_rounded_rect_offset ()
;;;GskRoundedRect *
;;;gsk_rounded_rect_offset (GskRoundedRect *self,
;;;                         float dx,
;;;                         float dy);
;;;Offsets the bound's origin by dx and dy .

;;;The size and corners of the rectangle are unchanged.

;;;Parameters
;;;self

;;;a GskRoundedRect


;;;dx

;;;the horizontal offset


;;;dy

;;;the vertical offset


;;;Returns
;;;the offset rectangle.

;;;[transfer none]

;;;gsk_rounded_rect_shrink ()
;;;GskRoundedRect *
;;;gsk_rounded_rect_shrink (GskRoundedRect *self,
;;;                         float top,
;;;                         float right,
;;;                         float bottom,
;;;                         float left);
;;;Shrinks (or grows) the given rectangle by moving the 4 sides according to the offsets given. The corner radii will be changed in a way that tries to keep the center of the corner circle intact. This emulates CSS behavior.

;;;This function also works for growing rectangles if you pass negative values for the top , right , bottom or left .

;;;Parameters
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

;;;[transfer none]

;;;gsk_rounded_rect_is_rectilinear ()
;;;gboolean
;;;gsk_rounded_rect_is_rectilinear (const GskRoundedRect *self);
;;;Checks if all corners of self are right angles and the rectangle covers all of its bounds.

;;;This information can be used to decide if gsk_clip_node_new() or gsk_rounded_clip_node_new() should be called.

;;;Parameters
;;;self

;;;the GskRoundedRect to check


;;;Returns
;;;TRUE if the rectangle is rectilinear

;;;gsk_rounded_rect_contains_point ()
;;;gboolean
;;;gsk_rounded_rect_contains_point (const GskRoundedRect *self,
;;;                                 const graphene_point_t *point);
;;;Checks if the given point is inside the rounded rectangle. This function returns FALSE if the point is in the rounded corner areas.

;;;Parameters
;;;self

;;;a GskRoundedRect


;;;point

;;;the point to check


;;;Returns
;;;TRUE if the point is inside the rounded rectangle

;;;gsk_rounded_rect_contains_rect ()
;;;gboolean
;;;gsk_rounded_rect_contains_rect (const GskRoundedRect *self,
;;;                                const graphene_rect_t *rect);
;;;Checks if the given rect is contained inside the rounded rectangle. This function returns FALSE if rect extends into one of the rounded corner areas.

;;;Parameters
;;;self

;;;a GskRoundedRect


;;;rect

;;;the rectangle to check


;;;Returns
;;;TRUE if the rect is fully contained inside the rounded rectangle

;;;gsk_rounded_rect_intersects_rect ()
;;;gboolean
;;;gsk_rounded_rect_intersects_rect (const GskRoundedRect *self,
;;;                                  const graphene_rect_t *rect);
;;;Checks if part of the given rect is contained inside the rounded rectangle. This function returns FALSE if rect only extends into one of the rounded corner areas but not into the rounded rectangle itself.

;;;Parameters
;;;self

;;;a GskRoundedRect


;;;rect

;;;the rectangle to check


;;;Returns
;;;TRUE if the rect intersects with the rounded rectangle

;;;Types and Values
;;;enum GskCorner
;;;The corner indices used by GskRoundedRect.

;;;Members
;;;GSK_CORNER_TOP_LEFT

;;;The top left corner


;;;GSK_CORNER_TOP_RIGHT

;;;The top right corner


;;;GSK_CORNER_BOTTOM_RIGHT

;;;The bottom right corner


;;;GSK_CORNER_BOTTOM_LEFT

;;;The bottom left corner


;;;struct GskRoundedRect
;;;struct GskRoundedRect {
;;;  graphene_rect_t bounds;

;;;  graphene_size_t corner[4];
;;;};
;;;A rectangular region with rounded corners.

;;;Application code should normalize rectangles using gsk_rounded_rect_normalize(); this function will ensure that the bounds of the rectangle are normalized and ensure that the corner values are positive and the corners do not overlap. All functions taking a GskRoundedRect as an argument will internally operate on a normalized copy; all functions returning a GskRoundedRect will always return a normalized one.

;;;Members
;;;graphene_rect_t bounds;

;;;the bounds of the rectangle


;;;graphene_size_t corner[4];

;;;the size of the 4 rounded corners

;;; --- End of file gsk.rounded-rect.lisp --------------------------------------

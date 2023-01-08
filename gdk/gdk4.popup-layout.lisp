;;; ----------------------------------------------------------------------------
;;; gdk.popup-layout.lisp
;;;
;;; The documentation of this file is taken from the GDK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GDK library.
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
;;; GdkPopupLayout
;;;
;;;     Information for presenting popups
;;;
;;; Types and Values
;;;
;;;     GdkPopupLayout
;;;     GdkAnchorHints
;;;
;;; Functions
;;;
;;;     gdk_popup_layout_new
;;;     gdk_popup_layout_ref
;;;     gdk_popup_layout_unref
;;;     gdk_popup_layout_copy
;;;     gdk_popup_layout_equal
;;;     gdk_popup_layout_set_anchor_rect
;;;     gdk_popup_layout_get_anchor_rect
;;;     gdk_popup_layout_set_rect_anchor
;;;     gdk_popup_layout_get_rect_anchor
;;;     gdk_popup_layout_set_surface_anchor
;;;     gdk_popup_layout_get_surface_anchor
;;;     gdk_popup_layout_set_anchor_hints
;;;     gdk_popup_layout_get_anchor_hints
;;;     gdk_popup_layout_set_offset
;;;     gdk_popup_layout_get_offset
;;;
;;; Object Hierarchy
;;;
;;;     GBoxed
;;;     ╰── GdkPopupLayout
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;;Description
;;;Popups are positioned relative to their parent surface. The GdkPopupLayout struct contains information that is necessary to do so.

;;;The positioning requires a negotiation with the windowing system, since it depends on external constraints, such as the position of the parent surface, and the screen dimensions.

;;;The basic ingredients are a rectangle on the parent surface, and the anchor on both that rectangle and the popup. The anchors specify a side or corner to place next to each other.

;;;Popup anchors

;;;For cases where placing the anchors next to each other would make the popup extend offscreen, the layout includes some hints for how to resolve this problem. The hints may suggest to flip the anchor position to the other side, or to 'slide' the popup along a side, or to resize it.

;;;Flipping popups

;;;Sliding popups

;;;These hints may be combined.

;;;Ultimatively, it is up to the windowing system to determine the position and size of the popup. You can learn about the result by calling gdk_popup_get_position_x(), gdk_popup_get_position_y(), gdk_popup_get_rect_anchor() and gdk_popup_get_surface_anchor() after the popup has been presented. This can be used to adjust the rendering. For example, GtkPopover changes its arrow position accordingly. But you have to be careful avoid changing the size of the popover, or it has to be presented again.

;;;Functions
;;;gdk_popup_layout_new ()
;;;GdkPopupLayout *
;;;gdk_popup_layout_new (const GdkRectangle *anchor_rect,
;;;                      GdkGravity rect_anchor,
;;;                      GdkGravity surface_anchor);
;;;Create a popup layout description. Used together with gdk_popup_present() to describe how a popup surface should be placed and behave on-screen.

;;;anchor_rect is relative to the top-left corner of the surface's parent. rect_anchor and surface_anchor determine anchor points on anchor_rect and surface to pin together.

;;;The position of anchor_rect 's anchor point can optionally be offset using gdk_popup_layout_set_offset(), which is equivalent to offsetting the position of surface.

;;;[constructor]

;;;Parameters
;;;anchor_rect

;;;the anchor GdkRectangle to align surface with.

;;;[not nullable]
;;;rect_anchor

;;;the point on anchor_rect to align with surface 's anchor point

;;;
;;;surface_anchor

;;;the point on surface to align with rect 's anchor point

;;;
;;;Returns
;;;newly created instance of GdkPopupLayout.

;;;[transfer full]

;;;gdk_popup_layout_ref ()
;;;GdkPopupLayout *
;;;gdk_popup_layout_ref (GdkPopupLayout *layout);
;;;Increases the reference count of value .

;;;Parameters
;;;layout

;;;a GdkPopupLayout

;;;
;;;Returns
;;;the same layout

;;;gdk_popup_layout_unref ()
;;;void
;;;gdk_popup_layout_unref (GdkPopupLayout *layout);
;;;Decreases the reference count of value .

;;;Parameters
;;;layout

;;;a GdkPopupLayout

;;;
;;;gdk_popup_layout_copy ()
;;;GdkPopupLayout *
;;;gdk_popup_layout_copy (GdkPopupLayout *layout);
;;;Create a new GdkPopupLayout and copy the contents of layout into it.

;;;Parameters
;;;layout

;;;a GdkPopupLayout

;;;
;;;Returns
;;;a copy of layout .

;;;[transfer full]

;;;gdk_popup_layout_equal ()
;;;gboolean
;;;gdk_popup_layout_equal (GdkPopupLayout *layout,
;;;                        GdkPopupLayout *other);
;;;Check whether layout and other has identical layout properties.

;;;Parameters
;;;layout

;;;a GdkPopupLayout

;;;
;;;other

;;;another GdkPopupLayout

;;;
;;;Returns
;;;TRUE if layout and other have identical layout properties, otherwise FALSE.

;;;gdk_popup_layout_set_anchor_rect ()
;;;void
;;;gdk_popup_layout_set_anchor_rect (GdkPopupLayout *layout,
;;;                                  const GdkRectangle *anchor_rect);
;;;Set the anchor rectangle.

;;;Parameters
;;;layout

;;;a GdkPopupLayout

;;;
;;;anchor_rect

;;;the new anchor rectangle

;;;
;;;gdk_popup_layout_get_anchor_rect ()
;;;const GdkRectangle *
;;;gdk_popup_layout_get_anchor_rect (GdkPopupLayout *layout);
;;;Get the anchor rectangle.

;;;Parameters
;;;layout

;;;a GdkPopupLayout

;;;
;;;Returns
;;;The anchor rectangle.

;;;gdk_popup_layout_set_rect_anchor ()
;;;void
;;;gdk_popup_layout_set_rect_anchor (GdkPopupLayout *layout,
;;;                                  GdkGravity anchor);
;;;Set the anchor on the anchor rectangle.

;;;Parameters
;;;layout

;;;a GdkPopupLayout

;;;
;;;anchor

;;;the new rect anchor

;;;
;;;gdk_popup_layout_get_rect_anchor ()
;;;GdkGravity
;;;gdk_popup_layout_get_rect_anchor (GdkPopupLayout *layout);
;;;Returns the anchor position on the anchor rectangle.

;;;Parameters
;;;layout

;;;a GdkPopupLayout

;;;
;;;Returns
;;;the anchor on the anchor rectangle.

;;;gdk_popup_layout_set_surface_anchor ()
;;;void
;;;gdk_popup_layout_set_surface_anchor (GdkPopupLayout *layout,
;;;                                     GdkGravity anchor);
;;;Set the anchor on the popup surface.

;;;Parameters
;;;layout

;;;a GdkPopupLayout

;;;
;;;anchor

;;;the new popup surface anchor

;;;
;;;gdk_popup_layout_get_surface_anchor ()
;;;GdkGravity
;;;gdk_popup_layout_get_surface_anchor (GdkPopupLayout *layout);
;;;Returns the anchor position on the popup surface.

;;;Parameters
;;;layout

;;;a GdkPopupLayout

;;;
;;;Returns
;;;the anchor on the popup surface.

;;;gdk_popup_layout_set_anchor_hints ()
;;;void
;;;gdk_popup_layout_set_anchor_hints (GdkPopupLayout *layout,
;;;                                   GdkAnchorHints anchor_hints);
;;;Set new anchor hints.

;;;The set anchor_hints determines how surface will be moved if the anchor points cause it to move off-screen. For example, GDK_ANCHOR_FLIP_X will replace GDK_GRAVITY_NORTH_WEST with GDK_GRAVITY_NORTH_EAST and vice versa if surface extends beyond the left or right edges of the monitor.

;;;Parameters
;;;layout

;;;a GdkPopupLayout

;;;
;;;anchor_hints

;;;the new GdkAnchorHints

;;;
;;;gdk_popup_layout_get_anchor_hints ()
;;;GdkAnchorHints
;;;gdk_popup_layout_get_anchor_hints (GdkPopupLayout *layout);
;;;Get the GdkAnchorHints.

;;;Parameters
;;;layout

;;;a GdkPopupLayout

;;;
;;;Returns
;;;the GdkAnchorHints.

;;;gdk_popup_layout_set_offset ()
;;;void
;;;gdk_popup_layout_set_offset (GdkPopupLayout *layout,
;;;                             int dx,
;;;                             int dy);
;;;Offset the position of the anchor rectangle with the given delta.

;;;Parameters
;;;layout

;;;a GdkPopupLayout

;;;
;;;dx

;;;x delta to offset the anchor rectangle with

;;;
;;;dy

;;;y delta to offset the anchor rectangle with

;;;
;;;gdk_popup_layout_get_offset ()
;;;void
;;;gdk_popup_layout_get_offset (GdkPopupLayout *layout,
;;;                             int *dx,
;;;                             int *dy);
;;;Retrieves the offset for the anchor rectangle.

;;;Parameters
;;;layout

;;;a GdkPopupLayout

;;;
;;;dx

;;;return location for the delta X coordinate.

;;;[out]
;;;dy

;;;return location for the delta Y coordinate.

;;;[out]
;;;Types and Values
;;;GdkPopupLayout
;;;typedef struct _GdkPopupLayout GdkPopupLayout;
;;;Struct containing information for gdk_popup_present()

;;;enum GdkAnchorHints
;;;Positioning hints for aligning a surface relative to a rectangle.

;;;These hints determine how the surface should be positioned in the case that the surface would fall off-screen if placed in its ideal position.

;;;For example, GDK_ANCHOR_FLIP_X will replace GDK_GRAVITY_NORTH_WEST with GDK_GRAVITY_NORTH_EAST and vice versa if the surface extends beyond the left or right edges of the monitor.

;;;If GDK_ANCHOR_SLIDE_X is set, the surface can be shifted horizontally to fit on-screen. If GDK_ANCHOR_RESIZE_X is set, the surface can be shrunken horizontally to fit.

;;;In general, when multiple flags are set, flipping should take precedence over sliding, which should take precedence over resizing.

;;;Members
;;;GDK_ANCHOR_FLIP_X

;;;allow flipping anchors horizontally

;;;
;;;GDK_ANCHOR_FLIP_Y

;;;allow flipping anchors vertically

;;;
;;;GDK_ANCHOR_SLIDE_X

;;;allow sliding surface horizontally

;;;
;;;GDK_ANCHOR_SLIDE_Y

;;;allow sliding surface vertically

;;;
;;;GDK_ANCHOR_RESIZE_X

;;;allow resizing surface horizontally

;;;
;;;GDK_ANCHOR_RESIZE_Y

;;;allow resizing surface vertically

;;;
;;;GDK_ANCHOR_FLIP

;;;allow flipping anchors on both axes

;;;
;;;GDK_ANCHOR_SLIDE

;;;allow sliding surface on both axes

;;;
;;;GDK_ANCHOR_RESIZE

;;;allow resizing surface on both axes


;;; --- End of file gdk.popup-layout.lisp --------------------------------------

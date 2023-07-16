;;; ----------------------------------------------------------------------------
;;; gdk4.popup-layout.lisp
;;;
;;; The documentation of this file is taken from the GDK 4 Reference Manual
;;; Version 4.10 and modified to document the Lisp binding to the GDK library.
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
;;;     gdk_popup_layout_set_shadow_width                  Since 4.2
;;;     gdk_popup_layout_get_shadow_width                  Since 4.2
;;;
;;; Object Hierarchy
;;;
;;;     GBoxed
;;;     ╰── GdkPopupLayout
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; enum GdkAnchorHints
;;;

;;; ----------------------------------------------------------------------------

(gobject:define-g-flags "GdkAnchorHints" anchor-hints
  (:export t
   :type-initializer "gdk_anchor_hints_get_type")
  (:flip-x #.(ash 1 0))
  (:flip-y #.(ash 1 1))
  (:slide-x #.(ash 1 2))
  (:slide-y #.(ash 1 3))
  (:resize-x #.(ash 1 4))
  (:resize-y #.(ash 1 5))
  (:flip #.(+ (ash 1 0) (ash 1 1)))
  (:slide #.(+ (ash 1 2) (ash 1 3)))
  (:resize #.(+ (ash 1 4) (ash 1 5))))

#+liber-documentation
(setf (liber:alias-for-symbol 'toplevel-state)
      "GFlags"
      (liber:symbol-documentation 'toplevel-state)
 "@version{#2023-4-10}
  @begin{short}
    Positioning hints for aligning a surface relative to a rectangle.
  @end{short}
  These hints determine how the surface should be positioned in the case that
  the surface would fall off-screen if placed in its ideal position.

  For example, @code{:flip-x} will replace the @code{:north-west} gravity with
  the @code{:north-east} gravity and vice versa if the surface extends beyond
  the left or right edges of the monitor.

  If @code{:slide-x} is set, the surface can be shifted horizontally to fit
  on-screen. If @code{:resize-x} is set, the surface can be shrunken
  horizontally to fit.

  In general, when multiple flags are set, flipping should take precedence over
  sliding, which should take precedence over resizing.
  @begin{pre}
(gobejct:define-g-flags \"GdkAnchorHints\" anchor-hints
  (:export t
   :type-initializer \"gdk_anchor_hints_get_type\")
  (:flip-x #.(ash 1 0))
  (:flip-y #.(ash 1 1))
  (:slide-x #.(ash 1 2))
  (:slide-y #.(ash 1 3))
  (:resize-x #.(ash 1 4))
  (:resize-y #.(ash 1 5))
  (:flip #.(+ (ash 1 0) (ash 1 1)))
  (:slide #.(+ (ash 1 2) (ash 1 3)))
  (:resize #.(+ (ash 1 4) (ash 1 5))))
  @end{pre}
  @begin[code]{table}
    @entry[:flip-x]{Allow flipping anchors horizontally.}
    @entry[:flip-y]{Allow flipping anchors vertically.}
    @entry[:slide-x]{Allow sliding surface horizontally.}
    @entry[:slide-y]{Allow sliding surface vertically.}
    @entry[:resize-x]{Allow resizing surface horizontally.}
    @entry[:resize-y]{Allow resizing surface vertically.}
    @entry[:flip]{Allow flipping anchors on both axes.}
    @entry[:slide]{Allow sliding surface on both axes.}
    @entry[:resize]{Allow resizing surface on both axes.}
  @end{table}
  @see-class{gdk:popup-layout}")

;;; ----------------------------------------------------------------------------
;;; GdkPopupLayout
;;; ----------------------------------------------------------------------------

(glib:define-g-boxed-opaque popup-layout "GdkPopupLayout"
  :type-initializer "gdk_popup_layout_get_type"
  :alloc (%popup-layout-new))

#+liber-documentation
(setf (liber:alias-for-class 'popup-layout)
      "GBoxed"
      (documentation 'popup-layout 'type)
 "@version{#2023-4-10}
  @begin{short}
    Popups are positioned relative to their parent surface.
  @end{short}
  The @class{gdk:popup-layout} structure contains information that is necessary
  to do so. The positioning requires a negotiation with the windowing system,
  since it depends on external constraints, such as the position of the parent
  surface, and the screen dimensions.

  The basic ingredients are a rectangle on the parent surface, and the anchor on
  both that rectangle and the popup. The anchors specify a side or corner to
  place next to each other.

  @image[popup-anchors]{Figure: Popup anchors}

  For cases where placing the anchors next to each other would make the popup
  extend offscreen, the layout includes some hints for how to resolve this
  problem. The hints may suggest to flip the anchor position to the other side,
  or to 'slide' the popup along a side, or to resize it.

  @image[popup-flip]{Figure: Flipping popups}

  @image[popup-slide]{Figure: Sliding popups}

  These hints may be combined.

  Ultimatively, it is up to the windowing system to determine the position and
  size of the popup. You can learn about the result by calling the
  @fun{gdk:popup-position-x}, @fun{gdk:popup-position-y},
  @fun{gdk:popup-rect-anchor} and @fun{gdk:popup-surface-anchor} functions
  after the popup has been presented. This can be used to adjust the rendering.
  For example, the @class{gtk:popover} object changes its arrow position
  accordingly. But you have to be careful avoid changing the size of the
  popover, or it has to be presented again.
  @see-class{gdk:popup}")

;;; ----------------------------------------------------------------------------
;;; gdk_popup_layout_new ()
;;;
;;; GdkPopupLayout *
;;; gdk_popup_layout_new (const GdkRectangle *anchor_rect,
;;;                       GdkGravity rect_anchor,
;;;                       GdkGravity surface_anchor);
;;;
;;;
;;; anchor_rect :
;;;     the anchor GdkRectangle to align surface with.
;;;
;;; rect_anchor :
;;;
;;;
;;; surface_anchor :
;;;
;;;
;;; Returns :
;;;     newly created instance of GdkPopupLayout.
;;; ----------------------------------------------------------------------------

(defun %popup-layout-new ()
  (cffi:foreign-funcall "gdk_popup_layout_new"
                        (g:boxed rectangle) (rectangle-new)
                        gravity :north-west
                        gravity :north-west
                        :pointer))

(defun popup-layout-new (rect rect-anchor surface-anchor)
 #+liber-documentation
 "@version{#2023-4-10}
  @argument[rect]{a @class{gdk:rectangle} instance with the anchor rectangle
    to align surface with}
  @argument[rect-anchor]{a @symbol{gdk:anchor-hints} value with the point on
    @arg{rect} to align with the anchor point of the surface}
  @argument[surface-anchor]{a @symbol{gdk:anchor-hints} value with the point on
    surface to align with the anchor point of @arg{rect}}
  @return{The newly created @class{gdk:popup-layout} instance.}
  @begin{short}
    Create a popup layout description.
  @end{short}
  Used together with the @fun{gdk:popup-present} function to describe how a
  popup surface should be placed and behave on-screen.

  The @arg{rect} value is relative to the top-left corner of the
  surface's parent. The @arg{rect-anchor} and @arg{surface-anchor} values
  determine anchor points on @arg{rect} and surface to pin together.

  The position of @arg{rect}'s anchor point can optionally be offset using the
  @fun{gdk:popup-layout-offset} function, which is equivalent to offsetting the
  position of surface.
  @see-class{gdk:popup-layout}
  @see-class{gdk:rectangle}
  @see-symbol{gdk:anchor-hints}
  @see-function{gdk:popup-present}
  @see-function{gdk:popup-layout-offset}"
  (let ((layout (make-instance 'popup-layout)))
    (setf (popup-layout-anchor-rect layout) rect)
    (setf (popup-layout-rect-anchor layout) rect-anchor)
    (setf (popup-layout-surface-anchor layout) surface-anchor)
    layout))

(export 'popup-layout-new)

;;; ----------------------------------------------------------------------------
;;; gdk_popup_layout_ref ()
;;;
;;; GdkPopupLayout *
;;; gdk_popup_layout_ref (GdkPopupLayout *layout);
;;;
;;; Increases the reference count of value .
;;;
;;; layout :
;;;     a GdkPopupLayout
;;;
;;; Returns :
;;;     the same layout
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_popup_layout_unref ()
;;;
;;; void
;;; gdk_popup_layout_unref (GdkPopupLayout *layout);
;;;
;;; Decreases the reference count of value .
;;;
;;; layout :
;;;     a GdkPopupLayout
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_popup_layout_copy ()
;;;
;;; GdkPopupLayout *
;;; gdk_popup_layout_copy (GdkPopupLayout *layout);
;;;
;;; Create a new GdkPopupLayout and copy the contents of layout into it.
;;;
;;; layout :
;;;     a GdkPopupLayout
;;;
;;; Returns :
;;;     a copy of layout .
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_popup_layout_copy" popup-layout-copy) (g:boxed popup-layout)
  (layout (g:boxed popup-layout)))

(export 'popup-layout-copy)

;;; ----------------------------------------------------------------------------
;;; gdk_popup_layout_equal ()
;;;
;;; gboolean
;;; gdk_popup_layout_equal (GdkPopupLayout *layout,
;;;                         GdkPopupLayout *other);
;;;
;;; Check whether layout and other has identical layout properties.
;;;
;;; layout :
;;;     a GdkPopupLayout
;;;
;;; other :
;;;     another GdkPopupLayout
;;;
;;; Returns :
;;;     TRUE if layout and other have identical layout properties, otherwise
;;;     FALSE.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_popup_layout_equal" popup-layout-equal) :boolean
  (layout (g:boxed popup-layout))
  (other (g:boxed popup-layout)))

(export 'popup-layout-equal)

;;; ----------------------------------------------------------------------------
;;; gdk_popup_layout_set_anchor_rect ()
;;;
;;; void
;;; gdk_popup_layout_set_anchor_rect (GdkPopupLayout *layout,
;;;                                   const GdkRectangle *anchor_rect);
;;;
;;; Set the anchor rectangle.
;;;
;;; layout :
;;;     a GdkPopupLayout
;;;
;;; anchor_rect :
;;;     the new anchor rectangle
;;; ----------------------------------------------------------------------------

(defun (setf popup-layout-anchor-rect) (rect layout)
  (cffi:foreign-funcall "gdk_popup_layout_set_anchor_rect"
                        (g:boxed popup-layout) layout
                        (g:boxed rectangle) rect
                        :void)
  rect)

;;; ----------------------------------------------------------------------------
;;; gdk_popup_layout_get_anchor_rect ()
;;;
;;; const GdkRectangle *
;;; gdk_popup_layout_get_anchor_rect (GdkPopupLayout *layout);
;;;
;;; Get the anchor rectangle.
;;;
;;; layout :
;;;     a GdkPopupLayout
;;;
;;; Returns :
;;;     The anchor rectangle.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_popup_layout_get_anchor_rect" popup-layout-anchor-rect)
    (g:boxed rectangle)
  (layout (g:boxed popup-layout)))

(export 'popup-layout-anchor-rect)

;;; ----------------------------------------------------------------------------
;;; gdk_popup_layout_set_rect_anchor ()
;;;
;;; void
;;; gdk_popup_layout_set_rect_anchor (GdkPopupLayout *layout,
;;;                                   GdkGravity anchor);
;;;
;;; Set the anchor on the anchor rectangle.
;;;
;;; layout :
;;;     a GdkPopupLayout
;;;
;;; anchor :
;;;     the new rect anchor
;;; ----------------------------------------------------------------------------

(defun (setf popup-layout-rect-anchor) (anchor layout)
  (cffi:foreign-funcall "gdk_popup_layout_set_rect_anchor"
                        (g:boxed popup-layout) layout
                        gravity anchor
                        :void)
  anchor)

;;; ----------------------------------------------------------------------------
;;; gdk_popup_layout_get_rect_anchor ()
;;;
;;; GdkGravity
;;; gdk_popup_layout_get_rect_anchor (GdkPopupLayout *layout);
;;;
;;; Returns the anchor position on the anchor rectangle.
;;;
;;; layout :
;;;     a GdkPopupLayout
;;;
;;; Returns :
;;;     the anchor on the anchor rectangle.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_popup_layout_get_rect_anchor" popup-layout-rect-anchor)
    gravity
  (layout (g:boxed popup-layout)))

(export 'popup-layout-rect-anchor)

;;; ----------------------------------------------------------------------------
;;; gdk_popup_layout_set_surface_anchor ()
;;;
;;; void
;;; gdk_popup_layout_set_surface_anchor (GdkPopupLayout *layout,
;;;                                      GdkGravity anchor);
;;;
;;; Set the anchor on the popup surface.
;;;
;;; layout :
;;;     a GdkPopupLayout
;;;
;;; anchor :
;;;     the new popup surface anchor
;;; ----------------------------------------------------------------------------

(defun (setf popup-layout-surface-anchor) (anchor layout)
  (cffi:foreign-funcall "gdk_popup_layout_set_surface_anchor"
                        (g:boxed popup-layout) layout
                        gravity anchor
                        :void)
  anchor)

;;; ----------------------------------------------------------------------------
;;; gdk_popup_layout_get_surface_anchor ()
;;;
;;; GdkGravity
;;; gdk_popup_layout_get_surface_anchor (GdkPopupLayout *layout);
;;;
;;; Returns the anchor position on the popup surface.
;;;
;;; layout :
;;;     a GdkPopupLayout
;;;
;;; Returns :
;;;     the anchor on the popup surface.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_popup_layout_get_surface_anchor"
               popup-layout-surface-anchor) gravity
  (layout (g:boxed popup-layout)))

(export 'popup-layout-surface-anchor)

;;; ----------------------------------------------------------------------------
;;; gdk_popup_layout_set_anchor_hints ()
;;;
;;; void
;;; gdk_popup_layout_set_anchor_hints (GdkPopupLayout *layout,
;;;                                    GdkAnchorHints anchor_hints);
;;;
;;; Set new anchor hints.
;;;
;;; The set anchor_hints determines how surface will be moved if the anchor
;;; points cause it to move off-screen. For example, GDK_ANCHOR_FLIP_X will
;;; replace GDK_GRAVITY_NORTH_WEST with GDK_GRAVITY_NORTH_EAST and vice versa
;;; if surface extends beyond the left or right edges of the monitor.
;;;
;;; layout :
;;;     a GdkPopupLayout
;;;
;;; anchor_hints :
;;;     the new GdkAnchorHints
;;; ----------------------------------------------------------------------------

(defun (setf popup-layout-anchor-hints) (hints layout)
  (cffi:foreign-funcall "gdk_popup_layout_set_anchor_hints"
                        (g:boxed popup-layout) layout
                        anchor-hints hints
                        :void)
  hints)

;;; ----------------------------------------------------------------------------
;;; gdk_popup_layout_get_anchor_hints ()
;;;
;;; GdkAnchorHints
;;; gdk_popup_layout_get_anchor_hints (GdkPopupLayout *layout);
;;;
;;; Get the GdkAnchorHints.
;;;
;;; layout :
;;;     a GdkPopupLayout
;;;
;;; Returns :
;;;     the GdkAnchorHints.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_popup_layout_get_anchor_hints" popup-layout-anchor-hints)
    anchor-hints
  (layout (g:boxed popup-layout)))

(export 'popup-layout-anchor-hints)

;;; ----------------------------------------------------------------------------
;;; gdk_popup_layout_set_offset ()
;;;
;;; void
;;; gdk_popup_layout_set_offset (GdkPopupLayout *layout,
;;;                              int dx,
;;;                              int dy);
;;;
;;; Offset the position of the anchor rectangle with the given delta.
;;;
;;; layout :
;;;     a GdkPopupLayout
;;;
;;; dx :
;;;     x delta to offset the anchor rectangle with
;;;
;;; dy :
;;;     y delta to offset the anchor rectangle with
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_popup_layout_get_offset ()
;;;
;;; void
;;; gdk_popup_layout_get_offset (GdkPopupLayout *layout,
;;;                              int *dx,
;;;                              int *dy);
;;;
;;; Retrieves the offset for the anchor rectangle.
;;;
;;; layout :
;;;     a GdkPopupLayout
;;;
;;; dx :
;;;     return location for the delta X coordinate.
;;;
;;; dy :
;;;     return location for the delta Y coordinate.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_popup_layout_set_shadow_width                      Since 4.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_popup_layout_get_shadow_width                      Since 4.2
;;; ----------------------------------------------------------------------------

;;; --- End of file gdk4.popup-layout.lisp -------------------------------------

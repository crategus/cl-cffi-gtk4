;;; ----------------------------------------------------------------------------
;;; gdk4.popup-layout.lisp
;;;
;;; The documentation in this file is taken from the GDK 4 Reference Manual
;;; version 4.20 and modified to document the Lisp binding to the GDK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 - 2026 Dieter Kaiser
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
;;; GdkAnchorHints
;;; ----------------------------------------------------------------------------

(gobject:define-gflags "GdkAnchorHints" anchor-hints
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
(setf (liber:alias-for-symbol 'anchor-hints)
      "GFlags"
      (liber:symbol-documentation 'anchor-hints)
 "@version{2025-07-30}
  @begin{declaration}
(gobject:define-gflags \"GdkAnchorHints\" anchor-hints
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
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:flip-x]{Allow flipping anchors horizontally.}
      @entry[:flip-y]{Allow flipping anchors vertically.}
      @entry[:slide-x]{Allow sliding surface horizontally.}
      @entry[:slide-y]{Allow sliding surface vertically.}
      @entry[:resize-x]{Allow resizing surface horizontally.}
      @entry[:resize-y]{Allow resizing surface vertically.}
      @entry[:flip]{Allow flipping anchors on both axes.}
      @entry[:slide]{Allow sliding surface on both axes.}
      @entry[:resize]{Allow resizing surface on both axes.}
    @end{simple-table}
  @end{values}
  @begin{short}
    Positioning hints for aligning a surface relative to a rectangle.
  @end{short}
  These hints determine how the surface should be positioned in the case that
  the surface would fall off-screen if placed in its ideal position.

  For example, the @val[gdk:anchor-hints]{:flip-x} value will replace the
  @val[gdk:gravity]{:north-west} gravity with the @val[gdk:gravity]{:north-east}
  gravity and vice versa if the surface extends beyond the left or right edges
  of the monitor.

  If the @val[gdk:anchor-hints]{:slide-x} value is set, the surface can be
  shifted horizontally to fit on-screen. If the
  @val[gdk:anchor-hints]{:resize-x} value is set, the surface can be shrunken
  horizontally to fit.

  In general, when multiple flags are set, flipping should take precedence over
  sliding, which should take precedence over resizing.
  @see-class{gdk:popup-layout}")

;;; ----------------------------------------------------------------------------
;;; GdkPopupLayout
;;; ----------------------------------------------------------------------------

(glib:define-gboxed-opaque popup-layout "GdkPopupLayout"
  :type-initializer "gdk_popup_layout_get_type"
  :alloc (%popup-layout-new))

#+liber-documentation
(setf (liber:alias-for-class 'popup-layout)
      "GBoxed"
      (documentation 'popup-layout 'type)
 "@version{2024-02-17}
  @begin{short}
    Popups are positioned relative to their parent surface.
  @end{short}
  The @class{gdk:popup-layout} structure contains information that is necessary
  to do so. The positioning requires a negotiation with the windowing system,
  since it depends on external constraints, such as the position of the parent
  surface, and the screen dimensions.

  The basic ingredients are a rectangle on the parent surface, and the anchor
  on both that rectangle and the popup. The anchors specify a side or corner to
  place next to each other.

  @image[popup-anchors]{Figure: Popup anchors}

  For cases where placing the anchors next to each other would make the popup
  extend offscreen, the layout includes some hints for how to resolve this
  problem. The hints may suggest to flip the anchor position to the other side,
  or to 'slide' the popup along a side, or to resize it. These hints may be
  combined.

  @image[popup-flip]{Figure: Flipping popups}

  @image[popup-slide]{Figure: Sliding popups}

  Ultimatively, it is up to the windowing system to determine the position and
  size of the popup. You can learn about the result by calling the
  @fun{gdk:popup-position-x}, @fun{gdk:popup-position-y},
  @fun{gdk:popup-rect-anchor} and @fun{gdk:popup-surface-anchor} functions
  after the popup has been presented. This can be used to adjust the rendering.
  For example, the @class{gtk:popover} object changes its arrow position
  accordingly. But you have to be careful avoid changing the size of the
  popover, or it has to be presented again.
  @see-constructor{gdk:popup-layout-new}
  @see-constructor{gdk:popup-layout-copy}
  @see-class{gdk:popup}
  @see-class{gtk:popover}
  @see-function{gdk:popup-position-x}
  @see-function{gdk:popup-position-y}
  @see-function{gdk:popup-rect-anchor}
  @see-function{gdk:popup-surface-anchor}")

;;; ----------------------------------------------------------------------------
;;; gdk_popup_layout_new
;;; ----------------------------------------------------------------------------

(defun %popup-layout-new ()
  (cffi:foreign-funcall "gdk_popup_layout_new"
                        (g:boxed rectangle) (rectangle-new)
                        gravity :north-west
                        gravity :north-west
                        :pointer))

(defun popup-layout-new (rect rect-anchor surface-anchor)
 #+liber-documentation
 "@version{2025-08-02}
  @argument[rect]{a @class{gdk:rectangle} instance for the anchor rectangle
    to align surface with}
  @argument[rect-anchor]{a @sym{gdk:gravity} value for the point on @arg{rect}
    to align with the anchor point of the surface}
  @argument[surface-anchor]{a @sym{gdk:gravity} value for the point on surface
    to align with the anchor point of @arg{rect}}
  @return{The newly created @class{gdk:popup-layout} instance.}
  @begin{short}
    Create a popup layout description.
  @end{short}
  Used together with the @fun{gdk:popup-present} function to describe how a
  popup surface should be placed and behave on-screen.

  The rectangle is relative to the top-left corner of the parent of the surface.
  The @arg{rect-anchor} and @arg{surface-anchor} values determine anchor points
  on the rectangle and surface to pin together.

  The position of the anchor point of the rectangle can optionally be offset
  using the @fun{gdk:popup-layout-offset} function, which is equivalent to
  offsetting the position of surface.
  @see-class{gdk:popup-layout}
  @see-class{gdk:rectangle}
  @see-symbol{gdk:gravity}
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
;;; gdk_popup_layout_copy
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_popup_layout_copy" popup-layout-copy)
    (g:boxed popup-layout :return)
 #+liber-documentation
 "@version{2025-08-04}
  @argument[layout]{a @class{gdk:popup-layout} instance}
  @return{The @class{gdk:popup-layout} instance for the copy of @arg{layout}.}
  @begin{short}
    Create a new popup layout and copy the contents of @arg{layout} into it.
  @end{short}
  @see-class{gdk:popup-layout}"
  (layout (g:boxed popup-layout)))

(export 'popup-layout-copy)

;;; ----------------------------------------------------------------------------
;;; gdk_popup_layout_equal
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_popup_layout_equal" popup-layout-equal) :boolean
 #+liber-documentation
 "@version{2025-08-04}
  @argument[layout1]{a @class{gdk:popup-layout} instance}
  @argument[layout2]{a @class{gdk:popup-layout} instance}
  @begin{return}
    @em{True} if @arg{layout1} and @arg{layout2} have identical layout
    properties, otherwise @em{false}.
  @end{return}
  @begin{short}
    Check whether @arg{layout1} and @arg{layout2} have identical layout
    properties.
  @end{short}
  @see-class{gdk:popup-layout}"
  (layout1 (g:boxed popup-layout))
  (layout2 (g:boxed popup-layout)))

(export 'popup-layout-equal)

;;; ----------------------------------------------------------------------------
;;; gdk_popup_layout_set_anchor_rect
;;; gdk_popup_layout_get_anchor_rect
;;; ----------------------------------------------------------------------------

(defun (setf popup-layout-anchor-rect) (rect layout)
  (cffi:foreign-funcall "gdk_popup_layout_set_anchor_rect"
                        (g:boxed popup-layout) layout
                        (g:boxed rectangle) rect
                        :void)
  rect)

(cffi:defcfun ("gdk_popup_layout_get_anchor_rect" popup-layout-anchor-rect)
    (g:boxed rectangle)
 #+liber-documentation
 "@version{2025-09-25}
  @syntax{(gdk:popup-layout-anchor-rect layout) => rect}
  @syntax{(setf (gdk:popup-layout-anchor-rect layout) rect)}
  @argument[layout]{a @class{gdk:popup-layout} instance}
  @argument[rect]{an anchor @class{gdk:rectangle} instance}
  @begin{short}
    Gets or sets the anchor rectangle.
  @end{short}
  @see-class{gdk:popup-layout}
  @see-class{gdk:rectangle}"
  (layout (g:boxed popup-layout)))

(export 'popup-layout-anchor-rect)

;;; ----------------------------------------------------------------------------
;;; gdk_popup_layout_set_rect_anchor
;;; gdk_popup_layout_get_rect_anchor
;;; ----------------------------------------------------------------------------

(defun (setf popup-layout-rect-anchor) (anchor layout)
  (cffi:foreign-funcall "gdk_popup_layout_set_rect_anchor"
                        (g:boxed popup-layout) layout
                        gravity anchor
                        :void)
  anchor)

(cffi:defcfun ("gdk_popup_layout_get_rect_anchor" popup-layout-rect-anchor)
    gravity
 #+liber-documentation
 "@version{2025-08-02}
  @syntax{(gdk:popup-layout-rect-anchor layout) => gravity}
  @syntax{(setf (gdk:popup-layout-rect-anchor layout) gravity)}
  @argument[layout]{a @class{gdk:popup-layout} instance}
  @argument[gravity]{a @sym{gdk:gravity} value for the anchor on the anchor
    rectangle}
  @begin{short}
    Gets or sets the anchor position on the anchor rectangle.
  @end{short}
  @see-class{gdk:popup-layout}
  @see-symbol{gdk:gravity}"
  (layout (g:boxed popup-layout)))

(export 'popup-layout-rect-anchor)

;;; ----------------------------------------------------------------------------
;;; gdk_popup_layout_set_surface_anchor
;;; gdk_popup_layout_get_surface_anchor
;;; ----------------------------------------------------------------------------

(defun (setf popup-layout-surface-anchor) (anchor layout)
  (cffi:foreign-funcall "gdk_popup_layout_set_surface_anchor"
                        (g:boxed popup-layout) layout
                        gravity anchor
                        :void)
  anchor)

(cffi:defcfun ("gdk_popup_layout_get_surface_anchor"
               popup-layout-surface-anchor) gravity
 #+liber-documentation
 "@version{2025-08-02}
  @syntax{(gdk:popup-layout-surface-anchor layout) => gravity}
  @syntax{(setf (gdk:popup-layout-surface-anchor layout) gravity)}
  @argument[layout]{a @class{gdk:popup-layout} instance}
  @argument[gravity]{a @sym{gdk:gravity} value for the anchor on the popup
    surface}
  @begin{short}
    Gets or sets the anchor position on the popup surface.
  @end{short}
  @see-class{gdk:popup-layout}
  @see-symbol{gdk:gravity}"
  (layout (g:boxed popup-layout)))

(export 'popup-layout-surface-anchor)

;;; ----------------------------------------------------------------------------
;;; gdk_popup_layout_set_anchor_hints
;;; gdk_popup_layout_get_anchor_hints
;;; ----------------------------------------------------------------------------

(defun (setf popup-layout-anchor-hints) (hints layout)
  (cffi:foreign-funcall "gdk_popup_layout_set_anchor_hints"
                        (g:boxed popup-layout) layout
                        anchor-hints hints
                        :void)
  hints)

(cffi:defcfun ("gdk_popup_layout_get_anchor_hints" popup-layout-anchor-hints)
    anchor-hints
 #+liber-documentation
 "@version{2025-08-02}
  @syntax{(gdk:popup-layout-anchor-hints layout) => hints}
  @syntax{(setf (gdk:popup-layout-anchor-hints layout) hints)}
  @argument[layout]{a @class{gdk:popup-layout} instance}
  @argument[hints]{a @sym{gdk:anchor-hints} value}
  @begin{short}
    Gets or sets the anchor hints.
  @end{short}
  The anchor hints determines how surface will be moved if the anchor points
  cause it to move off-screen. For example, the @val[gdk:anchor-hints]{:flip-x}
  value will replace the @val[gdk:gravity]{:north-west} gravity with the
  @val[gdk:gravity]{:north-east} gravity and vice versa if surface extends
  beyond the left or right edges of the monitor.
  @see-class{gdk:popup-layout}
  @see-symbol{gdk:anchor-hints}"
  (layout (g:boxed popup-layout)))

(export 'popup-layout-anchor-hints)

;;; ----------------------------------------------------------------------------
;;; gdk_popup_layout_set_offset
;;; gdk_popup_layout_get_offset
;;; ----------------------------------------------------------------------------

(defun (setf popup-layout-offset) (offset layout)
  (destructuring-bind (dx dy) offset
    (cffi:foreign-funcall "gdk_popup_layout_set_offset"
                          (g:boxed popup-layout) layout
                          :int dx
                          :int dy
                          :void)
    (values dx dy)))

(cffi:defcfun ("gdk_popup_layout_get_offset" %popup-layout-offset) :void
  (layout (g:boxed popup-layout))
  (dx (:pointer :int))
  (dy (:pointer :int)))

(defun popup-layout-offset (layout)
 #+liber-documentation
 "@version{2025-08-02}
  @syntax{(gdk:popup-layout-offset layout) => dx, dy}
  @syntax{(setf (gdk:popup-layout-offset layout) (list dx dy))}
  @argument[layout]{a @class{gdk:popup-layout} instance}
  @argument[dx]{an integer for the delta x coordinate}
  @argument[dx]{an integer for the delta y coordinate}
  @begin{short}
    Gets or sets the offset for the anchor rectangle within the given delta.
  @end{short}
  @see-class{gdk:popup-layout}"
  (cffi:with-foreign-objects ((dx :int) (dy :int))
    (%popup-layout-offset layout dx dy)
    (values (cffi:mem-ref dx :int)
            (cffi:mem-ref dy :int))))

(export 'popup-layout-offset)

;;; ----------------------------------------------------------------------------
;;; gdk_popup_layout_set_shadow_width                      Since 4.2
;;; gdk_popup_layout_get_shadow_width                      Since 4.2
;;; ----------------------------------------------------------------------------

#+gtk-4-2
(defun (setf popup-layout-shadow-width) (value layout)
  (destructuring-bind (left right top bottom) value
    (cffi:foreign-funcall "gdk_popup_layout_set_shadow_width"
                          (g:boxed popup-layout) layout
                          :int left
                          :int right
                          :int top
                          :int bottom
                          :void)
    (values left right top bottom)))

#+gtk-4-2
(cffi:defcfun ("gdk_popup_layout_get_shadow_width" %popup-layout-shadow-width)
    :void
  (layout (g:boxed popup-layout))
  (left (:pointer :int))
  (right (:pointer :int))
  (top (:pointer :int))
  (bottom (:pointer :int)))

#+gtk-4-2
(defun popup-layout-shadow-width (layout)
 #+liber-documentation
 "@version{2025-08-02}
  @syntax{(gdk:popup-layout-shadow-width layout) => left, right, top, bottom}
  @syntax{(setf (gdk:popup-layout-shadow-width layout)
    (list left right top bottom))}
  @argument[layout]{a @class{gdk:popup-layout} instance}
  @argument[left]{an integer for the left shadow width}
  @argument[right]{an integer for the right shadow width}
  @argument[top]{an integer for the top shadow width}
  @argument[bottom]{an integer for the bottom shadow width}
  @begin{short}
    Gets or sets the shadow widths of the popup layout.
  @end{short}

  The shadow width corresponds to the part of the computed surface size that
  would consist of the shadow margin surrounding the window, would there be any.

  Since 4.2
  @see-class{gdk:popup-layout}"
  (cffi:with-foreign-objects ((left :int) (right :int) (top :int) (bottom :int))
    (%popup-layout-shadow-width layout left right top bottom)
    (values (cffi:mem-ref left :int)
            (cffi:mem-ref right :int)
            (cffi:mem-ref top :int)
            (cffi:mem-ref bottom :int))))

#+gtk-4-2
(export 'popup-layout-shadow-width)

;;; --- End of file gdk4.popup-layout.lisp -------------------------------------

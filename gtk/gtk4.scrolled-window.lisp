;;; ----------------------------------------------------------------------------
;;; gtk4.scrolled-window.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.18 and modified to document the Lisp binding to the GTK library,
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
;;; GtkScrolledWindow
;;;
;;;     Adds scrollbars to its child widget
;;;
;;; Types and Values
;;;
;;;     GtkScrolledWindow
;;;     GtkPolicyType
;;;     GtkCornerType
;;;
;;; Accessors
;;;
;;;     gtk_scrolled_window_get_child
;;;     gtk_scrolled_window_set_child
;;;     gtk_scrolled_window_get_hadjustment
;;;     gtk_scrolled_window_set_hadjustment
;;;     gtk_scrolled_window_get_has_frame
;;;     gtk_scrolled_window_set_has_frame
;;;     gtk_scrolled_window_get_kinetic_scrolling
;;;     gtk_scrolled_window_set_kinetic_scrolling
;;;     gtk_scrolled_window_get_max_content_height
;;;     gtk_scrolled_window_set_max_content_height
;;;     gtk_scrolled_window_get_max_content_width
;;;     gtk_scrolled_window_set_max_content_width
;;;     gtk_scrolled_window_get_min_content_height
;;;     gtk_scrolled_window_set_min_content_height
;;;     gtk_scrolled_window_get_min_content_width
;;;     gtk_scrolled_window_set_min_content_width
;;;     gtk_scrolled_window_get_overlay_scrolling
;;;     gtk_scrolled_window_set_overlay_scrolling
;;;     gtk_scrolled_window_get_propagate_natural_height
;;;     gtk_scrolled_window_set_propagate_natural_height
;;;     gtk_scrolled_window_get_propagate_natural_width
;;;     gtk_scrolled_window_set_propagate_natural_width
;;;     gtk_scrolled_window_get_vadjustment
;;;     gtk_scrolled_window_set_vadjustment
;;;
;;; Functions
;;;
;;;     gtk_scrolled_window_new
;;;     gtk_scrolled_window_get_hscrollbar
;;;     gtk_scrolled_window_get_vscrollbar
;;;     gtk_scrolled_window_get_policy
;;;     gtk_scrolled_window_set_policy
;;;     gtk_scrolled_window_get_placement
;;;     gtk_scrolled_window_set_placement
;;;     gtk_scrolled_window_unset_placement
;;;
;;; Properties
;;;
;;;     child
;;;     hadjustment
;;;     has-frame
;;;     hscrollbar-policy
;;;     kinetic-scrolling
;;;     max-content-height
;;;     max-content-width
;;;     min-content-height
;;;     min-content-width
;;;     overlay-scrolling
;;;     propagate-natural-height
;;;     propagate-natural-width
;;;     vadjustment
;;;     vscrollbar-policy
;;;     window-placement
;;;
;;; Signals
;;;
;;;     edge-overshot
;;;     edge-reached
;;;     move-focus-out
;;;     scroll-child
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkScrolledWindow
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable,
;;;     GtkConstraintTarget
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkPolicyType
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GtkPolicyType" policy-type
  (:export t
   :type-initializer "gtk_policy_type_get_type")
  (:always 0)
  (:automatic 1)
  (:never 2)
  (:external))

#+liber-documentation
(setf (liber:alias-for-symbol 'policy-type)
      "GEnum"
      (liber:symbol-documentation 'policy-type)
 "@version{2025-04-26}
  @begin{declaration}
(gobject:define-genum \"GtkPolicyType\" policy-type
  (:export t
   :type-initializer \"gtk_policy_type_get_type\")
  (:always 0)
  (:automatic 1)
  (:never 2)
  (:external 3))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:always]{The scrollbar is always visible. The view size is
        independent of the content.}
      @entry[:automatic]{The scrollbar will appear and disappear as necessary.
        For example, when all of a @class{gtk:tree-view} widget cannot be seen.}
      @entry[:never]{The scrollbar should never appear. In this mode the content
        determines the size.}
      @entry[:external]{Do not show a scrollbar, but do not force the size to
        follow the content. This can be used, for example, to make multiple
        scrolled windows share a scrollbar.}
    @end{table}
  @end{values}
  @begin{short}
    Determines how the size should be computed to achieve one of the visibility
    mode for the scrollbars.
  @end{short}
  @see-class{gtk:scrolled-window}")

;;; ----------------------------------------------------------------------------
;;; GtkCornerType
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GtkCornerType" corner-type
  (:export t
   :type-initializer "gtk_corner_type_get_type")
  (:top-left 0)
  (:bottom-left 1)
  (:top-right 2)
  (:bottom-right 3))

#+liber-documentation
(setf (liber:alias-for-symbol 'corner-type)
      "GEnum"
      (liber:symbol-documentation 'corner-type)
 "@version{2025-04-26}
  @begin{declaration}
(gobject:define-genum \"GtkCornerType\" corner-type
  (:export t
   :type-initializer \"gtk_corner_type_get_type\")
  (:top-left 0)
  (:bottom-left 1)
  (:top-right 2)
  (:bottom-right 3))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:top-left]{Place the scrollbars on the right and bottom of the
        widget (default behaviour).}
      @entry[:bottom-left]{Place the scrollbars on the top and right of the
        widget.}
      @entry[:top-right]{Place the scrollbars on the left and bottom of the
        widget.}
      @entry[:bottom-right]{Place the scrollbars on the top and left of the
        widget.}
    @end{table}
  @end{values}
  @begin{short}
    Specifies which corner a child widget should be placed in when packed into
    a @class{gtk:scrolled-window} widget.
  @end{short}
  This is effectively the opposite of where the scrollbars are placed.
  @see-class{gtk:scrolled-window}")

;;; ----------------------------------------------------------------------------
;;; GtkScrolledWindow
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkScrolledWindow" scrolled-window
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget")
   :type-initializer "gtk_scrolled_window_get_type")
  ((child
    scrolled-window-child
    "child" "GtkWidget" t t)
   (hadjustment
    scrolled-window-hadjustment
    "hadjustment" "GtkAdjustment" t t)
   (has-frame
    scrolled-window-has-frame
    "has-frame" "gboolean" t t)
   (hscrollbar-policy
    scrolled-window-hscrollbar-policy
    "hscrollbar-policy" "GtkPolicyType" t t)
   (kinetic-scrolling
    scrolled-window-kinetic-scrolling
    "kinetic-scrolling" "gboolean" t t)
   (max-content-height
    scrolled-window-max-content-height
    "max-content-height" "gint" t t)
   (max-content-width
    scrolled-window-max-content-width
    "max-content-width" "gint" t t)
   (min-content-height
    scrolled-window-min-content-height
    "min-content-height" "gint" t t)
   (min-content-width
    scrolled-window-min-content-width
    "min-content-width" "gint" t t)
   (overlay-scrolling
    scrolled-window-overlay-scrolling
    "overlay-scrolling" "gboolean" t t)
   (propagate-natural-height
    scrolled-window-propagate-natural-height
    "propagate-natural-height" "gboolean" t t)
   (propagate-natural-width
    scrolled-window-propagate-natural-width
    "propagate-natural-width" "gboolean" t t)
   (vadjustment
    scrolled-window-vadjustment
    "vadjustment" "GtkAdjustment" t t)
   (vscrollbar-policy
    scrolled-window-vscrollbar-policy
    "vscrollbar-policy" "GtkPolicyType" t t)
   (window-placement
    scrolled-window-window-placement
    "window-placement" "GtkCornerType" t t)))

#+liber-documentation
(setf (documentation 'scrolled-window 'type)
 "@version{2025-04-26}
  @begin{short}
    The @class{gtk:scrolled-window} widget is a container that accepts a single
    child widget, makes that child scrollable using either internally added
    scrollbars or externally associated adjustments, and optionally draws a
    frame around the child.
  @end{short}

  @image[scrolled-window]{Figure: GtkScrolledWindow}

  Widgets with native scrolling support, that is, those whose classes implement
  the @class{gtk:scrollable} interface, are added directly. For other types of
  widgets, the @class{gtk:viewport} class acts as an adaptor, giving
  scrollability to other widgets. The @fun{gtk:scrolled-window-child} function
  intelligently accounts for whether or not the added child is a
  @class{gtk:scrollable} widget. If it is not, then it wraps the child in a
  @class{gtk:viewport} widget. Therefore, you can just add any child widget and
  not worry about the details.

  If the @fun{gtk:scrolled-window-child} function has added a
  @class{gtk:viewport} widget for you, you can remove both your added child
  widget from the @class{gtk:viewport} widget, and the @class{gtk:viewport}
  widget from the @class{gtk:scrolled-window} widget, like this:
  @begin{pre}
(let ((window (make-instance 'gtk:scrolled-window))
      (child (make-instance 'gtk:button)))

  ;; GtkButton is not a GtkScrollable, so GtkScrolledWindow will
  ;; automatically add a GtkViewport.
  (setf (gtk:scrolled-window window) child)

  ;; Either of these will result in child being unparented:
  (setf (gtk:scrolled-window-child window) nil)
  ;; or
  (setf (gtk:viewport-child (gtk:scrolled-window-child window)) nil)
  ... )
  @end{pre}
  Unless the @slot[gtk:scrolled-window]{hscrollbar-policy} and
  @slot[gtk:scrolled-window]{vscrollbar-policy} properties are @code{:never} or
  @code{:external}, the @class{gtk:scrolled-window} widget adds internal
  @class{gtk:scrollbar} widgets around its child. The scroll position of the
  child, and if applicable the scrollbars, is controlled by the
  @slot[gtk:scrolled-window]{hadjustment} and
  @slot[gtk:scrolled-window]{vadjustment} properties that are associated with
  the @class{gtk:scrolled-window} widget. See the docs on the
  @class{gtk:scrollbar} widget for the details, but note that the
  @slot[gtk:adjustment]{step-increment} and
  @slot[gtk:adjustment]{page-increment} properties are only effective if the
  policy causes scrollbars to be present.

  If a @class{gtk:scrolled-window} widget does not behave quite as you would
  like, or does not have exactly the right layout, it is very possible to set
  up your own scrolling with the @class{gtk:scrollbar} widget and for example
  a @class{gtk:grid} widget.

  @subheading{Touch support}
  The @class{gtk:scrolled-window} widget has built-in support for touch devices.
  When a touchscreen is used, swiping will move the scrolled window, and will
  expose 'kinetic' behavior. This can be turned off with the
  @slot[gtk:scrolled-window]{kinetic-scrolling} property if it is undesired.

  The @class{gtk:scrolled-window} widget also displays visual 'overshoot'
  indication when the content is pulled beyond the end, and this situation can
  be captured with the @code{\"edge-overshot\"} signal.

  If no mouse device is present, the scrollbars will overlayed as narrow,
  auto-hiding indicators over the content. If traditional scrollbars are
  desired although no mouse is present, this behaviour can be turned off with
  the @slot[gtk:scrolled-window]{overlay-scrolling} property.
  @begin[CSS nodes]{dictionary}
    The @class{gtk:scrolled-window} implementation has a main CSS node with
    name @code{scrolledwindow}. It uses subnodes with names @code{overshoot}
    and @code{undershoot} to draw the overflow and underflow indications. These
    nodes get the @code{.left}, @code{.right}, @code{.top} or @code{.bottom}
    style class added depending on where the indication is drawn.

    The @class{gtk:scrolled-window} implementation also sets the @code{.left},
    @code{.right}, @code{.top}, @code{.bottom}  positional style classes and
    @code{.overlay-indicator}, @code{.dragging}, @code{.hovering} style classes
    related to overlay scrolling on its scrollbars.

    If both scrollbars are visible, the area where they meet is drawn with a
    subnode named @code{junction}.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"edge-overshot\" signal}
      @begin{pre}
lambda (window pos)    :run-last
      @end{pre}
      @begin[code]{table}
        @entry[window]{The @class{gtk:scrolled-window} widget which received
          the signal.}
        @entry[pos]{The edge side as a value of the @symbol{gtk:position-type}
          enumeration that was hit.}
      @end{table}
      The signal is emitted whenever user initiated scrolling makes the
      scrolled window firmly surpass, for example, with some edge resistance,
      the lower or upper limits defined by the adjustment in that orientation.
      A similar behavior without edge resistance is provided by the
      @code{\"edge-reached\"} signal. Note: The @arg{pos} argument is LTR/RTL
      aware, so callers should be aware too if intending to provide behavior on
      horizontal edges.
    @subheading{The \"edge-reached\" signal}
      @begin{pre}
lambda (window pos)    :run-last
      @end{pre}
      @begin[code]{table}
        @entry[window]{The @class{gtk:scrolled-window} widget which received
          the signal.}
        @entry[pos]{The edge side as a value of the @symbol{gtk:position-type}
          enumeration that was hit.}
      @end{table}
      The signal is emitted whenever user-initiated scrolling makes the
      scrolled window exactly reach the lower or upper limits defined by the
      adjustment in that orientation. A similar behavior with edge resistance
      is provided by the @code{\"edge-overshot\"} signal. Note: The @arg{pos}
      argument is LTR/RTL aware, so callers should be aware too if intending to
      provide behavior on horizontal edges.
    @subheading{The \"move-focus-out\" signal}
      @begin{pre}
lambda (window direction)    :action
      @end{pre}
      @begin[code]{table}
        @entry[window]{The @class{gtk:scrolled-window} widget which received
          the signal.}
        @entry[direction]{Either the @code{:tab-forward} or @code{:tab-backward}
          value of the @symbol{gtk:direction-type} enumeration.}
      @end{table}
      The signal is a keybinding signal which gets emitted when focus is moved
      away from the scrolled window by a keybinding. The @code{\"move-focus\"}
      signal is emitted with the @arg{direction} value on this scrolled windows
      toplevel parent in the container hierarchy. The default bindings for this
      signal are the @kbd{Tab+Ctrl} and @kbd{Tab+Ctrl+Shift} keys.
      @subheading{The \"scroll-child\" signal}
        @begin{pre}
lambda (window scroll horizontal)    :action
        @end{pre}
        @begin[code]{table}
          @entry[window]{The @class{gtk:scrolled-window} widget which received
            the signal.}
          @entry[scroll]{The value of the @symbol{gtk:scroll-type} enumeration
            describing how much to scroll.}
          @entry[horizontal]{The boolean whether the keybinding scrolls the
            child horizontally or not.}
          @entry[Returns]{The boolean whether the scroll happened.}
        @end{table}
        The signal is a keybinding signal which gets emitted when a keybinding
        that scrolls is pressed. The horizontal or vertical adjustment is
        updated which triggers a signal that the scrolled windows child may
        listen to and scroll itself.
  @end{dictionary}
  @see-constructor{gtk:scrolled-window-new}
  @see-slot{gtk:scrolled-window-child}
  @see-slot{gtk:scrolled-window-hadjustment}
  @see-slot{gtk:scrolled-window-has-frame}
  @see-slot{gtk:scrolled-window-hscrollbar-policy}
  @see-slot{gtk:scrolled-window-kinetic-scrolling}
  @see-slot{gtk:scrolled-window-max-content-height}
  @see-slot{gtk:scrolled-window-max-content-width}
  @see-slot{gtk:scrolled-window-min-content-height}
  @see-slot{gtk:scrolled-window-min-content-width}
  @see-slot{gtk:scrolled-window-overlay-scrolling}
  @see-slot{gtk:scrolled-window-propagate-natural-height}
  @see-slot{gtk:scrolled-window-propagate-natural-width}
  @see-slot{gtk:scrolled-window-vadjustment}
  @see-slot{gtk:scrolled-window-vscrollbar-policy}
  @see-slot{gtk:scrolled-window-window-placement}
  @see-class{gtk:scrollable}
  @see-class{gtk:viewport}
  @see-class{gtk:adjustment}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:scrolled-window-child ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "child" 'scrolled-window) t)
 "The @code{child} property of type @class{gtk:widget} (Read / Write) @br{}
  The child widget.")

#+liber-documentation
(setf (liber:alias-for-function 'scrolled-window-child)
      "Accessor"
      (documentation 'scrolled-window-child 'function)
 "@version{2025-04-26}
  @syntax{(gtk:scrolled-window-child object) => child}
  @syntax{(setf (gtk:scrolled-window-child object) child)}
  @argument[object]{a @class{gtk:scrolled-window} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @begin{short}
    Accessor of the @slot[gtk:scrolled-window]{child} slot of the
    @class{gtk:scrolled-window} class.
  @end{short}
  The @fun{gtk:scrolled-window-child} function gets the child widget of the
  scrolled window. The @setf{gtk:scrolled-window-child} sets the child widget.
  @see-class{gtk:scrolled-window}
  @see-class{gtk:widget}")

;;; --- gtk:scrolled-window-hadjustment ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "hadjustment"
                                               'scrolled-window) t)
 "The @code{hadjustment} property of type @class{gtk:adjustment}
  (Read / Write / Construct) @br{}
  The adjustment for the horizontal position.")

#+liber-documentation
(setf (liber:alias-for-function 'scrolled-window-hadjustment)
      "Accessor"
      (documentation 'scrolled-window-hadjustment 'function)
 "@version{2025-04-26}
  @syntax{(gtk:scrolled-window-hadjustment object) => adjustment}
  @syntax{(setf (gtk:scrolled-window-hadjustment object) adjustment)}
  @argument[object]{a @class{gtk:scrolled-window} widget}
  @argument[adjustment]{a @class{gtk:adjustment} object for the horizontal
    scroll adjustment}
  @begin{short}
    Accessor of the @slot[gtk:scrolled-window]{hadjustment} slot of the
    @class{gtk:scrolled-window} class.
  @end{short}
  The @fun{gtk:scrolled-window-hadjustment} function returns the horizontal
  adjustment of the scrollbar, used to connect the horizontal scrollbar to the
  horizontal scroll functionality of the child widget. The
  @setf{gtk:scrolled-window-hadjustment} function sets the adjustment for the
  horizontal scrollbar.
  @see-class{gtk:scrolled-window}
  @see-class{gtk:adjustment}
  @see-function{gtk:scrolled-window-vadjustment}")

;;; --- gtk:scrolled-window-has-frame ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "has-frame" 'scrolled-window) t)
 "The @code{has-frame} property of type @code{:boolean} (Read / Write) @br{}
  Whether to draw a frame around the contents. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'scrolled-window-has-frame)
      "Accessor"
      (documentation 'scrolled-window-has-frame 'function)
 "@version{2025-04-26}
  @syntax{(gtk:scrolled-window-has-frame object) => setting}
  @syntax{(setf (gtk:scrolled-window-child object) setting)}
  @argument[object]{a @class{gtk:scrolled-window} widget}
  @argument[setting]{a boolean whether to draw a frame around the contents}
  @begin{short}
    Accessor of the @slot[gtk:scrolled-window]{has-frame} slot of the
    @class{gtk:scrolled-window} class.
  @end{short}
  The @fun{gtk:scrolled-window-has-frame} function gets whether the scrolled
  window draws a frame. The @setf{gtk:scrolled-window-has-frame} function
  changes the frame drawn around the contents of the scrolled window.
  @see-class{gtk:scrolled-window}")

;;; --- gtk:scrolled-window-hscrollbar-policy ----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "hscrollbar-policy"
                                               'scrolled-window) t)
 "The @code{hscrollbar-policy} property of type @symbol{gtk:policy-type}
  (Read / Write) @br{}
  When the horizontal scrollbar is displayed. @br{}
  Default value: @code{:automatic}")

#+liber-documentation
(setf (liber:alias-for-function 'scrolled-window-hscrollbar-policy)
      "Accessor"
      (documentation 'scrolled-window-hscrollbar-policy 'function)
 "@version{2025-04-26}
  @syntax{(gtk:scrolled-window-hscrollbar-policy object) => policy}
  @syntax{(setf (gtk:scrolled-window-hscrollbar-policy object) policy)}
  @argument[object]{a @class{gtk:scrolled-window} widget}
  @argument[policy]{a @symbol{gtk:policy-type} value}
  @begin{short}
    Accessor of the @slot[gtk:scrolled-window]{hscrollbar-policy} slot of the
    @class{gtk:scrolled-window} class.
  @end{short}
  @see-class{gtk:scrolled-window}
  @see-symbol{gtk:policy-type}
  @see-function{gtk:scrolled-window-vscrollbar-policy}")

;;; --- gtk:scrolled-window-kinetic-scrolling ----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "kinetic-scrolling"
                                               'scrolled-window) t)
 "The @code{kinetic-scrolling} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether kinetic scrolling is enabled or not. Kinetic scrolling only applies
  to input devices of @code{:touchscreen} type. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'scrolled-window-kinetic-scrolling)
      "Accessor"
      (documentation 'scrolled-window-kinetic-scrolling 'function)
 "@version{2025-04-26}
  @syntax{(gtk:scrolled-window-kinetic-scrolling object) => scrolling}
  @syntax{(setf (gtk:scrolled-window-kinetic-scrolling object) scrolling)}
  @argument[object]{a @class{gtk:scrolled-window} widget}
  @argument[scrolling]{@em{true} to enable kinetic scrolling}
  @begin{short}
    Accessor of the @slot[gtk:scrolled-window]{kinetic-scrolling} slot of the
    @class{gtk:scrolled-window} class.
  @end{short}
  The @fun{gtk:scrolled-window-kinetic-scrolling} function returns the
  specified kinetic scrolling behavior. The
  @setf{gtk:scrolled-window-kinetic-scrolling} function turns kinetic scrolling
  on or off.

  Kinetic scrolling only applies to input devices of @code{:touchscreen} type.
  @see-class{gtk:scrolled-window}")

;;; --- gtk:scrolled-window-max-content-height ---------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "max-content-height"
                                               'scrolled-window) t)
 "The @code{max-content-height} property of type @code{:int} (Read / Write)@br{}
  The maximum content height of the scrolled window, or -1 if not set. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'scrolled-window-max-content-height)
      "Accessor"
      (documentation 'scrolled-window-max-content-height 'function)
 "@version{2025-04-26}
  @syntax{(gtk:scrolled-window-max-content-height object) => height}
  @syntax{(setf (gtk:scrolled-window-max-content-height object) height)}
  @argument[object]{a @class{gtk:scrolled-window} widget}
  @argument[height]{an integer for the maximum content height}
  @begin{short}
    Accessor of the @slot[gtk:scrolled-window]{max-content-height} slot of the
    @class{gtk:scrolled-window} class.
  @end{short}
  The @fun{gtk:scrolled-window-max-content-height} function returns the maximum
  content height that the scrolled window should keep visible. The scrolled
  window will grow up to this height before it starts scrolling the content.
  The @setf{gtk:scrolled-window-max-content-height} function sets the maximum
  height.

  It is a programming error to set the maximum content height to a value
  smaller than the @slot[gtk:scrolled-window]{min-content-height} value.
  @see-class{gtk:scrolled-window}
  @see-function{gtk:scrolled-window-min-content-height}")

;;; --- gtk:scrolled-window-max-content-width ----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "max-content-width"
                                               'scrolled-window) t)
 "The @code{max-content-width} property of type @code{:int} (Read / Write) @br{}
  The maximum content width of the scrolled window, or -1 if not set. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'scrolled-window-max-content-width)
      "Accessor"
      (documentation 'scrolled-window-max-content-width 'function)
 "@version{2025-04-26}
  @syntax{(gtk:scrolled-window-max-content-width object) => width}
  @syntax{(setf (gtk:scrolled-window-max-content-width object) width)}
  @argument[object]{a @class{gtk:scrolled-window} widget}
  @argument[width]{an integer for the maximum content width}
  @begin{short}
    Accessor of the @slot[gtk:scrolled-window]{max-content-width} slot of the
    @class{gtk:scrolled-window} class.
  @end{short}
  The @fun{gtk:scrolled-window-max-content-width} function returns the maximum
  content width that the scrolled window should keep visible. The scrolled
  window will grow up to this width before it starts scrolling the content. The
  @setf{gtk:scrolled-window-max-content-width} function sets the maximum width.

  It is a programming error to set the maximum content width to a value
  smaller than the @slot[gtk:scrolled-window]{min-content-width} value.
  @see-class{gtk:scrolled-window}
  @see-function{gtk:scrolled-window-min-content-width}")

;;; --- gtk:scrolled-window-min-content-height ---------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "min-content-height"
                                               'scrolled-window) t)
 "The @code{min-content-height} property of type @code{:int} (Read / Write)
  @br{}
  The minimum content height of the scrolled window, or -1 if not set. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'scrolled-window-min-content-height)
      "Accessor"
      (documentation 'scrolled-window-min-content-height 'function)
 "@version{2025-04-26}
  @syntax{(gtk:scrolled-window-min-content-height object) => height}
  @syntax{(setf (gtk:scrolled-window-min-content-height object) height)}
  @argument[object]{a @class{gtk:scrolled-window} widget}
  @argument[height]{an integer for the minimal content height}
  @begin{short}
    Accessor of the @slot[gtk:scrolled-window]{min-content-height} slot of the
    @class{gtk:scrolled-window} class.
  @end{short}
  The @fun{gtk:scrolled-window-min-content-height} function gets the minimal
  content height of the scrolled window that the scrolled window should keep
  visible. Note that this can and, usually will, be smaller than the minimum
  size of the content. The @setf{gtk:scrolled-window-min-content-heigth}
  function sets the minimum height.
  @see-class{gtk:scrolled-window}
  @see-function{gtk:scrolled-window-max-content-height}")

;;; --- gtk:scrolled-window-min-content-width ----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "min-content-width"
                                               'scrolled-window) t)
 "The @code{min-content-width} property of type @code{:int} (Read / Write) @br{}
  The minimum content width of the scrolled window, or -1 if not set. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'scrolled-window-min-content-width)
      "Accessor"
      (documentation 'scrolled-window-min-content-width 'function)
 "@version{2025-04-26}
  @syntax{(gtk:scrolled-window-min-content-width object) => width}
  @syntax{(setf (gtk:scrolled-window-min-content-width object) width)}
  @argument[object]{a @class{gtk:scrolled-window} widget}
  @argument[width]{an integer for the minimal content width}
  @begin{short}
    Accessor of the @slot[gtk:scrolled-window]{min-content-width} slot of the
    @class{gtk:scrolled-window} class.
  @end{short}
  The @fun{gtk:scrolled-window-min-content-width} function gets the minimum
  content width of the scrolled window that the scrolled window should keep
  visible. Note that this can and, usually will, be smaller than the minimum
  size of the content. The @setf{gtk:scrolled-window-min-content-width} function
  sets the minimum width.
  @see-class{gtk:scrolled-window}
  @see-function{gtk:scrolled-window-max-content-width}")

;;; --- gtk:scrolled-window-overlay-scrolling ----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "overlay-scrolling"
                                               'scrolled-window) t)
 "The @code{overlay-scrolling} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether overlay scrolling is enabled or not. If it is, the scrollbars are only
  added as traditional widgets when a mouse is present. Otherwise, they are
  overlayed on top of the content, as narrow indicators. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'scrolled-window-overlay-scrolling)
      "Accessor"
      (documentation 'scrolled-window-overlay-scrolling 'function)
 "@version{2025-04-26}
  @syntax{(gtk:scrolled-window-overlay-scrolling object) => scrolling}
  @syntax{(setf (gtk:scrolled-window-overlay-scrolling object) scrolling)}
  @argument[object]{a @class{gtk:scrolled-window} widget}
  @argument[scrolling]{a boolean whether to enable overly scrolling}
  @begin{short}
    Accessor of the @slot[gtk:scrolled-window]{overlay-scrolling} slot of the
    @class{gtk:scrolled-window} class.
  @end{short}
  The @fun{gtk:scrolled-window-overlay-scrolling} function returns whether
  overlay scrolling is enabled for this scrolled window. The
  @setf{gtk:scrolled-window-overlay-scrolling} function enables or disables
  overlay scrolling.
  @see-class{gtk:scrolled-window}")

;;; --- gtk:scrolled-window-propagate-natural-height ---------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "propagate-natural-height"
                                               'scrolled-window) t)
 "The @code{propagate-natural-height} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the natural height of the child should be calculated and propagated
  through the requested natural height of the scrolled window. This is useful in
  cases where an attempt should be made to allocate exactly enough space for
  the natural size of the child. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'scrolled-window-propagate-natural-height)
      "Accessor"
      (documentation 'scrolled-window-propagate-natural-height 'function)
 "@version{2025-04-26}
  @syntax{(gtk:scrolled-window-propagate-natural-height object) => propagate}
  @syntax{(setf (gtk:scrolled-window-propagate-natural-height object) propagate)}
  @argument[object]{a @class{gtk:scrolled-window} widget}
  @argument[propagate]{a boolean whether to propagate natural height}
  @begin{short}
    Accessor of the @slot[gtk:scrolled-window]{propagate-natural-height} slot
    of the @class{gtk:scrolled-window} class.
  @end{short}
  The @fun{gtk:scrolled-window-propagate-natural-height} function reports
  whether the natural height of the child will be calculated and propagated
  through the requested natural height of the scrolled window. The
  @setf{gtk:scrolled-window-propagate-natural-height} function sets the
  property.
  @see-class{gtk:scrolled-window}
  @see-function{gtk:scrolled-window-propagate-natural-width}")

;;; --- gtk:scrolled-window-propagate-natural-width ----------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "propagate-natural-width"
                                               'scrolled-window) t)
 "The @code{propagate-natural-width} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the natural width of the child should be calculated and propagated
  through the scrolled window’s requested natural width. This is useful in cases
  where an attempt should be made to allocate exactly enough space for the
  natural size of the child. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'scrolled-window-propagate-natural-width)
      "Accessor"
      (documentation 'scrolled-window-propagate-natural-width 'function)
 "@version{2025-04-26}
  @syntax{(gtk:scrolled-window-propagate-natural-width object) => propagate}
  @syntax{(setf (gtk:scrolled-window-propagate-natural-width object) propagate)}
  @argument[object]{a @class{gtk:scrolled-window} widget}
  @argument[propagate]{a boolean whether to propagate natural width}
  @begin{short}
    Accessor of the @slot[gtk:scrolled-window]{propagate-natural-width} slot
    of the @class{gtk:scrolled-window} class.
  @end{short}
  The @fun{gtk:scrolled-window-propagate-natural-width} function reports whether
  the natural width of the child will be calculated and propagated through the
  requested natural width of the scrolled window. The
  @setf{gtk:scrolled-window-propagate-natural-width} function sets the property.
  @see-class{gtk:scrolled-window}
  @see-function{gtk:scrolled-window-propagate-natural-height}")

;;; --- gtk:scrolled-window-vadjustment ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "vadjustment"
                                               'scrolled-window) t)
 "The @code{vadjustment} property of type @class{gtk:adjustment}
  (Read / Write / Construct) @br{}
  The adjustment for the vertical position.")

#+liber-documentation
(setf (liber:alias-for-function 'scrolled-window-vadjustment)
      "Accessor"
      (documentation 'scrolled-window-vadjustment 'function)
 "@version{2025-04-26}
  @syntax{(gtk:scrolled-window-vadjustment object) => adjustment}
  @syntax{(setf (gtk:scrolled-window-vadjustment object) adjustment)}
  @argument[object]{a @class{gtk:scrolled-window} widget}
  @argument[adjustment]{a @class{gtk:adjustment} object for the vertical
    scroll adjustment}
  @begin{short}
    Accessor of the @slot[gtk:scrolled-window]{vadjustment} slot of the
    @class{gtk:scrolled-window} class.
  @end{short}
  The @fun{gtk:scrolled-window-vadjustment} function returns the vertical
  adjustment of the scrollbar, used to connect the vertical scrollbar to the
  vertical scroll functionality of the child widget. The
  @setf{gtk:scrolled-window-vadjustment} function sets the adjusment for the
  vertical scrollbar.
  @see-class{gtk:scrolled-window}
  @see-class{gtk:adjustment}
  @see-function{gtk:scrolled-window-hadjustment}")

;;; --- gtk:scrolled-window-vscrollbar-policy ----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "vscrollbar-policy"
                                               'scrolled-window) t)
 "The @code{vscrollbar-policy} property of type @symbol{gtk:policy-type}
  (Read / Write) @br{}
  When the vertical scrollbar is displayed. @br{}
  Default value: @code{:automatic}")

#+liber-documentation
(setf (liber:alias-for-function 'scrolled-window-vscrollbar-policy)
      "Accessor"
      (documentation 'scrolled-window-vscrollbar-policy 'function)
 "@version{2025-04-26}
  @syntax{(gtk:scrolled-window-vscrollbar-policy object) => policy}
  @syntax{(setf (gtk:scrolled-window-vscrollbar-policy object) policy)}
  @argument[object]{a @class{gtk:scrolled-window} widget}
  @argument[policy]{a value of the @symbol{gtk:policy-type} enumeration}
  @begin{short}
    Accessor of the @slot[gtk:scrolled-window]{vscrollbar-policy} slot of the
    @class{gtk:scrolled-window} class.
  @end{short}
  @see-class{gtk:scrolled-window}
  @see-symbol{gtk:policy-type}
  @see-function{gtk:scrolled-window-hscrollbar-policy}")

;;; --- gtk:scrolled-window-window-placement -----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "window-placement"
                                               'scrolled-window) t)
 "The @code{window-placement} property of type @symbol{gtk:corner-type}
  (Read / Write) @br{}
  Where the contents are located with respect to the scrollbars. @br{}
  Default value: @code{:top-left}")

#+liber-documentation
(setf (liber:alias-for-function 'scrolled-window-window-placement)
      "Accessor"
      (documentation 'scrolled-window-window-placement 'function)
 "@version{2025-04-26}
  @syntax{(gtk:scrolled-window-window-placement object) => placement}
  @syntax{(setf (gtk:scrolled-window-window-placement object) placement)}
  @argument[object]{a @class{gtk:scrolled-window} widget}
  @argument[placement]{a value of the @symbol{gtk:corner-type} enumeration}
  @begin{short}
    Accessor of the @slot[gtk:scrolled-window]{window-placement} slot of the
    @class{gtk:scrolled-window} class.
  @end{short}
  Where the contents are located with respect to the scrollbars.
  @see-class{gtk:scrolled-window}
  @see-symbol{gtk:corner-type}
  @see-function{gtk:scrolled-window-placement}")

;;; ----------------------------------------------------------------------------
;;; gtk_scrolled_window_new
;;; ----------------------------------------------------------------------------

(declaim (inline scrolled-window-new))

(defun scrolled-window-new (&optional hadjustment vadjustment)
 #+liber-documentation
 "@version{2025-04-26}
  @argument[hadjustment]{an optional @class{gtk:adjustment} object for the
    horizontal adjustment}
  @argument[vadjustment]{an optional @class{gtk:adjustment} object for the
    vertical adjustment}
  @return{The new @class{gtk:scrolled-window} widget.}
  @begin{short}
    Creates a new scrolled window.
  @end{short}
  The two optional arguments are the adjustments of the scrolled window. These
  will be shared with the scrollbars and the child widget to keep the bars in
  sync with the child. Usually you want to use the default values for the
  adjustments, which will cause the scrolled window to create them for you.
  @see-class{gtk:scrolled-window}
  @see-class{gtk:adjustment}"
  (make-instance 'scrolled-window
                 :hadjustment hadjustment
                 :vadjustment vadjustment))

(export 'scrolled-window-new)

;;; ----------------------------------------------------------------------------
;;; gtk_scrolled_window_get_hscrollbar
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_scrolled_window_get_hscrollbar" scrolled-window-hscrollbar)
    (g:object widget)
 #+liber-documentation
 "@version{2025-04-26}
  @argument[window]{a @class{gtk:scrolled-window} widget}
  @begin{return}
    The horizontal @class{gtk:scrollbar} widget of the scrolled window, or
    @code{nil} if it does not have one.
  @end{return}
  @begin{short}
    Returns the horizontal scrollbar of the scrolled window.
  @end{short}
  @see-class{gtk:scrolled-window}
  @see-class{gtk:scrollbar}
  @see-function{gtk:scrolled-window-vscrollbar}"
  (window (g:object scrolled-window)))

(export 'scrolled-window-hscrollbar)

;;; ----------------------------------------------------------------------------
;;; gtk_scrolled_window_get_vscrollbar
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_scrolled_window_get_vscrollbar" scrolled-window-vscrollbar)
    (g:object widget)
 #+liber-documentation
 "@version{2025-04-26}
  @argument[window]{a @class{gtk:scrolled-window} widget}
  @begin{return}
    The vertical @class{gtk:scrollbar} widget of the scrolled window, or
    @code{nil} if it does not have one.
  @end{return}
  @begin{short}
    Returns the vertical scrollbar of the scrolled window.
  @end{short}
  @see-class{gtk:scrolled-window}
  @see-class{gtk:scrollbar}
  @see-function{gtk:scrolled-window-hscrollbar}"
  (window (g:object scrolled-window)))

(export 'scrolled-window-vscrollbar)

;;; ----------------------------------------------------------------------------
;;; gtk_scrolled_window_set_policy
;;; gtk_scrolled_window_get_policy
;;; ----------------------------------------------------------------------------

(defun (setf scrolled-window-policy) (policy window)
  (destructuring-bind (hpolicy vpolicy) policy
    (cffi:foreign-funcall "gtk_scrolled_window_set_policy"
                          (g:object scrolled-window) window
                          policy-type hpolicy
                          policy-type vpolicy
                          :void)
    (values hpolicy vpolicy)))

(defun scrolled-window-policy (window)
 #+liber-documentation
 "@version{2025-04-26}
  @syntax{(gtk:scrolled-window-policy window) => hpolicy, vpolicy}
  @syntax{(setf (gtk:scrolled-window-policy window) (list hpolicy vpolicy))}
  @argument[window]{a @class{gtk:scrolled-window} widget}
  @argument[hpolicy]{a value of the @symbol{gtk:policy-type} enumeration for
    the policy of the horizontal scrollbar}
  @argument[vpolicy]{a value of the @symbol{gtk:policy-type} enumeration for
    the policy of the vertical scrollbar}
  @begin{short}
    The @fun{gtk:scrolled-window-policy} function retrieves the current policy
    values for the horizontal and vertical scrollbars.
  @end{short}
  The @setf{gtk:scrolled-window-policy} function sets the scrollbar policy.

  The policy determines when the scrollbar should appear. It is a value from
  the @symbol{gtk:policy-type} enumeration. If @code{:always}, the scrollbar is
  always present. If @code{:never}, the scrollbar is never present. If
  @code{:automatic}, the scrollbar is present only if needed, that is, if
  the slider part of the scrollbar would be smaller than the trough - the
  display is larger than the page size.
  @see-class{gtk:scrolled-window}
  @see-symbol{gtk:policy-type}"
  (values (scrolled-window-hscrollbar-policy window)
          (scrolled-window-vscrollbar-policy window)))

(export 'scrolled-window-policy)

;;; ----------------------------------------------------------------------------
;;; gtk_scrolled_window_set_placement
;;; gtk_scrolled_window_get_placement
;;; ----------------------------------------------------------------------------

;; gtk_scrolled_window_set_placement sets the new value for the placement
;; and than updates the scrolled window

(defun (setf scrolled-window-placement) (placement scrolled-window)
  (cffi:foreign-funcall "gtk_scrolled_window_set_placement"
                        (g:object scrolled-window) scrolled-window
                        corner-type placement
                        :void)
  placement)

(defun scrolled-window-placement (window)
 #+liber-documentation
 "@version{2025-04-26}
  @syntax{(gtk:scrolled-window-placement window) => placement}
  @syntax{(setf (gtk:scrolled-window-placement window) placement)}
  @argument[window]{a @class{gtk:scrolled-window} widget}
  @argument[placement]{a value of the @symbol{gtk:corner-type} enumeration for
    the position of the child window}
  @begin{short}
    The @fun{gtk:scrolled-window-placement} function gets the placement of the
    contents with respect to the scrollbars for the scrolled window.
  @end{short}
  The @setf{gtk:scrolled-window-placement} function sets the placement of the
  contents.

  The default is @code{:top-left}, meaning the child is in the top left, with
  the scrollbars underneath and to the right. Other values in the
  @symbol{gtk:corner-type} enumeration are @code{:top-right},
  @code{:bottom-left}, and @code{:bottom-right}.

  See also the @fun{gtk:scrolled-window-unset-placement} function.
  @begin[Notes]{dictionary}
    In contrast to the @fun{gtk:scrolled-window-window-placement} function the
    @fun{gtk:scrolled-window-placement} function updates the scrolled window
    after setting the new value for the window placement.
  @end{dictionary}
  @see-class{gtk:scrolled-window}
  @see-function{gtk:scrolled-window-unset-placement}
  @see-function{gtk:scrolled-window-window-placement}"
  (scrolled-window-window-placement window))

(export 'scrolled-window-placement)

;;; ----------------------------------------------------------------------------
;;; gtk_scrolled_window_unset_placement
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_scrolled_window_unset_placement"
               scrolled-window-unset-placement) :void
 #+liber-documentation
 "@version{2025-04-26}
  @argument[window]{a @class{gtk:scrolled-window} widget}
  @begin{short}
    Unsets the placement of the contents with respect to the scrollbars for the
    scrolled window.
  @end{short}
  If no window placement is set for a scrolled window, it defaults to
  the @code{:top-left} value of the @symbol{gtk:corner-type} enumeration.

  See also the @fun{gtk:scrolled-window-placement} function.
  @see-class{gtk:scrolled-window}
  @see-symbol{gtk:corner-type}
  @see-function{gtk:scrolled-window-placement}"
  (window (g:object scrolled-window)))

(export 'scrolled-window-unset-placement)

;;; --- End of file gtk4.scrolled-window.lisp ----------------------------------

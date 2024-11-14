;;; ----------------------------------------------------------------------------
;;; gtk4.popover.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.16 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2019 - 2024 Dieter Kaiser
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
;;; GtkPopover
;;;
;;;     Context dependent bubbles
;;;
;;; Types and Values
;;;
;;;     GtkPopover
;;;
;;; Accessors
;;;
;;;     gtk_popover_set_autohide
;;;     gtk_popover_get_autohide
;;;     gtk_popover_set_cascade_popdown
;;;     gtk_popover_get_cascade_popdown
;;;     gtk_popover_set_child
;;;     gtk_popover_get_child
;;;     gtk_popover_set_default_widget
;;;     gtk_popover_set_has_arrow
;;;     gtk_popover_get_has_arrow
;;;     gtk_popover_get_mnemonics_visible
;;;     gtk_popover_set_mnemonics_visible
;;;     gtk_popover_set_pointing_to
;;;     gtk_popover_get_pointing_to
;;;     gtk_popover_set_position
;;;     gtk_popover_get_position
;;;
;;; Functions
;;;
;;;     gtk_popover_new
;;;     gtk_popover_popup
;;;     gtk_popover_popdown
;;;     gtk_popover_present
;;;     gtk_popover_set_offset
;;;     gtk_popover_get_offset
;;;
;;; Properties
;;;
;;;     autohide
;;;     cascade-popdown
;;;     child
;;;     default-widget
;;;     has-arrow
;;;     mnemonics-visible
;;;     pointing-to
;;;     position
;;;
;;; Signals
;;;
;;;     activate-default
;;;     closed
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkPopover
;;;                 ├── GtkEmojiChooser
;;;                 ╰── GtkPopoverMenu
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;;     GtkShortcutManager
;;;     GtkNative
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkPopover
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkPopover" popover
  (:superclass widget
    :export t
    :interfaces ("GtkAccessible"
                 "GtkBuildable"
                 "GtkConstraintTarget"
                 "GtkNative"
                 "GtkShortcutManager")
    :type-initializer "gtk_popover_get_type")
  ((autohide
    popover-autohide
    "autohide" "gboolean" t t)
   (cascade-popdown
    popover-cascade-popdown
    "cascade-popdown" "gboolean" t t)
   (child
    popover-child
    "child" "GtkWidget" t t)
   (default-widget
    popover-default-widget
    "default-widget" "GtkWidget" t t)
   (has-arrow
    popover-has-arrow
    "has-arrow" "gboolean" t t)
   (mnemonics-visible
    popover-mnemonics-visible
    "mnemonics-visible" "gboolean" t t)
   (pointing-to
    popover-pointing-to
    "pointing-to" "GdkRectangle" t t)
   (position
    popover-position
    "position" "GtkPositionType" t t)))

#+liber-documentation
(setf (documentation 'popover 'type)
 "@version{2024-10-26}
  @begin{short}
    The @class{gtk:popover} widget is a bubble-like context window, primarily
    meant to provide context-dependent information or options.
  @end{short}
  Popovers are attached to a parent widget. By default, they point to the whole
  widget area, although this behavior can be changed with the
  @fun{gtk:popover-pointing-to} function.

  @image[popover]{Figure: GtkPopover}

  The position of a popover relative to the widget it is attached to can also
  be changed through the @fun{gtk:popover-position} function.

  By default, the @class{gtk:popover} widget performs a GTK grab, in order to
  ensure input events get redirected to it while it is shown, and also so the
  popover is dismissed in the expected situations, clicks outside the popover,
  or the @kbd{Escape} key being pressed. If no such modal behavior is desired
  on a popover, the @fun{gtk:popover-autohide} function may be called on it to
  tweak its behavior.

  @subheading{GtkPopover as menu replacement}
  The @class{gtk:popover} widget is often used to replace menus. The best was to
  do this is to use the @class{gtk:popover-menu} subclass which supports being
  populated from a @class{g:menu-model} object with the
  @fun{gtk:popover-menu-new-from-model} function.
  @begin[Examples]{dictionary}
    @begin{pre}
<section>
  <attribute name=\"display-hint\">horizontal-buttons</attribute>
  <item>
    <attribute name=\"label\">Cut</attribute>
    <attribute name=\"action\">app.cut</attribute>
    <attribute name=\"verb-icon\">edit-cut-symbolic</attribute>
  </item>
  <item>
    <attribute name=\"label\">Copy</attribute>
    <attribute name=\"action\">app.copy</attribute>
    <attribute name=\"verb-icon\">edit-copy-symbolic</attribute>
  </item>
  <item>
    <attribute name=\"label\">Paste</attribute>
    <attribute name=\"action\">app.paste</attribute>
    <attribute name=\"verb-icon\">edit-paste-symbolic</attribute>
  </item>
</section>
    @end{pre}
  @end{dictionary}
  @begin[CSS nodes]{dictionary}
    The contents child node always gets the @code{.background} style class and
    the popover itself gets the @code{.menu} style class if the popover is
    menu-like, that is a @class{gtk:popover-menu} widget.

    Particular uses of the @class{gtk:popover} widget, such as touch selection
    popups or magnifiers in @class{gtk:entry} or @class{gtk:text-view} widgets
    get style classes like @code{.touch-selection} or @code{.magnifier} to
    differentiate from plain popovers.

    When styling a popover directly, the popover node should usually not have
    any background. The visible part of the popover can have a shadow. To
    specify it in CSS, set the box-shadow of the contents node.

    Note that, in order to accomplish appropriate arrow visuals, the
    @class{gtk:popover} widget uses custom drawing for the arrow node. This
    makes it possible for the arrow to change its shape dynamically, but it
    also limits the possibilities of styling it using CSS. In particular, the
    arrow gets drawn over the content node's border so they look like one shape,
    which means that the border-width of the content node and the arrow node
    should be the same. The arrow also does not support any border shape other
    than solid, no border-radius, only one border width, border-bottom-width is
    used, and no box-shadow.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"activate-default\" signal}
      @begin{pre}
lambda (popover)    :action
      @end{pre}
      @begin[code]{table}
        @entry[popover]{The @class{gtk:popover} widget which received the
          signal.}
      @end{table}
      The signal is a keybinding signal which gets emitted when the user
      activates the default widget of the popover.
    @subheading{The \"closed\" signal}
      @begin{pre}
lambda (popover)    :run-last
      @end{pre}
      @begin[code]{table}
        @entry[popover]{The @class{gtk:popover} widget which received the
          signal.}
      @end{table}
      The signal is emitted when the popover is dismissed either through API or
      user interaction.
  @end{dictionary}
  @see-constructor{gtk:popover-new}
  @see-slot{gtk:popover-autohide}
  @see-slot{gtk:popover-cascade-popdown}
  @see-slot{gtk:popover-child}
  @see-slot{gtk:popover-default-widget}
  @see-slot{gtk:popover-has-arrow}
  @see-slot{gtk:popover-mnemonics-visible}
  @see-slot{gtk:popover-pointing-to}
  @see-slot{gtk:popover-position}
  @see-class{gtk:popover-menu}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:popover-autohide ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "autohide" 'popover) t)
 "The @code{autohide} property of type @code{:boolean} (Read / Write) @br{}
  Whether to dismiss the popover on outside clicks. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'popover-autohide)
      "Accessor"
      (documentation 'popover-autohide 'function)
 "@version{2024-10-26}
  @syntax{(gtk:popover-autohide object) => setting}
  @syntax{(setf (gtk:popover-autohide object) setting)}
  @argument[object]{a @class{gtk:popover} widget}
  @argument[setting]{a boolean whether to dismiss the popover on outside clicks}
  @begin{short}
    Accessor of the @slot[gtk:popover]{autohide} slot of the @class{gtk:popover}
    class.
  @end{short}
  The @fun{gtk:popover-autohide} function returns whether the popover is modal.
  The @setf{gtk:popover-autohide} function sets whether popover is modal.

  A modal popover will grab the keyboard focus on it when being displayed.
  Clicking outside the popover area or pressing the @kbd{Esc} key will dismiss
  the popover.

  Called this function on an already showing popup with a new autohide value
  different from the current one, will cause the popup to be hidden.
  @see-class{gtk:popover}")

;;; --- gtk:popover-cascade-popdown --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "cascade-popdown" 'popover) t)
 "The @code{cascade-popdown} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether the popover pops down after a child popover. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'popover-cascade-popdown)
      "Accessor"
      (documentation 'popover-cascade-popdown 'function)
 "@version{2024-10-26}
  @syntax{(gtk:popover-cascade-popdown object) => setting}
  @syntax{(setf (gtk:popover-cascade-popdown object) setting)}
  @argument[object]{a @class{gtk:popover} widget}
  @argument[setting]{a boolean whether the popover should follow a child
    closing}
  @begin{short}
    Accessor of the @slot[gtk:popover]{cascade-popdown} slot of the
    @class{gtk:popover} class.
  @end{short}
  The @fun{gtk:popover-cascade-popdown} function returns whether the popover
  will close after a modal child is closed. If @em{true}, the popover will be
  closed when a child modal popover is closed. If @em{false}, the popover will
  stay visible.
  @see-class{gtk:popover}")

;;; --- gtk:popover-child ------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "child" 'popover) t)
 "The @code{child} property of type @class{gtk:widget} (Read / Write) @br{}
  The child widget.")

#+liber-documentation
(setf (liber:alias-for-function 'popover-child)
      "Accessor"
      (documentation 'popover-child 'function)
 "@version{2024-10-26}
  @syntax{(gtk:popover-child object) => child}
  @syntax{(setf (gtk:popover-child object) child)}
  @argument[object]{a @class{gtk:popover} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @begin{short}
    Accessor of the @slot[gtk:popover]{child} slot of the @class{gtk:popover}
    class.
  @end{short}
  The @fun{gtk:popover-child} function gets the child widget of the popover.
  The @setf{gtk:popover-child} function sets the child widget.
  @see-class{gtk:popover}
  @see-class{gtk:widget}")

;;; --- gtk:popover-default-widget ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "default-widget" 'popover) t)
 "The @code{default-widget} property of type @class{gtk:widget} (Read / Write)
  @br{}
  The default widget.")

#+liber-documentation
(setf (liber:alias-for-function 'popover-default-widget)
      "Accessor"
      (documentation 'popover-default-widget 'function)
 "@version{2024-10-26}
  @syntax{(gtk:popover-default-widget object) => widget}
  @syntax{(setf (gtk:popover-default-widget object) widget)}
  @argument[object]{a @class{gtk:popover} widget}
  @argument[widget]{a @class{gtk:widget} default widget, or @code{nil} to
    unset the default widget}
  @begin{short}
    Accessor of the @slot[gtk:popover]{default-widget} slot of the
    @class{gtk:popover} class.
  @end{short}
  The @fun{gtk:popover-default-widget} function gets the default widget of the
  popover. The @setf{gtk:popover-default-widget} function sets or unsets the
  default widget.

  The default widget is the widget that is activated when the user presses the
  @kbd{Enter} key in a dialog, for example.
  @see-class{gtk:popover}
  @see-class{gtk:widget}")

;;; --- gtk:popover-has-arrow --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "has-arrow" 'popover) t)
 "The @code{has-arrow} property of type @code{:boolean} (Read / Write) @br{}
  Whether to draw an arrow. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'popover-has-arrow)
      "Accessor"
      (documentation 'popover-has-arrow 'function)
 "@version{2024-10-26}
  @syntax{(gtk:popover-has-arrow object) => setting}
  @syntax{(setf (gtk:popover-has-arrow object) setting)}
  @argument[object]{a @class{gtk:popover} widget}
  @argument[setting]{a boolean whether the popover has an arrow}
  @begin{short}
    Accessor of the @slot[gtk:popover]{has-arrow} slot of the
    @class{gtk:popover} class.
  @end{short}
  The @fun{gtk:popover-has-arrow} function gets whether the popover is showing
  an arrow pointing at the widget that it is relative to. The
  @setf{gtk:popover-has-arrow} function sets whether the popover should draw an
  arrow.
  @see-class{gtk:popover}")

;;; --- gtk:popover-mnemonics-visible ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "mnemonics-visible" 'popover) t)
 "The @code{mnemonics-visible} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether mnemonics are currently visible in this popover. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'popover-mnemonics-visible)
      "Accessor"
      (documentation 'popover-mnemonics-visible 'function)
 "@version{2024-10-26}
  @syntax{(gtk:popover-mnemonics-visible object) => setting}
  @syntax{(setf (gtk:popover-mnemonics-visible object) setting)}
  @argument[object]{a @class{gtk:popover} widget}
  @argument[setting]{a boolean whether mnemonics are currently visible in the
    popover}
  @begin{short}
    Accessor of the @slot[gtk:popover]{mnemonics-visible} slot of the
    @class{gtk:popover} class.
  @end{short}
  Whether mnemonics are currently visible in this popover.
  @see-class{gtk:popover}")

;;; --- gtk:popover-pointing-to ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "pointing-to" 'popover) t)
 "The @code{pointing-to} property of type @class{gdk:rectangle} (Read / Write)
  @br{}
  Marks a specific rectangle to be pointed.")

#+liber-documentation
(setf (liber:alias-for-function 'popover-pointing-to)
      "Accessor"
      (documentation 'popover-pointing-to 'function)
 "@version{2024-10-26}
  @syntax{(gtk:popover-pointing-to object) => rect}
  @syntax{(setf (gtk:popover-pointing-to object) rect)}
  @argument[object]{a @class{gtk:popover} widget}
  @argument[rect]{a @class{gdk:rectangle} instance to point to}
  @begin{short}
    Accessor of the @slot[gtk:popover]{pointing-to} slot of the
    @class{gtk:popover} class.
  @end{short}
  The @fun{gtk:popover-pointing-to} function gets the rectangle that the popover
  points to. The @setf{gtk:popover-pointing-to} function sets the rectangle that
  popover points to. This is in the coordinate space of the popover parent.
  @see-class{gtk:popover}
  @see-class{gdk:rectangle}")

;;; --- gtk:popover-position ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "position" 'popover) t)
 "The @code{position} property of type @symbol{gtk:position-type} (Read / Write)
  @br{}
  Sets the preferred position of the popover. @br{}
  Default value: @code{:top}")

#+liber-documentation
(setf (liber:alias-for-function 'popover-position)
      "Accessor"
      (documentation 'popover-position 'function)
 "@version{2024-10-26}
  @syntax{(gtk:popover-pointing-to object) => position}
  @syntax{(setf (gtk:popover-pointing-to object) position)}
  @argument[object]{a @class{gtk:popover} widget}
  @argument[position]{a @symbol{gtk:position-type} value with the preferred
    popover position}
  @begin{short}
    Accessor of the @slot[gtk:popover]{position} slot of the
    @class{gtk:popover} class.
  @end{short}
  The @fun{gtk:popover-position} function returns the preferred position of the
  popover to appear. The @setf{gtk:popover-position} function sets the preferred
  position. If the popover is currently visible, it will be immediately updated.

  This preference will be respected where possible, although on lack of space,
  for example, if close to the window edges, the @class{gtk:popover} widget may
  choose to appear on the opposite side.
  @see-class{gtk:popover}
  @see-symbol{gtk:position-type}")

;;; ----------------------------------------------------------------------------
;;; gtk_popover_new
;;; ----------------------------------------------------------------------------

(declaim (inline popover-new))

(defun popover-new ()
 #+liber-documentation
 "@version{2024-10-26}
  @return{The new @class{gtk:popover} widget.}
  @short{Creates a new popover.}
  @see-class{gtk:popover}
  @see-class{gtk:widget}"
  (make-instance 'popover))

(export 'popover-new)

;;; ----------------------------------------------------------------------------
;;; gtk_popover_popup
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_popover_popup" popover-popup) :void
 #+liber-documentation
 "@version{#2024-10-26}
  @argument[popover]{a @class{gtk:popover} widget}
  @begin{short}
    Pops the popover up.
  @end{short}
  @see-class{gtk:popover}"
  (popover (g:object popover)))

(export 'popover-popup)

;;; ----------------------------------------------------------------------------
;;; gtk_popover_popdown
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_popover_popdown" popover-popdown) :void
 #+liber-documentation
 "@version{#2024-10-26}
  @argument[popover]{a @class{gtk:popover} widget}
  @begin{short}
    Pops the popover down.
  @end{short}
  This may have the side-effect of closing a parent popover as well. See
  the @slot[gtk:popover]{cascade-popdown} property.
  @see-class{gtk:popover}
  @see-function{gtk:popover-cascade-popdown}"
  (popover (g:object popover)))

(export 'popover-popdown)

;;; ----------------------------------------------------------------------------
;;; gtk_popover_present
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_popover_present" popover-present) :void
 #+liber-documentation
 "@version{#2024-10-26}
  @argument[popover]{a @class{gtk:popover} widget}
  @begin{short}
    Allocate a size for the @class{gtk:popover} widget.
  @end{short}
  This function needs to be called in size-allocate by widgets who have a
  @class{gtk:popover} widget as child. When using a layout manager, this is
  happening automatically.

  To make a popover appear on screen, use the @fun{gtk:popover-popup} function.
  @see-class{gtk:popover}
  @see-function{gtk:popover-popup}"
  (popover (g:object popover)))

(export 'popover-present)

;;; ----------------------------------------------------------------------------
;;; gtk_popover_get_offset
;;; gtk_popover_set_offset
;;; ----------------------------------------------------------------------------

;; TODO: Is it possible to write a setf-expander macro which allows to
;; assign multiple values?!

(defun (setf popover-offset) (value popover)
  (destructuring-bind (xoffset yoffset) value
    (cffi:foreign-funcall "gtk_popover_set_offset"
                          (g:object popover) popover
                          :int xoffset
                          :int yoffset
                          :void)
    (values xoffset yoffset)))

(cffi:defcfun ("gtk_popover_get_offset" %popover-get-offset) :void
  (popover (g:object popover))
  (xoffset (:pointer :int))
  (yoffset (:pointer :int)))

(defun popover-offset (popover)
 #+liber-documentation
 "@version{#2024-10-26}
  @syntax{(gtk:popover-offset popover) => xoffset, yoffset}
  @syntax{(setf (gtk:popover-offset popover) '(xoffset yoffset))}
  @argument[popover]{a @class{gtk:popover} widget}
  @argument[xoffset]{an integer with the xoffset}
  @argument[yoffset]{an integer with the yoffset}
  @begin{short}
    The @fun{gtk:popover-offset} function gets the offset to use when
    calculating the position of the popover.
  @end{short}
  The @setf{gtk:popover-offset} function sets the offset. These values are used
  when preparing the @class{gdk:popup-layout} object for positioning the
  popover.
  @see-class{gtk:popover}
  @see-class{gdk:popup-layout}"
  (cffi:with-foreign-objects ((xoffset :int) (yoffset :int))
    (%popover-get-offset popover xoffset yoffset)
    (values (cffi:mem-ref xoffset :int)
            (cffi:mem-ref yoffset :int))))

(export 'popover-offset)

;;; --- End of file gtk4.popover.lisp ------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk.popover.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.6 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2019 - 2022 Dieter Kaiser
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
;;; struct GtkPopover
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkPopover" popover
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
 "@version{#2022-7-29}
  @begin{short}
    A @sym{gtk:popover} widget is a bubble-like context window, primarily meant
    to provide context-dependent information or options.
  @end{short}
  Popovers are attached to a parent widget. By default, they point to the whole
  widget area, although this behavior can be changed with the
  @fun{gtk:popover-pointing-to} function.

  @image[popover]{Figure: GtkPopover}

  The position of a popover relative to the widget it is attached to can also
  be changed through the @fun{gtk:popover-position} function.

  By default, the @sym{gtk:popover} widget performs a GTK grab, in order to
  ensure input events get redirected to it while it is shown, and also so the
  popover is dismissed in the expected situations, clicks outside the popover,
  or the @kbd{Escape} key being pressed. If no such modal behavior is desired
  on a popover, the @fun{gtk:popover-modal} function may be called on it to
  tweak its behavior.

  @subheading{GtkPopover as menu replacement}
  The @sym{gtk:popover} widget is often used to replace menus. The best was to
  do this is to use the @class{gtk:popover-menu} subclass which supports being
  populated from a @class{g:menu-model} object with the
  @fun{gtk:popover-menu-new-from-model} function.
  @begin[Example]{dictionary}
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
    menu-like, i.e. a @class{gtk:popover-menu} widget.

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
      The signal is a keybinding signal which gets emitted when the user
      activates the default widget of the popover.
      @begin[code]{table}
        @entry[popover]{The @sym{gtk:popover} widget which received the signal.}
      @end{table}
    @subheading{The \"closed\" signal}
      @begin{pre}
lambda (popover)    :run-last
      @end{pre}
      The signal is emitted when the popover is dismissed either through API or
      user interaction.
      @begin[code]{table}
        @entry[popover]{The @sym{gtk:popover} widget which received the signal.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk:popover-autohide}
  @see-slot{gtk:popover-cascade-popdown}
  @see-slot{gtk:popover-child}
  @see-slot{gtk:popover-default-widget}
  @see-slot{gtk:popover-has-arrow}
  @see-slot{gtk:popover-mnemonics-visible}
  @see-slot{gtk:popover-pointing-to}
  @see-slot{gtk:popover-position}
  @see-constructor{gtk:popover-new}
  @see-class{gtk:popover-menu}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- popover-autohide ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "autohide" 'popover) t)
 "The @code{autohide} property of type @code{:boolean} (Read / Write) @br{}
  Whether to dismiss the popover on outside clicks. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'popover-autohide)
      "Accessor"
      (documentation 'popover-autohide 'function)
 "@version{#2022-7-29}
  @syntax[]{(gtk:popover-autohide object) => setting}
  @syntax[]{(setf (gtk:popover-autohide object) setting)}
  @argument[object]{a @class{gtk:popover} widget}
  @argument[setting]{a boolean whether to dismiss the popover on outside clicks}
  @begin{short}
    Accessor of the @slot[gtk:popover]{autohide} slot of the @class{gtk:popover}
    class.
  @end{short}
  The @sym{gtk:popover-autohide} function returns whether the popover is modal.
  The @sym{(setf gtk:popover-autohide)} function sets whether popover is modal.

  A modal popover will grab the keyboard focus on it when being displayed.
  Clicking outside the popover area or pressing the @kbd{Esc} key will dismiss
  the popover.

  Called this function on an already showing popup with a new autohide value
  different from the current one, will cause the popup to be hidden.
  @see-class{gtk:popover}")

;;; --- popover-cascade-popdown --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "cascade-popdown"
                                               'popover) t)
 "The @code{cascade-popdown} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether the popover pops down after a child popover. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'popover-cascade-popdown)
      "Accessor"
      (documentation 'popover-cascade-popdown 'function)
 "@version{#2022-7-29}
  @syntax[]{(gtk:popover-cascade-popdown object) => setting}
  @syntax[]{(setf (gtk:popover-cascade-popdown object) setting)}
  @argument[object]{a @class{gtk:popover} widget}
  @argument[setting]{a boolean whether the popover should follow a child
    closing}
  @begin{short}
    Accessor of the @slot[gtk:popover]{cascade-popdown} slot of the
    @class{gtk:popover} class.
  @end{short}
  The @sym{gtk:popover-cascade-popdown} function returns whether the popover
  will close after a modal child is closed. If @em{true}, the popover will be
  closed when a child modal popover is closed. If @em{false}, the popover will
  stay visible.
  @see-class{gtk:popover}")

;;; --- popover-child ------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "child" 'popover) t)
 "The @code{child} property of type @class{gtk:widget} (Read / Write) @br{}
  The child widget.")

#+liber-documentation
(setf (liber:alias-for-function 'popover-child)
      "Accessor"
      (documentation 'popover-child 'function)
 "@version{#2022-7-29}
  @syntax[]{(gtk:popover-child object) => child}
  @syntax[]{(setf (gtk:popover-child object) child)}
  @argument[object]{a @class{gtk:popover} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @begin{short}
    Accessor of the @slot[gtk:popover]{child} slot of the @class{gtk:popover}
    class.
  @end{short}
  The @sym{gtk:popover-child} function gets the child widget of the popover.
  The @sym{(setf gtk:popover-child)} function sets the child widget.
  @see-class{gtk:popover}
  @see-class{gtk:widget}")

;;; --- popover-default-widget ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "default-widget" 'popover) t)
 "The @code{default-widget} property of type @class{gtk:widget} (Read / Write)
  @br{}
  The default widget.")

#+liber-documentation
(setf (liber:alias-for-function 'popover-default-widget)
      "Accessor"
      (documentation 'popover-default-widget 'function)
 "@version{#2022-7-29}
  @syntax[]{(gtk:popover-default-widget object) => widget}
  @syntax[]{(setf (gtk:popover-default-widget object) widget)}
  @argument[object]{a @class{gtk:popover} widget}
  @argument[widget]{a @class{gtk:widget} default widget, or @code{nil} to
    unset the default widget}
  @begin{short}
    Accessor of the @slot[gtk:popover]{default-widget} slot of the
    @class{gtk:popover} class.
  @end{short}
  The @sym{gtk:popover-default-widget} function gets the default widget of the
  popover. The @sym{(setf gtk:popover-default-widget)} function sets or unsets
  the default widget.

  The default widget is the widget that is activated when the user presses the
  @kbd{Enter} key in a dialog, for example.
  @see-class{gtk:popover}
  @see-class{gtk:widget}")

;;; --- popover-has-arrow --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "has-arrow" 'popover) t)
 "The @code{has-arrow} property of type @code{:boolean} (Read / Write) @br{}
  Whether to draw an arrow. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'popover-has-arrow)
      "Accessor"
      (documentation 'popover-has-arrow 'function)
 "@version{#2022-7-29}
  @syntax[]{(gtk:popover-has-arrow object) => setting}
  @syntax[]{(setf (gtk:popover-has-arrow object) setting)}
  @argument[object]{a @class{gtk:popover} widget}
  @argument[setting]{a boolean whether the popover has an arrow}
  @begin{short}
    Accessor of the @slot[gtk:popover]{has-arrow} slot of the
    @class{gtk:popover} class.
  @end{short}
  The @sym{gtk:popover-has-arrow} function gets whether the popover is showing
  an arrow pointing at the widget that it is relative to. The
  @sym{(setf gtk:popover-has-arrow)} function sets whether the popover should
  draw an arrow.
  @see-class{gtk:popover}")

;;; --- popover-mnemonics-visible ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "mnemonics-visible"
                                               'popover) t)
 "The @code{mnemonics-visible} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether mnemonics are currently visible in this popover. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'popover-mnemonics-visible)
      "Accessor"
      (documentation 'popover-mnemonics-visible 'function)
 "@version{#2022-7-29}
  @syntax[]{(gtk:popover-mnemonics-visible object) => setting}
  @syntax[]{(setf (gtk:popover-mnemonics-visible object) setting)}
  @argument[object]{a @class{gtk:popover} widget}
  @argument[setting]{a boolean whether mnemonics are currently visible in the
    popover}
  @begin{short}
    Accessor of the @slot[gtk:popover]{mnemonics-visible} slot of the
    @class{gtk:popover} class.
  @end{short}
  Whether mnemonics are currently visible in this popover.
  @see-class{gtk:popover}")

;;; --- popover-pointing-to ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "pointing-to" 'popover) t)
 "The @code{pointing-to} property of type @class{gdk:rectangle} (Read / Write)
  @br{}
  Marks a specific rectangle to be pointed.")

#+liber-documentation
(setf (liber:alias-for-function 'popover-pointing-to)
      "Accessor"
      (documentation 'popover-pointing-to 'function)
 "@version{#2022-7-29}
  @syntax[]{(gtk:popover-pointing-to object) => rect}
  @syntax[]{(setf (gtk:popover-pointing-to object) rect)}
  @argument[object]{a @class{gtk:popover} widget}
  @argument[rect]{a @class{gdk:rectangle} instance to point to}
  @begin{short}
    Accessor of the @slot[gtk:popover]{pointing-to} slot of the
    @class{gtk:popover} class.
  @end{short}
  Sets the rectangle that the popover will point to, in the coordinate space of
  the widget the popover is attached to, see the @fun{gtk:popover-relative-to}
  function.
  @see-class{gtk:popover}
  @see-class{gdk:rectangle}
  @see-function{gtk:popover-relative-to}")

;;; --- popover-position ---------------------------------------------------

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
 "@version{#2022-7-29}
  @syntax[]{(gtk:popover-pointing-to object) => position}
  @syntax[]{(setf (gtk:popover-pointing-to object) position)}
  @argument[object]{a @class{gtk:popover} widget}
  @argument[position]{a @symbol{gtk:position-type} value with the preferred
    popover position}
  @begin{short}
    Accessor of the @slot[gtk:popover]{position} slot of the
    @class{gtk:popover} class.
  @end{short}
  The @sym{gtk:popover-position} function returns the preferred position of the
  popover to appear. The @sym{(setf gtk:popover-position)} function sets the
  preferred position. If the popover is currently visible, it will be
  immediately updated.

  This preference will be respected where possible, although on lack of space,
  e.g. if close to the window edges, the @class{gtk:popover} widget may choose
  to appear on the opposite side.
  @see-class{gtk:popover}
  @see-symbol{gtk:position-type}")

;;; ----------------------------------------------------------------------------
;;; gtk_popover_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline popover-new))

(defun popover-new ()
 #+liber-documentation
 "@version{#2022-7-29}
  @return{A new @class{gtk:popover} widget.}
  @short{Creates a new popover.}
  @see-class{gtk:popover}
  @see-class{gtk:widget}"
  (make-instance 'popover))

(export 'popover-new)

;;; ----------------------------------------------------------------------------
;;; gtk_popover_popup ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_popover_popup" popover-popup) :void
 #+liber-documentation
 "@version{#2022-7-29}
  @argument[popover]{a @class{gtk:popover} widget}
  @begin{short}
    Pops the popover up.
  @end{short}
  This is different than a @fun{gtk:widget-show} call in that it shows the
  popover with a transition. If you want to show the popover without a
  transition, use the @fun{gtk:widget-show} function.
  @see-class{gtk:popover}
  @see-function{gtk:widget-show}"
  (popover (g:object popover)))

(export 'popover-popup)

;;; ----------------------------------------------------------------------------
;;; gtk_popover_popdown ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_popover_popdown" popover-popdown) :void
 #+liber-documentation
 "@version{#2022-7-29}
  @argument[popover]{a @class{gtk:popover} widget}
  @begin{short}
    Pops the popover down.
  @end{short}
  This is different than a @fun{gtk:widget-hide} call in that it shows the
  popover with a transition. If you want to hide the popover without a
  transition, use the @fun{gtk:widget-hide} function.
  @see-class{gtk:popover}
  @see-function{gtk:widget-hide}"
  (popover (g:object popover)))

(export 'popover-popdown)

;;; ----------------------------------------------------------------------------
;;; gtk_popover_present ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_popover_present" popover-present) :void
 #+liber-documentation
 "@version{#2022-7-29}
  @argument[popover]{a @class{gtk:popover} widget}
  @begin{short}
    Presents the popover to the user.
  @end{short}
  @see-class{gtk:popover}"
  (popover (g:object popover)))

(export 'popover-present)

;;; ----------------------------------------------------------------------------
;;; gtk_popover_get_offset ()
;;; gtk_popover_set_offset () -> popover-offset
;;; ----------------------------------------------------------------------------

(defun (setf popover-offset) (value popover)
  (destructuring-bind (x-offset y-offset) value
    (foreign-funcall "gtk_popover_set_offset"
                     (g:object popover) popover
                     :int x-offset
                     :int y-offset
                     :void)
    (values x-offset y-offset)))

(defcfun ("gtk_popover_get_offset" %popover-get-offset) :void
  (popover (g:object popover))
  (x-offset (:pointer :int))
  (y-offset (:pointer :int)))

(defun popover-offset (popover)
 #+liber-documentation
 "@version{#2022-7-29}
  @syntax[]{(gtk:popover-offset popover) => x-offset, y-offset}
  @syntax[]{(setf (gtk:popover-offset popover) '(x-offset y-offset))}
  @argument[popover]{a @class{gtk:popover} widget}
  @argument[x-offset]{an integer with the x-offset}
  @argument[y-offset]{an integer with the y-offset}
  @begin{short}
    Accessor of the offset values for the popover.
  @end{short}
  The @sym{gtk:popover-offset} function gets the offset to use when calculating
  the position of the popover. The @sym{(setf gtk:popover-offset} function sets
  the offset.

  These values are used when preparing the @class{gtk:popup-layout} object for
  positioning the popover.
  @see-class{gtk:popover}
  @see-class{gtk:popover-layout}"
  (with-foreign-objects ((x-offset :int) (y-offset :int))
    (%popover-get-offset popover x-offset y-offset)
    (values (mem-ref x-offset :int)
            (mem-ref y-offset :int))))

(export 'popover-offset)

;;; --- End of file gtk.popover.lisp -------------------------------------------

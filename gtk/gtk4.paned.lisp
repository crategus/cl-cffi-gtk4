;;; ----------------------------------------------------------------------------
;;; gtk4.paned.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.10 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2023 Dieter Kaiser
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
;;; GtkPaned
;;;
;;;     A widget with two adjustable panes
;;;
;;; Types and Values
;;;
;;;     GtkPaned
;;;
;;; Accessors
;;;
;;;     gtk_paned_set_end_child
;;;     gtk_paned_get_end_child
;;;     gtk_paned_set_position
;;;     gtk_paned_get_position
;;;     gtk_paned_set_resize_end_child
;;;     gtk_paned_get_resize_end_child
;;;     gtk_paned_set_resize_start_child
;;;     gtk_paned_get_resize_start_child
;;;     gtk_paned_set_shrink_end_child
;;;     gtk_paned_get_shrink_end_child
;;;     gtk_paned_set_shrink_start_child
;;;     gtk_paned_get_shrink_start_child
;;;     gtk_paned_set_start_child
;;;     gtk_paned_get_start_child
;;;     gtk_paned_set_wide_handle
;;;     gtk_paned_get_wide_handle
;;;
;;; Functions
;;;
;;;     gtk_paned_new
;;;
;;; Properties
;;;
;;;     end-child
;;;     max-position
;;;     min-position
;;;     position
;;;     position-set
;;;     resize-end-child
;;;     resize-start-child
;;;     shrink-end-child
;;;     shrink-start-child
;;;     start-child
;;;     wide-handle
;;;
;;; Signals
;;;
;;;     accept-position
;;;     cancel-position
;;;     cycle-child-focus
;;;     cycle-handle-focus
;;;     move-handle
;;;     toggle-handle-focus
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkPaned
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkAccessibleRange                                 Since 4.10
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;;     GtkOrientable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkPaned
;;; ----------------------------------------------------------------------------

;; TODO: Implement the GtkAccessibleRange interface

(gobject:define-g-object-class "GtkPaned" paned
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget"
                "GtkOrientable")
   :type-initializer "gtk_paned_get_type")
  ((end-child
    paned-end-child
    "end-child" "GtkWidget" t t)
   (max-position
    paned-max-position
    "max-position" "gint" t nil)
   (min-position
    paned-min-position
    "min-position" "gint" t nil)
   (position
    paned-position
    "position" "gint" t t)
   (position-set
    paned-position-set
    "position-set" "gboolean" t t)
   (resize-end-child
    paned-resize-end-child
    "resize-end-child" "gboolean" t t)
   (resize-start-child
    paned-resize-start-child
    "resize-start-child" "gboolean" t t)
   (shrink-start-child
    paned-shrink-start-child
    "shrink-start-child" "gboolean" t t)
   (shrink-end-child
    paned-shrink-end-child
    "shrink-end-child" "gboolean" t t)
   (start-child
    paned-start-child
    "start-child" "GtkWidget" t t)
   (wide-handle
    paned-wide-handle
    "wide-handle" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'paned 'type)
 "@version{#2022-7-31}
  @begin{short}
    The @sym{gtk:paned} widget has two panes, arranged either horizontally or
    vertically.
  @end{short}
  The division between the two panes is adjustable by the user by dragging a
  handle.

  @image[paned]{Figure: GtkPaned}

  Child widgets are added to the panes of the paned widget with the
  @fun{gtk:paned-start-child} and @fun{gtk:paned-end-child} functions. The
  division between the two children is set by default from the size requests of
  the children, but it can be adjusted by the user.

  A paned widget draws a separator between the two child widgets and a small
  handle that the user can drag to adjust the division. It does not draw any
  relief around the children or around the separator. The space in which the
  separator is called the gutter. Often, it is useful to put each child widget
  inside a @class{gtk:frame} widget with the shadow type set to the @code{:in}
  value so that the gutter appears as a ridge. No separator is drawn if one of
  the children is missing.

  Each child widget has two child properties that can be set, the @code{resize}
  and @code{shrink} child properties. If the @code{resize} child property is
  @em{true}, then when the @sym{gtk:paned} widget is resized, that child widget
  will expand or shrink along with the paned widget. If the @code{shrink}
  property is @em{true}, then that child widget can be made smaller than its
  requisition by the user. Setting the @code{shrink} child property to
  @em{false} allows the application to set a minimum size. If the @code{resize}
  child property is @em{false} for both children, then this is treated as if
  the @code{resize} child property is @em{true} for both children.

  The application can set the position of the slider as if it were set by the
  user, by calling the @fun{gtk:paned-position} function.
  @begin[CSS nodes]{dictionary}
    @begin{pre}
paned
├── <child>
├── separator\[.wide\]
╰── <child>
    @end{pre}
    The @sym{gtk:paned} implementation has a main CSS node with name
    @code{paned}, and a subnode for the separator with name @code{separator}.
    The subnode gets a @code{.wide} style class when the paned is supposed to
    be wide. In horizontal orientation, the nodes of the children are always
    arranged from left to right. So @code{:first-child} will always select the
    leftmost child widget, regardless of text direction.
  @end{dictionary}
  @begin[Example]{dictionary}
    Creating a paned widget with minimum sizes.
    @begin{pre}
(let ((frame1 (make-instance 'gtk:frame
                             :width-request 50))
      (frame2 (make-instance 'gtk:frame
                             :width-request 50))
      (paned (make-instance 'gtk:paned
                            :orientation :horizontal
                            :start-child frame1
                            :end-child frame2
                            :resize-start-child t
                            :width-request 250
                            :height-request 150)))
    ... )
    @end{pre}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"accept-position\" signal}
      @begin{pre}
lambda (widget)    :action
      @end{pre}
      The signal is a keybinding signal which gets emitted to accept the current
      position of the handle when moving it using key bindings. The default
      binding for this signal is the @kbd{Return} or @kbd{Space} key.
      @begin[code]{table}
        @entry[widget]{The @sym{gtk:paned} widget that received the signal.}
      @end{table}
    @subheading{The \"cancel-position\" signal}
      @begin{pre}
lambda (widget)    :action
      @end{pre}
      The signal is a keybinding signal which gets emitted to cancel moving the
      position of the handle using key bindings. The position of the handle will
      be reset to the value prior to moving it. The default binding for this
      signal is the @kbd{Escape} key.
      @begin[code]{table}
        @entry[widget]{The @sym{gtk:paned} widget that received the signal.}
      @end{table}
    @subheading{The \"cycle-child-focus\" signal}
      @begin{pre}
lambda (widget reversed)    :action
      @end{pre}
      The signal is a keybinding signal which gets emitted to cycle the focus
      between the children of the paned widget. The default binding is the
      @kbd{F6} key.
      @begin[code]{table}
        @entry[widget]{The @sym{gtk:paned} widget that received the signal.}
        @entry[reversed]{A boolean whether cycling backward or forward.}
      @end{table}
    @subheading{The \"cycle-handle-focus\" signal}
      @begin{pre}
lambda (widget reversed)    :action
      @end{pre}
      The signal is a keybinding signal which gets emitted to cycle whether the
      paned widget should grab focus to allow the user to change position of the
      handle by using key bindings. The default binding for this signal is the
      @kbd{F8} key.
      @begin[code]{table}
        @entry[widget]{The @sym{gtk:paned} widget that received the signal.}
        @entry[reversed]{A boolean whether cycling backward or forward.}
      @end{table}
    @subheading{The \"move-handle\" signal}
      @begin{pre}
lambda (widget scrolltype)    :action
      @end{pre}
      The signal is a keybinding signal which gets emitted to move the handle
      when the user is using key bindings to move it.
      @begin[code]{table}
        @entry[widget]{The @sym{gtk:paned} widget that received the signal.}
        @entry[scrolltype]{A value of the @symbol{gtk:scroll-type} enumeration.}
      @end{table}
    @subheading{The \"toggle-handle-focus\" signal}
      @begin{pre}
lambda (widget)    :action
      @end{pre}
      The signal is a keybinding signal which gets emitted to accept the current
      position of the handle and then move focus to the next widget in the focus
      chain. The default binding is the @kbd{Tab} key.
      @begin[code]{table}
        @entry[widget]{The @sym{gtk:paned} widget that received the signal.}
      @end{table}
  @end{dictionary}
  @see-constructor{gtk:paned-new}
  @see-slot{gtk:paned-end-child}
  @see-slot{gtk:paned-max-position}
  @see-slot{gtk:paned-min-position}
  @see-slot{gtk:paned-position}
  @see-slot{gtk:paned-position-set}
  @see-slot{gtk:paned-resize-end-child}
  @see-slot{gtk:paned-resize-start-child}
  @see-slot{gtk:paned-shrink-end-child}
  @see-slot{gtk:paned-shrink-start-child}
  @see-slot{gtk:paned-start-child}
  @see-slot{gtk:paned-wide-handle}
  @see-symbol{gtk:scroll-type}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- paned-end-child --------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "end-child" 'paned) t)
 "The @code{end-child} property of type @class{gtk:widget} (Read / Write) @br{}
  The second child widget.")

#+liber-documentation
(setf (liber:alias-for-function 'paned-end-child)
      "Accessor"
      (documentation 'paned-end-child 'function)
 "@version{#2022-1-25}
  @syntax[]{(gtk:paned-end-child object) => child}
  @syntax[]{(setf (gtk:paned-end-child) object) child)}
  @argument[object]{a @class{gtk:paned} widget}
  @argument[child]{a @class{gtk:widget} second child widget}
  @begin{short}
    Accessor of the @slot[gtk:paned]{end-child} slot of the @class{gtk:paned}
    class.
  @end{short}
  The @sym{gtk:paned-end-child} function retrieves the end child widget of the
  given paned. The @sym{(setf gtk:paned-end-child)} function sets the end child
  widget.
  @see-class{gtk:paned}
  @see-class{gtk:widget}")

;;; --- paned-max-position -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "max-position" 'paned) t)
 "The @code{max-position} property of type @code{:int} (Read) @br{}
  The largest possible value for the position property. This property is
  derived from the size and shrinkability of the children of the paned widget.
  @br{}
  Allowed values: >= 0 @br{}
  Default value: 2147483647")

#+liber-documentation
(setf (liber:alias-for-function 'paned-max-position)
      "Accessor"
      (documentation 'paned-max-position 'function)
 "@version{#2022-7-31}
  @syntax[]{(gtk:paned-max-position object) => position}
  @argument[object]{a @class{gtk:paned} widget}
  @argument[position]{an integer with the largest possible position}
  @begin{short}
    Accessor of the @slot[gtk:paned]{max-position} slot of the
    @class{gtk:paned} class.
  @end{short}
  The @sym{gtk:paned-max-position} function gets the largest possible value for
  the position property.
  @see-class{gtk:paned}
  @see-function{gtk:paned-position}
  @see-function{gtk:paned-min-position}")

;;; --- paned-min-position -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "min-position" 'paned) t)
 "The @code{min-position} property of type @code{:int} (Read) @br{}
  The smallest possible value for the position property. This property is
  derived from the size and shrinkability of the children of the paned widget.
  @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'paned-min-position)
      "Accessor"
      (documentation 'paned-min-position 'function)
 "@version{#2022-7-31}
  @syntax[]{(gtk:paned-min-position object) => position}
  @argument[object]{a @class{gtk:paned} widget}
  @argument[position]{an integer with the smallest possible position}
  @begin{short}
    Accessor of the @slot[gtk:paned]{min-position} slot of the
    @class{gtk:paned} class.
  @end{short}
  The @sym{gtk:paned-min-position} function gets the smallest possible value
  for the position property.
  @see-class{gtk:paned}
  @see-function{gtk:paned-position}
  @see-function{gtk:paned-max-position}")

;;; --- paned-position ---------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "position" 'paned) t)
 "The @code{position} property of type @code{:int} (Read / Write) @br{}
  Position of the paned separator in pixels, 0 means all the way to the
  left/top. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'paned-position)
      "Accessor"
      (documentation 'paned-position 'function)
 "@version{#2022-7-31}
  @syntax[]{(gtk:paned-position object) => position}
  @syntax[]{(setf (gtk:paned-position position) position)}
  @argument[object]{a @class{gtk:paned} widget}
  @argument[position]{an integer with the pixel position of divider, a negative
    value means that the position is unset}
  @begin{short}
    Accessor of the @slot[gtk:paned]{position} slot of the @class{gtk:paned}
    class.
  @end{short}
  The @sym{gtk:paned-position} function obtains the position of the divider
  between the two panes. The @sym{(setf gtk:paned-position)} function sets the
  position.
  @see-class{gtk:paned}
  @see-function{gtk:paned-max-position}
  @see-function{gtk:paned-min-position}")

;;; --- paned-position-set -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "position-set" 'paned) t)
 "The @code{position-set} property of type @code{:boolean}
  (Read / Write) @br{}
  @em{True} if the @code{position} property should be used. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'paned-position-set)
      "Accessor"
      (documentation 'paned-position-set 'function)
 "@version{#2022-7-31}
  @syntax[]{(gtk:paned-position-set object) => setting}
  @syntax[]{(setf (gtk:paned-position-set position) setting)}
  @argument[object]{a @class{gtk:paned} widget}
  @argument[setting]{a boolean whether the @slot[gtk:paned]{position} property
    should be used}
  @begin{short}
    Accessor of the @slot[gtk:paned]{position-set} slot of the
    @class{gtk:paned} class.
  @end{short}
  The @sym{gtk:paned-position-set} function gets whether the
  @slot[gtk:paned]{position} property should be used. The
  @sym{(setf gtk:paned-position-set)} function sets whether the
  @slot[gtk:paned]{position} property should be used.
  @see-class{gtk:paned}
  @see-function{gtk:paned-position}")

;;; --- paned-resize-end-child -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "resize-end-child" 'paned) t)
 "The @code{resize-end-child} property of type @code{:boolean} (Read / Write)
  @br{}
  Determines whether the second child expands and shrinks along with the paned
  widget. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'paned-resize-end-child)
      "Accessor"
      (documentation 'paned-resize-end-child 'function)
 "@version{#2022-7-31}
  @syntax[]{(gtk:paned-resize-end-child object) => resize}
  @syntax[]{(setf (gtk:paned-resize-end-child) object) resize)}
  @argument[object]{a @class{gtk:paned} widget}
  @argument[resize]{a boolean whether the second child expands and shrinks}
  @begin{short}
    Accessor of the @slot[gtk:paned]{resize-end-child} slot of the
    @class{gtk:paned} class.
  @end{short}
  The @sym{gtk:paned-resize-end-child} function returns whether the end child
  widget can be resized. The @sym{(setf gtk:paned-resize-end-child)} function
  sets the property.
  @see-class{gtk:paned}")

;;; --- paned-resize-start-child -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "resize-start-child" 'paned) t)
 "The @code{resize-start-child} property of type @code{:boolean} (Read / Write)
  @br{}
  Determines whether the first child expands and shrinks along with the paned
  widget. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'paned-resize-start-child)
      "Accessor"
      (documentation 'paned-resize-start-child 'function)
 "@version{#2022-7-31}
  @syntax[]{(gtk:paned-resize-start-child object) => resize}
  @syntax[]{(setf (gtk:paned-resize-start-child) object) resize)}
  @argument[object]{a @class{gtk:paned} widget}
  @argument[resize]{a boolean whether the first child expands and shrinks}
  @begin{short}
    Accessor of the @slot[gtk:paned]{resize-start-child} slot of the
    @class{gtk:paned} class.
  @end{short}
  The @sym{gtk:paned-resize-start-child} function returns whether the first
  child widget can be resized. The @sym{(setf gtk:paned-resize-start-child)}
  function sets the property.
  @see-class{gtk:paned}")

;;; --- paned-shrink-end-child -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "shrink-end-child" 'paned) t)
 "The @code{shrink-end-child} property of type @code{:boolean} (Read / Write)
  @br{}
  Determines whether the second child can be made smaller than its requisition.
  @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'paned-shrink-end-child)
      "Accessor"
      (documentation 'paned-shrink-end-child 'function)
 "@version{#2022-7-31}
  @syntax[]{(gtk:paned-shrink-end-child object) => shrink}
  @syntax[]{(setf (gtk:paned-shrink-end-child) object) shrink)}
  @argument[object]{a @class{gtk:paned} widget}
  @argument[resize]{a boolean whether the second child can be made smaller than
    its requisition}
  @begin{short}
    Accessor of the @slot[gtk:paned]{shrink-end-child} slot of the
    @class{gtk:paned} class.
  @end{short}
  The @sym{gtk:paned-shrink-end-child} function returns whether the second
  child widget can be made smaller than its requisition. The
  @sym{(setf gtk:paned-shrink-end-child)} function sets the property.
  @see-class{gtk:paned}")

;;; --- paned-shrink-start-child -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "shrink-start-child" 'paned) t)
 "The @code{shrink-start-child} property of type @code{:boolean} (Read / Write)
  @br{}
  Determines whether the first child can be made smaller than its requisition.
  @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'paned-shrink-start-child)
      "Accessor"
      (documentation 'paned-shrink-start-child 'function)
 "@version{#2022-7-31}
  @syntax[]{(gtk:paned-shrink-start-child object) => shrink}
  @syntax[]{(setf (gtk:paned-shrink-start-child) object) shrink)}
  @argument[object]{a @class{gtk:paned} widget}
  @argument[resize]{a boolean whether the first child can be made smaller than
    its requisition}
  @begin{short}
    Accessor of the @slot[gtk:paned]{shrink-start-child} slot of the
    @class{gtk:paned} class.
  @end{short}
  The @sym{gtk:paned-shrink-start-child} function returns whether the first
  child widget can be made smaller than its requisition. The
  @sym{(setf gtk:paned-shrink-start-child)} function sets the property.
  @see-class{gtk:paned}")

;;; --- paned-start-child ------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "start-child" 'paned) t)
 "The @code{start-child} property of type @class{gtk:widget} (Read / Write)@br{}
  The first child widget.")

#+liber-documentation
(setf (liber:alias-for-function 'paned-start-child)
      "Accessor"
      (documentation 'paned-start-child 'function)
 "@version{#2022-7-31}
  @syntax[]{(gtk:paned-start-child object) => child}
  @syntax[]{(setf (gtk:paned-start-child) object) child)}
  @argument[object]{a @class{gtk:paned} widget}
  @argument[child]{a @class{gtk:widget} first child widget}
  @begin{short}
    Accessor of the @slot[gtk:paned]{start-child} slot of the @class{gtk:paned}
    class.
  @end{short}
  The @sym{gtk:paned-start-child} function retrieves the start child widget of
  the given paned. The @sym{(setf gtk:paned-start-child)} function sets the
  start child widget.
  @see-class{gtk:paned}
  @see-class{gtk:widget}")

;;; --- paned-wide-handle --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "wide-handle" 'paned) t)
 "The @code{wide-handle} property of type @code{:boolean} (Read / Write) @br{}
  Setting this property to @em{true} indicates that the paned widget needs to
  provide stronger visual separation, e.g. because it separates between two
  notebooks, whose tab rows would otherwise merge visually. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'paned-wide-handle)
      "Accessor"
      (documentation 'paned-wide-handle 'function)
 "@version{#2022-7-31}
  @syntax[]{(gtk:paned-wide-handle object) => wide}
  @syntax[]{(setf (gtk:paned-wide-handle object) wide)}
  @argument[object]{a @class{gtk:paned} widget}
  @argument[wide]{a boolean with the value of the @slot[gtk:paned]{wide-handle}
    property}
  @begin{short}
    Accessor of the @slot[gtk:paned]{wide-handle} slot of the @class{gtk:paned}
    class.
  @end{short}
  The @sym{gtk:paned-wide-handle} function gets the
  @slot[gtk:paned]{wide-handle} property. The @sym{(setf gtk:paned-wide-handle)}
  function sets the property.
  @see-class{gtk:paned}")

;;; ----------------------------------------------------------------------------
;;; gtk_paned_new
;;; ----------------------------------------------------------------------------

(declaim (inline paned-new))

(defun paned-new (orientation)
 #+liber-documentation
 "@version{#2022-7-31}
  @argument[orientation]{a @symbol{gtk:orientation} value for the orientation
    of the paned widget}
  @return{A new @class{gtk:paned} widget.}
  @short{Creates a new paned widget.}
  @see-class{gtk:paned}
  @see-symbol{gtk:orientation}"
  (make-instance 'paned
                 :orientation orientation))

(export 'paned-new)

;;; --- End of file gtk4.paned.lisp --------------------------------------------

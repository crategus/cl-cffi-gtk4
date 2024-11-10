;;; ----------------------------------------------------------------------------
;;; gtk4.header-bar.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.16 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2016 - 2024 Dieter Kaiser
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
;;; GtkHeaderBar
;;;
;;;     A box with a centered child
;;;
;;; Types and Values
;;;
;;;     GtkHeaderBar
;;;
;;; Accessors
;;;
;;;     gtk_header_bar_set_decoration_layout
;;;     gtk_header_bar_get_decoration_layout
;;;     gtk_header_bar_set_show_title_buttons
;;;     gtk_header_bar_get_show_title_buttons
;;;     gtk_header_bar_set_title_widget
;;;     gtk_header_bar_get_title_widget
;;;
;;; Functions
;;;
;;;     gtk_header_bar_new
;;;     gtk_header_bar_pack_start
;;;     gtk_header_bar_pack_end
;;;     gtk_header_bar_remove
;;;
;;; Properties
;;;
;;;     decoration-layout
;;;     show-title-buttons
;;;     title-widget
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkHeaderBar
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkHeaderBar
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkHeaderBar" header-bar
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget")
   :type-initializer "gtk_header_bar_get_type")
  ((decoration-layout
    header-bar-decoration-layout
    "decoration-layout" "gchararray" t t)
   (show-title-buttons
    header-bar-show-title-buttons
    "show-title-buttons" "gboolean" t t)
   (title-widget
    header-bar-title-widget
    "title-widget" "GtkWidget" t t)))

#+liber-documentation
(setf (documentation 'header-bar 'type)
 "@version{2024-4-16}
  @begin{short}
    The @class{gtk:header-bar} widget is similar to a horizontal @class{gtk:box}
    widget. It allows children to be placed at the start or the end. In
    addition, it allows a title and subtitle to be displayed.
  @end{short}

  @image[header-bar]{Figure: GtkHeaderBar}

  The title will be centered with respect to the width of the box, even if the
  children at either side take up different amounts of space.

  The @class{gtk:header-bar} widget can add typical window frame controls, such
  as Minimize, Maximize and Close buttons, or the window icon.

  For these reasons, the @class{gtk:header-bar} widget is the natural choice for
  use as the custom titlebar widget of a @class{gtk:window} widget, see the
  @fun{gtk:window-titlebar} function, as it gives features typical of titlebars
  while allowing the addition of child widgets.
  @begin[GtkHeaderBar as GtkBuildable]{dictionary}
    The @class{gtk:header-bar} widget implementation of the
    @class{gtk:buildable} interface supports adding children at the start or
    end sides by specifying @code{\"start\"} or @code{\"end\"} as the
    @code{\"type\"} attribute of a @code{<child>} element, or setting the title
    widget by specifying @code{\"title\"} value.

    By default the @class{gtk:header-bar} widget uses a @class{gtk:label} widget
    displaying the title of the window it is contained in as the title widget,
    equivalent to the following UI definition:
    @begin{pre}
<object class=\"GtkHeaderBar\">
  <property name=\"title-widget\">
    <object class=\"GtkLabel\">
      <property name=\"label\" translatable=\"yes\">Label</property>
      <property name=\"single-line-mode\">True</property>
      <property name=\"ellipsize\">end</property>
      <property name=\"width-chars\">5</property>
      <style>
        <class name=\"title\"/>
      </style>
    </object>
  </property>
</object>
    @end{pre}
  @end{dictionary}
  @begin[CSS nodes]{dictionary}
    @begin{pre}
headerbar
╰── windowhandle
    ╰── box
        ├── box.start
        │   ├── windowcontrols.start
        │   ╰── [other children]
        ├── [Title Widget]
        ╰── box.end
            ├── [other children]
            ╰── windowcontrols.end
    @end{pre}
    The @class{gtk:header-bar} implementation has a CSS node with the name
    @code{headerbar}. It contains a @code{windowhandle} subnode, which contains
    a @code{box} subnode, which contains two @code{box} subnodes at the start
    and end of the header bar, as well as a @code{center} node that represents
    the  title.

    Each of the boxes contains a @code{windowcontrols} subnode, see the
    @class{gtk:window-controls} widget for details, as well as other children.
  @end{dictionary}
  @begin[Accessibility]{dictionary}
    The @class{gtk:header-bar} implementation uses the @code{:group} role
    of the @symbol{gtk:accessible-role} enumeration.
  @end{dictionary}
  @see-constructor{gtk:header-bar-new}
  @see-slot{gtk:header-bar-decoration-layout}
  @see-slot{gtk:header-bar-show-title-buttons}
  @see-slot{gtk:header-bar-title-widget}
  @see-class{gtk:box}
  @see-class{gtk:action-bar}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:header-bar-decoration-layout ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "decoration-layout"
                                               'header-bar) t)
 "The @code{decoration-layout} property of type @code{:string} (Read / Write)
  @br{}
  The decoration layout for buttons. If this property is not set, the
  @slot[gtk:settings]{gtk-decoration-layout} setting is used. See the
  @fun{gtk:header-bar-decoration-layout} function for information about the
  format of this string. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'header-bar-decoration-layout)
      "Accessor"
      (documentation 'header-bar-decoration-layout 'function)
 "@version{2023-8-9}
  @syntax{(gtk:header-bar-decoration-layout object) => layout}
  @syntax{(setf (gtk:header-bar-decoration-layout object) layout)}
  @argument[object]{a @class{gtk:header-bar} widget}
  @argument[layout]{a string with the decoration layout, or @code{nil} to unset
    the layout}
  @begin{short}
    Accessor of the @slot[gtk:header-bar]{decoration-layout} slot of the
    @class{gtk:header-bar} class.
  @end{short}
  The @fun{gtk:header-bar-decoration-layout} function gets the decoration
  layout. The @setf{gtk:header-bar-decoration-layout} function sets the
  decoration layout for the header bar, overriding the
  @slot[gtk:settings]{gtk-decoration-layout} setting.

  There can be valid reasons for overriding the setting, such as a header bar
  design that does not allow for buttons to take room on the right, or only
  offers room for a single Close button. Split header bars are another example
  for overriding the setting.

  The format of the string is button names, separated by commas. A colon
  separates the buttons that should appear on the left from those on the right.
  Recognized button names are @code{\"minimize\"}, @code{\"maximize\"},
  @code{\"close\"}, @code{\"icon\"} for the window icon and @code{\"menu\"} for
  a menu button for the fallback application menu.

  For example, @code{\"menu:minimize,maximize,close\"} specifies a Menu on the
  left, and Minimize, Maximize and Close buttons on the right.
  @see-class{gtk:header-bar}
  @see-function{gtk:settings-gtk-decoration-layout}")

;;; --- gtk:header-bar-show-title-buttons --------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "show-title-buttons"
                                               'header-bar) t)
 "The @code{show-title-buttons} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether to show title buttons like Close, Minimize, Maximize. Which buttons
  are actually shown and where is determined by the
  @slot[gtk:header-bar]{decoration-layout} property, and by the state of the
  window, for example, a Close button will not be shown if the window cannot be
  closed. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'header-bar-show-title-buttons)
      "Accessor"
      (documentation 'header-bar-show-title-buttons 'function)
 "@version{2023-3-9}
  @syntax{(gtk:header-bar-show-title-buttons object) => setting}
  @syntax{(setf (gtk:header-bar-show-title-buttons object) setting)}
  @argument[object]{a @class{gtk:header-bar} widget}
  @argument[setting]{@em{true} if title buttons are shown}
  @begin{short}
    Accessor of the @slot[gtk:header-bar]{show-title-buttons} slot of the
    @class{gtk:header-bar} class.
  @end{short}
  The @fun{gtk:header-bar-show-title-buttons} function returns whether the
  header bar shows the standard window title buttons. The
  @setf{gtk:header-bar-show-title-buttons} function sets whether the header bar
  shows the standard window title buttons including Close, Maximize, and
  Minimize.
  @see-class{gtk:header-bar}")

;;; --- gtk:header-bar-title-widget --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "title-widget" 'header-bar) t)
 "The @code{title-widget} property of type @class{gtk:widget} (Read / Write)
  @br{}
  Title widget to display.")

#+liber-documentation
(setf (liber:alias-for-function 'header-bar-title-widget)
      "Accessor"
      (documentation 'header-bar-title-widget 'function)
 "@version{2023-8-9}
  @syntax{(gtk:header-bar-title-widget object) => title}
  @syntax{(setf (gtk:header-bar-title-widget object) title)}
  @argument[object]{a @class{gtk:header-bar} widget}
  @argument[title]{a @class{gtk:widget} title widget of the header bar}
  @begin{short}
    Accessor of the @slot[gtk:header-bar]{title-widget} slot of the
    @class{gtk:header-bar} class.
  @end{short}
  The @fun{gtk:header-bar-title-widget} function retrieves the title widget of
  the header bar. The @setf{gtk:header-bar-title-widget} function sets the title
  widget.

  When set to @code{nil}, the header bar will display the title of the window
  it is contained in. The title should help a user identify the current view.
  To achieve the same style as the builtin title, use the @code{title} style
  class. You should set the title widget to @code{nil}, for the window title
  label to be visible again.
  @see-class{gtk:header-bar}
  @see-class{gtk:widget}")

;;; ----------------------------------------------------------------------------
;;; gtk_header_bar_new
;;; ----------------------------------------------------------------------------

(declaim (inline header-bar-new))

(defun header-bar-new ()
 #+liber-documentation
 "@version{2024-4-16}
  @return{The new @class{gtk:header-bar} widget.}
  @short{Creates a new header bar.}
  @see-class{gtk:header-bar}"
  (make-instance 'header-bar))

(export 'header-bar-new)

;;; ----------------------------------------------------------------------------
;;; gtk_header_bar_pack_start
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_header_bar_pack_start" header-bar-pack-start) :void
 #+liber-documentation
 "@version{2024-4-16}
  @argument[header]{a @class{gtk:header-bar} widget}
  @argument[child]{a @class{gtk:widget} child widget to be added to the
    header bar}
  @begin{short}
    Adds a child widget to the header bar, packed with reference to the start
    of the header bar.
  @end{short}
  @begin[Examples]{dictionary}
    Code fragment for a header bar with two buttons. The Cancel button is
    placed on the left side and the Done button on the right side of the header
    bar.
    @begin{pre}
(let (...
      (header (make-instance 'gtk:header-bar
                             :show-title-buttons nil))
      (button (make-instance 'gtk:button
                             :label \"_Done\"
                             :use-underline t
                             :sensitive nil))
      (cancel (make-instance 'gtk:button
                             :label \"_Cancel\"
                             :use-underline t))
      ...)
  ...
  (gtk:header-bar-pack-start header cancel)
  (gtk:header-bar-pack-end header button)
  ... )
    @end{pre}
  @end{dictionary}
  @see-class{gtk:header-bar}
  @see-class{gtk:widget}
  @see-function{gtk:header-bar-pack-end}"
  (header (g:object header-bar))
  (child (g:object widget)))

(export 'header-bar-pack-start)

;;; ----------------------------------------------------------------------------
;;; gtk_header_bar_pack_end
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_header_bar_pack_end" header-bar-pack-end) :void
 #+liber-documentation
 "@version{2023-9-30}
  @argument[header]{a @class{gtk:header-bar} widget}
  @argument[child]{a @class{gtk:widget} child widget to be added to the
    header bar}
  @begin{short}
    Adds a child widget to the header bar, packed with reference to the end of
    the header bar.
  @end{short}
  See the @fun{gtk:header-bar-pack-start} function for an example.
  @see-class{gtk:header-bar}
  @see-class{gtk:widget}
  @see-function{gtk:header-bar-pack-start}"
  (header (g:object header-bar))
  (child (g:object widget)))

(export 'header-bar-pack-end)

;;; ----------------------------------------------------------------------------
;;; gtk_header_bar_remove
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_header_bar_remove" header-bar-remove) :void
 #+liber-documentation
 "@version{#2023-8-9}
  @argument[header]{a @class{gtk:header-bar} widget}
  @argument[child]{a @class{gtk:widget} child widget to remove}
  @begin{short}
    Removes a child widget from the header bar, after it has been added with
    the @fun{gtk:header-bar-pack-start}, @fun{gtk:header-bar-pack-end} or
    @fun{gtk:header-bar-title-widget} functions.
  @end{short}
  @see-class{gtk:header-bar}
  @see-class{gtk:widget}
  @see-function{gtk:header-bar-pack-start}
  @see-function{gtk:header-bar-pack-end}
  @see-function{gtk:header-bar-title-widget}"
  (headerbar (g:object header-bar))
  (child (g:object widget)))

(export 'header-bar-remove)

;;; --- End of file gtk4.header-bar.lisp ---------------------------------------

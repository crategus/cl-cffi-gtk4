;;; ----------------------------------------------------------------------------
;;; gtk4.window-controls.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
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
;;; GtkWindowControls
;;;
;;;     A widget displaying window buttons
;;;
;;; Types and Values
;;;
;;;     GtkWindowControls
;;;
;;; Accessors
;;;
;;;     gtk_window_controls_get_side
;;;     gtk_window_controls_set_side
;;;     gtk_window_controls_get_decoration_layout
;;;     gtk_window_controls_set_decoration_layout
;;;     gtk_window_controls_get_empty
;;;
;;; Functions
;;;
;;;     gtk_window_controls_new
;;;
;;; Properties
;;;
;;;     decoration-layout
;;;     empty
;;;     side
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkWindowControls
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkWindowControls
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkWindowControls" window-controls
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget")
   :type-initializer "gtk_window_controls_get_type")
  ((decoration-layout
    window-controls-decoration-layout
    "decoration-layout" "gchararray" t t)
   (empty
    window-controls-empty
    "empty" "gboolean" t nil)
   (side
    window-controls-side
    "side" "GtkPackType" t t)))

#+liber-documentation
(setf (documentation 'window-controls 'type)
 "@version{2023-8-9}
  @begin{short}
    The @sym{gtk:window-controls} widget shows window frame controls, such as
    minimize, maximize and close buttons, and the window icon.
  @end{short}

  @image[window-controls]{Figure: GtkWindowControls}

  The @sym{gtk:window-controls} widget only displays start or end side of the
  controls, see the @slot[gtk:window-controls]{side} property, so it is
  intended to be always used in pair with another @sym{gtk:window-controls}
  widget using the opposite side, for example:
  @begin{pre}
<object class=\"GtkBox\">
  <child>
    <object class=\"GtkWindowControls\">
      <property name=\"side\">start</property>
    </object>
  </child>
  ...
  <child>
    <object class=\"GtkWindowControls\">
      <property name=\"side\">end</property>
    </object>
  </child>
</object>
  @end{pre}
  @begin[CSS nodes]{dictionary}
    @begin{pre}
windowcontrols
├── [image.icon]
├── [button.minimize]
├── [button.maximize]
╰── [button.close]
    @end{pre}
    The @sym{gtk:window-controls} implementation has a CSS node called
    @code{windowcontrols}. It contains subnodes corresponding to each title
    button. Which of the title buttons exist and where they are placed exactly
    depends on the desktop environment and the
    @slot[gtk:window-controls]{decoration-layout} property. When the
    @slot[gtk:window-controls]{empty} property is @em{true}, it gets the
    @code{.empty} style class.
  @end{dictionary}
  @begin[Accessibility]{dictionary}
    The @sym{gtk:window-controls} implementation uses the @code{:group} role.
  @end{dictionary}
  @see-constructor{gtk:window-controls-new}
  @see-slot{gtk:window-controls-decoration-layout}
  @see-slot{gtk:window-controls-empty}
  @see-slot{gtk:window-controls-side}
  @see-class{gtk:header-bar}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- window-contols-decoration-layout ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "decoration-layout"
                                               'window-controls) t)
 "The @code{decoration-layout} property of type @code{:string} (Read / Write)
  @br{}
  The decoration layout for window buttons. If this property is not set, the
  @slot[gtk:settings]{gtk-decoration-layout} setting is used. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'window-controls-decoration-layout)
      "Accessor"
      (documentation 'window-controls-decoration-layout 'function)
 "@version{2023-8-9}
  @syntax[]{(gtk:window-controls-decoration-layout object) => layout}
  @syntax[]{(setf (gtk:window-controls-decoration-layout object) layout)}
  @argument[object]{a @class{gtk:window-controls} widget}
  @argument[layout]{a string with the decoration layout, or @code{nil} to unset
    the decoration layout}
  @begin{short}
    Accessor of the @slot[gtk:window-controls]{decoration-layout} slot of the
    @class{gtk:window-controls} class.
  @end{short}
  The @sym{gtk:window-controls-decoration-layout} function gets the decoration
  layout. The @sym{(setf gtk:window-controls-decoration-layout)} function
  sets the decoration layout for the title buttons, overriding the
  @slot[gtk:settings]{gtk-decoration-layout} setting.

  The format of the string is button names, separated by commas. A colon
  separates the buttons that should appear on the left from those on the right.
  Recognized button names are minimize, maximize, close and icon (the window
  icon).

  For example, \"icon:minimize,maximize,close\" specifies a icon on the left,
  and minimize, maximize and close buttons on the right.

  If the @slot[gtk:window-controls]{side} property is @code{:start},
  @arg{object} will display the part before the colon, otherwise after that.
  @see-class{gtk:window-controls}
  @see-symbol{gtk:pack-type}")

;;; --- window-contols-empty ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "empty" 'window-controls) t)
 "The @code{empty} property of type @code{:boolean} (Read) @br{}
  Whether the widget has any window buttons. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'window-controls-empty)
      "Accessor"
      (documentation 'window-controls-empty 'function)
 "@version{2023-8-9}
  @syntax[]{(gtk:window-controls-empty object) => empty}
  @syntax[]{(setf (gtk:window-controls-empty object) empty)}
  @argument[object]{a @class{gtk:window-controls} widget}
  @argument[empty]{@em{true} if the widget has window buttons, otherwise
    @em{false}}
  @begin{short}
    Accessor of the @slot[gtk:window-controls]{empty} slot of the
    @class{gtk:window-controls} class.
  @end{short}
  The @sym{gtk:window-controls-empty} function gets whether the widget has any
  window buttons.
  @see-class{gtk:window-controls}")

;;; --- window-contols-side ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "side" 'window-controls) t)
 "The @code{empty} property of type @symbol{gtk:pack-type} (Read / Write) @br{}
  Whether the widget shows start or end side of the decoration layout. @br{}
  Default value: @code{:start}")

#+liber-documentation
(setf (liber:alias-for-function 'window-controls-side)
      "Accessor"
      (documentation 'window-controls-side 'function)
 "@version{2023-8-9}
  @syntax[]{(gtk:window-controls-side object) => side}
  @syntax[]{(setf (gtk:window-controls-side object) side)}
  @argument[object]{a @class{gtk:window-controls} widget}
  @argument[side]{a @symbol{gtk:pack-type} value}
  @begin{short}
    Accessor of the @slot[gtk:window-controls]{side} slot of the
    @class{gtk:window-controls} class.
  @end{short}
  The @sym{gtk:window-controls-side} function gets the side. The
  @sym{(setf gtk:window-controls-side)} function sets the side for @arg{object},
  determining which part of decoration layout it uses.
  @see-class{gtk:window-controls}
  @see-symbol{gtk:pack-type}")

;;; ----------------------------------------------------------------------------
;;; gtk_window_controls_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline window-controls-new))

(defun window-controls-new (side)
 #+liber-documentation
 "@version{2023-8-9}
  @argument[side]{a @symbol{gtk:pack-type} value}
  @return{A new @class{gtk:window-controls} widget.}
  @short{Creates a new @class{gtk:window-controls} widget.}
  @see-class{gtk:window-controls}
  @see-symbol{gtk:pack-type}"
  (make-instance 'window-controls
                 :side side))

(export 'window-controls-new)

;;; --- End of file gtk4.window-controls.lisp ----------------------------------

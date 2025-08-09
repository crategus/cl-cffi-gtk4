;;; ----------------------------------------------------------------------------
;;; gtk4.menu-button.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.18 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2019 - 2025 Dieter Kaiser
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
;;; GtkMenuButton
;;;
;;;     A widget that shows a popup when clicked on
;;;
;;; Types and Values
;;;
;;;     GtkArrowType
;;;     GtkMenuButton
;;;
;;; Accessors
;;;
;;;     gtk_menu_button_set_active
;;;     gtk_menu_button_get_active
;;;     gtk_menu_button_set_always_show_arrow
;;;     gtk_menu_button_get_always_show_arrow
;;;     gtk_menu_button_set_child
;;;     gtk_menu_button_get_child
;;;     gtk_menu_button_set_direction
;;;     gtk_menu_button_get_direction
;;;     gtk_menu_button_set_has_frame
;;;     gtk_menu_button_get_has_frame
;;;     gtk_menu_button_set_icon_name
;;;     gtk_menu_button_get_icon_name
;;;     gtk_menu_button_set_label
;;;     gtk_menu_button_get_label
;;;     gtk_menu_button_set_menu_model
;;;     gtk_menu_button_get_menu_model
;;;     gtk_menu_button_set_popover
;;;     gtk_menu_button_get_popover
;;;     gtk_menu_button_set_primary
;;;     gtk_menu_button_get_primary
;;;     gtk_menu_button_get_use_underline
;;;     gtk_menu_button_set_use_underline
;;;
;;; Functions
;;;
;;;     gtk_menu_button_new
;;;     gtk_menu_button_popup
;;;     gtk_menu_button_popdown
;;;
;;;     GtkMenuButtonCreatePopupFunc
;;;     gtk_menu_button_set_create_popup_func
;;;
;;; Properties
;;;
;;;     active                                              Since 4.10
;;;     always-show-arrow                                   Since 4.4
;;;     can-shrink                                          Since 4.12
;;;     child                                               Since 4.6
;;;     direction
;;;     has-frame
;;;     icon-name
;;;     label
;;;     menu-model
;;;     popover
;;;     primary                                             Since 4.4
;;;     use-underline
;;;
;;; Signals
;;;
;;;     activate
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkMenuButton
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkArrowType
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GtkArrowType" arrow-type
  (:export t
   :type-initializer "gtk_arrow_type_get_type")
  (:up 0)
  (:down 1)
  (:left 2)
  (:right 3)
  (:none 4))

#+liber-documentation
(setf (liber:alias-for-symbol 'arrow-type)
      "GEnum"
      (liber:symbol-documentation 'arrow-type)
 "@version{2025-06-29}
  @begin{declaration}
(gobject:define-genum \"GtkArrowType\" arrow-type
  (:export t
   :type-initializer \"gtk_arrow_type_get_type\")
  (:up 0)
  (:down 1)
  (:left 2)
  (:right 3)
  (:none 4))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:up]{Represents an upward pointing arrow.}
      @entry[:down]{Represents a downward pointing arrow.}
      @entry[:left]{Represents a left pointing arrow.}
      @entry[:right]{Represents a right pointing arrow.}
      @entry[:none]{No arrow.}
    @end{simple-table}
  @end{values}
  @begin{short}
    Used to indicate the direction in which an arrow should point in a
    @class{gtk:menu-button} widget.
  @end{short}
  @see-class{gtk:menu-button}")

;;; ----------------------------------------------------------------------------
;;; GtkMenuButton
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkMenuButton" menu-button
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget")
   :type-initializer "gtk_menu_button_get_type")
  (#+gtk-4-10
   (active
    menu-button-active
    "active" "gboolean" t t)
   #+gtk-4-4
   (always-show-arrow
    menu-button-always-show-arrow
    "always-show-arrow" "gboolean" t t)
    #+gtk-4-12
   (can-shrink
    menu-button-can-shrink
    "can-shrink" "gboolean" t t)
   #+gtk-4-6
   (child
    menu-button-child
    "child" "GtkWidget" t t)
   (direction
    menu-button-direction
    "direction" "GtkArrowType" t t)
   (has-frame
    menu-button-has-frame
    "has-frame" "gboolean" t t)
   (icon-name
    menu-button-icon-name
    "icon-name" "gchararray" t t)
   (label
    menu-button-label
    "label" "gchararray" t t)
   (menu-model
    menu-button-menu-model
    "menu-model" "GMenuModel" t t)
   (popover
    menu-button-popover
    "popover" "GtkPopover" t t)
   #+gtk-4-4
   (primary
    menu-button-primary
    "primary" "gboolean" t t)
   (use-underline
    menu-button-use-underline
    "use-underline" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'menu-button 'type)
 "@version{2025-07-12}
  @begin{short}
    The @class{gtk:menu-button} widget is used to display a popup when clicked
    on.
  @end{short}
  This popup can be provided either as a @class{gtk:popover} widget or as an
  abstract @class{g:menu-model} object.

  @image[menu-button]{Figure: GtkMenuButton}

  The @class{gtk:menu-button} widget can show either an icon, set with the
  @slot[gtk:menu-button]{icon-name} property, or a label, set with the
  @slot[gtk:menu-button]{label} property. If neither is explicitly set, a
  @class{gtk:image} widget is automatically created, using an arrow image
  oriented according to the @slot[gtk:menu-button]{direction} property or the
  generic @code{\"open-menu-symbolic\"} icon if the direction is not set.

  The positioning of the popup is determined by the
  @slot[gtk:menu-button]{direction} property of the menu button.

  For menus, the @slot[gtk:widget]{halign} and @slot[gtk:widget]{valign}
  properties of the menu are also taken into account. For example, when the
  direction is @val[gtk:arrow-type]{:down} and the horizontal alignment is
  @val[gtk:align]{:start}, the menu will be positioned below the button, with
  the starting edge, depending on the text direction, of the menu aligned with
  the starting edge of the button. If there is not enough space below the
  button, the menu is popped up above the button instead. If the alignment
  would move part of the menu offscreen, it is \"pushed in\".
  @begin[CSS nodes]{dictionary}
    @begin{pre}
 menubutton
 ╰── button.toggle
     ╰── <content>
          ╰── [arrow]
    @end{pre}
    The @class{gtk:menu-button} implementation has a single CSS node with name
    @code{menubutton} which contains a button node with a @code{.toggle} style
    class.

    If the menu button contains an icon, it will have the @code{.image-button}
    style class, if it contains text, it will have the @code{.text-button} style
    class. If an arrow is visible in addition to an icon, text or a custom
    child widget, it will also have the @code{.arrow-button} style class.

    Inside the toggle button content, there is an arrow node for the indicator,
    which will carry one of the @code{.none}, @code{.up}, @code{.down},
    @code{.left} or @code{.right} style classes to indicate the direction that
    the menu will appear in. The CSS is expected to provide a suitable image
    for each of these cases using the @code{-gtk-icon-source} property.

    Optionally, the @code{menubutton} node can carry the @code{.circular} style
    class to request a round appearance.
  @end{dictionary}
  @begin[Accessibility]{dictionary}
    The @class{gtk:menu-button} implementation uses the
    @val[gtk:accessible-role]{:button} role of the @sym{gtk:accessible-role}
    enumeration.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @begin[menu-button::activate]{signal}
      @begin{pre}
lambda (button)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[button]{The @class{gtk:button} widget that received the signal.}
      @end{simple-table}
      The signal on the @class{gtk:menu-button} widget is an action signal and
      emitting it causes the button to pop up its menu. Since 4.4
    @end{signal}
  @end{dictionary}
  @see-constructor{gtk:menu-button-new}
  @see-slot{gtk:menu-button-active}
  @see-slot{gtk:menu-button-always-show-arrow}
  @see-slot{gtk:menu-button-child}
  @see-slot{gtk:menu-button-direction}
  @see-slot{gtk:menu-button-has-frame}
  @see-slot{gtk:menu-button-icon-name}
  @see-slot{gtk:menu-button-label}
  @see-slot{gtk:menu-button-menu-model}
  @see-slot{gtk:menu-button-popover}
  @see-slot{gtk:menu-button-primary}
  @see-slot{gtk:menu-button-use-underline}
  @see-class{gtk:image}
  @see-class{gtk:popover}
  @see-class{g:menu-model}")

;;; ----------------------------------------------------------------------------
;;; Accessor and Property Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:menu-button-active -------------------------------------------------

#+(and gtk-4-10 liber-documentation)
(setf (documentation (liber:slot-documentation "active" 'menu-button) t)
 "The @code{active} property of type @code{:boolean} (Read / Write) @br{}
  Whether the menu button is active. Since 4.10 @br{}
  Default value: @em{false}")

#+(and gtk-4-10 liber-documentation)
(setf (liber:alias-for-function 'menu-button-active)
      "Accessor"
      (documentation 'menu-button-active 'function)
 "@version{2025-08-05}
  @syntax{(gtk:menu-button-active object) => active}
  @syntax{(setf (gtk:menu-button-active object) active)}
  @argument[object]{a @class{gtk:menu-button} widget}
  @argument[active]{a boolean whether the menu button is active}
  @begin{short}
    The accessor for the @slot[gtk:menu-button]{active} slot of the
    @class{gtk:menu-button} class gets or sets whether the menu button is
    active.
  @end{short}

  Since 4.10
  @see-class{gtk:menu-button}")

;;; --- gtk:menu-button-always-show-arrow --------------------------------------

#+(and gtk-4-4 liber-documentation)
(setf (documentation (liber:slot-documentation "always-show-arrow"
                                               'menu-button) t)
 "The @code{always-show-arrow} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether to show a dropdown arrow even when using an icon or a custom child.
  Since 4.4 @br{}
  Default value: @em{false}")

#+(and gtk-4-4 liber-documentation)
(setf (liber:alias-for-function 'menu-button-always-show-arrow)
      "Accessor"
      (documentation 'menu-button-always-show-arrow 'function)
 "@version{2025-08-05}
  @syntax{(gtk:menu-button-always-show-arrow object) => setting}
  @syntax{(setf (gtk:menu-button-always-show-arrow object) setting)}
  @argument[object]{a @class{gtk:menu-button} widget}
  @argument[setting]{a boolean whether to show a dropdown arrow even when using
    an icon or a custom child}
  @begin{short}
    The accessor for the @slot[gtk:menu-button]{always-show-arrow} slot of the
    @class{gtk:menu-button} class gets or sets whether to show a dropdown arrow
    even when using an icon or a custom child.
  @end{short}

  Since 4.4
  @see-class{gtk:menu-button}")

;;; --- gtk:menu-button-can-shrink ---------------------------------------------

#+(and gtk-4-12 liber-documentation)
(setf (documentation (liber:slot-documentation "can-shrink" 'menu-button) t)
 "The @code{can-shrink} property of type @code{:boolean} (Read / Write) @br{}
  Whether the size of the menu button can be made smaller than the natural size
  of its contents. Since 4.12 @br{}
  Default value: @em{false}")

#+(and gtk-4-12 liber-documentation)
(setf (liber:alias-for-function 'menu-button-can-shrink)
      "Accessor"
      (documentation 'menu-button-can-shrink 'function)
 "@version{2025-08-05}
  @syntax{(gtk:menu-button-can-shrink object) => setting}
  @syntax{(setf (gtk:menu-button-can-shrink object) setting)}
  @argument[object]{a @class{gtk:menu-button} widget}
  @argument[setting]{a boolean whether the menu button can shrink}
  @begin{short}
    The accessor for the @slot[gtk:menu-button]{can-shrink} slot of the
    @class{gtk:menu-button} class gets or sets whether the menu button can be
    smaller than the natural size of its contents.
  @end{short}

  For text menu buttons, setting the @slot[gtk:menu-button]{can-shrink} property
  to @em{true} will ellipsize the label. For icon menu buttons, this function
  has no effect.

  Since 4.12
  @see-class{gtk:menu-button}")

;;; --- gtk:menu-button-child --------------------------------------------------

#+(and gtk-4-6 liber-documentation)
(setf (documentation (liber:slot-documentation "child" 'menu-button) t)
 "The @code{child} property of type @class{gtk:widget} (Read / Write) @br{}
  The child widget. Since 4.6")

#+(and gtk-4-6 liber-documentation)
(setf (liber:alias-for-function 'menu-button-child)
      "Accessor"
      (documentation 'menu-button-child 'function)
 "@version{2025-08-05}
  @syntax{(gtk:menu-button-child object) => child}
  @syntax{(setf (gtk:menu-button-child object) child)}
  @argument[object]{a @class{gtk:menu-button} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @begin{short}
    The accessor for the @slot[gtk:menu-button]{child} slot of the
    @class{gtk:menu-button} class gets or sets the child widget.
  @end{short}

  Setting a child widget resets the @slot[gtk:menu-button]{label} and
  @slot[gtk:menu-button]{icon-name} properties. If the
  @slot[gtk:menu-button]{always-show-arrow} property is set to @em{true} and
  the @slot[gtk:menu-button]{direction} property is not
  @val[gtk:arrow-type]{:none}, a dropdown arrow will be shown next to the child
  widget.

  Since 4.6
  @see-class{gtk:menu-button}
  @see-class{gtk:widget}
  @see-symbol{gtk:arrow-type}
  @see-function{gtk:menu-button-label}
  @see-function{gtk:menu-button-icon-name}
  @see-function{gtk:menu-button-always-show-arrow}
  @see-function{gtk:menu-button-direction}")

;;; --- gtk:menu-button-direction ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "direction" 'menu-button) t)
 "The @code{direction} property of type @sym{gtk:arrow-type} (Read / Write)
  @br{}
  The arrow type representing the direction in which the menu or popover will
  be popped out. @br{}
  Default value: @val[gtk:arrow-type]{:down}")

#+liber-documentation
(setf (liber:alias-for-function 'menu-button-direction)
      "Accessor"
      (documentation 'menu-button-direction 'function)
 "@version{2025-08-05}
  @syntax{(gtk:menu-button-direction object) => direction}
  @syntax{(setf (gtk:menu-button-direction object) direction)}
  @argument[object]{a @class{gtk:menu-button} widget}
  @argument[direction]{a value of the @sym{gtk:arrow-type} enumeration}
  @begin{short}
    The accessor for the @slot[gtk:menu-button]{direction} slot of the
    @class{gtk:menu-button} class gets or sets the direction the popup will be
    pointing at when popped up.
  @end{short}
  The child widget will not be changed to an arrow if it was customized.

  If the popup does not fit in the available space in the given direction, GTK
  will its best to keep it inside the screen and fully visible.

  If you pass the @val[gtk:arrow-type]{:none} value for a direction, the popup
  will behave as if you passed the @val[gtk:arrow-type]{:down} value, although
  you will not see any arrows.
  @see-class{gtk:menu-button}
  @see-symbol{gtk:arrow-type}")

;;; --- gtk:menu-button-has-frame ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "has-frame" 'menu-button) t)
 "The @code{has-frame} property of type @code{:boolean} (Read / Write) @br{}
  Whether the button has a frame. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'menu-button-has-frame)
      "Accessor"
      (documentation 'menu-button-has-frame 'function)
 "@version{2025-08-05}
  @syntax{(gtk:menu-button-has-frame object) => setting}
  @syntax{(setf (gtk:menu-button-has-frame object) setting)}
  @argument[object]{a @class{gtk:menu-button} widget}
  @argument[setting]{a boolean whether the button has a frame}
  @begin{short}
    The accessor for the @slot[gtk:menu-button]{has-frame} slot of the
    @class{gtk:menu-button} class gets or sets whether the button has a frame.
  @end{short}
  @see-class{gtk:menu-button}")

;;; --- gtk:menu-button-icon-name ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "icon-name" 'menu-button) t)
 "The @code{icon-name} property of type @code{:string} (Read / Write) @br{}
  The name of the icon used to automatically populate the button. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'menu-button-icon-name)
      "Accessor"
      (documentation 'menu-button-icon-name 'function)
 "@version{2025-08-05}
  @syntax{(gtk:menu-button-icon-name object) => name}
  @syntax{(setf (gtk:menu-button-icon-name object) name)}
  @argument[object]{a @class{gtk:menu-button} widget}
  @argument[name]{a string for the name of the icon}
  @begin{short}
    The accessor for the @slot[gtk:menu-button]{icon-name} slot of the
    @class{gtk:menu-button} class gets or sets the name of the icon shown in
    the button.
  @end{short}
  @see-class{gtk:menu-button}
  @see-function{gtk:menu-button-label}")

;;; --- gtk:menu-button-label --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "label" 'menu-button) t)
 "The @code{label} property of type @code{:string} (Read / Write) @br{}
  The label for the button. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'menu-button-label)
      "Accessor"
      (documentation 'menu-button-label 'function)
 "@version{2025-08-05}
  @syntax{(gtk:menu-button-label object) => label}
  @syntax{(setf (gtk:menu-button-label object) label)}
  @argument[object]{a @class{gtk:menu-button} widget}
  @argument[label]{a string for the label}
  @begin{short}
    The accessor for the @slot[gtk:menu-button]{label} slot of the
    @class{gtk:menu-button} class gets or sets the label shown in the button.
  @end{short}
  @see-class{gtk:menu-button}
  @see-function{gtk:menu-button-icon-name}")

;;; --- gtk:menu-button-menu-model ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "menu-model" 'menu-button) t)
 "The @code{menu-model} property of type @class{g:menu-model} (Read / Write)
  @br{}
  The menu model from which the popup will be created. See the
  @fun{gtk:menu-button-menu-model} function for the interaction with the
  @slot[gtk:menu-button]{popover} property.")

#+liber-documentation
(setf (liber:alias-for-function 'menu-button-menu-model)
      "Accessor"
      (documentation 'menu-button-menu-model 'function)
 "@version{2025-08-05}
  @syntax{(gtk:menu-button-menu-model object) => model}
  @syntax{(setf (gtk:menu-button-menu-model object) model)}
  @argument[object]{a @class{gtk:menu-button} widget}
  @argument[model]{a @class{g:menu-model} object, or @code{nil} to unset and
    disable the button}
  @begin{short}
    The accessor for the @slot[gtk:menu-button]{menu-model} slot of the
    @class{gtk:menu-button} class gets or sets the menu model used to generate
    the popup.
  @end{short}
  Pass @code{nil} to dissociate any existing menu model and disable the button.

  A @class{gtk:popover} widget will be created from the menu model with the
  @fun{gtk:popover-menu-new-from-model} function. Actions will be connected as
  documented for this function.

  If the @slot[gtk:menu-button]{popover} property is already set, it will be
  dissociated from the menu button, and the property is set to @code{nil}.
  @see-class{gtk:menu-button}
  @see-class{g:menu-model}
  @see-class{gtk:popover}
  @see-function{gtk:popover-menu-new-from-model}")

;;; --- gtk:menu-button-popover ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "popover" 'menu-button) t)
 "The @code{popover} property of type @class{gtk:popover} (Read / Write) @br{}
  The popover that will be popped up when the button is clicked.")

#+liber-documentation
(setf (liber:alias-for-function 'menu-button-popover)
      "Accessor"
      (documentation 'menu-button-popover 'function)
 "@version{2025-08-05}
  @syntax{(gtk:menu-button-popover object) => popover}
  @syntax{(setf (gtk:menu-button-popover object) popover)}
  @argument[object]{a @class{gtk:menu-button} widget}
  @argument[popover]{a @class{gtk:popover} widget, or @code{nil} to unset and
    disable the button}
  @begin{short}
    The accessor for the @slot[gtk:menu-button]{popover} slot of the
    @class{gtk:menu-button} class gets or sets the popover that pops out of the
    button.
  @end{short}
  If the button is not using a popover, this function returns @code{nil}. Pass
  @code{nil} to dissociate any existing popover and disable the button.

  If the @slot[gtk:menu-button]{menu-model} property is set, the menu model is
  dissociated from the menu button, and the property is set to @code{nil}.
  @see-class{gtk:menu-button}
  @see-class{gtk:popover}
  @see-function{gtk:menu-button-menu-model}")

;;; --- gtk:menu-button-primary ------------------------------------------------

#+(and gtk-4-4 liber-documentation)
(setf (documentation (liber:slot-documentation "primary" 'menu-button) t)
 "The @code{primary} property of type @code{:boolean} (Read / Write) @br{}
  Whether the menu button acts as a primary menu. Primary menus can be opened
  using the @kbd{F10} key. Since 4.4 @br{}
  Default value: @em{false}")

#+(and gtk-4-4 liber-documentation)
(setf (liber:alias-for-function 'menu-button-primary)
      "Accessor"
      (documentation 'menu-button-primary 'function)
 "@version{2025-08-05}
  @syntax{(gtk:menu-button-primary object) => setting}
  @syntax{(setf (gtk:menu-button-primary object) setting)}
  @argument[object]{a @class{gtk:menu-button} widget}
  @argument[setting]{a boolean whether the menu button acts as a primary menu}
  @begin{short}
    The accessor for the @slot[gtk:menu-button]{primary} slot of the
    @class{gtk:menu-button} class gets or sets whether the menu button acts as
    a primary menu.
  @end{short}
  Primary menus can be opened with the @kbd{F10} key.

  Since 4.4
  @see-class{gtk:menu-button}")

;;; --- gtk:menu-button-use-underline ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "use-underline" 'menu-button) t)
 "The @code{use-underline} property of type @code{:boolean} (Read / Write) @br{}
  If set, an underline in the text indicates the next character should be used
  for the mnemonic accelerator key. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'menu-button-use-underline)
      "Accessor"
      (documentation 'menu-button-use-underline 'function)
 "@version{2025-08-05}
  @syntax{(gtk:menu-button-use-underline object) => setting}
  @syntax{(setf (gtk:menu-button-use-underline object) setting)}
  @argument[object]{a @class{gtk:menu-button} widget}
  @argument[setting]{a boolean whether underlines in the text indicates
    mnemonics}
  @begin{short}
    The accessor for the @slot[gtk:menu-button]{use-underline} slot of the
    @class{gtk:menu-button} class gets or sets whether an embedded underline in
    the text indicates a mnemonic.
  @end{short}
  @see-class{gtk:menu-button}")

;;; ----------------------------------------------------------------------------
;;; gtk_menu_button_new
;;; ----------------------------------------------------------------------------

(declaim (inline menu-button-new))

(defun menu-button-new ()
 #+liber-documentation
 "@version{2024-04-20}
  @return{The new @class{gtk:menu-button} widget.}
  @begin{short}
    Creates a new menu button with downwards pointing arrow as the only child.
  @end{short}
  You can replace the child widget with another widget should you wish to.
  @see-class{gtk:menu-button}"
  (make-instance 'menu-button))

(export 'menu-button-new)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_button_popup
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_menu_button_popup" menu-button-popup) :void
 #+liber-documentation
 "@version{2024-05-04}
  @argument[button]{a @class{gtk:menu-button} widget}
  @short{Pop up the menu.}
  @see-class{gtk:menu-button}"
  (button (g:object menu-button)))

(export 'menu-button-popup)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_button_popdown
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_menu_button_popdown" menu-button-popdown) :void
 #+liber-documentation
 "@version{2024-05-04}
  @argument[button]{a @class{gtk:menu-button} widget}
  @short{Dismiss the menu.}
  @see-class{gtk:menu-button}"
  (button (g:object menu-button)))

(export 'menu-button-popdown)

;;; ----------------------------------------------------------------------------
;;; GtkMenuButtonCreatePopupFunc
;;; ----------------------------------------------------------------------------

(cffi:defcallback menu-button-create-popup-func :void
    ((button (g:object menu-button))
     (data :pointer))
  (let ((func (glib:get-stable-pointer-value data)))
    (funcall func button)))

#+liber-documentation
(setf (liber:alias-for-symbol 'menu-button-create-popup-func)
      "Callback"
      (liber:symbol-documentation 'menu-button-create-popup-func)
 "@version{2024-11-03}
  @syntax{lambda (button)}
  @argument[button]{a @class{gtk:menu-button} widget}
  @begin{short}
    User-provided callback function to create a popup for @arg{button} on
    demand.
  @end{short}
  This function is called when the popup of the menu button is shown, but none
  has been provided by the @fun{gtk:menu-button-popover} or
  @fun{gtk:menu-button-menu-model} functions.
  @see-class{gtk:menu-button}
  @see-function{gtk:menu-button-popover}
  @see-function{gtk:menu-button-menu-model}")

(export 'menu-button-create-popup-func)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_button_set_create_popup_func
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_menu_button_set_create_popup_func"
               %menu-button-set-create-popup-func) :void
  (button (g:object menu-button))
  (func :pointer)
  (data :pointer)
  (notify :pointer))

(defun menu-button-set-create-popup-func (button func)
 #+liber-documentation
 "@version{2025-07-13}
  @argument[button]{a @class{gtk:menu-button} widget}
  @argument[func]{a @sym{gtk:menu-button-create-popup-func} callback function
    to call when a popup is about to be shown, but none has been provided by
    other means, or @code{nil} to reset to default behavior}
  @begin{short}
    Sets @arg{func} to be called when a popup is about to be shown.
  @end{short}
  The callback function should use one of the @fun{gtk:menu-button-popover} or
  @fun{gtk:menu-button-menu-model} functions to set a popup for the menu button.
  If @arg{func} is non-@code{nil}, the menu button will always be sensitive.

  Using this function will not reset the menu widget attached to the menu
  button. Instead, this can be done manually in the callback function.
  @see-class{gtk:menu-button}
  @see-symbol{gtk:menu-button-create-popup-func}
  @see-function{gtk:menu-button-popover}
  @see-function{gtk:menu-button-menu-model}"
  (if func
      (%menu-button-set-create-popup-func
              button
              (cffi:callback menu-button-create-popup-func)
              (glib:allocate-stable-pointer func)
              (cffi:callback glib:stable-pointer-destroy-notify))
      (%menu-button-set-create-popup-func
              button
              (cffi:null-pointer)
              (cffi:null-pointer)
              (cffi:null-pointer))))

(export 'menu-button-set-create-popup-func)

;;; --- End of file gtk4.menu-button.lisp --------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk.menu-button.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GTK library.
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
;;;     gtk_menu_button_set_popover
;;;     gtk_menu_button_get_popover
;;;     gtk_menu_button_set_menu_model
;;;     gtk_menu_button_get_menu_model
;;;     gtk_menu_button_set_direction
;;;     gtk_menu_button_get_direction
;;;     gtk_menu_button_set_icon_name
;;;     gtk_menu_button_get_icon_name
;;;     gtk_menu_button_set_label
;;;     gtk_menu_button_get_label
;;;     gtk_menu_button_set_has_frame
;;;     gtk_menu_button_get_has_frame
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
;;;     direction
;;;     has-frame
;;;     icon-name
;;;     label
;;;     menu-model
;;;     popover
;;;     use-underline
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
;;; enum GtkArrowType
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkArrowType" arrow-type
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
 "@version{#2021-12-23}
  @begin{short}
    Used to indicate the direction in which an arrow should point in a
    @class{gtk:menu-button} widget.
  @end{short}
  @begin{pre}
(define-g-enum \"GtkArrowType\" arrow-type
  (:export t
   :type-initializer \"gtk_arrow_type_get_type\")
  (:up 0)
  (:down 1)
  (:left 2)
  (:right 3)
  (:none 4))
  @end{pre}
  @begin[code]{table}
    @entry[:up]{Represents an upward pointing arrow.}
    @entry[:down]{Represents a downward pointing arrow.}
    @entry[:left]{Represents a left pointing arrow.}
    @entry[:right]{Represents a right pointing arrow.}
    @entry[:none]{No arrow.}
  @end{table}
  @see-class{gtk:menu-button}")

;;; ----------------------------------------------------------------------------
;;; struct GtkMenuButton
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkMenuButton" menu-button
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget")
   :type-initializer "gtk_menu_button_get_type")
  ((direction
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
   (use-underline
    menu-button-use-underline
    "use-underline" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'menu-button 'type)
 "@version{#2021-12-23}
  @begin{short}
    The @sym{gtk:menu-button} widget is used to display a popup when clicked on.
  @end{short}
  This popup can be provided either as a @class{gtk:popover} widget or as an
  abstract @class{g:menu-model} object.

  @image[menu-button]{Figure: GtkMenuButton}

  The @sym{gtk:menu-button} widget can show either an icon, set with the
  @code{icon-name} property, or a label, set with the @code{label} property. if
  neither is explicitly set, a @class{gtk:image} widget is automatically
  created, using an arrow image oriented according to \"direction\" or the
  generic \"open-menu-symbolic\" icon if the direction is not set.

  The positioning of the popup is determined by the @code{direction} property
  of the menu button.

  For menus, the @slot[widget]{halign} and @slot[widget]{valign}
  properties of the menu are also taken into account. For example, when the
  direction is @code{:down} and the horizontal alignment is @code{:start}, the
  menu will be positioned below the button, with the starting edge, depending
  on the text direction, of the menu aligned with the starting edge of the
  button. If there is not enough space below the button, the menu is popped up
  above the button instead. If the alignment would move part of the menu
  offscreen, it is \"pushed in\".
  @begin[CSS nodes]{dictionary}
    The @sym{gtk:menu-button} implementation has a single CSS node with name
    @code{button}. To differentiate it from a plain @class{gtk:button} widget,
    it gets the @code{.popup} style class.
  @end{dictionary}
  @begin[Accessibility]{dictionary}
    The @class{gtk:menu-button} implementation uses the @code{:button} role of
    the @symbol{gtk:accessible-role} enumeration.
  @end{dictionary}
  @see-slot{gtk:menu-button-direction}
  @see-slot{gtk:menu-button-has-frame}
  @see-slot{gtk:menu-button-icon-name}
  @see-slot{gtk:menu-button-label}
  @see-slot{gtk:menu-button-menu-model}
  @see-slot{gtk:menu-button-popover}
  @see-slot{gtk:menu-button-use-underline}
  @see-class{gtk:menu}
  @see-class{gtk:image}
  @see-class{gtk:popover}
  @see-class{g:menu-model}")

;;; ----------------------------------------------------------------------------
;;; Accessor and Property Details
;;; ----------------------------------------------------------------------------

;;; --- menu-button-direction ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "direction" 'menu-button) t)
 "The @code{direction} property of type @symbol{gtk:arrow-type} (Read / Write)
  @br{}
  The arrow type representing the direction in which the menu or popover will
  be popped out. @br{}
  Default value: @code{:down}")

#+liber-documentation
(setf (liber:alias-for-function 'menu-button-direction)
      "Accessor"
      (documentation 'menu-button-direction 'function)
 "@version{#2021-12-23}
  @syntax[]{(gtk:menu-button-direction object) => direction}
  @syntax[]{(setf (gtk:menu-button-direction object) direction)}
  @argument[object]{a @class{gtk:menu-button} widget}
  @argument[direction]{a value of the @symbol{gtk:arrow-type} enumeration}
  @begin{short}
    Accessor of the @slot[gtk:menu-button]{direction} slot of the
    @class{gtk:menu-button} class.
  @end{short}

  The @sym{gtk:menu-button-direction} function returns the direction the popup
  will be pointing at when popped up. The @sym{(setf gtk:menu-button-direction)}
  function sets the direction in which the popup will be popped up, as well as
  changing the direction of the arrow. The child will not be changed to an arrow
  if it was customized.

  If the popup does not fit in the available space in the given direction, GTK
  will its best to keep it inside the screen and fully visible.

  If you pass the @code{:none} value for a direction, the popup will behave as
  if you passed the @code{:down} value, although you will not see any arrows.
  @see-class{gtk:menu-button}
  @see-symbol{gtk:arrow-type}")

;;; --- menu-button-has-frame ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "has-frame" 'menu-button) t)
 "The @code{has-frame} property of type @code{:boolean} (Read / Write) @br{}
  Whether the button has a frame. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'menu-button-has-frame)
      "Accessor"
      (documentation 'menu-button-has-frame 'function)
 "@version{#2022-5-29}
  @syntax[]{(gtk:menu-button-has-frame object) => setting}
  @syntax[]{(setf (gtk:menu-button-has-frame object) setting)}
  @argument[object]{a @class{gtk:menu-button} widget}
  @argument[setting]{a boolean whether the button has a frame}
  @begin{short}
    Accessor of the @slot[gtk:menu-button]{has-frame} slot of the
    @class{gtk:menu-button} class.
  @end{short}

  The @sym{gtk:menu-button-has-frame} function returns whether the button
  has a frame. The @sym{(setf gtk:menu-button-has-frame)} function sets the
  style of the button.
  @see-class{gtk:menu-button}")

;;; --- menu-button-icon-name ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "icon-name" 'menu-button) t)
 "The @code{icon-name} property of type @code{:string} (Read / Write) @br{}
  The name of the icon used to automatically populate the button. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'menu-button-icon-name)
      "Accessor"
      (documentation 'menu-button-icon-name 'function)
 "@version{#2022-5-29}
  @syntax[]{(gtk:menu-button-icon-name object) => icon-name}
  @syntax[]{(setf (gtk:menu-button-icon-name object) icon-name)}
  @argument[object]{a @class{gtk:menu-button} widget}
  @argument[icon-name]{a string with the name of the icon}
  @begin{short}
    Accessor of the @slot[gtk:menu-button]{icon-name} slot of the
    @class{gtk:menu-button} class.
  @end{short}

  The @sym{gtk:menu-button-icon-name} function gets the name of the icon shown
  in the button. The @sym{(setf gtk:menu-button-icon-name)} function sets the
  name of the icon to show inside the menu button.
  @see-class{gtk:menu-button}
  @see-function{gtk:menu-button-label}")

;;; --- menu-button-label --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "label" 'menu-button) t)
 "The @code{label} property of type @code{:string} (Read / Write) @br{}
  The label for the button. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'menu-button-label)
      "Accessor"
      (documentation 'menu-button-label 'function)
 "@version{#2022-5-29}
  @syntax[]{(gtk:menu-button-label object) => label}
  @syntax[]{(setf (gtk:menu-button-label object) label)}
  @argument[object]{a @class{gtk:menu-button} widget}
  @argument[label]{a string with the label}
  @begin{short}
    Accessor of the @slot[gtk:menu-button]{label} slot of the
    @class{gtk:menu-button} class.
  @end{short}

  The @sym{gtk:menu-button-label} function gets the label shown in the button.
  The @sym{(setf gtk:menu-button-label)} function sets the label to show inside
  the menu button.
  @see-class{gtk:menu-button}
  @see-function{gtk:menu-button-icon-name}")

;;; --- menu-button-menu-model ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "menu-model"
                                               'menu-button) t)
 "The @code{menu-model} property of type @class{g:menu-model} (Read / Write)
  @br{}
  The menu model from which the popup will be created. Depending on the
  @code{use-popover} property, that may be a menu or a popover. See the
  @fun{gtk:menu-button-menu-model} function for the interaction with the
  @code{popup} property.")

#+liber-documentation
(setf (liber:alias-for-function 'menu-button-menu-model)
      "Accessor"
      (documentation 'menu-button-menu-model 'function)
 "@version{#2021-12-23}
  @syntax[]{(gtk:menu-button-menu-model object) => model}
  @syntax[]{(setf (gtk:menu-button-menu-model object) model)}
  @argument[object]{a @class{gtk:menu-button} widget}
  @argument[model]{a @class{g:menu-model} object, or @code{nil} to unset and
    disable the button}
  @begin{short}
    Accessor of the @slot[gtk:menu-button]{menu-model} slot of the
    @class{gtk:menu-button} class.
  @end{short}

  The @sym{gtk:menu-button-menu-model} function returns the menu model used to
  generate the popup. The @sym{(setf gtk:menu-button-menu-model)} function sets
  the menu model from which the popup will be constructed, or @code{nil} to
  dissociate any existing menu model and disable the button.

  A @class{gtk:popover} widget will be created from the menu model with the
  @fun{gtk:popover-menu-new-from-model} function. Actions will be connected as
  documented for this function.

  If the @slot[gtk:menu-button]{popover} property is already set, it will be
  dissociated from the menu button, and the property is set to @code{nil}.
  @see-class{gtk:menu-button}
  @see-class{g:menu-model}
  @see-class{gtk:popover}
  @see-function{gtk:popover-new-from-model}")

;;; --- menu-button-popover ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "popover" 'menu-button) t)
 "The @code{popover} property of type @class{gtk:popover} (Read / Write) @br{}
  The popover that will be popped up when the button is clicked.")

#+liber-documentation
(setf (liber:alias-for-function 'menu-button-popover)
      "Accessor"
      (documentation 'menu-button-popover 'function)
 "@version{#2021-12-23}
  @syntax[]{(gtk:menu-button-popover object) => popover}
  @syntax[]{(setf (gtk:menu-button-popover object) popover)}
  @argument[object]{a @class{gtk:menu-button} widget}
  @argument[popover]{a @class{gtk:popover} widget, or @code{nil} to unset and
    disable the button}
  @begin{short}
    Accessor of the @slot[gtk:menu-button]{popover} slot of the
    @class{gtk:menu-button} class.
  @end{short}

  The @sym{gtk:menu-button-popover} function returns the popover that pops out
  of the button. If the button is not using a popover, this function returns
  @code{nil}. The @sym{(setf gtk:menu-button-popover)} function sets the popover
  that will be popped up when the menu button is clicked, or @code{nil} to
  dissociate any existing popover and disable the button.

  If the @slot[gtk:menu-button]{menu-model} property is set, the menu model is
  dissociated from the menu button, and the property is set to @code{nil}.
  @see-class{gtk:menu-button}
  @see-function{gtk:menu-button-menu-model}")

;;; --- menu-button-use-underline ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "use-underline"
                                               'menu-button) t)
 "The @code{use-underline} property of type @code{:boolean} (Read / Write) @br{}
  If set, an underline in the text indicates the next character should be used
  for the mnemonic accelerator key.")

#+liber-documentation
(setf (liber:alias-for-function 'menu-button-use-underline)
      "Accessor"
      (documentation 'menu-button-use-underline 'function)
 "@version{#2022-5-29}
  @syntax[]{(gtk:menu-button-use-underline object) => setting}
  @syntax[]{(setf (gtk:menu-button-use-underline object) setting)}
  @argument[object]{a @class{gtk:menu-button} widget}
  @argument[setting]{a boolean whether underlines in the text indicates
    mnemonics}
  @begin{short}
    Accessor of the @slot[gtk:menu-button]{use-underline} slot of the
    @class{gtk:menu-button} class.
  @end{short}

  The @sym{gtk:menu-button-use-underline} function returns whether an embedded
  underline in the text indicates a mnemonic. The
  @sym{(setf gtk:menu-button-use-underline)} function sets the property.
  @see-class{gtk:menu-button}")

;;; ----------------------------------------------------------------------------
;;; gtk_menu_button_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline menu-button-new))

(defun menu-button-new ()
 #+liber-documentation
 "@version{#2021-12-23}
  @return{The new @class{gtk:menu-button} widget.}
  @begin{short}
    Creates a new menu button with downwards pointing arrow as the only child.
  @end{short}
  You can replace the child widget with another widget should you wish to.
  @see-class{gtk:menu-button}"
  (make-instance 'menu-button))

(export 'menu-button-new)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_button_popup ()
;;;
;;; void
;;; gtk_menu_button_popup (GtkMenuButton *menu_button);
;;;
;;; Pop up the menu.
;;;
;;; menu_button :
;;;     a GtkMenuButton
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_menu_button_popdown ()
;;;
;;; void
;;; gtk_menu_button_popdown (GtkMenuButton *menu_button);
;;;
;;; Dismiss the menu.
;;;
;;; menu_button :
;;;     a GtkMenuButton
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkMenuButtonCreatePopupFunc ()
;;;
;;; void
;;; (*GtkMenuButtonCreatePopupFunc) (GtkMenuButton *menu_button,
;;;                                  gpointer user_data);
;;;
;;; User-provided callback function to create a popup for menu_button on demand.
;;; This function is called when the popup of menu_button is shown, but none has
;;; been provided via gtk_menu_button_set_popover() or
;;; gtk_menu_button_set_menu_model().
;;;
;;; menu_button :
;;;     the GtkMenuButton
;;;
;;; user_data :
;;;     User data passed to gtk_menu_button_set_create_popup_func()
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_menu_button_set_create_popup_func ()
;;;
;;; void
;;; gtk_menu_button_set_create_popup_func (GtkMenuButton *menu_button,
;;;                                        GtkMenuButtonCreatePopupFunc func,
;;;                                        gpointer user_data,
;;;                                        GDestroyNotify destroy_notify);
;;;
;;; Sets func to be called when a popup is about to be shown. func should use
;;; one of
;;;
;;; gtk_menu_button_set_popover()
;;; gtk_menu_button_set_menu_model()
;;;
;;; to set a popup for menu_button . If func is non-NULL, menu_button will
;;; always be sensitive.
;;;
;;; Using this function will not reset the menu widget attached to menu_button .
;;; Instead, this can be done manually in func .
;;;
;;; menu_button :
;;;     a GtkMenuButton
;;;
;;; func :
;;;     function to call when a popuop is about to be shown, but none has been
;;;     provided via other means, or NULL to reset to default behavior.
;;;
;;; user_data :
;;;     user data to pass to func .
;;;
;;; destroy_notify :
;;;     destroy notify for user_data .
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.menu-button.lisp ---------------------------------------

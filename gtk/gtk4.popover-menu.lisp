;;; ----------------------------------------------------------------------------
;;; gtk4.popover-menu.lisp
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
;;; GtkPopoverMenu
;;;
;;;     Popovers to use as menus
;;;
;;; Types and Values
;;;
;;;     GtkPopoverMenu
;;;     GtkPopoverMenuFlags
;;;
;;; Accessors
;;;
;;;     gtk_popover_menu_get_flags                          Since 4.14
;;;     gtk_popover_menu_set_flags                          Since 4.14
;;;     gtk_popover_menu_set_menu_model
;;;     gtk_popover_menu_get_menu_model
;;;
;;; Functions
;;;
;;;     gtk_popover_menu_new_from_model
;;;     gtk_popover_menu_new_from_model_full
;;;     gtk_popover_menu_add_child
;;;     gtk_popover_menu_remove_child
;;;
;;; Properties
;;;
;;;     flags                                               Since 4.14
;;;     menu-model
;;;     visible-submenu
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkPopover
;;;                 ╰── GtkPopoverMenu
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;;     GtkNative
;;;     GtkShortcutManager
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkPopoverMenuFlags
;;; ----------------------------------------------------------------------------

(gobject:define-gflags "GtkPopoverMenuFlags" popover-menu-flags
  (:export t
   :type-initializer "gtk_popover_menu_flags_get_type")
  #-gtk-4-14
  (:none 0)
  #+gtk-4-14
  (:sliding 0)
  (:nested #.(ash 1 0)))

#+liber-documentation
(setf (liber:alias-for-symbol 'popover-menu-flags)
      "GFlags"
      (liber:symbol-documentation 'popover-menu-flags)
 "@version{2024-10-26}
  @begin{declaration}
(gobject:define-gflags \"GtkPopoverMenuFlags\" popover-menu-flags
  (:export t
   :type-initializer \"gtk_popover_menu_flags_get_type\")
  (:none 0)
  (:nested #.(ash 1 0)))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:sliding]{Submenus are presented as sliding submenus that replace
        the main menu. Since 4.14}
      @entry[:nested]{Create submenus as nested popovers. Without this flag,
        submenus are created as sliding pages that replace the main menu.}
    @end{table}
  @end{values}
  @begin{short}
    Flags that affect how popover menus built from a @class{g:menu-model}
    object are created and displayed.
  @end{short}
  @see-class{gtk:popover-menu}
  @see-class{g:menu-model}")

;;; ----------------------------------------------------------------------------
;;; GtkPopoverMenu
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkPopoverMenu" popover-menu
  (:superclass popover
    :export t
    :interfaces ("GtkAccessible"
                 "GtkBuildable"
                 "GtkConstraintTarget"
                 "GtkNative"
                 "GtkShortcutManager")
    :type-initializer "gtk_popover_menu_get_type")
  (#+gtk-4-14
   (flags
    popover-menu-flags
    "flags" "GtkPopoverMenuFlags" t t)
   (menu-model
    popover-menu-menu-model
    "menu-model" "GMenuModel" t t)
   (visible-submenu
    popover-menu-visible-submenu
    "visible-submenu" "gchararray" t t)))

#+liber-documentation
(setf (documentation 'popover-menu 'type)
 "@version{2024-10-26}
  @begin{short}
    The @class{gtk:popover-menu} class is a subclass of the @class{gtk:popover}
    class that treats its children like menus and allows switching between them.
  @end{short}
  It can open submenus as traditional, nested submenus, or in a more
  touch-friendly sliding fashion.

  @image[popover-menu]{Figure: GtkPopoverMenu}

  The @class{gtk:popover-menu} widget is meant to be used primarily with menu
  models, using the @fun{gtk:popover-menu-new-from-model} function. If you need
  to put other widgets such as @class{gtk:spin-button} or @class{gtk:switch}
  widgets into a popover, use a plain @class{gtk:popover} widget.

  @subheading{Menu models}
  The XML format understood by the @class{gtk:builder} object for the
  @class{g:menu-model} object consists of a toplevel @code{<menu>} element,
  which contains one or more @code{<item>} elements. Each @code{<item>} element
  contains @code{<attribute>} and @code{<link>} elements with a mandatory name
  @code{attribute}. @code{<link>} elements have the same content model as
  @code{<menu>}. Instead of @code{<link name=\"submenu\">} or
  @code{<link name=\"section\">}, you can use @code{<submenu>\} or
  @code{<section>} elements.
  @begin{pre}
<menu id='app-menu'>
  <section>
    <item>
      <attribute name='label' translatable='yes'>_New Window</attribute>
      <attribute name='action'>app.new</attribute>
    </item>
    <item>
      <attribute name='label' translatable='yes'>_About Sunny</attribute>
      <attribute name='action'>app.about</attribute>
    </item>
    <item>
      <attribute name='label' translatable='yes'>_Quit</attribute>
      <attribute name='action'>app.quit</attribute>
    </item>
  </section>
</menu>
  @end{pre}
  Attribute values can be translated using GNU @code{gettext}, like other
  GtkBuilder content. @code{<attribute>} elements can be marked for translation
  with a @code{translatable=\"yes\"} attribute. It is also possible to specify
  message context and translator comments, using the context and comments
  attributes. To make use of this, the GtkBuilder must have been given the
  GNU @code{gettext} domain to use.

  The following attributes are used when constructing menu items:
  @begin[code]{table}
    @entry[\"label\"]{a user-visible string to display}
    @entry[\"action\"]{the prefixed name of the action to trigger}
    @entry[\"target\"]{the parameter to use when activating the action}
    @entry[\"icon\", \"verb-icon\"]{names of icons that may be displayed}
    @entry[\"submenu-action\"]{name of an action that may be used to determine
      if a submenu can be opened}
    @entry[\"hidden-when\"]{a string used to determine when the item will be
      hidden. Possible values include @code{\"action-disabled\"},
      @code{\"action-missing\"}, @code{\"macos-menubar\"}. This is mainly useful
      for exported menus, see the @fun{gtk:application-menubar} function.}
    @entry[\"custom\"]{a string used to match against the ID of a custom child
      added with the @fun{gtk:popover-menu-add-child},
      @fun{gtk:popover-menu-bar-add-child} functions, or in the UI file with
      @code{<child type=\"ID\">}.}
  @end{table}
  The following attributes are used when constructing sections:
  @begin[code]{table}
    @entry[\"label\"]{a user-visible string to use as section heading}
    @entry[\"display-hint\"]{a string used to determine special formatting for
      the section. Possible values include @code{\"horizontal-buttons\"},
      @code{\"circular-buttons\"} and @code{\"inline-buttons\"}. They all
      indicate that section should be displayed as a horizontal row of buttons.}
    @entry[\"text-direction\"]{a string used to determine the GtkTextDirection
      to use when @code{\"display-hint\"} is set to
      @code{\"horizontal-buttons\"}. Possible values include @code{\"rtl\"},
      @code{\"ltr\"}, and @code{\"none\"}.}
  @end{table}
  The following attributes are used when constructing submenus:
  @begin[code]{table}
    @entry[\"label\"]{a user-visible string to display}
    @entry[\"icon\"]{icon name to display}
  @end{table}
  Menu items will also show accelerators, which are usually associated with
  actions via the @fun{gtk:application-accels-for-action},
  @fun{gtk:widget-class-add-binding-action} or
  @fun{gtk:shortcut-controller-add-shortcut} functions.
  @begin[CSS nodes]{dictionary}
    The @class{gtk:popover-menu} implementation is just a subclass of the
    @class{gtk:popover} class that adds custom content to it, therefore it has
    the same CSS nodes. It is one of the cases that add a @code{.menu} style
    class to the popover's main node.
  @end{dictionary}
  @begin[Accessibility]{dictionary}
    The @class{gtk:popover-menu} implementation uses the @code{:menu} role of
    the @symbol{gtk:accessible-role} enumeration, and its items use the
    @code{:menu-item}, @code{:menu-item-checkbox} role or @code{menu-item-radio}
    roles, depending on the action they are connected to.
  @end{dictionary}
  @see-constructor{gtk:popover-menu-new-from-model}
  @see-constructor{gtk:popover-menu-new-from-model-full}
  @see-slot{gtk:popover-menu-flags}
  @see-slot{gtk:popover-menu-menu-model}
  @see-slot{gtk:popover-menu-visible-submenu}
  @see-class{gtk:popover}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:popover-menu-flags -------------------------------------------------

#+(and gtk-4-14 liber-documentation)
(setf (documentation (liber:slot-documentation "flags" 'popover-menu) t)
 "The @code{flags} property of type @symbol{gtk:popover-menu-flags}
  (Read / Write) @br{}
  The flags that popover uses to create/display a menu from its model. If a
  model is set and the flags change, contents are rebuilt, so if setting
  properties individually, set flags before model to avoid a redundant rebuild.
  Since 4.14 @br{}
  Default value: @code{:sliding}")

#+(and gtk-4-14 liber-documentation)
(setf (liber:alias-for-function 'popover-menu-flags)
      "Accessor"
      (documentation 'popover-menu-flags 'function)
 "@version{2024-5-26}
  @syntax{(gtk:popover-menu-flags object) => flags}
  @syntax{(setf (gtk:popover-menu-flags object) flags)}
  @argument[object]{a @class{gtk:popover-menu} widget}
  @argument[flags]{a @symbol{gtk:popover-menu-flags} value}
  @begin{short}
    Accessor of the @slot[gtk:popover-menu]{flags} slot of the
    @class{gtk:popover-menu} class.
  @end{short}
  The @fun{gtk:popover-menu-flags} function returns the flags that popover uses
  to create/display a menu from its model. The @setf{gtk:popover-menu-flags}
  function sets the flags that popover uses.

  If a model is set and the flags change, contents are rebuilt, so if setting
  properties individually, set flags before model to avoid a redundant rebuild.

  Since 4.14
  @see-class{gtk:popover-menu}
  @see-symbol{gtk:popover-menu-flags}")

;;; --- gtk:popover-menu-menu-model --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "menu-model" 'popover-menu) t)
 "The @code{menu-model} property of type @class{g:menu-model} (Read / Write)
  @br{}
  The model from which the menu is made.")

#+liber-documentation
(setf (liber:alias-for-function 'popover-menu-menu-model)
      "Accessor"
      (documentation 'popover-menu-menu-model 'function)
 "@version{2024-5-26}
  @syntax{(gtk:popover-menu-menu-model object) => model}
  @syntax{(setf (gtk:popover-menu-menu-model object) model)}
  @argument[object]{a @class{gtk:popover-menu} widget}
  @argument[model]{a @class{g:menu-model} object}
  @begin{short}
    Accessor of the @slot[gtk:popover-menu]{menu-model} slot of the
    @class{gtk:popover-menu} class.
  @end{short}
  The @fun{gtk:popover-menu-menu-model} function returns the menu model used to
  populate the popover. The @setf{gtk:popover-menu-menu-model} function sets the
  menu model. The existing contents of the popover are removed, and the popover
  is populated with new contents according to the menu model.
  @see-class{gtk:popover-menu}
  @see-class{g:menu-model}")

;;; --- gtk:popover-menu-visible-submenu ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "visible-submenu"
                                               'popover-menu) t)
 "The @code{visible-submenu} property of type @code{:string} (Read / Write)@br{}
  The name of the visible submenu. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'popover-menu-visible-submenu)
      "Accessor"
      (documentation 'popover-menu-visible-submenu 'function)
 "@version{2024-5-26}
  @syntax{(gtk:popover-menu-visible-submenu object) => submenu}
  @syntax{(setf (gtk:popover-menu-visible-submenu object) submenu)}
  @argument[object]{a @class{gtk:popover-menu} widget}
  @argument[submenu]{a string with the name of the submenu}
  @begin{short}
    Accessor of the @slot[gtk:popover-menu]{visible-submenu} slot of the
    @class{gtk:popover-menu} class.
  @end{short}
  The name of the visible submenu.
  @see-class{gtk:popover-menu}")

;;; ----------------------------------------------------------------------------
;;; gtk_popover_menu_new_from_model
;;; ----------------------------------------------------------------------------

(defun popover-menu-new-from-model (model)
 #+liber-documentation
 "@version{#2024-10-26}
  @argument[model]{a @class{g:menu-model} object, or @code{nil}}
  @return{The new @class{gtk:popover-menu} widget.}
  @begin{short}
    Creates a popover menu and populates it according to the menu model.
  @end{short}
  The created buttons are connected to actions found in the
  @class{gtk:application-window} widget to which the popover belongs - typically
  by means of being attached to a widget that is contained within the widget
  hierarchy of the application window. Actions can also be added using the
  @fun{gtk:widget-insert-action-group} function on the menus attach widget or
  on any of its parent widgets.

  This function creates menus with sliding submenus. See the
  @fun{gtk:popover-menu-new-from-model-full} function for a way to control this.
  @see-class{gtk:popover-menu}
  @see-class{g:menu-model}
  @see-class{gtk:application-window}
  @see-function{gtk:popover-menu-new-from-model-full}
  @see-function{gtk:widget-insert-action-group}"
  (if model
      (make-instance 'popover-menu
                     :menu-model model)
      (make-instance 'popover-menu)))

(export 'popover-menu-new-from-model)

;;; ----------------------------------------------------------------------------
;;; gtk_popover_menu_new_from_model_full
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_popover_menu_new_from_model_full"
               popover-menu-new-from-model-full)
    (g:object popover-menu :return)
 #+liber-documentation
 "@version{#2024-10-26}
  @argument[model]{a @class{g:menu-model} object, or @code{nil}}
  @argument[flags]{a @symbol{gtk:popover-menu-flags} value, that affect hwo the
    menu is created}
  @return{The new @class{gtk:popover-menu} widget.}
  @begin{short}
    Creates a popover menu and populates it according to the menu model.
  @end{short}
  The created buttons are connected to actions found in the action groups that
  are accessible from the parent widget. This includes the
  @class{gtk:application-window} widget to which the popover belongs. Actions
  can also be added using the @fun{gtk:widget-insert-action-group} function on
  the parent widget or on any of its parent widgets.
  @see-class{gtk:popover-menu}
  @see-class{g:menu-model}
  @see-class{gtk:application-window}
  @see-symbol{gtk:popover-menu-flags}
  @see-function{gtk:widget-insert-action-group}"
  (model (g:object g:menu-model))
  (flags popover-menu-flags))

(export 'popover-menu-new-from-model-full)

;;; ----------------------------------------------------------------------------
;;; gtk_popover_menu_add_child
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_popover_menu_add_child" popover-menu-add-child) :boolean
 #+liber-documentation
 "@version{#2024-10-26}
  @argument[popover]{a @class{gtk:popover-menu} widget}
  @argument[child]{a @class{gtk:widget} child widget to add}
  @argument[id]{a string with the ID to insert the child widget at}
  @return{@em{True} if the ID was found and the child widget added.}
  @begin{short}
    Adds a custom widget to a generated menu.
  @end{short}
  For this to work, the menu model of the popover must have an item with a
  custom attribute that matches the ID.
  @see-class{gtk:popover-menu}
  @see-class{gtk:widget}
  @see-function{gtk:popover-menu-remove-child}"
  (popover (g:object popover-menu))
  (child (g:object widget))
  (id :string))

(export 'popover-menu-add-child)

;;; ----------------------------------------------------------------------------
;;; gtk_popover_menu_remove_child
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_popover_menu_remove_child" popover-menu-remove-child)
    :boolean
 #+liber-documentation
 "@version{#2024-10-26}
  @argument[popover]{a @class{gtk:popover-menu} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @return{@em{True} if the child widget was removed.}
  @begin{short}
    Removes a widget that has previously been added with the
    @fun{gtk:popover-menu-add-child} function.
  @end{short}
  @see-class{gtk:popover-menu}
  @see-function{gtk:popover-menu-add-child}"
  (popover (g:object popover-menu))
  (child (g:object widget)))

(export 'popover-menu-remove-child)

;;; --- End of file gtk4.popover-menu.lisp -------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk4.shortcut-controller.lisp
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
;;; GtkShortcutController
;;;
;;;     Event controller for shortcuts
;;;
;;; Types and Values
;;;
;;;     GtkShortcutController
;;;     GtkShortcutScope                         -> gtk.enuerations.lisp
;;;     GtkShortcutManager                       -> gtk.shortcut-manager.lisp
;;;
;;; Accessors
;;;
;;;     gtk_shortcut_controller_set_mnemonics_modifiers
;;;     gtk_shortcut_controller_get_mnemonics_modifiers
;;;     gtk_shortcut_controller_set_scope
;;;     gtk_shortcut_controller_get_scope
;;;
;;; Functions
;;;
;;;     gtk_shortcut_controller_new
;;;     gtk_shortcut_controller_new_for_model
;;;     gtk_shortcut_controller_add_shortcut
;;;     gtk_shortcut_controller_remove_shortcut
;;;
;;; Properties
;;;
;;;     item-type                                          Since 4.8
;;;     mnemonic-modifiers
;;;     model
;;;     n-items                                            Since 4.8
;;;     scope
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkEventController
;;;         ╰── GtkShortcutController
;;;
;;; Implemented Interfaces
;;;
;;;     GListModel
;;;     GtkBuildable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; enum GtkShortcutScope
;;; ----------------------------------------------------------------------------

;; -> gtk4.enumerations.lisp

;;; ----------------------------------------------------------------------------
;;; struct GtkShortcutController
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkShortcutController" shortcut-controller
  (:superclass event-controller
   :export t
   :interfaces ("GtkBuildable"
                "GListModel")
   :type-initializer "gtk_shortcut_controller_get_type")
  (#+gtk-4-8
   (item-type
    shortcut-controller-item-type
    "item-type" "GType" t nil)
   (mnemonic-modifiers
    shortcut-controller-mnemonic-modifiers
    "mnemonic-modifiers" "GdkModifierType" t t)
   (model
    shortcut-controller-model
    "model" "GListModel" nil t)
   #+gtk-4-8
   (n-items
    shortcut-controller-n-items
    "n-items" "guint" t nil)
   (scope
    shortcut-controller-scope
    "scope" "GtkShortcutScope" t t)))

#+liber-documentation
(setf (documentation 'shortcut-controller 'type)
 "@version{2023-7-23}
  @begin{short}
    The @class{gtk:shortcut-controller} class is an event controller that
    manages shortcuts.
  @end{short}
  Most common shortcuts are using this controller implicitly, e.g. by adding a
  mnemonic underline to a @class{gtk:label} widget, or by installing a key
  binding using the @fun{gtk:widget-class-add-binding} function, or by adding
  accelerators to global actions using the
  @fun{gtk:application-accels-for-action} function. But it is possible to
  create your own shortcut controller, and add shortcuts to it.

  The @class{gtk:shortcut-controller} class implements the @class{g:list-model}
  interface for querying the shortcuts that have been added to it.

  @subheading{GtkShortcutController as a GtkBuildable}
  The @class{gtk:shortcut-controller} object can be created in UI files to set up
  shortcuts in the same place as the widgets. An example of a UI definition
  fragment with a @class{gtk:shortcut-controller} object:
  @begin{pre}
<object class='GtkButton'>
  <child>
    <object class='GtkShortcutController'>
      <property name='scope'>managed</property>
      <child>
        <object class='GtkShortcut'>
          <property name='trigger'>&lt;Control&gt;k</property>
          <property name='action'>activate</property>
        </object>
      </child>
    </object>
  </child>
</object>
  @end{pre}
  This example creates a @class{gtk:activate-action} object for triggering the
  activate signal of the @class{gtk:button} widget. See the
  @fun{gtk:shortcut-action-parse-string} function for the syntax for other
  kinds of @class{gtk:shortcut-action} objects. See the
  @fun{gtk:shortcut-trigger-parse-string} function to learn more about the
  syntax for triggers.
  @see-constructor{gtk:shortcut-controller-new}
  @see-constructor{gtk:shortcut-controller-new-for-model}
  @see-slot{gtk:shortcut-controller-item-type}
  @see-slot{gtk:shortcut-controller-mnemonic-modifers}
  @see-slot{gtk:shortcut-controller-model}
  @see-slot{gtk:shortcut-controller-n-items}
  @see-slot{gtk:shortcut-controller-scope}
  @see-class{gtk:event-controller}
  @see-class{gtk:shortcut}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- shortcut-controller-item-type ------------------------------------------

#+(and gtk-4-8 liber-documentation)
(setf (documentation (liber:slot-documentation "item-type"
                                               'shortcut-controller) t)
 "The @code{item-type} property of type @class{g:type-t} (Read) @br{}
  The type of items. Since 4.8")

#+(and gtk-4-8 liber-documentation)
(setf (liber:alias-for-function 'shortcut-controller-item-type)
      "Accessor"
      (documentation 'shortcut-controller-item-type 'function)
 "@version{2023-7-23}
  @syntax[]{(gtk:shortcut-controller-item-type object) => gtype)}
  @argument[object]{a @class{gtk:shortcut-controller} object}
  @argument[gtype]{a @class{g:type-t} item type}
  @begin{short}
    Accessor of the @slot[gtk:shortcut-controller]{item-type} slot of the
    @class{gtk:shortcut-controller} class.
  @end{short}

  Since 4.8
  @see-class{gtk:shortcut-controller}
  @see-class{g:list-model}")

;;; --- shortcut-controller-mnemonic-modifiers ---------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "mnemonic-modifiers"
                                               'shortcut-controller) t)
 "The @code{mnemonic-modifiers} property of type @symbol{gdk:modifier-type}
  (Read / Write) @br{}
  The modifiers that need to be pressed to allow mnemonics activation. @br{}
  Default value: @code{:alt-mask}")

#+liber-documentation
(setf (liber:alias-for-function 'shortcut-controller-mnemonic-modifiers)
      "Accessor"
      (documentation 'shortcut-controller-mnemonic-modifiers 'function)
 "@version{2023-7-23}
  @syntax[]{(gtk:shortcut-controller-mnemonic-modifiers object) => modifiers)}
  @syntax[]{(setf (gtk:shortcut-controller-mnemonic-modifiers object) modifiers)}
  @argument[object]{a @class{gtk:shortcut-controller} object}
  @argument[modifiers]{a @symbol{gdk:modifier-type} value}
  @begin{short}
    Accessor of the @slot[gtk:shortcut-controller]{mnemonic-modifiers} slot of
    the @class{gtk:shortcut-controller} class.
  @end{short}
  The @fun{gtk:shortcut-controller-mnemonic-modifiers} function gets the
  mnemonics modifiers for when this controller activates its shortcuts. The
  @setf{gtk:shortcut-controller-mnemonic-modifiers} function sets the controller
  to have the given @arg{modifiers}.

  The mnemonics modifiers determines which modifiers need to be pressed to
  allow activation of shortcuts with mnemonics triggers. GTK normally uses the
  @kbd{Alt} modifier for mnemonics, except in @class{gtk:popover-menu} widgets,
  where mnemonics can be triggered without any modifiers. It should be very
  rarely necessary to change this, and doing so is likely to interfere with
  other shortcuts.

  This value is only relevant for local shortcut controllers. Global and
  managed shortcut controllers will have their shortcuts activated from other
  places which have their own modifiers for activating mnemonics.
  @see-class{gtk:shortcut-controller}
  @see-symbol{gdk:modifier-type}")

;;; --- shortcut-controller-model ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "model" 'shortcut-controller) t)
 "The @code{model} property of type @class{g:list-model}
  (Write / Construct Only) @br{}
  A list model to take shortcuts from.")

#+liber-documentation
(setf (liber:alias-for-function 'shortcut-controller-model)
      "Accessor"
      (documentation 'shortcut-controller-model 'function)
 "@version{2023-7-23}
  @syntax[]{(gtk:shortcut-controller-model object) => model)}
  @syntax[]{(setf (gtk:shortcut-controller-model object) model)}
  @argument[object]{a @class{gtk:shortcut-controller} object}
  @argument[model]{a @class{g:list-model} object}
  @begin{short}
    Accessor of the @slot[gtk:shortcut-controller]{model} slot of the
    @class{gtk:shortcut-controller} class.
  @end{short}
  A list model to take shortcuts from.
  @see-class{gtk:shortcut-controller}
  @see-class{g:list-model}")

;;; --- shortcut-controller-n-items --------------------------------------------

#+(and gtk-4-8 liber-documentation)
(setf (documentation (liber:slot-documentation "n-items"
                                               'shortcut-controller) t)
 "The @code{n-items} property of type @symbol{:uint} (Read) @br{}
  The number of items in the list model. Since 4.8 @br{}
  Default value: 0")

#+(and gtk-4-8 liber-documentation)
(setf (liber:alias-for-function 'shortcut-controller-n-items)
      "Accessor"
      (documentation 'shortcut-controller-n-items 'function)
 "@version{2023-7-23}
  @syntax[]{(gtk:shortcut-controller-n-items object) => n-items)}
  @argument[object]{a @class{gtk:shortcut-controller} object}
  @argument[n-items]{an unsigned integer with the number of items in the
    list model}
  @begin{short}
    Accessor of the @slot[gtk:shortcut-controller]{n-items} slot of the
    @class{gtk:shortcut-controller} class.
  @end{short}

  Since 4.8
  @see-class{gtk:shortcut-controller}
  @see-class{g:list-model}")

;;; --- shortcut-controller-scope ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "scope" 'shortcut-controller) t)
 "The @code{scope} property of type @symbol{gtk:shortcut-scope} (Read / Write)
  @br{}
  What scope the shortcuts will be handled in. @br{}
  Default value: @code{:local}")

#+liber-documentation
(setf (liber:alias-for-function 'shortcut-controller-scope)
      "Accessor"
      (documentation 'shortcut-controller-scope 'function)
 "@version{2023-7-23}
  @syntax[]{(gtk:shortcut-controller-scope object) => scope)}
  @syntax[]{(setf (gtk:shortcut-controller-scope object) scope)}
  @argument[object]{a @class{gtk:shortcut-controller} object}
  @argument[scope]{a @symbol{gtk:shortcut-scope} value}
  @begin{short}
    Accessor of the @slot[gtk:shortcut-controller]{scope} slot of the
    @class{gtk:shortcut-controller} class.
  @end{short}
  The @fun{gtk:shortcut-controller-scope} function gets the scope for when the
  controller activates its shortcuts. The @setf{gtk:shortcut-controller-scope}
  function sets the controller to have the given @arg{scrope}.

  The scope allows shortcuts to be activated outside of the normal event
  propagation. In particular, it allows installing global keyboard shortcuts
  that can be activated even when a widget does not have focus.

  With the @code{:local} value, shortcuts will only be activated when the
  widget has focus.
  @see-class{gtk:shortcut-controller}
  @see-symbol{gtk:shortcut-scope}")

;;; ----------------------------------------------------------------------------
;;; gtk_shortcut_controller_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline shortcut-controller-new))

(defun shortcut-controller-new ()
 #+liber-documentation
 "@version{2023-7-23}
  @return{A newly created @class{gtk:shortcut-controller} object.}
  @short{Creates a new shortcut controller.}
  @see-class{gtk:shortcut-controller}
  @see-function{gtk:shortcut-controller-new-for-model}"
  (make-instance 'shortcut-controller))

(export 'shortcut-controller-new)

;;; ----------------------------------------------------------------------------
;;; gtk_shortcut_controller_new_for_model ()
;;; ----------------------------------------------------------------------------

(declaim (inline shortcut-controller-new-for-model))

(defun shortcut-controller-new-for-model (model)
  #+liber-documentation
  "@version{#2023-7-23}
  @argument[model]{a @class{g:list-model} object containing shortcuts}
  @return{A newly creaged @class{gtk:shortcut-controller} object.}
  @begin{short}
    Creates a new shortcut controller that takes its shortcuts from the given
    list model.
  @end{short}
  A controller created by this function does not let you add or remove
  individual shortcuts using the shortcut controller API, but you can change
  the contents of the model.
  @see-class{gtk:shortcut-controller}
  @see-class{g:list-model}"
  (make-instance 'shortcut-controller
                 :model model))

(export 'shortcut-controller-new-for-model)

;;; ----------------------------------------------------------------------------
;;; gtk_shortcut_controller_add_shortcut ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_shortcut_controller_add_shortcut"
               shortcut-controller-add-shortcut) :void
 #+liber-documentation
 "@version{#2023-7-23}
  @argument[controller]{a @class{gtk:shortcut-controller} object}
  @argument[shortcut]{a @class{gtk:shortcut} object to add}
  @begin{short}
    Adds a shortcut to the list of shortcuts handled by @arg{controller}.
  @end{short}
  If this controller uses an external shortcut list, this function does nothing.
  @see-class{gtk:shortcut-controller}
  @see-class{gtk:shortcut}
  @see-function{gtk:shortcut-controller-remove-shortcut}"
  (controller (g:object shortcut-controller))
  (shortcut (g:object shortcut)))

(export 'shortcut-controller-add-shortcut)

;;; ----------------------------------------------------------------------------
;;; gtk_shortcut_controller_remove_shortcut ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_shortcut_controller_remove_shortcut"
               shortcut-controller-remove-shortcut) :void
 #+liber-documentation
 "@version{#2023-7-23}
  @argument[controller]{a @class{gtk:shortcut-controller} object}
  @argument[shortcut]{a @class{gtk:shortcut} object to remove}
  @begin{short}
    Removes a shortcut from the list of shortcuts handled by @arg{controller}.
  @end{short}
  If @arg{shortcut} had not been added to @arg{controller} or this controller
  uses an external shortcut list, this function does nothing.
  @see-class{gtk:shortcut-controller}
  @see-class{gtk:shortcut}
  @see-function{gtk:shortcut-controller-add-shortcut}"
  (controller (g:object shortcut-controller))
  (shortcut (g:object shortcut)))

(export 'shortcut-controller-remove-shortcut)

;;; --- End of file gtk4.shortcut-controller.lisp ------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk4.shortcut-controller.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.10 and modified to document the Lisp binding to the GTK library.
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
;;;     GtkShortcutManagerInterface
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

(define-g-enum "GtkShortcutScope" shortcut-scope
  (:export t
   :type-initializer "gtk_shortcut_scope_get_type")
  (:local 0)
  (:managed 1)
  (:global 2))

#+liber-documentation
(setf (liber:alias-for-symbol 'shortcut-scope)
      "GEnum"
      (liber:symbol-documentation 'shortcut-scope)
 "@version{#2022-8-24}
  @begin{short}
    Describes where @class{gtk:shortcut} objects added to a
    @class{gtk:shortcut-controller} object get handled.
  @end{short}
  @begin{pre}
(define-g-enum \"GtkShortcutScope\" shortcut-scope
  (:export t
   :type-initializer \"gtk_shortcut_scope_get_type\")
  (:local 0)
  (:managed 1)
  (:global 2))
  @end{pre}
  @begin[code]{table}
    @entry[:local]{Shortcuts are handled inside the widget the controller
      belongs to.}
    @entry[:managed]{Shortcuts are handled by the first ancestor that is a
      @class{gtk:shortcut-manager} object.}
    @entry[:global]{Shortcuts are handled by the root widget.}
  @end{table}
  @see-class{gtk:shortcut}
  @see-class{gtk:shortcut-controller}
  @see-class{gtk:shortcut-manager}")

;;; ----------------------------------------------------------------------------
;;; struct GtkShortcutController
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkShortcutController" shortcut-controller
  (:superclass event-controller
   :export t
   :interfaces nil
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
 "@version{2023-4-15}
  @begin{short}
    The @sym{gtk:shortcut-controller} class is an event controller that manages
    shortcuts.
  @end{short}

  Most common shortcuts are using this controller implicitly, e.g. by adding a
  mnemonic underline to a @class{gtk:label} widget, or by installing a key
  binding using the @fun{gtk:widget-class-add-binding} function, or by adding
  accelerators to global actions using the
  @fun{gtk:application-accels-for-action} function.

  But it is possible to create your own shortcut controller, and add shortcuts
  to it.

  The @sym{gtk:shortcut-controller} class implements the @class{g:list-model}
  interface for querying the shortcuts that have been added to it.

  @subheading{GtkShortcutController as a GtkBuildable}
  @sym{gtk:shortcut-controller} objects can be creates in UI files to set up
  shortcuts in the same place as the widgets.

  An example of a UI definition fragment with a @sym{gtk:shortcut-controller}
  object:
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
  @fun{gtk:shortcut-action-parse-string} function for the syntax for other kinds
  of @class{gtk:shortcut-action} objects. See the
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
 "@version{#2023-4-15}
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
  Default value: @code{GDK_ALT_MASK}")

#+liber-documentation
(setf (liber:alias-for-function 'shortcut-controller-mnemonic-modifiers)
      "Accessor"
      (documentation 'shortcut-controller-mnemonic-modifiers 'function)
 "@version{#2022-8-24}
  @syntax[]{(gtk:shortcut-controller-mnemonic-modifiers object) => modifers)}
  @syntax[]{(setf (gtk:shortcut-controller-mnemonic-modifiers object) modifies)}
  @argument[object]{a @class{gtk:shortcut-controller} object}
  @argument[modifiers]{a @symbol{gdk:modifier-type} value}
  @begin{short}
    Accessor of the @slot[gtk:shortcut-controller]{mnemonic-modifiers} slot of
    the @class{gtk:shortcut-controller} class.
  @end{short}
  The @sym{gtk:shortcut-controller-mnemonic-modifiers} function gets the
  mnemonics modifiers for when this controller activates its shortcuts. The
  @sym{(setf gtk:shortcut-controller-mnemonic-modifiers)} function sets the
  controller to have the given @arg{modifiers}.

  The mnemonics modifiers determines which modifiers need to be pressed to
  allow activation of shortcuts with mnemonics triggers.

  GTK normally uses the @kbd{Alt} modifier for mnemonics, except in
  @class{gtk:popover-menu} widgets, where mnemonics can be triggered without
  any modifiers. It should be very rarely necessary to change this, and doing
  so is likely to interfere with other shortcuts.

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
 "@version{#2022-8-24}
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
  The number of items. Since 4.8 @br{}
  Default value: 0")

#+(and gtk-4-8 liber-documentation)
(setf (liber:alias-for-function 'shortcut-controller-n-items)
      "Accessor"
      (documentation 'shortcut-controller-n-items 'function)
 "@version{#2023-4-15}
  @syntax[]{(gtk:shortcut-controller-n-items object) => n-items)}
  @argument[object]{a @class{gtk:shortcut-controller} object}
  @argument[n-items]{an unsigned integer with the number of items}
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
 "@version{#2022-8-24}
  @syntax[]{(gtk:shortcut-controller-scope object) => scope)}
  @syntax[]{(setf (gtk:shortcut-controller-scope object) scope)}
  @argument[object]{a @class{gtk:shortcut-controller} object}
  @argument[scope]{a @symbol{gtk:shortcut-scope} value}
  @begin{short}
    Accessor of the @slot[gtk:shortcut-controller]{scope} slot of the
    @class{gtk:shortcut-controller} class.
  @end{short}
  The @sym{gtk:shortcut-controller-scope} function gets the scope for when the
  controller activates its shortcuts. The
  @sym{(setf gtk:shortcut-controller-scope)} function sets the controller to
  have the given @arg{scrope}.

  The scope allows shortcuts to be activated outside of the normal event
  propagation. In particular, it allows installing global keyboard shortcuts
  that can be activated even when a widget does not have focus.

  With the @code{:local} value, shortcuts will only be activated when the
  widget has focus.
  @see-class{gtk:shortcut-controller}
  @see-symbol{gtk:shortcut-scope}")

;;; ----------------------------------------------------------------------------
;;; gtk_shortcut_controller_new ()
;;;
;;; GtkEventController *
;;; gtk_shortcut_controller_new (void);
;;;
;;; Creates a new shortcut controller.
;;;
;;; Returns :
;;;     a newly created shortcut controller
;;; ----------------------------------------------------------------------------

(declaim (inline shortcut-controller-new))

(defun shortcut-controller-new ()
  (make-instance 'shortcut-controller))

(export 'shortcut-controller-new)

;;; ----------------------------------------------------------------------------
;;; gtk_shortcut_controller_new_for_model ()
;;;
;;; GtkEventController *
;;; gtk_shortcut_controller_new_for_model (GListModel *model);
;;;
;;; Creates a new shortcut controller that takes its shortcuts from the given
;;; list model.
;;;
;;; A controller created by this function does not let you add or remove
;;; individual shortcuts using the shortcut controller api, but you can change
;;; the contents of the model.
;;;
;;; model :
;;;     a GListModel containing shortcuts
;;;
;;; Returns :
;;;     a newly created shortcut controller
;;; ----------------------------------------------------------------------------

(declaim (inline shortcut-controller-new-for-model))

(defun shortcut-controller-new-for-model (model)
  (make-instance 'shortcut-controller
                 :model model))

(export 'shortcut-controller-new-for-model)

;;; ----------------------------------------------------------------------------
;;; gtk_shortcut_controller_add_shortcut ()
;;;
;;; void
;;; gtk_shortcut_controller_add_shortcut (GtkShortcutController *self,
;;;                                       GtkShortcut *shortcut);
;;;
;;; Adds shortcut to the list of shortcuts handled by self .
;;;
;;; If this controller uses an external shortcut list, this function does
;;; nothing.
;;;
;;; self :
;;;     the controller
;;;
;;; shortcut :
;;;     a GtkShortcut.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_shortcut_controller_add_shortcut"
           shortcut-controller-add-shortcut) :void
  (controller (g:object shortcut-controller))
  (shortcut (g:object shortcut)))

(export 'shortcut-controller-add-shortcut)

;;; ----------------------------------------------------------------------------
;;; gtk_shortcut_controller_remove_shortcut ()
;;;
;;; void
;;; gtk_shortcut_controller_remove_shortcut
;;;                                (GtkShortcutController *self,
;;;                                 GtkShortcut *shortcut);
;;;
;;; Removes shortcut from the list of shortcuts handled by self .
;;;
;;; If shortcut had not been added to controller or this controller uses an
;;; external shortcut list, this function does nothing.
;;;
;;; self :
;;;     the controller
;;;
;;; shortcut :
;;;     a GtkShortcut
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_shortcut_controller_remove_shortcut"
           shortcut-controller-remove-shortcut) :void
  (controller (g:object shortcut-controller))
  (shortcut (g:object shortcut)))

(export 'shortcut-controller-remove-shortcut)

;;; --- End of file gtk4.shortcut-controller.lisp ------------------------------

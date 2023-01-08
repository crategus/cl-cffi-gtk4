;;; ----------------------------------------------------------------------------
;;; gtk.shortcut.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.6 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 Dieter Kaiser
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
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program and the preamble to the Gnu Lesser
;;; General Public License. If not, see <http://www.gnu.org/licenses/>
;;; and <http://opensource.franz.com/preamble.html>.
;;; ----------------------------------------------------------------------------
;;;
;;; GtkShortcut
;;;
;;;     An object describing a keyboard shortcut
;;;
;;; Types and Values
;;;
;;;     GtkShortcut
;;;
;;; Accessors
;;;
;;;     gtk_shortcut_get_action
;;;     gtk_shortcut_set_action
;;;     gtk_shortcut_get_arguments
;;;     gtk_shortcut_set_arguments
;;;     gtk_shortcut_get_trigger
;;;     gtk_shortcut_set_trigger
;;;
;;; Functions
;;;
;;;     gtk_shortcut_new
;;;     gtk_shortcut_new_with_arguments
;;;
;;; Properties
;;;
;;;     action
;;;     arguments
;;;     trigger
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkShortcut
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkShortcut
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkShortcut" shortcut
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gtk_shortcut_get_type")
  ((action
    shortcut-action
    "action" "GtkShortcutAction" t t)
   (arguments
    shortcut-arguments
    "arguments" "GVariant" t t)
   (trigger
    shortcut-trigger
    "trigger" "GtkShortcutTrigger" t t)))

#+liber-documentation
(setf (documentation 'shortcut 'type)
 "@version{#2022-8-22}
  @begin{short}
    The @sym{gtk:shortcut} object is the low level object used for managing
    keyboard shortcuts.
  @end{short}
  It contains a description of how to trigger the shortcut via a
  @class{gtk:shortcut-trigger} object and a way to activate the shortcut on a
  widget via the @class{gtk:shortcut-action} object.

  The actual work is usually done via the @class{gtk:shortcut-controller}
  object, which decides if and when to activate a shortcut. Using that
  controller directly however is rarely necessary as various higher level
  convenience APIs exist on @class{gtk:widget} objects that make it easier to
  use shortcuts in GTK.

  The @sym{gtk:shortcut} class does provide functionality to make it easy for
  users to work with shortcuts, either by providing informational strings for
  display purposes or by allowing shortcuts to be configured.
  @see-slot{gtk:shortcut-action}
  @see-slot{gtk:shortcut-arguments}
  @see-slot{gtk:shortcut-trigger}
  @see-constructor{gtk:shortcut-new}
  @see-constructor{gtk:shortcut-new-with-arguments}
  @see-class{gtk:shortcut-controller}
  @see-class{gtk:shortcut-action}
  @see-class{gtk:shortcut-trigger}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- shortcut-action ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "action" 'shortcut) t)
 "The @code{action} property of type @class{gtk:shortcut-action} (Read / Write)
  @br{}
  The action that gets activated by the shortcut.")

#+liber-documentation
(setf (liber:alias-for-function 'shortcut-action)
      "Accessor"
      (documentation 'shortcut-action 'function)
 "@version{#2022-8-24}
  @syntax[]{(gtk:shortcut-action object) => action)}
  @syntax[]{(setf (gtk:shortcut-action object) action)}
  @argument[object]{a @class{gtk:shortcut} object}
  @argument[action]{a @class{gtk:shortcut-action} object}
  @begin{short}
    Accessor of the @slot[gtk:shortcut]{action} slot of the
    @class{gtk:shortcut} class.
  @end{short}
  The @sym{gtk:shortcut-action} function gets the action that is activated by
  the shortcut. The @sym{(setf gtk:shortcut-action)} function sets the new
  action. If the action is @code{nil}, the nothing action will be used.
  @see-class{gtk:shortcut}
  @see-class{gtk:shortcut-action}")

;;; --- shortcut-arguments -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "arguments" 'shortcut) t)
 "The @code{arguments} property of type @type{glib:variant} (Read / Write) @br{}
  Arguments passed to activation. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'shortcut-arguments)
      "Accessor"
      (documentation 'shortcut-arguments 'function)
 "@version{#2022-8-24}
  @syntax[]{(gtk:shortcut-arguments object) => args)}
  @syntax[]{(setf (gtk:shortcut-arguments object) args)}
  @argument[object]{a @class{gtk:shortcut} object}
  @argument[args]{the @type{glib:variant} arguments to pass when activating the
    shortcut}
  @begin{short}
    Accessor of the @slot[gtk:shortcut]{arguments} slot of the
    @class{gtk:shortcut} class.
  @end{short}
  The @sym{gtk:shortcut-arguments} function gets the arguments that are passed
  when activating the shortcut. The @sym{(setf gtk:shortcut-arguments)} function
  sets the arguments.
  @see-class{gtk:shortcut}
  @see-type{glib:variant}")

;;; --- shortcut-trigger ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "trigger" 'shortcut) t)
 "The @code{trigger} property of type @class{GtkShortcutTrigger} (Read / Write)
  @br{}
  The trigger that triggers the shortcut.")

#+liber-documentation
(setf (liber:alias-for-function 'shortcut-trigger)
      "Accessor"
      (documentation 'shortcut-trigger 'function)
 "@version{#2022-8-24}
  @syntax[]{(gtk:shortcut-trigger object) => trigger)}
  @syntax[]{(setf (gtk:shortcut-trigger object) trigger)}
  @argument[object]{a @class{gtk:shortcut} object}
  @argument[trigger]{a @class{gtk:shortcut-trigger} object}
  @begin{short}
    Accessor of the @slot[gtk:shortcut]{trigger} slot of the
    @class{gtk:shortcut} class.
  @end{short}
  The @sym{gtk:shortcut-trigger} function gets the trigger used to trigger
  the shortcut. The @sym{(setf gtk:shortcut-trigger)} function sets the new
  trigger. If the trigger is @code{nil}, the never trigger will be used.
  @see-class{gtk:shortcut}
  @see-class{gtk:shortcut-trigger}")

;;; ----------------------------------------------------------------------------
;;; gtk_shortcut_new ()
;;;
;;; GtkShortcut *
;;; gtk_shortcut_new (GtkShortcutTrigger *trigger,
;;;                   GtkShortcutAction *action);
;;;
;;; Creates a new GtkShortcut that is triggered by trigger and then activates
;;; action .
;;;
;;; trigger :
;;;     The trigger that will trigger the shortcut.
;;;
;;; action :
;;;     The action that will be activated upon triggering.
;;;
;;; Returns :
;;;     a new GtkShortcut
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_shortcut_new_with_arguments ()
;;;
;;; GtkShortcut *
;;; gtk_shortcut_new_with_arguments (GtkShortcutTrigger *trigger,
;;;                                  GtkShortcutAction *action,
;;;                                  const char *format_string,
;;;                                  ...);
;;;
;;; Creates a new GtkShortcut that is triggered by trigger and then activates
;;; action with arguments given by format_string .
;;;
;;; trigger :
;;;     The trigger that will trigger the shortcut.
;;;
;;; action :
;;;     The action that will be activated upon triggering.
;;;
;;; format_string :
;;;     GVariant format string for arguments or NULL for no arguments.
;;;
;;; ... :
;;;     arguments, as given by format string.
;;;
;;; Returns :
;;;     a new GtkShortcut
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.shortcut.lisp ------------------------------------------

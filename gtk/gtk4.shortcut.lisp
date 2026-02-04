;;; ----------------------------------------------------------------------------
;;; gtk4.shortcut.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.20 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 - 2026 Dieter Kaiser
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

(gobject:define-gobject "GtkShortcut" shortcut
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
 "@version{2025-09-27}
  @begin{short}
    The @class{gtk:shortcut} object is the low level object used for managing
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

  The @class{gtk:shortcut} class does provide functionality to make it easy for
  users to work with shortcuts, either by providing informational strings for
  display purposes or by allowing shortcuts to be configured.
  @see-constructor{gtk:shortcut-new}
  @see-constructor{gtk:shortcut-new-with-arguments}
  @see-slot{gtk:shortcut-action}
  @see-slot{gtk:shortcut-arguments}
  @see-slot{gtk:shortcut-trigger}
  @see-class{gtk:shortcut-controller}
  @see-class{gtk:shortcut-action}
  @see-class{gtk:shortcut-trigger}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:shortcut-action ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "action" 'shortcut) t)
 "The @code{action} property of type @class{gtk:shortcut-action} (Read / Write)
  @br{}
  The action that gets activated by the shortcut.")

#+liber-documentation
(setf (liber:alias-for-function 'shortcut-action)
      "Accessor"
      (documentation 'shortcut-action 'function)
 "@version{2025-09-27}
  @syntax{(gtk:shortcut-action object) => action)}
  @syntax{(setf (gtk:shortcut-action object) action)}
  @argument[object]{a @class{gtk:shortcut} object}
  @argument[action]{a @class{gtk:shortcut-action} object}
  @begin{short}
    The accessor for the @slot[gtk:shortcut]{action} slot of the
    @class{gtk:shortcut} class gets or sets the action that is activated by the
    shortcut.
  @end{short}
  If @arg{action} is @code{nil}, the nothing action will be used.
  @see-class{gtk:shortcut}
  @see-class{gtk:shortcut-action}")

;;; --- gtk:shortcut-arguments -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "arguments" 'shortcut) t)
 "The @code{arguments} property of type @sym{g:variant} (Read / Write) @br{}
  The arguments passed to activation. @br{}
  Default value: @code{cffi:null-pointer}")

#+liber-documentation
(setf (liber:alias-for-function 'shortcut-arguments)
      "Accessor"
      (documentation 'shortcut-arguments 'function)
 "@version{2025-09-27}
  @syntax{(gtk:shortcut-arguments object) => args)}
  @syntax{(setf (gtk:shortcut-arguments object) args)}
  @argument[object]{a @class{gtk:shortcut} object}
  @argument[args]{a @sym{g:variant} parameter to pass when activating the
    shortcut}
  @begin{short}
    The accessor for the @slot[gtk:shortcut]{arguments} slot of the
    @class{gtk:shortcut} class gets or sets the arguments that are passed when
    activating the shortcut.
  @end{short}
  @see-class{gtk:shortcut}
  @see-symbol{g:variant}")

;;; --- gtk:shortcut-trigger ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "trigger" 'shortcut) t)
 "The @code{trigger} property of type @class{gtk:shortcut-trigger}
  (Read / Write) @br{}
  The trigger that triggers the shortcut.")

#+liber-documentation
(setf (liber:alias-for-function 'shortcut-trigger)
      "Accessor"
      (documentation 'shortcut-trigger 'function)
 "@version{2025-09-27}
  @syntax{(gtk:shortcut-trigger object) => trigger)}
  @syntax{(setf (gtk:shortcut-trigger object) trigger)}
  @argument[object]{a @class{gtk:shortcut} object}
  @argument[trigger]{a @class{gtk:shortcut-trigger} object}
  @begin{short}
    The accessor for the @slot[gtk:shortcut]{trigger} slot of the
    @class{gtk:shortcut} class gets or sets the trigger used to trigger the
    shortcut.
  @end{short}
  If @arg{trigger} is @code{nil}, the never trigger will be used.
  @see-class{gtk:shortcut}
  @see-class{gtk:shortcut-trigger}")

;;; ----------------------------------------------------------------------------
;;; gtk_shortcut_new
;;; ----------------------------------------------------------------------------

(declaim (inline shortcut-new))

(defun shortcut-new (trigger action)
 #+liber-documentation
 "@version{2025-09-30}
  @argument[trigger]{a @class{gtk:shortcut-trigger} object that will trigger
    the shortcut}
  @argument[action]{a @class{gtk:shortcut-action} object that will be
    activated upon triggering}
  @return{The new @class{gtk:shortcut} object.}
  @begin{short}
    Creates a new shortcut that is triggered by @arg{trigger} and then
    activates @arg{action}.
  @end{short}
  @see-class{gtk:shortcut}
  @see-class{gtk:shortcut-trigger}
  @see-class{gtk:shortcut-action}"
  (make-instance 'shortcut
                 :trigger trigger
                 :action action))

(export 'shortcut-new)

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

;;; --- End of file gtk4.shortcut.lisp -----------------------------------------

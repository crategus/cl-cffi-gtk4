;;; ----------------------------------------------------------------------------
;;; gtk.lock-button.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2021 - 2022 Dieter Kaiser
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
;;; GtkLockButton
;;;
;;;     A widget to unlock or lock privileged operations
;;;
;;; Types and Values
;;;
;;;     GtkLockButton
;;;
;;; Accessors
;;;
;;;     gtk_lock_button_get_permission
;;;     gtk_lock_button_set_permission
;;;
;;; Functions
;;;
;;;     gtk_lock_button_new
;;;
;;; Properties
;;;
;;;     permission
;;;     text-lock
;;;     text-unlock
;;;     tooltip-lock
;;;     tooltip-not-authorized
;;;     tooltip-unlock
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkButton
;;;                 ╰── GtkLockButton
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;;     GtkActionable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkLockButton
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkLockButton" lock-button
  (:superclass button
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget"
                "GtkActionable")
   :type-initializer "gtk_lock_button_get_type")
  ((permission
    lock-button-permission
    "permission" "GPermission" t t)
   (text-lock
    lock-button-text-lock
    "text-lock" "gchararray" t t)
   (text-unlock
    lock-button-text-unlock
    "text-unlock" "gchararray" t t)
   (tooltip-lock
    lock-button-tooltip-lock
    "tooltip-lock" "gchararray" t t)
   (tooltip-not-authorized
    lock-button-tooltip-not-authorized
    "tooltip-not-authorized" "gchararray" t t)
   (tooltip-unlock
    lock-button-tooltip-unlock
    "tooltip-unlock" "gchararray" t t)))

#+liber-documentation
(setf (documentation 'lock-button 'type)
 "@version{#2021-12-23}
  @begin{short}
    The @sym{gtk:lock-button} widget is a widget that can be used in control
    panels or preference dialogs to allow users to obtain and revoke
    authorizations needed to operate the controls.
  @end{short}

  @image[lock-button]{Figure: GtkLockButton}

  The required authorization is represented by a @class{g-permission} object.
  Concrete implementations of the @class{g-permission} may use @code{PolicyKit}
  or some other authorization framework. To obtain a @code{PolicyKit}-based
  @class{g-permission} object, use the @code{polkit_permission_new()} function.

  If the user is not currently allowed to perform the action, but can obtain
  the permission, the widget looks like this:

  @image[lock-button-locked]{}

  The user can click the button to request the permission. Depending on the
  platform, this may pop up an authentication dialog or ask the user to
  authenticate in some other way. Once the user has obtained the permission,
  the widget changes to this:

  @image[lock-button-unlocked]{}

  The permission can be dropped again by clicking the button. If the user is
  not able to obtain the permission at all, the widget looks like this:

  @image[lock-button-sorry]{}

  If the user has the permission and cannot drop it, the button is hidden.

  The text (and tooltips) that are shown in the various cases can be adjusted
  with the @code{text-lock}, @code{text-unlock}, @code{tooltip-lock},
  @code{tooltip-unlock} and @code{tooltip-not-authorized} properties.
  @see-slot{gtk:lock-button-permission}
  @see-slot{gtk:lock-button-text-lock}
  @see-slot{gtk:lock-button-text-unlock}
  @see-slot{gtk:lock-button-tooltip-lock}
  @see-slot{gtk:lock-button-tooltip-not-authorized}
  @see-slot{gtk:lock-button-tooltip-unlock}
  @see-class{gtk:button}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- lock-button-permission ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "permission"
                                               'lock-button) t)
 "The @code{permission} property of type @class{g-permission} (Read / Write)
  @br{}
  The permission controlling this button.")

#+liber-documentation
(setf (liber:alias-for-function 'lock-button-permission)
      "Accessor"
      (documentation 'lock-button-permission 'function)
 "@version{#2021-12-23}
  @syntax[]{(gtk:lock-button-permission object) => permission}
  @syntax[]{(setf (gtk:lock-button-permission object) permission)}
  @argument[object]{a @class{gtk:lock-button} widget}
  @argument[permission]{a @class{g-permission} object, or @code{nil}}
  @begin{short}
    Accessor of the @slot[gtk:lock-button]{permission} slot of the
    @class{gtk:lock-button} class.
  @end{short}

  The @sym{gtk:lock-button-permission} function obtains the permission that
  controls the lock button. The @sym{gtk:lock-button-permission} function sets
  the permission.
  @see-class{gtk:lock-button}
  @see-class{g-permission}")

;;; --- lock-button-text-lock ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "text-lock"
                                               'lock-button) t)
 "The @code{text-lock} property of type @class{:string}
  (Read / Write / Construct) @br{}
  The text to display when prompting the user to lock. @br{}
  Default value: \"Lock\"")

#+liber-documentation
(setf (liber:alias-for-function 'lock-button-text-lock)
      "Accessor"
      (documentation 'lock-button-text-lock 'function)
 "@version{#2021-12-23}
  @syntax[]{(gtk:lock-button-text-lock object) => text}
  @syntax[]{(setf (gtk:lock-button-text-lock object) text)}
  @argument[object]{a @class{gtk:lock-button} widget}
  @argument[text]{a string with the text to display}
  @begin{short}
    Accessor of the @slot[gtk:lock-button]{text-lock} slot of the
    @class{gtk:lock-button} class.
  @end{short}

  The text to display when prompting the user to lock.
  @see-class{gtk:lock-button}")

;;; --- lock-button-text-unlock --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "text-unlock"
                                               'lock-button) t)
 "The @code{text-unlock} property of type @class{:string}
  (Read / Write / Construct) @br{}
  The text to display when prompting the user to unlock. @br{}
  Default value: \"Unlock\"")

#+liber-documentation
(setf (liber:alias-for-function 'lock-button-text-unlock)
      "Accessor"
      (documentation 'lock-button-text-unlock 'function)
 "@version{#2021-12-23}
  @syntax[]{(gtk:lock-button-text-unlock object) => text}
  @syntax[]{(setf (gtk:lock-button-text-unlock object) text)}
  @argument[object]{a @class{gtk:lock-button} widget}
  @argument[text]{a string with the text to display}
  @begin{short}
    Accessor of the @slot[gtk:lock-button]{text-unlock} slot of the
    @class{gtk:lock-button} class.
  @end{short}

  The text to display when prompting the user to unlock.
  @see-class{gtk:lock-button}")

;;; --- lock-button-tooltip-lock -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "tooltip-lock"
                                               'lock-button) t)
 "The @code{tooltip-lock} property of type @class{:string}
  (Read / Write / Construct) @br{}
  The tooltip to display when prompting the user to lock. @br{}
  Default value: \"Dialog is unlocked.\n Click to prevent further changes\"")

#+liber-documentation
(setf (liber:alias-for-function 'lock-button-tooltip-lock)
      "Accessor"
      (documentation 'lock-button-tooltip-lock 'function)
 "@version{#2021-12-23}
  @syntax[]{(gtk:lock-button-tooltip-lock object) => tooltip}
  @syntax[]{(setf (gtk:lock-button-tooltip-lock object) tooltip)}
  @argument[object]{a @class{gtk:lock-button} widget}
  @argument[tooltip]{a string with the tooltip to display}
  @begin{short}
    Accessor of the @slot[gtk:lock-button]{tooltip-lock} slot of the
    @class{gtk:lock-button} class.
  @end{short}

  The tooltip to display when prompting the user to lock.
  @see-class{gtk:lock-button}")

;;; --- lock-button-tooltip-not-authorized ---------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "tooltip-not-authorized"
                                               'lock-button) t)
 "The @code{tooltip-not-authorized} property of type @class{:string}
  (Read / Write / Construct) @br{}
  The tooltip to display when prompting the user cannot obtain authorization.
  @br{}
  Default value: \"System policy prevents changes.\nContact your system administrator\"")

#+liber-documentation
(setf (liber:alias-for-function 'lock-button-tooltip-not-authorized)
      "Accessor"
      (documentation 'lock-button-tooltip-not-authorized 'function)
 "@version{#2021-12-23}
  @syntax[]{(gtk:lock-button-tooltip-not-authorized object) => tooltip}
  @syntax[]{(setf (gtk:lock-button-tooltip-not-authorized object) tooltip)}
  @argument[object]{a @class{gtk:lock-button} widget}
  @argument[tooltip]{a string with the tooltip to display}
  @begin{short}
    Accessor of the @slot[gtk:lock-button]{tooltip-lock} slot of the
    @class{gtk:lock-button} class.
  @end{short}

  The tooltip to display when prompting the user cannot obtain authorization.
  @see-class{gtk:lock-button}")

;;; --- lock-button-tooltip-unlock -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "tooltip-unlock"
                                               'lock-button) t)
 "The @code{tooltip-unlock} property of type @class{:string}
  (Read / Write / Construct) @br{}
  The tooltip to display when prompting the user to unlock. @br{}
  Default value: \"Dialog is locked.\nClick to make changes\"")

#+liber-documentation
(setf (liber:alias-for-function 'lock-button-tooltip-unlock)
      "Accessor"
      (documentation 'lock-button-tooltip-unlock 'function)
 "@version{#2021-12-23}
  @syntax[]{(gtk:lock-button-tooltip-unlock object) => tooltip}
  @syntax[]{(setf (gtk:lock-button-tooltip-unlock object) tooltip)}
  @argument[object]{a @class{gtk:lock-button} widget}
  @argument[tooltip]{a string with the tooltip to display}
  @begin{short}
    Accessor of the @slot[gtk:lock-button]{tooltip-lock} slot of the
    @class{gtk:lock-button} class.
  @end{short}

  The tooltip to display when prompting the user to unlock.
  @see-class{gtk:lock-button}")

;;; ----------------------------------------------------------------------------
;;;gtk_lock_button_new ()
;;; ----------------------------------------------------------------------------

(defun lock-button-new (permission)
 #+liber-documentation
 "@version{#2021-12-23}
  @argument[permission]{a @class{g-permission} object}
  @return{A new @class{gtk:lock-button} widget.}
  @short{Creates a new lock button which reflects the permission.}
  @see-class{gtk:lock-button}
  @see-class{g-permission}"
  (make-instance 'lock-button
                 :permission permission))

(export 'lock-button-new)

;;; --- End of file gtk.lock-button.lisp ---------------------------------------

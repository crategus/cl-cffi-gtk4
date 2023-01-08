;;; ----------------------------------------------------------------------------
;;; gtk.native-dialog.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GTK library.
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
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program and the preamble to the Gnu Lesser
;;; General Public License.  If not, see <http://www.gnu.org/licenses/>
;;; and <http://opensource.franz.com/preamble.html>.
;;; ----------------------------------------------------------------------------
;;;
;;; GtkNativeDialog
;;;
;;;     Integrate with native dialogs
;;;
;;; Types and Values
;;;
;;;     GtkNativeDialog
;;;
;;; Accessor
;;;
;;;     gtk_native_dialog_set_modal
;;;     gtk_native_dialog_get_modal
;;;     gtk_native_dialog_set_title
;;;     gtk_native_dialog_get_title
;;;     gtk_native_dialog_set_transient_for
;;;     gtk_native_dialog_get_transient_for
;;;     gtk_native_dialog_get_visible
;;;
;;; Functions
;;;
;;;     gtk_native_dialog_show
;;;     gtk_native_dialog_hide
;;;     gtk_native_dialog_destroy
;;;
;;; Properties
;;;
;;;     modal
;;;     title
;;;     transient-for
;;;     visible
;;;
;;; Signals
;;;
;;;     response
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkNativeDialog
;;;         ╰── GtkFileChooserNative
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkNativeDialog
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkNativeDialog" native-dialog
  (:superclass g:object
   :export t
   :interfaces ()
   :type-initializer "gtk_native_dialog_get_type")
  ((modal
    native-dialog-modal
    "modal" "gboolean" t t)
   (title
    native-dialog-title
    "title" "gchararray" t t)
   (transient-for
    native-dialog-transient-for
    "transient-for" "GtkWindow" t t)
   (visible
    native-dialog-visible
    "visible" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'native-dialog 'type)
 "@version{#2022-9-11}
  @begin{short}
    Native dialogs are platform dialogs that do not use the @class{gtk:dialog}
    or @class{gtk:window} classes.
  @end{short}
  They are used in order to integrate better with a platform, by looking the
  same as other native applications and supporting platform specific features.

  The @class{gtk:dialog} functions cannot be used on such objects, but we need
  a similar API in order to drive them. The @sym{gtk:native-dialog} object is
  an API that allows you to do this. It allows you to set various common
  properties on the dialog, as well as show and hide it and get a \"response\"
  signal when the user finished with the dialog.
  @begin[Signal Details]{dictionary}
    @subheading{The \"response\" signal}
      @begin{pre}
lambda (dialog response)    :run-last
      @end{pre}
      Emitted when the user responds to the dialog. When this is called the
      dialog has been hidden. If you call the @fun{gtk:native-dialog-hide}
      function before the user responds to the dialog this signal will not be
      emitted.
      @begin[code]{table}
        @entry[dialog]{The @sym{gtk:native-dialog} object on which the signal
          is emitted.}
        @entry[response]{An integer with the response ID.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk:native-dialog-modal}
  @see-slot{gtk:native-dialog-title}
  @see-slot{gtk:native-dialog-transient-for}
  @see-slot{gtk:native-dialog-visible}
  @see-class{gtk:dialog}
  @see-class{gtk:file-chooser-native}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- native-dialog-modal ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "modal" 'native-dialog) t)
 "The @code{modal} property of type @code{:boolean} (Read / Write) @br{}
  Whether the window should be modal with respect to its transient parent. @br{}
  Default value: @em{false}")


;;; --- native-dialog-title ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "title" 'native-dialog) t)
 "The @code{title} property of type @code{:string} (Read / Write) @br{}
  The title of the dialog window. @br{}
  Default value: @code{nil}")



;;; ----------------------------------------------------------------------------
;;; The “transient-for” property
;;;
;;;  “transient-for”            GtkWindow *
;;;
;;; The transient parent of the dialog, or NULL for none.
;;;
;;; Owner: GtkNativeDialog
;;;
;;; Flags: Read / Write / Construct
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “visible” property
;;;
;;;  “visible”                  gboolean
;;;
;;; Whether the window is currently visible.
;;;
;;; Owner: GtkNativeDialog
;;;
;;; Flags: Read / Write
;;;
;;; Default value: FALSE
;;; ----------------------------------------------------------------------------


;;; ----------------------------------------------------------------------------
;;; gtk_native_dialog_show ()
;;;
;;; void
;;; gtk_native_dialog_show (GtkNativeDialog *self);
;;;
;;; Shows the dialog on the display, allowing the user to interact with it. When
;;; the user accepts the state of the dialog the dialog will be automatically
;;; hidden and the “response” signal will be emitted.
;;;
;;; Multiple calls while the dialog is visible will be ignored.
;;;
;;; self :
;;;     a GtkNativeDialog
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_native_dialog_hide ()
;;;
;;; void
;;; gtk_native_dialog_hide (GtkNativeDialog *self);
;;;
;;; Hides the dialog if it is visilbe, aborting any interaction. Once this is
;;; called the “response” signal will not be emitted until after the next call
;;; to gtk_native_dialog_show().
;;;
;;; If the dialog is not visible this does nothing.
;;;
;;; self :
;;;     a GtkNativeDialog
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_native_dialog_destroy ()
;;;
;;; void
;;; gtk_native_dialog_destroy (GtkNativeDialog *self);
;;;
;;; Destroys a dialog.
;;;
;;; When a dialog is destroyed, it will break any references it holds to other
;;; objects. If it is visible it will be hidden and any underlying window system
;;; resources will be destroyed.
;;;
;;; Note that this does not release any reference to the object (as opposed to
;;; destroying a GtkWindow) because there is no reference from the windowing
;;; system to the GtkNativeDialog.
;;;
;;; self :
;;;     a GtkNativeDialog
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_native_dialog_get_visible ()
;;;
;;; gboolean
;;; gtk_native_dialog_get_visible (GtkNativeDialog *self);
;;;
;;; Determines whether the dialog is visible.
;;;
;;; self :
;;;     a GtkNativeDialog
;;;
;;; Returns :
;;;     TRUE if the dialog is visible
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_native_dialog_set_modal ()
;;;
;;; void
;;; gtk_native_dialog_set_modal (GtkNativeDialog *self,
;;;                              gboolean modal);
;;;
;;; Sets a dialog modal or non-modal. Modal dialogs prevent interaction with
;;; other windows in the same application. To keep modal dialogs on top of main
;;; application windows, use gtk_native_dialog_set_transient_for() to make the
;;; dialog transient for the parent; most window managers will then disallow
;;; lowering the dialog below the parent.
;;;
;;; self :
;;;     a GtkNativeDialog
;;;
;;; modal :
;;;     whether the window is modal
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_native_dialog_get_modal ()
;;;
;;; gboolean
;;; gtk_native_dialog_get_modal (GtkNativeDialog *self);
;;;
;;; Returns whether the dialog is modal. See gtk_native_dialog_set_modal().
;;;
;;; self :
;;;     a GtkNativeDialog
;;;
;;; Returns :
;;;     TRUE if the dialog is set to be modal
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_native_dialog_set_title ()
;;;
;;; void
;;; gtk_native_dialog_set_title (GtkNativeDialog *self,
;;;                              const char *title);
;;;
;;; Sets the title of the GtkNativeDialog.
;;;
;;; self :
;;;     a GtkNativeDialog
;;;
;;; title :
;;;     title of the dialog
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_native_dialog_get_title ()
;;;
;;; const char *
;;; gtk_native_dialog_get_title (GtkNativeDialog *self);
;;;
;;; Gets the title of the GtkNativeDialog.
;;;
;;; self :
;;;     a GtkNativeDialog
;;;
;;; Returns :
;;;     the title of the dialog, or NULL if none has been set explicitly. The
;;;     returned string is owned by the widget and must not be modified or
;;;     freed.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_native_dialog_set_transient_for ()
;;;
;;; void
;;; gtk_native_dialog_set_transient_for (GtkNativeDialog *self,
;;;                                      GtkWindow *parent);
;;;
;;; Dialog windows should be set transient for the main application window they
;;; were spawned from. This allows window managers to e.g. keep the dialog on
;;; top of the main window, or center the dialog over the main window.
;;;
;;; Passing NULL for parent unsets the current transient window.
;;;
;;; self :
;;;     a GtkNativeDialog
;;;
;;; parent :
;;;     parent window, or NULL.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_native_dialog_get_transient_for ()
;;;
;;; GtkWindow *
;;; gtk_native_dialog_get_transient_for (GtkNativeDialog *self);
;;;
;;; Fetches the transient parent for this window. See
;;; gtk_native_dialog_set_transient_for().
;;;
;;; self :
;;;     a GtkNativeDialog
;;;
;;; Returns :
;;;     the transient parent for this window, or NULL if no transient parent
;;;     has been set.
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.native-dialog.lisp -------------------------------------

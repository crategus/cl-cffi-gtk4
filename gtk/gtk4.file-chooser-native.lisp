;;; ----------------------------------------------------------------------------
;;; gtk.file-chooser-native.lisp
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
;;; GtkFileChooserNative
;;;
;;;     A native file chooser dialog, suitable for “File Open” or “File Save”
;;;     commands
;;;
;;; Types and Values
;;;
;;;     GtkFileChooserNative
;;;
;;; Accessors
;;;
;;;     gtk_file_chooser_native_get_accept_label
;;;     gtk_file_chooser_native_set_accept_label
;;;     gtk_file_chooser_native_get_cancel_label
;;;     gtk_file_chooser_native_set_cancel_label
;;;
;;; Functions
;;;
;;;     gtk_file_chooser_native_new
;;;
;;; Properties
;;;
;;;     accept-label
;;;     cancel-label
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkNativeDialog
;;;         ╰── GtkFileChooserNative
;;;
;;; Implemented Interfaces
;;;
;;;     GtkFileChooser
;;;
;;; Description
;;;
;;; GtkFileChooserNative is an abstraction of a dialog box suitable for use with
;;; “File Open” or “File Save as” commands. By default, this just uses a
;;; GtkFileChooserDialog to implement the actual dialog. However, on certain
;;; platforms, such as Windows and macOS, the native platform file chooser is
;;; used instead. When the application is running in a sandboxed environment
;;; without direct filesystem access (such as Flatpak), GtkFileChooserNative may
;;; call the proper APIs (portals) to let the user choose a file and make it
;;; available to the application.
;;;
;;; While the API of GtkFileChooserNative closely mirrors GtkFileChooserDialog,
;;; the main difference is that there is no access to any GtkWindow or GtkWidget
;;; for the dialog. This is required, as there may not be one in the case of a
;;; platform native dialog.
;;;
;;; Showing, hiding and running the dialog is handled by the GtkNativeDialog
;;; functions.
;;;
;;; Typical usage
;;;
;;; In the simplest of cases, you can the following code to use
;;; GtkFileChooserDialog to select a file for opening:
;;;
;;; To use a dialog for saving, you can use this:
;;;
;;; static void
;;; on_response (GtkNativeDialog *native,
;;;              int              response)
;;; {
;;;   if (response == GTK_RESPONSE_ACCEPT)
;;;     {
;;;       GtkFileChooser *chooser = GTK_FILE_CHOOSER (native);
;;;       GFile *file = gtk_file_chooser_get_file (chooser);
;;;
;;;       open_file (file);
;;;
;;;       g_object_unref (file);
;;;     }
;;;
;;;   g_object_unref (native);
;;; }
;;;
;;;   // ...
;;;   GtkFileChooserNative *native;
;;;   GtkFileChooserAction action = GTK_FILE_CHOOSER_ACTION_OPEN;
;;;
;;;   native = gtk_file_chooser_native_new ("Open File",
;;;                                         parent_window,
;;;                                         action,
;;;                                         "_Open",
;;;                                         "_Cancel");
;;;
;;;   g_signal_connect (native, "response", G_CALLBACK (on_response), NULL);
;;;   gtk_native_dialog_show (GTK_NATIVE_DIALOG (native));
;;;
;;; For more information on how to best set up a file dialog, see
;;; GtkFileChooserDialog.
;;;
;;; Response Codes
;;;
;;; GtkFileChooserNative inherits from GtkNativeDialog, which means it will
;;; return GTK_RESPONSE_ACCEPT if the user accepted, and GTK_RESPONSE_CANCEL if
;;; he pressed cancel. It can also return GTK_RESPONSE_DELETE_EVENT if the
;;; window was unexpectedly closed.
;;;
;;; Differences from GtkFileChooserDialog
;;;
;;; There are a few things in the GtkFileChooser API that are not possible to
;;; use with GtkFileChooserNative, as such use would prohibit the use of a
;;; native dialog.
;;;
;;; No operations that change the dialog work while the dialog is visible. Set
;;; all the properties that are required before showing the dialog.
;;;
;;; Win32 details
;;;
;;; On windows the IFileDialog implementation (added in Windows Vista) is used.
;;; It supports many of the features that GtkFileChooserDialog does, but there
;;; are some things it does not handle:
;;;
;;; Any GtkFileFilter added using a mimetype
;;;
;;; If any of these features are used the regular GtkFileChooserDialog will be
;;; used in place of the native one.
;;;
;;; Portal details
;;;
;;; When the org.freedesktop.portal.FileChooser portal is available on the
;;; session bus, it is used to bring up an out-of-process file chooser.
;;; Depending on the kind of session the application is running in, this may or
;;; may not be a GTK file chooser.
;;;
;;; macOS details
;;;
;;; On macOS the NSSavePanel and NSOpenPanel classes are used to provide native
;;; file chooser dialogs. Some features provided by GtkFileChooserDialog are not
;;; supported:
;;;
;;; Shortcut folders.
;;;
;;; See Also
;;; GtkFileChooser, GtkNativeDialog, GtkFileChooserDialog
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkFileChooserNative
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkFileChooserNative" file-chooser-native
  (:superclass native-dialog
   :export t
   :interfaces ("GtkFileChooser")
   :type-initializer "gtk_file_chooser_native_get_type")
  ((accept-label
    file-chooser-native-accept-label
    "accept-label" "gchararray" t t)
   (cancel-label
    file-chooser-native-cancel-label
    "cancel-label" "gchararray" t t)))

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “accept-label” property
;;;
;;;   “accept-label”             char *
;;;
;;; The text used for the label on the accept button in the dialog, or NULL to
;;; use the default text.
;;;
;;; Owner: GtkFileChooserNative
;;;
;;; Flags: Read / Write
;;;
;;; Default value: NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “cancel-label” property
;;;
;;;  “cancel-label”             char *
;;;
;;; The text used for the label on the cancel button in the dialog, or NULL to
;;; use the default text.
;;;
;;; Owner: GtkFileChooserNative
;;;
;;; Flags: Read / Write
;;;
;;; Default value: NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_native_new ()
;;;
;;; GtkFileChooserNative *
;;; gtk_file_chooser_native_new (const char *title,
;;;                              GtkWindow *parent,
;;;                              GtkFileChooserAction action,
;;;                              const char *accept_label,
;;;                              const char *cancel_label);
;;;
;;; Creates a new GtkFileChooserNative.
;;;
;;; title :
;;;     Title of the native, or NULL.
;;;
;;; parent :
;;;     Transient parent of the native, or NULL.
;;;
;;; action :
;;;     Open or save mode for the dialog
;;;
;;; accept_label :
;;;     text to go in the accept button, or NULL for the default.
;;;
;;; cancel_label :
;;;     text to go in the cancel button, or NULL for the default.
;;;
;;; Returns :
;;;     a new GtkFileChooserNative
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;gtk_file_chooser_native_get_accept_label ()
;;;const char *
;;;gtk_file_chooser_native_get_accept_label
;;;                               (GtkFileChooserNative *self);
;;;Retrieves the custom label text for the accept button.

;;;Parameters
;;;self

;;;a GtkFileChooserNative

;;;Returns
;;;The custom label, or NULL for the default. This string is owned by GTK+ and should not be modified or freed.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;gtk_file_chooser_native_set_accept_label ()
;;;void
;;;gtk_file_chooser_native_set_accept_label
;;;                               (GtkFileChooserNative *self,
;;;                                const char *accept_label);
;;;Sets the custom label text for the accept button.

;;;If characters in label are preceded by an underscore, they are underlined. If you need a literal underscore character in a label, use “__” (two underscores). The first underlined character represents a keyboard accelerator called a mnemonic. Pressing Alt and that key activates the button.

;;;Parameters
;;;self

;;;a GtkFileChooserNative

;;;accept_label

;;;custom label or NULL for the default.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;gtk_file_chooser_native_get_cancel_label ()
;;;const char *
;;;gtk_file_chooser_native_get_cancel_label
;;;                               (GtkFileChooserNative *self);
;;;Retrieves the custom label text for the cancel button.

;;;Parameters
;;;self

;;;a GtkFileChooserNative

;;;Returns
;;;The custom label, or NULL for the default. This string is owned by GTK+ and should not be modified or freed.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;gtk_file_chooser_native_set_cancel_label ()
;;;void
;;;gtk_file_chooser_native_set_cancel_label
;;;                               (GtkFileChooserNative *self,
;;;                                const char *cancel_label);
;;;Sets the custom label text for the cancel button.

;;;If characters in label are preceded by an underscore, they are underlined. If you need a literal underscore character in a label, use “__” (two underscores). The first underlined character represents a keyboard accelerator called a mnemonic. Pressing Alt and that key activates the button.

;;;Parameters
;;;self

;;;a GtkFileChooserNative

;;;cancel_label

;;;custom label or NULL for the default.
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.file-chooser-native.lisp -------------------------------

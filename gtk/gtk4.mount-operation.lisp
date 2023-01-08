;;; ----------------------------------------------------------------------------
;;; gtk.mount-operation.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2013 - 2022 Dieter Kaiser
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
;;; Filesystem utilities
;;;
;;;     Functions for working with GIO
;;;
;;; Types and Values
;;;
;;;     GtkMountOperation
;;;
;;; Accessors
;;;
;;;     gtk_mount_operation_is_showing
;;;     gtk_mount_operation_set_parent
;;;     gtk_mount_operation_get_parent
;;;     gtk_mount_operation_set_display
;;;     gtk_mount_operation_get_display
;;;
;;; Functions
;;;
;;;     gtk_mount_operation_new
;;;     gtk_show_uri_full
;;;     gtk_show_uri_full_finish
;;;     gtk_show_uri
;;;
;;; Properties
;;;
;;;     display
;;;     is-showing
;;;     parent
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GMountOperation
;;;         ╰── GtkMountOperation
;;;
;;; Description
;;;
;;; The functions and objects described here make working with GTK and GIO more
;;; convenient.
;;;
;;; GtkMountOperation is needed when mounting volumes: It is an implementation
;;; of GMountOperation that can be used with GIO functions for mounting volumes
;;; such as g_file_mount_enclosing_volume(), g_file_mount_mountable(),
;;; g_volume_mount(), g_mount_unmount_with_operation() and others.
;;;
;;; When necessary, GtkMountOperation shows dialogs to ask for passwords,
;;; questions or show processes blocking unmount.
;;;
;;; gtk_show_uri_on_window() is a convenient way to launch applications for
;;; URIs.
;;;
;;; Another object that is worth mentioning in this context is
;;; GdkAppLaunchContext, which provides visual feedback when launching
;;; applications.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkMountOperation
;;;
;;; This should not be accessed directly. Use the accessor functions below.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; Property Details
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “display” property
;;;
;;;   “display”                  GdkDisplay *
;;;
;;; The display where this window will be displayed.
;;;
;;; Owner: GtkMountOperation
;;;
;;; Flags: Read / Write
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “is-showing” property
;;;
;;;   “is-showing”               gboolean
;;;
;;; Are we showing a dialog.
;;;
;;; Owner: GtkMountOperation
;;;
;;; Flags: Read
;;;
;;; Default value: FALSE
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “parent” property
;;;
;;;   “parent”                   GtkWindow *
;;;
;;; The parent window.
;;;
;;; Owner: GtkMountOperation
;;;
;;; Flags: Read / Write
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_mount_operation_new ()
;;;
;;; GMountOperation *
;;; gtk_mount_operation_new (GtkWindow *parent);
;;;
;;; Creates a new GtkMountOperation
;;;
;;; parent :
;;;     transient parent of the window, or NULL.
;;;
;;; Returns :
;;;     a new GtkMountOperation
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_mount_operation_is_showing ()
;;;
;;; gboolean
;;; gtk_mount_operation_is_showing (GtkMountOperation *op);
;;;
;;; Returns whether the GtkMountOperation is currently displaying a window.
;;;
;;; op :
;;;     a GtkMountOperation
;;;
;;; Returns :
;;;     TRUE if op is currently displaying a window
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_mount_operation_set_parent ()
;;;
;;; void
;;; gtk_mount_operation_set_parent (GtkMountOperation *op,
;;;                                 GtkWindow *parent);
;;;
;;; Sets the transient parent for windows shown by the GtkMountOperation.
;;;
;;; op :
;;;     a GtkMountOperation
;;;
;;; parent :
;;;     transient parent of the window, or NULL.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_mount_operation_get_parent ()
;;;
;;; GtkWindow *
;;; gtk_mount_operation_get_parent (GtkMountOperation *op);
;;;
;;; Gets the transient parent used by the GtkMountOperation
;;;
;;; op :
;;;     a GtkMountOperation
;;;
;;; Returns :
;;;     the transient parent for windows shown by op .
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_mount_operation_set_display ()
;;;
;;; void
;;; gtk_mount_operation_set_display (GtkMountOperation *op,
;;;                                  GdkDisplay *display);
;;;
;;; Sets the display to show windows of the GtkMountOperation on.
;;;
;;; op :
;;;     a GtkMountOperation
;;;
;;; display :
;;;     a GdkDisplay
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_mount_operation_get_display ()
;;;
;;; GdkDisplay *
;;; gtk_mount_operation_get_display (GtkMountOperation *op);
;;;
;;; Gets the display on which windows of the GtkMountOperation will be shown.
;;;
;;; op :
;;;     a GtkMountOperation
;;;
;;; Returns :
;;;     the display on which windows of op are shown.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_show_uri_full ()
;;;
;;; void
;;; gtk_show_uri_full (GtkWindow *parent,
;;;                    const char *uri,
;;;                    guint32 timestamp,
;;;                    GCancellable *cancellable,
;;;                    GAsyncReadyCallback callback,
;;;                    gpointer user_data);
;;;
;;; This function launches the default application for showing a given uri.
;;;
;;; The callback will be called when the launch is completed. It should call
;;; gtk_show_uri_full_finish() to obtain the result.
;;;
;;; This is the recommended call to be used as it passes information necessary
;;; for sandbox helpers to parent their dialogs properly.
;;;
;;; parent :
;;;     parent window.
;;;
;;; uri :
;;;     the uri to show
;;;
;;; timestamp :
;;;     timestamp from the event that triggered this call, or GDK_CURRENT_TIME
;;;
;;; cancellable :
;;;     a GCancellable to cancel the launch.
;;;
;;; callback :
;;;     a callback to call when the action is complete.
;;;
;;; user_data :
;;;     data to pass to callback .
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_show_uri_full_finish ()
;;;
;;; gboolean
;;; gtk_show_uri_full_finish (GtkWindow *parent,
;;;                           GAsyncResult *result,
;;;                           GError **error);
;;;
;;; Finished the gtk_show_uri() call and returns the result of the operation.
;;;
;;; parent ;
;;;     the GtkWindow passed to gtk_show_uri()
;;;
;;; result :
;;;     GAsyncResult that was passed to callback
;;;
;;; error :
;;;     return location for an error
;;;
;;; Returns :
;;;     TRUE if the URI was shown successfully. Otherwise, FALSE is returned
;;;     and error is set
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_show_uri
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_show_uri" show-uri) :void
 #+liber-documentation
 "@version{#2022-1-20}
  @argument[parent]{a @class{gtk:window} widget}
  @argument[uri]{a string with the URI to show}
  @argument[timestamp]{an unsigned integer with the timestamp from the event
    that triggered this call, or the @variable{+gdk-current-time+} value}
  @begin{short}
    This function launches the default application for showing a given URI, or
    shows an error dialog if that fails.
  @end{short}
  @see-class{gtk:window}"
  (parent (g:object window))
  (uri :string)
  (timestamp :uint32))

(export 'show-uri)

;;; --- End of file gtk.mount-operation.lisp -----------------------------------

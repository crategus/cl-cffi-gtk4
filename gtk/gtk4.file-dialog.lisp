;;; ----------------------------------------------------------------------------
;;; gtk4.file-dialog.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.10 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2023 Dieter Kaiser
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
;;; GtkFileDialog
;;;
;;; Types and Values
;;;
;;;     GtkFileDialog
;;;
;;; Accessors
;;;
;;;     gtk_file_dialog_get_accept_label
;;;     gtk_file_dialog_set_accept_label
;;;     gtk_file_dialog_get_default_filter
;;;     gtk_file_dialog_set_default_filter
;;;     gtk_file_dialog_get_filters
;;;     gtk_file_dialog_set_filters
;;;     gtk_file_dialog_get_initial_file
;;;     gtk_file_dialog_set_initial_file
;;;     gtk_file_dialog_get_initial_folder
;;;     gtk_file_dialog_set_initial_folder
;;;     gtk_file_dialog_get_initial_name
;;;     gtk_file_dialog_set_initial_name
;;;     gtk_file_dialog_get_modal
;;;     gtk_file_dialog_set_modal
;;;     gtk_file_dialog_get_title
;;;     gtk_file_dialog_set_title
;;;
;;; Functions
;;;
;;;     gtk_file_dialog_new
;;;     gtk_file_dialog_open
;;;     gtk_file_dialog_open_finish
;;;     gtk_file_dialog_open_multiple
;;;     gtk_file_dialog_open_multiple_finish
;;;     gtk_file_dialog_save
;;;     gtk_file_dialog_save_finish
;;;     gtk_file_dialog_select_folder
;;;     gtk_file_dialog_select_folder_finish
;;;     gtk_file_dialog_select_multiple_folders
;;;     gtk_file_dialog_select_multiple_folders_finish
;;;
;;; Properties
;;;
;;;     accept-label
;;;     default-filter
;;;     filters
;;;     initial-file
;;;     initial-folder
;;;     initial-name
;;;     modal
;;;     title
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkFileDialog
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkFileDialog
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkFileDialog" file-dialog
  (:superclass g:object
   :export t
   :interfaces ()
   :type-initializer "gtk_file_dialog_get_type")
  ((accept-label
    file-dialog-accept-label
    "accept-label" "gchararray" t t)
   (default-filter
    file-dialog-default-filter
    "default-filter" "GtkFileFilter" t t)
   (filters
    file-dialog-filters
    "filters" "GListModel" t t)
   (initial-file
    file-dialog-initial-file
    "initial-file" "GFile" t t)
   (initial-folder
    file-dialog-initial-folder
    "initial-folder" "GFile" t t)
   (initial-name
    file-dialog-initial-name
    "initial-name" "gchararray" t t)
   (modal
    file-dialog-modal
    "modal" "gboolean" t t)
   (title
    file-dialog-title
    "title" "gchararray" t t)))

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;;     accept-label
;;;     default-filter
;;;     filters
;;;     initial-file
;;;     initial-folder
;;;     initial-name
;;;     modal
;;;     title

;;; ----------------------------------------------------------------------------
;;; gtk_file_dialog_new
;;; ----------------------------------------------------------------------------

(declaim (inline file-dialog-new))

(defun file-dialog-new ()
  (make-instance 'file-dialog))

(export 'file-dialog-new)

;;; ----------------------------------------------------------------------------
;;; gtk_file_dialog_open
;;;
;;; void
;;; gtk_file_dialog_open (
;;;   GtkFileDialog* self,
;;;   GtkWindow* parent,
;;;   GCancellable* cancellable,
;;;   GAsyncReadyCallback callback,
;;;   gpointer user_data
;;; )
;;;
;;; This function initiates a file selection operation by presenting a file
;;; chooser dialog to the user.
;;;
;;; The callback will be called when the dialog is dismissed. It should call
;;; gtk_file_dialog_open_finish() to obtain the result.
;;;
;;; Since 4.10
;;;
;;; parent :
;;;     The parent GtkWindow. The argument can be NULL. The data is owned by
;;;     the caller of the function.
;;;
;;; cancellable :
;;;     A GCancellable to cancel the operation. The argument can be NULL. The
;;;     data is owned by the caller of the function.
;;;
;;; callback :
;;;     A callback to call when the operation is complete. The argument can be
;;;     NULL.
;;;
;;; user_data :
;;;     Data to pass to callback. The argument can be NULL. The data is owned
;;;     by the caller of the function.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_file_dialog_open" %file-dialog-open) :void
  (dialog (g:object file-dialog))
  (parent (g:object window))
  (cancellable (g:object g:cancellable))
  (func :pointer)
  (data :pointer))

(defun file-dialog-open (dialog parent cancellable func)
  (glib:with-stable-pointer (ptr func)
    (%file-dialog-open dialog
                       parent
                       cancellable
                       (cffi:callback g:async-ready-callback)
                       ptr)))

(export 'file-dialog-open)

;;; ----------------------------------------------------------------------------
;;;     gtk_file_dialog_open_finish
#|
FileDialogopen_finish
since: 4.10

[−]
Declaration
[src]
GFile*
gtk_file_dialog_open_finish (
  GtkFileDialog* self,
  GAsyncResult* result,
  GError** error
)

Finishes the gtk_file_dialog_open() call and returns the resulting file.

Available since: 4.10

result
Type: GAsyncResult

A GAsyncResult

The data is owned by the caller of the function.
error
Type: GError **

The return location for a recoverable error.

The argument can be NULL.
If the return location is not NULL, then you must initialize it to a NULL GError*.
The argument will left initialized to NULL by the method if there are no errors.
In case of error, the argument will be set to a newly allocated GError; the caller will take ownership of the data, and be responsible for freeing it.
[−]
Return value 
Type: GFile

The file that was selected. Otherwise, NULL is returned and error is set.

The caller of the method takes ownership of the data, and is responsible for freeing it.
The return value can be NULL.
|#
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_file_dialog_open_finish" %file-dialog-open-finish)
    (g:object g:file)
  (dialog (g:object file-dialog))
  (result (g:object g:async-result))
  (err :pointer))

(defun file-dialog-open-finish (dialog result)
  (%file-dialog-open-finish dialog result (cffi:null-pointer)))

(export 'file-dialog-open-finish)

;;;     gtk_file_dialog_open_multiple
;;;     gtk_file_dialog_open_multiple_finish
;;;     gtk_file_dialog_save
;;;     gtk_file_dialog_save_finish
;;;     gtk_file_dialog_select_folder
;;;     gtk_file_dialog_select_folder_finish
;;;     gtk_file_dialog_select_multiple_folders
;;;     gtk_file_dialog_select_multiple_folders_finish

;;; --- End of file gtk4.file-dialog.lisp --------------------------------------

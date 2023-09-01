;;; ----------------------------------------------------------------------------
;;; gtk4.file-launcher.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.12 and modified to document the Lisp binding to the GTK library.
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
;;; Types and Values
;;;
;;;     GtkFileLauncher
;;;
;;; Accessors
;;;
;;;     gtk_file_launcher_get_always_ask                   Since 4.12
;;;     gtk_file_launcher_set_always_ask                   Since 4.12
;;;     gtk_file_launcher_get_file
;;;     gtk_file_launcher_set_file
;;;
;;; Functions
;;;
;;;     gtk_file_launcher_new
;;;
;;;     gtk_file_launcher_launch
;;;     gtk_file_launcher_launch_finish
;;;     gtk_file_launcher_open_containing_folder
;;;     gtk_file_launcher_open_containing_folder_finish
;;;
;;; Properties
;;;
;;;     always-ask                                         Since 4.12
;;;     file
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkFileLauncher
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkFileLauncher
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkFileLauncher" file-launcher
  (:superclass g:object
   :export t
   :interfaces ()
   :type-initializer "gtk_file_launcher_get_type")
  (#+gtk-4-12
   (always-ask
    file-launcher-always-ask
    "always-ask" "gboolean" t t)
   (file
    file-launcher-file
    "file" "GFile" t t)))

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; Gtk.FileLauncher:always-ask
;;;
;;; Whether to ask the user to choose an app for opening the file. If FALSE,
;;; the file might be opened with a default app or the previous choice.
;;;
;;; Since 4.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; Gtk.FileLauncher:file
;;;
;;; The file to launch.
;;;
;;; Since 4.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_launcher_new
;;;
;;; Creates a new GtkFileLauncher object.
;;;
;;; Since 4.10
;;; ----------------------------------------------------------------------------

(declaim (inline file-launcher-new))

(defun file-launcher-new (&optional file)
  (make-instance 'file-launcher
                 :file file))

(export 'file-launcher-new)

;;; ----------------------------------------------------------------------------
;;; gtk_file_launcher_launch
;;;
;;; Launch an application to open the file.
;;;
;;; Since 4.10
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_file_launcher_launch" %file-launcher-launch) :void
  (launcher (g:object file-launcher))
  (parent (g:object window))
  (cancellable (g:object g:cancellable))
  (func :pointer)
  (data :pointer))

(defun file-launcher-launch (launcher parent cancellable func)
  (let ((ptr (glib:allocate-stable-pointer func)))
    (%file-launcher-launch launcher
                           parent
                           cancellable
                           (cffi:callback g:async-ready-callback)
                           ptr)))

(export 'file-launcher-launch)

;;; ----------------------------------------------------------------------------
;;; gtk_file_launcher_launch_finish
;;;
;;; Finishes the gtk_file_launcher_launch() call and returns the result.
;;;
;;; Since 4.10
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_file_launcher_launch-finish" %file-launcher-launch-finish)
    :boolean
  (launcher (g:object file-launcher))
  (result (g:object g:async-result))
  (err :pointer))

(defun file-launcher-launch-finish (launcher result)
  (glib:with-ignore-g-error (err)
    (%file-launcher-launch-finish launcher result err)))

(export 'file-launcher-launch-finish)

;;; ----------------------------------------------------------------------------
;;; gtk_file_launcher_open_containing_folder
;;;
;;; Launch a file manager to show the file in its parent directory.
;;;
;;; Since 4.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_launcher_open_containing_folder_finish
;;;
;;; Finishes the gtk_file_launcher_open_containing_folder() call and returns
;;; the result.
;;;
;;; Since: 4.10
;;; ----------------------------------------------------------------------------


;;;gtk_file_launcher_get_always_ask
;;;Returns whether to ask the user to choose an app for opening the file.

;;;since: 4.12

;;;gtk_file_launcher_get_file
;;;Gets the file that will be opened.

;;;since: 4.10



;;;gtk_file_launcher_set_always_ask
;;;Sets whether to awlays ask the user to choose an app for opening the file. If FALSE, the file might be opened with a default app or the previous choice.

;;;since: 4.12

;;;gtk_file_launcher_set_file
;;;Sets the file that will be opened.

;;;since: 4.10

;;; --- End of file gtk4.file-launcher.lisp ------------------------------------

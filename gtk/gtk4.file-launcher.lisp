;;; ----------------------------------------------------------------------------
;;; gtk4.file-launcher.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.18 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2023 - 2025 Dieter Kaiser
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
;;;     gtk_file_launcher_get_always_ask                    Since 4.12
;;;     gtk_file_launcher_set_always_ask                    Since 4.12
;;;     gtk_file_launcher_get_file
;;;     gtk_file_launcher_set_file
;;;     gtk_file_launcher_get_writable                      Since 4.14
;;;     gtk_file_launcher_set_writable                      Since 4.14
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
;;;     always-ask                                          Since 4.12
;;;     file
;;;     writable                                            Since 4.14
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

(gobject:define-gobject "GtkFileLauncher" file-launcher
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
    "file" "GFile" t t)
   #+gtk-4-14
   (writable
    file-launcher-writable
    "writable" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'file-launcher 'type)
 "@version{2024-05-26}
  @begin{short}
    The @class{gtk:file-launcher} object collects the arguments that are needed
    to open a file with an application.
  @end{short}
  Depending on system configuration, user preferences and available APIs, this
  may or may not show an app chooser dialog or launch the default application
  right away.

  The operation is started with the @fun{gtk:file-launcher-launch} function.
  This API follows the GIO async pattern, and the result can be obtained by
  calling the @fun{gtk:file-launcher-launch-finish} function.

  To launch uris that do not represent files, use the @class{gtk:uri-launcher}
  class.

  Since 4.10
  @see-constructor{gtk:file-launcher-new}
  @see-slot{gtk:file-launcher-always-ask}
  @see-slot{gtk:file-launcher-file}
  @see-slot{gtk:file-launcher-writable}
  @see-class{gtk:uri-launcher}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:file-launcher-always-ask -------------------------------------------

#+(and gtk-4-12 liber-documentation)
(setf (documentation (liber:slot-documentation "always-ask" 'file-launcher) t)
 "The @code{always-ask} property of type @code{:boolean} (Read / Write) @br{}
  Whether to ask the user to choose an application for opening the file. If
  @em{false}, the file might be opened with a default application or the
  previous choice. Since 4.12 @br{}
  Default value: @em{false}")

#+(and gtk-4-12 liber-documentation)
(setf (liber:alias-for-function 'file-launcher-always-ask)
      "Accessor"
      (documentation 'file-launcher-always-ask 'function)
 "@version{2025-09-21}
  @syntax{(gtk:file-launcher-always-ask object) => setting}
  @syntax{(setf (gtk:file-launcher-always-ask object) setting)}
  @argument[object]{a @class{gtk:file-launcher} object}
  @argument[setting]{a boolean whether to always ask the user}
  @begin{short}
    The accessor for the @slot[gtk:file-launcher]{always-ask} slot of the
    @class{gtk:file-launcher} class gets or sets whether to ask the user to
    choose an application for opening the file.
  @end{short}
  If @em{false}, the file might be opened with a default application or the
  previous choice.

  Since 4.12
  @see-class{gtk:file-launcher}")

;;; --- gtk:file-launcher-file -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "file" 'file-launcher) t)
 "The @code{file} property of type @class{g:file} (Read / Write) @br{}
  The file to launch. Since 4.10")

#+liber-documentation
(setf (liber:alias-for-function 'file-launcher-file)
      "Accessor"
      (documentation 'file-launcher-file 'function)
 "@version{2025-09-21}
  @syntax{(gtk:file-launcher-file object) => file}
  @syntax{(setf (gtk:file-launcher-file object) file)}
  @argument[object]{a @class{gtk:file-launcher} object}
  @argument[file]{a @class{g:file} object}
  @begin{short}
    The accessor for the @slot[gtk:file-launcher]{file} slot of the
    @class{gtk:file-launcher} class gets or sets the file that will be opened.
  @end{short}

  Since 4.10
  @see-class{gtk:file-launcher}
  @see-class{g:file}")

;;; --- gtk:file-launcher-writable ---------------------------------------------

#+(and gtk-4-14 liber-documentation)
(setf (documentation (liber:slot-documentation "writable" 'file-launcher) t)
 "The @code{writable} property of type @code{gboolean} (Read / Write) @br{}
  Whether to make the file writable for the handler. Since 4.14 @br{}
  Default value: @em{false}")

#+(and gtk-4-14 liber-documentation)
(setf (liber:alias-for-function 'file-launcher-writable)
      "Accessor"
      (documentation 'file-launcher-writable 'function)
 "@version{2025-09-21}
  @syntax{(gtk:file-launcher-writable object) => writable}
  @syntax{(setf (gtk:file-launcher-writable object) writable)}
  @argument[object]{a @class{gtk:file-launcher} object}
  @argument[writable]{a boolean whether to make the file writable}
  @begin{short}
    The accessor for the @slot[gtk:file-launcher]{writable} slot of the
    @class{gtk:file-launcher} class gets or sets whether to make the file
    writable for the handler.
  @end{short}

  Since 4.14
  @see-class{gtk:file-launcher}")

;;; ----------------------------------------------------------------------------
;;; gtk_file_launcher_new
;;; ----------------------------------------------------------------------------

(declaim (inline file-launcher-new))

(defun file-launcher-new (&optional file)
 #+liber-documentation
 "@version{#2025-07-13}
  @argument[file]{a @class{g:file} object}
  @return{The new @class{gtk:file-launcher} object.}
  @short{Creates a new @class{gtk:file-launcher} object.}

  Since 4.10
  @see-class{gtk:file-launcher}
  @see-class{g:file}"
  (make-instance 'file-launcher
                 :file file))

(export 'file-launcher-new)

;;; ----------------------------------------------------------------------------
;;; gtk_file_launcher_launch
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_file_launcher_launch" %file-launcher-launch) :void
  (launcher (g:object file-launcher))
  (parent (g:object window))
  (cancellable (g:object g:cancellable))
  (func :pointer)
  (data :pointer))

(defun file-launcher-launch (launcher parent cancellable func)
 #+liber-documentation
 "@version{#2025-07-25}
  @argument[launcher]{a @class{gtk:file-launcher} object}
  @argument[parent]{a @class{gtk:window} parent window}
  @argument[cancellable]{a @class{g:cancellable} object to cancel the operation}
  @argument[func]{a @sym{g:async-ready-callback} callback function to call when
    the operation is complete}
  @begin{short}
    Launch an application to open the file.
  @end{short}
  This may present an app chooser dialog to the user.

  The callback will be called when the operation is completed. It should call
  the @fun{gtk:file-launcher-launch-finish} function to obtain the result.

  Since 4.10
  @see-class{gtk:file-launcher}
  @see-class{gtk:window}
  @see-class{g:cancellable}
  @see-symbol{g:async-ready-callback}
  @see-function{gtk:file-launcher-launch-finish}"
  (let ((ptr (glib:allocate-stable-pointer func)))
    (%file-launcher-launch launcher
                           parent
                           cancellable
                           (cffi:callback g:async-ready-callback)
                           ptr)))

(export 'file-launcher-launch)

;;; ----------------------------------------------------------------------------
;;; gtk_file_launcher_launch_finish
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_file_launcher_launch-finish" %file-launcher-launch-finish)
    :boolean
  (launcher (g:object file-launcher))
  (result (g:object g:async-result))
  (err :pointer))

(defun file-launcher-launch-finish (launcher result)
 #+liber-documentation
 "@version{#2024-11-21}
  @argument[launcher]{a @class{gtk:file-launcher} object}
  @argument[result]{a @class{g:async-result} object}
  @return{@em{True} if an application was launched, or @em{false} on error.}
  @begin{short}
    Finishes the the @fun{gtk:file-launcher-launch} function call and returns
    the result.
  @end{short}

  Since 4.10
  @see-class{gtk:file-launcher}
  @see-class{g:async-result}
  @see-function{gtk:file-launcher-launch}"
  (glib:with-ignore-error (err)
    (%file-launcher-launch-finish launcher result err)))

(export 'file-launcher-launch-finish)

;;; ----------------------------------------------------------------------------
;;; gtk_file_launcher_open_containing_folder
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_file_launcher_open_containing_folder"
               %file-launcher-open-containing-folder) :void
  (launcher (g:object file-launcher))
  (parent (g:object window))
  (cancellable (g:object g:cancellable))
  (func :pointer)
  (data :pointer))

(defun file-launcher-open-containing-folder (launcher parent cancellable func)
 #+liber-documentation
 "@version{#2025-07-25}
  @argument[launcher]{a @class{gtk:file-launcher} object}
  @argument[parent]{a @class{gtk:window} parent window}
  @argument[cancellable]{a @class{g:cancellable} object to cancel the operation}
  @argument[func]{a @sym{g:async-ready-callback} callback function to call when
    the operation is complete}
  @begin{short}
    Launch a file manager to show the file in its parent directory.
  @end{short}
  This is only supported native files. It will fail if @arg{file} is, for
  example, a @file{http://} URI.

  The callback function will be called when the operation is completed. It
  should call the @fun{gtk:file-launcher-open-containing-folder-finish} function
  to obtain the result.

  Since 4.10
  @see-class{gtk:file-launcher}
  @see-class{gtk:window}
  @see-class{g:cancellable}
  @see-symbol{g:async-ready-callback}
  @see-function{gtk:file-launcher-open-containing-folder-finish}"
  (let ((ptr (glib:allocate-stable-pointer func)))
    (%file-launcher-open-containing-folder
            launcher
            parent
            cancellable
            (cffi:callback g:async-ready-callback)
            ptr)))

(export 'file-launcher-open-containing-folder)

;;; ----------------------------------------------------------------------------
;;; gtk_file_launcher_open_containing_folder_finish
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_file_launcher_open_containing_folder_finish"
               %file-launcher-open-containing-folder-finish) :boolean
  (launcher (g:object file-launcher))
  (result (g:object g:async-result))
  (err :pointer))

(defun file-launcher-open-containing-folder-finish (launcher result)
 #+liber-documentation
 "@version{#2024-11-21}
  @argument[launcher]{a @class{gtk:file-launcher} object}
  @argument[result]{a @class{g:async-result} object}
  @begin{short}
    Finishes the @fun{gtk:file-launcher-open-containing-folder} function call
    and returns the result.
  @end{short}

  Since 4.10
  @see-class{gtk:file-launcher}
  @see-class{g:async-result}
  @see-function{gtk:file-launcher-open-containing-folder}"
  (glib:with-ignore-error (err)
    (%file-launcher-open-containing-folder-finish launcher result err)))

(export 'file-launcher-open-containing-folder-finish)

;;; --- End of file gtk4.file-launcher.lisp ------------------------------------

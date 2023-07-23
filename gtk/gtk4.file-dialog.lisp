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

(gobject:define-g-object-class "GtkFileDialog" file-dialog
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

#+liber-documentation
(setf (documentation 'file-dialog 'type)
 "@version{2023-7-22}
  @begin{short}
    A @sym{gtk:file-dialog} object collects the arguments that are needed to
    present a file chooser dialog to the user, such as a title for the dialog
    and whether it should be modal.
  @end{short}

  The dialog is shown with the @fun{gtk:file-dialog-open} function, the
  @fun{gtk:file-dialog-save} function, etc. These APIs follow the GIO async
  pattern, and the result can be obtained by calling the corresponding finish
  function, for example the @fun{gtk:file-dialog-open-finish} function.

  Since 4.10
  @see-constructor{gtk:file-dialog-new}
  @see-slot{gtk:accept-label}
  @see-slot{gtk:default-filter}
  @see-slot{gtk:filters}
  @see-slot{gtk:initial-file}
  @see-slot{gtk:initial-folder}
  @see-slot{gtk:initial-name}
  @see-slot{gtk:modal}
  @see-slot{gtk:title}
  @see-class{gio:async-result}
  @see-class{gio:task}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- file-dialog-accept-label -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "accept-label" 'file-dialog) t)
 "The @code{accept-label} property of type @code{:string} (Read / Write) @br{}
  Label for the accept button of the file dialog.")

#+liber-documentation
(setf (liber:alias-for-function 'file-dialog-accept-label)
      "Accessor"
      (documentation 'file-dialog-accept-label 'function)
 "@version{2023-7-22}
  @syntax[]{(gtk:file-dialog-accept-label object) => label}
  @syntax[]{(setf (gtk:file-dialog-accept-label object) label)}
  @argument[object]{a @class{gtk:file-dialog} object}
  @argument[label]{a string with the accept label}
  @begin{short}
    Accessor of the @slot[gtk:file-dialog]{accept-label} slot of the
    @class{gtk:file-dialog} class.
  @end{short}
  The @sym{gtk:file-dialog-accept-label} function gets the label shown on the
  accept button of the file chooser dialog. The
  @sym{(setf gtk:file-dialog-accept-label)} function sets the label.

  Leaving the accept label unset or setting it as @code{nil} will fall back to
  a default label, depending on what API is used to launch the file chooser
  dialog.

  Since 4.10
  @see-class{gtk:file-dialog}")

;;; --- file-dialog-default-filter ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "default-filter" 'file-dialog) t)
 "The @code{default-filter} property of type @class{gtk:file-filter}
  (Read / Write) @br{}
  The default filter, that is, the filter that is initially active in the file
  chooser dialog. If the default filter is @code{nil}, the first filter of
  the @slot[gtk:file-dialog]{filters} property is used as the default filter.
  If that property contains no filter, the dialog will be unfiltered. If the
  @slot[gtk:file-dialog]{filters} property is not @code{nil}, the default
  filter should be part of the list. If it is not, the dialog may choose to not
  make it available.")

#+liber-documentation
(setf (liber:alias-for-function 'file-dialog-default-filter)
      "Accessor"
      (documentation 'file-dialog-default-filter 'function)
 "@version{2023-7-22}
  @syntax[]{(gtk:file-dialog-default-filter object) => filter}
  @syntax[]{(setf (gtk:file-dialog-default-filter object) filter)}
  @argument[object]{a @class{gtk:file-dialog} object}
  @argument[filter]{a @class{gtk:file-filter} object}
  @begin{short}
    Accessor of the @slot[file-dialog]{default-filter} slot of the
    @class{gtk:file-dialog} class.
  @end{short}
  The @sym{gtk:file-dialog-default-filter} function gets the filter that will
  be selected by default in the file chooser dialog. The
  @sym{(setf gtk:file-dialog-default-filter)} function sets the filter.

  If set to @code{nil}, the first item in @slot[gtk:file-dialog]{filters} will
  be used as the default filter. If that list is empty, the dialog will be
  unfiltered.

  Since 4.10
  @see-class{gtk:file-dialog}
  @see-class{gtk:file-filter}
  @see-function{gtk:file-dialog-filters}")

;;; --- file-dialog-filters ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "filters" 'file-dialog) t)
 "The @code{filters} property of type @class{g:list-model} (Read / Write) @br{}
  The list of filters.")

#+liber-documentation
(setf (liber:alias-for-function 'file-dialog-filters)
      "Accessor"
      (documentation 'file-dialog-filters 'function)
 "@version{2023-7-22}
  @syntax[]{(gtk:file-dialog-filters object) => filters}
  @syntax[]{(setf (gtk:file-dialog-filters object) filters)}
  @argument[object]{a @class{gtk:file-dialog} object}
  @argument[filters]{a @class{g:list-model} object with the
    @class{gtk:file-filter} objects}
  @begin{short}
    Accessor of the @slot[file-dialog]{filters} slot of the
    @class{gtk:file-dialog} class.
  @end{short}
  The @sym{gtk:file-dialog-filters} function gets the filters that will be
  offered to the user in the file chooser dialog. The
  @sym{(setf gtk:file-dialog-filters)} function sets the filters.

  Since 4.10
  @see-class{gtk:file-dialog}
  @see-class{gtk:file-filter}
  @see-class{g:list-model}
  @see-function{gtk:file-dialog-default-filter}")

;;; --- file-dialog-initial-file -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "initial-file" 'file-dialog) t)
 "The @code{initial-file} property of type @class{g:file} (Read / Write) @br{}
  The initial file, that is, the file that is initially selected in the file
  chooser dialog. This is a utility property that sets both
  @slot[gtk:file-dialog]{initial-folder} and
  @slot[gtk:file-dialog]{initial-name} properties.")

#+liber-documentation
(setf (liber:alias-for-function 'file-dialog-initial-file)
      "Accessor"
      (documentation 'file-dialog-initial-file 'function)
 "@version{2023-7-22}
  @syntax[]{(gtk:file-dialog-initial-file object) => file}
  @syntax[]{(setf (gtk:file-dialog-initial-file object) file)}
  @argument[object]{a @class{gtk:file-dialog} object}
  @argument[file]{a @class{g:file} object with the file}
  @begin{short}
    Accessor of the @slot[file-dialog]{initial-file} slot of the
    @class{gtk:file-dialog} class.
  @end{short}
  The @sym{gtk:file-dialog-initial-file} function gets the file that will be
  initially selected in the file chooser dialog. The
  @sym{(setf gtk:file-dialog-initial-file)} function sets the file.

  This function is a shortcut for calling both the
  @fun{gtk:file-dialog-initial-folder} and @fun{gtk:file-dialog-initial-name}
  functions with the directory and name of file respectively.

  Since 4.10
  @see-class{gtk:file-dialog}
  @see-class{g:file}
  @see-function{gtk:file-dialog-initial-folder}
  @see-function{gtk:file-dialog-initial-name}")

;;; --- file-dialog-initial-folder -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "initial-folder" 'file-dialog) t)
 "The @code{initial-folder} property of type @class{g:file} (Read / Write) @br{}
  The initial folder, that is, the directory that is initially opened in the
  file chooser dialog.")

#+liber-documentation
(setf (liber:alias-for-function 'file-dialog-initial-folder)
      "Accessor"
      (documentation 'file-dialog-initial-folder 'function)
 "@version{2023-7-22}
  @syntax[]{(gtk:file-dialog-initial-folder object) => folder}
  @syntax[]{(setf (gtk:file-dialog-initial-folder object) folder)}
  @argument[object]{a @class{gtk:file-dialog} object}
  @argument[folder]{a @class{g:file} object with the folder}
  @begin{short}
    Accessor of the @slot[file-dialog]{initial-folder} slot of the
    @class{gtk:file-dialog} class.
  @end{short}
  The @sym{gtk:file-dialog-initial-folder} function gets the folder that will
  be set as the initial folder in the file chooser dialog. The
  @sym{(setf gtk:file-dialog-initial-folder)} function sets the folder.

  Since 4.10
  @see-class{gtk:file-dialog}
  @see-class{g:file}")

;;; --- file-dialog-initial-name -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "initial-name" 'file-dialog) t)
 "The @code{initial-name} property of type @code{:string} (Read / Write) @br{}
  The initial name, that is, the filename that is initially selected in the file
  chooser dialog.")

#+liber-documentation
(setf (liber:alias-for-function 'file-dialog-initial-name)
      "Accessor"
      (documentation 'file-dialog-initial-name 'function)
 "@version{2023-7-22}
  @syntax[]{(gtk:file-dialog-initial-name object) => name}
  @syntax[]{(setf (gtk:file-dialog-initial-name object) name)}
  @argument[object]{a @class{gtk:file-dialog} object}
  @argument[name]{a string with the name}
  @begin{short}
    Accessor of the @slot[file-dialog]{initial-name} slot of the
    @class{gtk:file-dialog} class.
  @end{short}
  The @sym{gtk:file-dialog-initial-name} function gets the name for the file
  that should be initially set. The @sym{(setf gtk:file-dialog-initial-name)}
  function sets the name. For saving dialogs, this will usually be pre-entered
  into the name field.

  If a file with this name already exists in the directory set via the
  @slot[gtk:file-dialog]{initial-folder} property, the dialog should preselect
  it.

  Since 4.10
  @see-class{gtk:file-dialog}
  @see-function{gtk:file-dialog-initial-folder}")

;;; --- file-dialog-modal ------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "modal" 'file-dialog) t)
 "The @code{modal} property of type @code{:boolean} (Read / Write) @br{}
  Whether the file chooser dialog is modal. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'file-dialog-modal)
      "Accessor"
      (documentation 'file-dialog-modal 'function)
 "@version{2023-7-22}
  @syntax[]{(gtk:file-dialog-modal object) => modal}
  @syntax[]{(setf (gtk:file-dialog-modal object) modal)}
  @argument[object]{a @class{gtk:file-dialog} object}
  @argument[modal]{a boolean whether the file chooser dialog is modal}
  @begin{short}
    Accessor of the @slot[file-dialog]{modal} slot of the
    @class{gtk:file-dialog} class.
  @end{short}
  The @sym{gtk:file-dialog-modal} function returns whether the file chooser
  dialog blocks interaction with the parent window while it is presented.
  The @sym{(setf gtk:file-dialog-modal)} function sets the property.

  Since 4.10
  @see-class{gtk:file-dialog}")

;;; --- file-dialog-title ------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "title" 'file-dialog) t)
 "The @code{title} property of type @code{:string} (Read / Write) @br{}
  A title that may be shown on the file chooser dialog. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'file-dialog-title)
      "Accessor"
      (documentation 'file-dialog-title 'function)
 "@version{2023-7-22}
  @syntax[]{(gtk:file-dialog-title object) => title}
  @syntax[]{(setf (gtk:file-dialog-title object) title)}
  @argument[object]{a @class{gtk:file-dialog} object}
  @argument[title]{a string with the title}
  @begin{short}
    Accessor of the @slot[file-dialog]{title} slot of the
    @class{gtk:file-dialog} class.
  @end{short}
  The @sym{gtk:file-dialog-title} function returns the title that will be shown
  on the file chooser dialog. The @sym{(setf gtk:file-dialog-title)} function
  sets the title.

  Since 4.10
  @see-class{gtk:file-dialog}")

;;; ----------------------------------------------------------------------------
;;; gtk_file_dialog_new
;;; ----------------------------------------------------------------------------

(declaim (inline file-dialog-new))

(defun file-dialog-new ()
 #+liber-documentation
 "@version{2023-7-22}
  @return{The new @class{gtk:file-dialog} object.}
  @short{Creates a new file chooser dialog.}
  @see-class{gtk:file-dialog}"
  (make-instance 'file-dialog))

(export 'file-dialog-new)

;;; ----------------------------------------------------------------------------
;;; gtk_file_dialog_open
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_file_dialog_open" %file-dialog-open) :void
  (dialog (g:object file-dialog))
  (parent (g:object window))
  (cancellable (g:object g:cancellable))
  (func :pointer)
  (data :pointer))

;; FIXME: We must use glib:allocate-stable-pointer but cannot install a
;; glib:stable-pointer-destroy-notify function to free the pointer. This causes
;; a memory leak. Can we free the stable pointer in the gio:async-ready-callback
;; function?

(defun file-dialog-open (dialog parent cancellable func)
 #+liber-documentation
 "@version{#2023-7-22}
  @argument[dialog]{a @class{gtk:file-dialog} object}
  @argument[parent]{a @class{gtk:window} parent window}
  @argument[cancellable]{a @class{g:cancellable} object to cancel the operation,
    the argument can be @code{nil}}
  @argument[func]{a @symbol{g:async-ready-callback} function to call when the
    operation is complete, the argument can be @code{nil}}
  @begin{short}
    This function initiates a file selection operation by presenting a file
    chooser dialog to the user.
  @end{short}
  The callback will be called when the dialog is dismissed. It should call the
  @fun{gtk:file-dialog-open-finish} function to obtain the result.

  Since 4.10
  @see-class{gtk:file-dialog}
  @see-class{gtk:window}
  @see-class{g:cancellable}
  @see-symbol{g:async-ready-callback}
  @see-function{gtk:file-dialog-open-finish}"
  (let ((ptr (glib:allocate-stable-pointer func)))
    (%file-dialog-open dialog
                       parent
                       cancellable
                       (cffi:callback g:async-ready-callback)
                       ptr)))

(export 'file-dialog-open)

;;; ----------------------------------------------------------------------------
;;; gtk_file_dialog_open_finish
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_file_dialog_open_finish" %file-dialog-open-finish)
    (g:object g:file)
  (dialog (g:object file-dialog))
  (result (g:object g:async-result))
  (err :pointer))

(defun file-dialog-open-finish (dialog result)
 #+liber-documentation
 "@version{#2023-7-22}
  @argument[dialog]{a @class{gtk:file-dialog} object}
  @argument[result]{a @class{g:async-result} object}
  @return{A @class{g:file} object with the file that was selected, otherwise
    @code{nil}.}
  @begin{short}
    Finishes the @fun{gtk:file-dialog-open} function call and returns the
    resulting file.
  @end{short}

  Since 4.10
  @see-class{gtk:file-dialog}
  @see-class{g:file}
  @see-class{g:async-result}
  @see-function{gtk:file-dialog-open}"
  (glib:with-ignore-g-error (err)
    (%file-dialog-open-finish dialog result err)))

(export 'file-dialog-open-finish)

;;; ----------------------------------------------------------------------------
;;;     gtk_file_dialog_open_multiple
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_file_dialog_open_multiple" %file-dialog-open-multiple) :void
  (dialog (g:object file-dialog))
  (parent (g:object window))
  (cancellable (g:object g:cancellable))
  (func :pointer)
  (data :pointer))

(defun file-dialog-open-multiple (dialog parent cancellable func)
 #+liber-documentation
 "@version{#2023-7-22}
  @argument[dialog]{a @class{gtk:file-dialog} object}
  @argument[parent]{a @class{gtk:window} parent window}
  @argument[cancellable]{a @class{g:cancellable} object to cancel the operation,
    the argument can be @code{nil}}
  @argument[func]{a @symbol{g:async-ready-callback} function to call when the
    operation is complete, the argument can be @code{nil}}
  @begin{short}
    This function initiates a multi-file selection operation by presenting a
    file chooser dialog to the user.
  @end{short}
  The file chooser will initially be opened in the
  @slot[gtk:file-dialog]{initial-folder} directory. The callback will be called
  when the dialog is dismissed. It should call the
  @fun{gtk:file-dialog-open-multiple-finish} function to obtain the result.

  Since 4.10
  @see-class{gtk:file-dialog}
  @see-class{gtk:window}
  @see-class{g:cancellable}
  @see-symbol{g:async-ready-callback}
  @see-function{gtk:file-dialog-open-multiple-finish}
  @see-function{gtk:file-dialog-initial-folder}"
  (let ((ptr (glib:allocate-stable-pointer func)))
    (%file-dialog-open-multiple dialog
                                parent
                                cancellable
                                (cffi:callback g:async-ready-callback)
                                ptr)))

(export 'file-dialog-open-multiple)

;;; ----------------------------------------------------------------------------
;;;     gtk_file_dialog_open_multiple_finish
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_file_dialog_open_multiple_finish"
               %file-dialog-open-multiple-finish) (g:object g:list-model)
  (dialog (g:object file-dialog))
  (result (g:object g:async-result))
  (err :pointer))

(defun file-dialog-open-multiple-finish (dialog result)
 #+liber-documentation
 "@version{#2023-7-22}
  @argument[dialog]{a @class{gtk:file-dialog} object}
  @argument[result]{a @class{g:async-result} object}
  @return{A @class{g:list-model} object with the @class{g:file} objects that
    were selected, otherwise @code{nil}.}
  @begin{short}
    Finishes the @fun{gtk:file-dialog-open-multiple} function call and returns
    the resulting files in a @class{g:list-model} object.
  @end{short}

  Since 4.10
  @see-class{gtk:file-dialog}
  @see-class{g:file}
  @see-class{g:list-model}
  @see-class{g:async-result}
  @see-function{gtk:file-dialog-open-multiple}"
  (glib:with-ignore-g-error (err)
    (%file-dialog-open-multiple-finish dialog result err)))

(export 'file-dialog-open-multiple-finish)

;;; ----------------------------------------------------------------------------
;;;     gtk_file_dialog_save
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_file_dialog_save" %file-dialog-save) :void
  (dialog (g:object file-dialog))
  (parent (g:object window))
  (cancellable (g:object g:cancellable))
  (func :pointer)
  (data :pointer))

(defun file-dialog-save (dialog parent cancellable func)
 #+liber-documentation
 "@version{#2023-7-22}
  @argument[dialog]{a @class{gtk:file-dialog} object}
  @argument[parent]{a @class{gtk:window} parent window}
  @argument[cancellable]{a @class{g:cancellable} object to cancel the operation,
    the argument can be @code{nil}}
  @argument[func]{a @symbol{g:async-ready-callback} function to call when the
    operation is complete, the argument can be @code{nil}}
  @begin{short}
    This function initiates a file save operation by presenting a file chooser
    dialog to the user.
  @end{short}
  The callback will be called when the dialog is dismissed. It should call the
  @fun{gtk:file-dialog-save-finish} function to obtain the result.

  Since 4.10
  @see-class{gtk:file-dialog}
  @see-class{gtk:window}
  @see-class{g:cancellable}
  @see-symbol{g:async-ready-callback}
  @see-function{gtk:file-dialog-save-finish}"
  (let ((ptr (glib:allocate-stable-pointer func)))
    (%file-dialog-save dialog
                       parent
                       cancellable
                       (cffi:callback g:async-ready-callback)
                       ptr)))

(export 'file-dialog-save)

;;; ----------------------------------------------------------------------------
;;;     gtk_file_dialog_save_finish
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_file_dialog_save_finish" %file-dialog-save-finish)
    (g:object g:file)
  (dialog (g:object file-dialog))
  (result (g:object g:async-result))
  (err :pointer))

(defun file-dialog-save-finish (dialog result)
 #+liber-documentation
 "@version{#2023-7-22}
  @argument[dialog]{a @class{gtk:file-dialog} object}
  @argument[result]{a @class{g:async-result} object}
  @return{A @class{g:file} object with the file that was selected, otherwise
    @code{nil}.}
  @begin{short}
    Finishes the @fun{gtk:file-dialog-save} function call and returns the
    resulting file.
  @end{short}

  Since 4.10
  @see-class{gtk:file-dialog}
  @see-class{g:file}
  @see-class{g:async-result}
  @see-function{gtk:file-dialog-save}"
  (glib:with-ignore-g-error (err)
    (%file-dialog-save-finish dialog result err)))

(export 'file-dialog-save-finish)

;;; ----------------------------------------------------------------------------
;;;     gtk_file_dialog_select_folder
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_file_dialog_select_folder" %file-dialog-select-folder) :void
  (dialog (g:object file-dialog))
  (parent (g:object window))
  (cancellable (g:object g:cancellable))
  (func :pointer)
  (data :pointer))

(defun file-dialog-select-folder (dialog parent cancellable func)
 #+liber-documentation
 "@version{#2023-7-22}
  @argument[dialog]{a @class{gtk:file-dialog} object}
  @argument[parent]{a @class{gtk:window} parent window}
  @argument[cancellable]{a @class{g:cancellable} object to cancel the operation,
    the argument can be @code{nil}}
  @argument[func]{a @symbol{g:async-ready-callback} function to call when the
    operation is complete, the argument can be @code{nil}}
  @begin{short}
    This function initiates a directory selection operation by presenting a
    file chooser dialog to the user.
  @end{short}
  The file chooser will initially be opened in the
  @slot[gtk:file-dialog]{initial-folder} directory. The callback will  be
  called when the dialog is dismissed. It should call the
  @fun{gtk:file-dialog-select-folder-finish} function to obtain the result.

  Since 4.10
  @see-class{gtk:file-dialog}
  @see-class{gtk:window}
  @see-class{g:cancellable}
  @see-symbol{g:async-ready-callback}
  @see-function{gtk:file-dialog-select-folder-finish}
  @see-function{gtk:file-dialog-initial-folder}"
  (let ((ptr (glib:allocate-stable-pointer func)))
    (%file-dialog-select-folder dialog
                                parent
                                cancellable
                                (cffi:callback g:async-ready-callback)
                                ptr)))

(export 'file-dialog-select-folder)

;;; ----------------------------------------------------------------------------
;;;     gtk_file_dialog_select_folder_finish
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_file_dialog_select_folder_finish"
               %file-dialog-select-folder-finish) (g:object g:file)
  (dialog (g:object file-dialog))
  (result (g:object g:async-result))
  (err :pointer))

(defun file-dialog-select-folder-finish (dialog result)
 #+liber-documentation
 "@version{#2023-7-22}
  @argument[dialog]{a @class{gtk:file-dialog} object}
  @argument[result]{a @class{g:async-result} object}
  @return{A @class{g:file} object with the file that was selected, otherwise
    @code{nil}.}
  @begin{short}
    Finishes the @fun{gtk:file-dialog-select-folder} function call and returns
    the resulting folder.
  @end{short}

  Since 4.10
  @see-class{gtk:file-dialog}
  @see-class{g:file}
  @see-class{g:async-result}
  @see-function{gtk:file-dialog-select-folder}"
  (glib:with-ignore-g-error (err)
    (%file-dialog-select-folder-finish dialog result err)))

(export 'file-dialog-select-folder-finish)

;;; ----------------------------------------------------------------------------
;;;     gtk_file_dialog_select_multiple_folders
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_file_dialog_select_multiple_folders"
               %file-dialog-select-multiple-folders) :void
  (dialog (g:object file-dialog))
  (parent (g:object window))
  (cancellable (g:object g:cancellable))
  (func :pointer)
  (data :pointer))

(defun file-dialog-select-multiple-folders (dialog parent cancellable func)
 #+liber-documentation
 "@version{#2023-7-22}
  @argument[dialog]{a @class{gtk:file-dialog} object}
  @argument[parent]{a @class{gtk:window} parent window}
  @argument[cancellable]{a @class{g:cancellable} object to cancel the operation,
    the argument can be @code{nil}}
  @argument[func]{a @symbol{g:async-ready-callback} function to call when the
    operation is complete, the argument can be @code{nil}}
  @begin{short}
    This function initiates a multi-directory selection operation by presenting
    a file chooser dialog to the user.
  @end{short}
  The file chooser will initially be opened in the
  @slot[gtk:file-dialog]{initial-folder} directory. The callback will be called
  when the dialog is dismissed. It should call the
  @fun{gtk:file-dialog-select-multiple-folders-finish} function to obtain the
  result.

  Since 4.10
  @see-class{gtk:file-dialog}
  @see-class{gtk:window}
  @see-class{g:cancellable}
  @see-symbol{g:async-ready-callback}
  @see-function{gtk:file-dialog-select-multiple-folders-finish}
  @see-function{gtk:file-dialog-initial-folder}"
  (let ((ptr (glib:allocate-stable-pointer func)))
    (%file-dialog-select-multiple-folders dialog
                                          parent
                                          cancellable
                                          (cffi:callback g:async-ready-callback)
                                          ptr)))

(export 'file-dialog-select-multiple-folders)

;;; ----------------------------------------------------------------------------
;;;     gtk_file_dialog_select_multiple_folders_finish
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_file_dialog_select_multiple_folders_finish"
               %file-dialog-select-multiple-folders-finish)
    (g:object g:list-model)
  (dialog (g:object file-dialog))
  (result (g:object g:async-result))
  (err :pointer))

(defun file-dialog-select-multiple-folders-finish (dialog result)
 #+liber-documentation
 "@version{#2023-7-22}
  @argument[dialog]{a @class{gtk:file-dialog} object}
  @argument[result]{a @class{g:async-result} object}
  @return{A @class{g:file} object with the file that was selected, otherwise
    @code{nil}.}
  @begin{short}
    Finishes the @fun{gtk:file-dialog-select-multiple-folders} function call
    and returns the resulting folders in a @class{g:list-model} object.
  @end{short}

  Since 4.10
  @see-class{gtk:file-dialog}
  @see-class{g:file}
  @see-class{g:list-model}
  @see-class{g:async-result}
  @see-function{gtk:file-dialog-select-multiple-folders}"
  (glib:with-ignore-g-error (err)
    (%file-dialog-select-multiple-folders-finish dialog result err)))

(export 'file-dialog-select-multiple-folders-finish)

;;; --- End of file gtk4.file-dialog.lisp --------------------------------------

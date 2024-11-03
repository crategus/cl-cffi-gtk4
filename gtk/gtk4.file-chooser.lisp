;;; ----------------------------------------------------------------------------
;;; gtk4.file-chooser.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.12 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2012 - 2024 Dieter Kaiser
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
;;; GtkFileChooser
;;;
;;;     File chooser interface used by GtkFileChooserWidget and
;;;     GtkFileChooserDialog
;;;
;;; Types and Values
;;;
;;;     GtkFileChooser
;;;     GtkFileChooserAction
;;;
;;; Accessors
;;;
;;;     gtk_file_chooser_set_action
;;;     gtk_file_chooser_get_action
;;;     gtk_file_chooser_set_create_folders
;;;     gtk_file_chooser_get_create_folders
;;;     gtk_file_chooser_get_filter
;;;     gtk_file_chooser_get_filters
;;;     gtk_file_chooser_set_filter
;;;     gtk_file_chooser_set_select_multiple
;;;     gtk_file_chooser_get_select_multiple
;;;     gtk_file_chooser_get_shortcut_folders
;;;
;;; Functions
;;;
;;;     gtk_file_chooser_set_current_name
;;;     gtk_file_chooser_get_current_name
;;;     gtk_file_chooser_get_file
;;;     gtk_file_chooser_set_file
;;;     gtk_file_chooser_get_files
;;;     gtk_file_chooser_set_current_folder
;;;     gtk_file_chooser_get_current_folder
;;;     gtk_file_chooser_add_filter
;;;     gtk_file_chooser_remove_filter
;;;     gtk_file_chooser_add_shortcut_folder
;;;     gtk_file_chooser_remove_shortcut_folder
;;;     gtk_file_chooser_add_choice
;;;     gtk_file_chooser_remove_choice
;;;     gtk_file_chooser_set_choice
;;;     gtk_file_chooser_get_choice
;;;
;;; Properties
;;;
;;;     action
;;;     create-folders
;;;     filter
;;;     filters
;;;     select-multiple
;;;     shortcut-folders
;;;
;;; Object Hierarchy
;;;
;;;     GInterface
;;;     ╰── GtkFileChooser
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkFileChooserAction
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GtkFileChooserAction" file-chooser-action
  (:export t
   :type-initializer "gtk_file_chooser_action_get_type")
  (:open 0)
  (:save 1)
  (:select-folder 2))

#+liber-documentation
(setf (liber:alias-for-symbol 'file-chooser-action)
      "GEnum"
      (liber:symbol-documentation 'file-chooser-action)
 "@version{2024-4-26}
  @begin{declaration}
(gobject:define-genum \"GtkFileChooserAction\" gtk:file-chooser-action
  (:export t
   :type-initializer \"gtk_file_chooser_action_get_type\")
  (:open 0)
  (:save 1)
  (:select-folder 2))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:open]{Indicates Open mode. The file chooser will only let the user
        pick an existing file.}
      @entry[:save]{Indicates Save mode. The file chooser will let the user pick
        an existing file, or type in a new filename.}
      @entry[:select-folder]{Indicates an Open mode for selecting folders. The
        file chooser will let the user pick an existing folder.}
    @end{table}
  @end{values}
  @begin{short}
    Describes whether a @class{gtk:file-chooser} widget is being used to
    open existing files or to save to a possibly new file.
  @end{short}
  @see-class{gtk:file-chooser}")

;;; ----------------------------------------------------------------------------
;;; GtkFileChooser
;;; ----------------------------------------------------------------------------

(gobject:define-ginterface "GtkFileChooser" file-chooser
  (:export t
   :type-initializer "gtk_file_chooser_get_type")
  ((action
    file-chooser-action
    "action" "GtkFileChooserAction" t t)
   (create-folders
    file-chooser-create-folders
    "create-folders" "gboolean" t t)
   (filter
    file-chooser-filter
    "filter" "GtkFileFilter" t t)
   (filters
    file-chooser-filters
    "filters" "GListModel" t nil)
   (select-multiple
    file-chooser-select-multiple
    "select-multiple" "gboolean" t t)
   (shortcut-folders
    file-chooser-shortcut-folders
    "shortcut-folders" "GListModel" t nil)))

#+(and gtk-4-10 gtk-warn-deprecated)
(defmethod initialize-instance :after ((obj file-chooser) &key)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:FILE-CHOOSER is deprecated since 4.10")))

#+liber-documentation
(setf (liber:alias-for-class 'file-chooser)
      "Interface"
      (documentation 'file-chooser 'type)
 "@version{2024-5-20}
  @begin{short}
    The @class{gtk:file-chooser} interface is an interface that can be
    implemented by file selection widgets.
  @end{short}
  The main widgets that implement this interface are the
  @class{gtk:file-chooser-widget} and @class{gtk:file-chooser-dialog} widgets.
  You do not need to write a widget that implements the @class{gtk:file-chooser}
  interface unless you are trying to adapt an existing file selector to expose
  a standard programming interface.

  The @class{gtk:file-chooser} interface allows for shortcuts to various places
  in the filesystem. In the default implementation these are displayed in the
  left pane. It may be a bit confusing at first that these shortcuts come from
  various sources and in various flavours, so lets explain the terminology here:
  @begin{table}
    @begin[Bookmarks]{entry}
      are created by the user, by dragging folders from the right pane to the
      left pane, or by using the \"Add\". Bookmarks can be renamed and deleted
      by the user.
    @end{entry}
    @begin[Shortcuts]{entry}
      can be provided by the application or by the underlying filesystem
      abstraction, for example, both the gnome-vfs and the Windows filesystems
      provide \"Desktop\" shortcuts. Shortcuts cannot be modified by the user.
    @end{entry}
    @begin[Volumes]{entry}
      are provided by the underlying filesystem abstraction. Volumes are the
      \"roots\" of the filesystem.
    @end{entry}
  @end{table}
  @subheading{File Names and Encodings}
    When the user is finished selecting files in a @class{gtk:file-chooser}
    widget, the program can get the selected filenames as @class{g:file}
    objects.
  @begin[Examples]{dictionary}
    Sample usage with a @class{gtk:file-chooser-dialog} widget.
    @begin{pre}
(defun create-file-chooser-dialog (parent)
  (let ((filter-all (gtk:file-filter-new))
        (filter-picture (gtk:file-filter-new))
        (dialog (gtk:file-chooser-dialog-new \"File Chooser Dialog\"
                                             parent
                                             :open
                                             \"Open\" 100
                                             \"Cancel\" :cancel)))
    (setf (gtk:window-modal dialog) t)
    (g:signal-connect dialog \"response\"
                      (lambda (dialog response)
                        (format t \"  Response is ~a~%\" response)
                        (unless (eq :cancel
                                    (gtk:response-type-keyword response))
                          (format t \"Selected file is ~a~%\"
                                  (gtk:file-chooser-namestring dialog)))
                        (gtk:window-destroy dialog)))
    ;; Add a file filter
    (setf (gtk:file-filter-name filter-all) \"All Files\")
    (gtk:file-filter-add-pattern filter-all \"*\")
    (gtk:file-chooser-add-filter dialog filter-all)
    ;; Add a second file filter for pictures
    (setf (gtk:file-filter-name filter-picture) \"All Pictures\")
    (gtk:file-filter-add-pixbuf-formats filter-picture)
    (gtk:file-chooser-add-filter dialog filter-picture)
    ;; Present the dialog
    (gtk:window-present dialog)))
    @end{pre}
  @end{dictionary}
  @begin[Warning]{dictionary}
    The @class{gtk:file-chooser} implementation is deprecated since 4.10. Use
    the @class{gtk:file-dialog} object instead.
  @end{dictionary}
  @see-slot{gtk:file-chooser-action}
  @see-slot{gtk:file-chooser-create-folders}
  @see-slot{gtk:file-chooser-filter}
  @see-slot{gtk:file-chooser-filters}
  @see-slot{gtk:file-chooser-select-multiple}
  @see-slot{gtk:file-chooser-shortcut-folders}
  @see-class{gtk:file-chooser-dialog}
  @see-class{gtk:file-chooser-widget}
  @see-class{gtk:file-dialog}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;: --- gtk:file-chooser-action ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "action" 'file-chooser) t)
 "The @code{action} property of type @symbol{gtk:file-chooser-action}
  (Read / Write) @br{}
  The type of operation that the file selector is performing. @br{}
  Default value: @code{:open}")

#+liber-documentation
(setf (liber:alias-for-function 'file-chooser-action)
      "Accessor"
      (documentation 'file-chooser-action 'function)
 "@version{2024-4-26}
  @syntax{(gtk:file-chooser-action object) => action}
  @syntax{(setf (gtk:file-chooser-action object) action)}
  @argument[object]{a @class{gtk:file-chooser} widget}
  @argument[action]{a @symbol{gtk:file-chooser-action} value}
  @begin{short}
    Accessor of the @slot[gtk:file-chooser]{action} slot of the
    @class{gtk:file-chooser} interface.
  @end{short}
  The @fun{gtk:file-chooser-action} function gets the type of operation that
  the file chooser is performing. The @setf{gtk:file-chooser-action} function
  sets the type of operation. The user interface is adapted to suit the selected
  action. For example, an option to create a new folder might be shown if the
  action is @code{:save} but not if the action is @code{:open}.
  @begin[Warning]{dictionary}
    The @class{gtk:file-chooser} implementation is deprecated since 4.10. Use
    the @class{gtk:file-dialog} object instead.
  @end{dictionary}
  @see-class{gtk:file-chooser}
  @see-class{gtk:file-dialog}
  @see-symbol{gtk:file-chooser-action}")

;;; --- gtk:file-chooser-create-folders ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "create-folders"
                                               'file-chooser) t)
 "The @code{create-folders} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether a file chooser not in @code{:open} mode will offer the user to create
  new folders. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'file-chooser-create-folders)
      "Accessor"
      (documentation 'file-chooser-create-folders 'function)
 "@version{2024-4-26}
  @syntax{(gtk:file-chooser-create-folders object) => setting}
  @syntax{(setf (gtk:file-chooser-create-folders object) setting)}
  @argument[object]{a @class{gtk:file-chooser} widget}
  @argument[setting]{@em{true} if the @kbd{New Folder} button should be
    displayed}
  @begin{short}
    Accessor of the @slot[gtk:file-chooser]{create-folders} slot of the
    @class{gtk:file-chooser} interface.
  @end{short}
  The @fun{gtk:file-chooser-create-folders} function gets whether the file
  chooser will offer to create new folders. The
  @setf{gtk:file-chooser-create-folders} function sets whether the file chooser
  will offer to create new folders. This is only relevant if the action of the
  file chooser is not set to be in @code{:open} mode.
  @begin[Warning]{dictionary}
    The @class{gtk:file-chooser} implementation is deprecated since 4.10. Use
    the @class{gtk:file-dialog} object instead.
  @end{dictionary}
  @see-class{gtk:file-chooser}
  @see-class{gtk:file-dialog}
  @see-symbol{gtk:file-chooser-action}")

;;; --- gtk:file-chooser-filter ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "filter" 'file-chooser) t)
 "The @code{filter} property of type @class{gtk:file-filter} (Read / Write)
  @br{}
  The current filter for selecting which files are displayed.")

#+liber-documentation
(setf (liber:alias-for-function 'file-chooser-filter)
      "Accessor"
      (documentation 'file-chooser-filter 'function)
 "@version{2024-4-26}
  @syntax{(gtk:file-chooser-filter object) => filter}
  @syntax{(setf (gtk:file-chooser-filter object) filter)}
  @argument[object]{a @class{gtk:file-chooser} widget}
  @argument[filter]{a @class{gtk:file-filter} object}
  @begin{short}
    Accessor of the @slot[gtk:file-chooser]{filter} slot of the
    @class{gtk:file-chooser} interface.
  @end{short}
  The @fun{gtk:file-chooser-filter} function gets the current filter. The
  @setf{gtk:file-chooser-filter} function sets the current filter. Only the
  files that pass the filter will be displayed.

  If the user selectable list of filters is non-empty, then the filter should
  be one of the filters in that list. Setting the current filter when the list
  of filters is empty is useful if you want to restrict the displayed set of
  files without letting the user change it.
  @begin[Warning]{dictionary}
    The @class{gtk:file-chooser} implementation is deprecated since 4.10. Use
    the @class{gtk:file-dialog} object instead.
  @end{dictionary}
  @see-class{gtk:file-chooser}
  @see-class{gtk:file-filter}
  @see-class{gtk:file-dialog}")

;;; --- gtk:file-chooser-filters -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "filters" 'file-chooser) t)
 "The @code{filters} property of type @class{g:list-model} (Read) @br{}
  The list model containing the filters that have been added with the
  @fun{gtk:file-chooser-add-filter} function.")

#+liber-documentation
(setf (liber:alias-for-function 'file-chooser-filters)
      "Accessor"
      (documentation 'file-chooser-filters 'function)
 "@version{2024-5-20}
  @syntax{(gtk:file-chooser-filters object) => filters}
  @argument[object]{a @class{gtk:file-chooser} widget}
  @argument[filters]{a @class{g:list-model} object containing the current set
    of user-selectable filters}
  @begin{short}
    Accessor of the @slot[gtk:file-chooser]{filters} slot of the
    @class{gtk:file-chooser} interface.
  @end{short}
  The @fun{gtk:file-chooser-filters} function gets the current set of
  user-selectable filters, as a list model. See the
  @fun{gtk:file-chooser-add-filter}, and @fun{gtk:file-chooser-remove-filter}
  functions.
  @begin[Warning]{dictionary}
    The @class{gtk:file-chooser} implementation is deprecated since 4.10. Use
    the @class{gtk:file-dialog} object instead.
  @end{dictionary}
  @see-class{gtk:file-chooser}
  @see-class{g:list-model}
  @see-class{gtk:file-dialog}
  @see-function{gtk:file-chooser-add-filter}
  @see-function{gtk:file-chooser-remove-filter}")

;;; --- gtk:file-chooser-select-multiple ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "select-multiple"
                                               'file-chooser) t)
 "The @code{select-multiple} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether to allow multiple files to be selected. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'file-chooser-select-multiple)
      "Accessor"
      (documentation 'file-chooser-select-multiple 'function)
 "@version{2024-4-26}
  @syntax{(gtk:file-chooser-select-multiple object) => setting}
  @syntax{(setf (gtk:file-chooser-select-multiple object) setting)}
  @argument[object]{a @class{gtk:file-chooser} widget}
  @argument[setting]{@em{true} if multiple files can be selected}
  @begin{short}
    Accessor of the @slot[gtk:file-chooser]{select-multiple} slot of the
    @class{gtk:file-chooser} interface.
  @end{short}
  The @fun{gtk:file-chooser-select-multiple} function gets whether multiple
  files can be selected in the file selector. The
  @setf{gtk:file-chooser-select-multiple} function sets whether multiple files
  can be selected. This is only relevant if the action of the file chooser is
  set to be in @code{:open} or @code{:select-folder} mode.
  @begin[Warning]{dictionary}
    The @class{gtk:file-chooser} implementation is deprecated since 4.10. Use
    the @class{gtk:file-dialog} object instead.
  @end{dictionary}
  @see-class{gtk:file-chooser}
  @see-class{gtk:file-dialog}
  @see-symbol{gtk:file-chooser-action}")

;;; --- gtk:file-chooser-shortcut-folders --------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "shortcut-folders"
                                               'file-chooser) t)
 "The @code{shortcut-folders} property of type @class{g:list-model} (Read) @br{}
  The list model containing the shortcut folders that have been added with the
  @fun{gtk:file-chooser-add-shortcut-folder} function.")

#+liber-documentation
(setf (liber:alias-for-function 'file-chooser-shortcut-folders)
      "Accessor"
      (documentation 'file-chooser-shortcut-folders 'function)
 "@version{2024-5-20}
  @syntax{(gtk:file-chooser-shortcut-folders object) => folders}
  @argument[object]{a @class{gtk:file-chooser} widget}
  @argument[folders]{a @class{g:list-model} object of @class{g:file} objects}
  @begin{short}
    Accessor of the @slot[gtk:file-chooser]{shortcut-folders} slot of the
    @class{gtk:file-chooser} interface.
  @end{short}
  The @fun{gtk:file-chooser-shortcut-folders} function queries the list of
  shortcut folders in the file chooser, as set by the
  @fun{gtk:file-chooser-add-shortcut-folder} function.
  @begin[Warning]{dictionary}
    The @class{gtk:file-chooser} implementation is deprecated since 4.10. Use
    the @class{gtk:file-dialog} object instead.
  @end{dictionary}
  @see-class{gtk:file-chooser}
  @see-class{g:list-model}
  @see-class{g:file}
  @see-class{gtk:file-dialog}
  @see-symbol{gtk:file-chooser-add-shortcut-folder}")

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_get_current_name
;;; gtk_file_chooser_set_current_name
;;; ----------------------------------------------------------------------------

(defun (setf file-chooser-current-name) (name chooser)
  (cffi:foreign-funcall "gtk_file_chooser_set_current_name"
                        (g:object file-chooser) chooser
                        (:string :free-to-foreign t :encoding :utf-8) name
                        :void)
  name)

(cffi:defcfun ("gtk_file_chooser_get_current_name" file-chooser-current-name)
    (:string :free-from-foreign t :encoding :utf-8)
 #+liber-documentation
 "@version{2024-4-26}
  @syntax{(gtk:file-chooser-current-name chooser) => name}
  @syntax{(setf (gtk:file-chooser-current-name chooser) name)}
  @argument[chooser]{a @class{gtk:file-chooser} widget}
  @argument[name]{a string with the filename to use, as a UTF-8 string}
  @begin{short}
    The @fun{gtk:file-chooser-current-name} function gets the current name in
    the file selector, as entered by the user in the text entry for \"Name\".
  @end{short}
  The @setf{gtk:file-chooser-current-name} function sets the current name.

  This is meant to be used in save dialogs, to get the currently typed filename
  when the file itself does not exist yet. For example, an application that adds
  a custom extra widget to the file chooser for \"file format\" may want to
  change the extension of the typed filename based on the chosen format, say,
  from @file{\".jpg\"} to @file{\".png\"}.

  Note that the name passed in here is a UTF-8 string rather than a filename.
  This function is meant for such uses as a suggested name in a \"Save As...\"
  dialog. You can pass @file{\"Untitled.doc\"} or a similarly suitable
  suggestion for the name.

  If you want to preselect a particular existing file, you should use the
  @fun{gtk:file-chooser-file} function instead. Please see the documentation for
  this function for an example of using the @fun{gtk:file-chooser-current-name}
  function as well.
  @begin[Warning]{dictionary}
    The @class{gtk:file-chooser} implementation is deprecated since 4.10. Use
    the @class{gtk:file-dialog} object instead.
  @end{dictionary}
  @see-class{gtk:file-chooser}
  @see-class{gtk:file-dialog}
  @see-function{gtk:file-chooser-file}"
  (chooser (g:object file-chooser)))

(export 'file-chooser-current-name)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_get_file
;;; gtk_file_chooser_set_file
;;; ----------------------------------------------------------------------------

(defun (setf file-chooser-file) (file chooser)
  (glib:with-g-error (err)
    (cffi:foreign-funcall "gtk_file_chooser_set_file"
                          (g:object file-chooser) chooser
                          g:object file
                          :pointer err
                          :boolean)
     file))

(cffi:defcfun ("gtk_file_chooser_get_file" file-chooser-file) g:object
 #+liber-documentation
 "@version{2024-4-26}
  @syntax{(gtk:file-chooser-file chooser) => file}
  @syntax{(setf (gtk:file-chooser-file chooser) file)}
  @argument[chooser]{a @class{gtk:file-chooser} widget}
  @argument[file]{a @class{g:file} object for the file}
  @begin{short}
    The @fun{gtk:file-chooser-file} function gets the @class{g:file} object for
    the currently selected file in the file chooser.
  @end{short}
  If multiple files are selected, one of the files will be returned at random.
  If the file chooser is in folder mode, this function returns the selected
  folder.

  The @setf{gtk:file-chooser-file} function sets @arg{file} as the current
  filename for the file chooser, by changing to the parent folder of the file
  and actually selecting the file. If the file chooser is in @code{:save} mode,
  the base name of the file will also appear in the file name entry of the
  dialog.

  If the file name is not in the current folder of the file chooser, then the
  current folder of the file chooser will be changed to the folder containing
  @arg{file}.

  Note that the file must exist, or nothing will be done except for the
  directory change.
  @begin[Examples]{dictionary}
    If you are implementing a save dialog, you should use this function if you
    already have a file name to which the user may save. For example, when the
    user opens an existing file and then does \"File/Save As...\" on it. If you
    do not have a file name already - for example, if the user just created a
    new file and is saving it for the first time, do not call this function.
    Instead, use something similar to this:
    @begin{pre}
(if document-is-new
    (progn
      ;; the user just created a new document
      (setf (gtk:file-chooser-current-folder-file chooser)
            default-file-for-saving)
      (setf (gtk:file-chooser-current-name chooser) \"Untitled document\"))
      (progn
        ;; the user edited an existing document
        (setf (gtk:file-chooser-file chooser) existing-file)))
    @end{pre}
  @end{dictionary}
  @begin[Warning]{dictionary}
    The @class{gtk:file-chooser} implementation is deprecated since 4.10. Use
    the @class{gtk:file-dialog} object instead.
  @end{dictionary}
  @see-class{gtk:file-chooser}
  @see-class{g:file}
  @see-class{gtk:file-dialog}"
  (chooser (g:object file-chooser)))

(export 'file-chooser-file)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_namestring
;;; ----------------------------------------------------------------------------

(defun (setf file-chooser-namestring) (path chooser)
  (glib:with-g-error (err)
    (let ((file (cffi:convert-to-foreign path 'g:file-as-namestring)))
      (cffi:foreign-funcall "gtk_file_chooser_set_file"
                            (g:object file-chooser) chooser
                            g:object file
                            :pointer err
                            :boolean)
       path)))

(cffi:defcfun ("gtk_file_chooser_get_file" file-chooser-namestring)
    g:file-as-namestring
 #+liber-documentation
 "@version{2024-10-12}
  @syntax{(gtk:file-chooser-namestring chooser) => namestring}
  @syntax{(setf (gtk:file-chooser-namestring chooser) namestring)}
  @argument[chooser]{a @class{gtk:file-chooser} widget}
  @argument[namestring]{a pathname or a namestring for the file}
  @begin{short}
    The @fun{gtk:file-chooser-namestring} function gets the namestring for the
    currently selected file in the file selector.
  @end{short}
  If multiple files are selected, one of the files will be returned at random.
  If the file chooser is in folder mode, this function returns the selected
  folder.

  The @setf{gtk:file-chooser-namestring} function sets @arg{namestring} as the
  current file for the file chooser, by changing to the file's parent folder
  and actually selecting the file in list. If the chooser is in @code{:save}
  mode, the file's base name will also appear in the dialog's file name entry.
  @begin[Lisp implementation]{dictionary}
   This function corresponds to the @fun{gtk:file-chooser-file} function, but
   has a Lisp namestring as an argument for the file. The type conversion
   between a @class{g:file} object and a namestring is automatically done with
   the @type{g:file-as-namestring} type specifier.
  @end{dictionary}
  @begin[Warning]{dictionary}
    The @class{gtk:file-chooser} implementation is deprecated since 4.10. Use
    the @class{gtk:file-dialog} object instead.
  @end{dictionary}
  @see-class{gtk:file-chooser}
  @see-class{g:file}
  @see-class{gtk:file-dialog}
  @see-type{g:file-as-namestring}"
  (chooser (g:object file-chooser)))

(export 'file-chooser-namestring)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_get_files
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_file_chooser_get_files" file-chooser-files)
    (g:object g:list-model)
 #+liber-documentation
 "@version{2023-8-22}
  @argument[chooser]{a @class{gtk:file-chooser} widget}
  @begin{return}
    The @class{g:list-model} object containing a @class{g:file} object for each
    selected file and subfolder in the current folder.
  @end{return}
  @begin{short}
    Lists all the selected files and subfolders in the current folder of
    the file chooser as a list of @class{g:file} objects.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:file-chooser} implementation is deprecated since 4.10. Use
    the @class{gtk:file-dialog} object instead.
  @end{dictionary}
  @see-class{gtk:file-chooser}
  @see-class{g:file}
  @see-class{gtk:file-dialog}"
  (chooser (g:object file-chooser)))

(export 'file-chooser-files)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_get_current_folder
;;; gtk_file_chooser_set_current_folder
;;; ----------------------------------------------------------------------------

(defun (setf file-chooser-current-folder) (path chooser)
  (glib:with-g-error (err)
    (let ((file (cffi:convert-to-foreign (namestring path)
                                         'g:file-as-namestring)))
      (cffi:foreign-funcall "gtk_file_chooser_set_current_folder"
                            (g:object file-chooser) chooser
                            g:object file
                            :pointer err
                            :boolean)
      path)))

(cffi:defcfun ("gtk_file_chooser_get_current_folder"
               file-chooser-current-folder) g:file-as-namestring
 #+liber-documentation
 "@version{2024-10-12}
  @syntax{(gtk:file-chooser-current-folder chooser) => namestring}
  @syntax{(setf (gtk:file-chooser-current-folder chooser) namestring)}
  @argument[chooser]{a @class{gtk:file-chooser} widget}
  @argument[path]{a pathname or namestring with the full path of the new
    current folder}
  @begin{short}
    The @fun{gtk:file-chooser-current-folder} function gets the current folder
    of the file chooser as a local filename.
  @end{short}
  The @setf{gtk:file-chooser-current-folder} function sets the current folder.

  The user will be shown the full contents of the current folder, plus user
  interface elements for navigating to other folders.

  Note that this is the folder that the file chooser is currently displaying,
  for example, @file{/home/username/Documents}, which is not the same as the
  currently selected folder if the chooser is in @code{:select-folder} mode,
  for example, @file{/home/username/Documents/selected-folder/}.

  In general, you should not use this function. See the section on setting up
  a file chooser dialog for the rationale behind this.
  @begin[Warning]{dictionary}
    The @class{gtk:file-chooser} implementation is deprecated since 4.10. Use
    the @class{gtk:file-dialog} object instead.
  @end{dictionary}
  @see-class{gtk:file-chooser}
  @see-class{gtk:file-dialog}"
  (chooser (g:object file-chooser)))

(export 'file-chooser-current-folder)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_add_filter
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_file_chooser_add_filter" file-chooser-add-filter) :void
 #+liber-documentation
 "@version{2023-8-22}
  @argument[chooser]{a @class{gtk:file-chooser} widget}
  @argument[filter]{a @class{gtk:file-filter} object}
  @begin{short}
    Adds a file filter to the list of filters that the user can select between.
  @end{short}
  When a file filter is selected, only files that are passed by that filter are
  displayed.
  @begin[Warning]{dictionary}
    The @class{gtk:file-chooser} implementation is deprecated since 4.10. Use
    the @class{gtk:file-dialog} object instead.
  @end{dictionary}
  @see-class{gtk:file-chooser}
  @see-class{gtk:file-filter}
  @see-class{gtk:file-dialog}
  @see-function{gtk:file-chooser-remove-filter}"
  (chooser (g:object file-chooser))
  (filter (g:object file-filter)))

(export 'file-chooser-add-filter)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_remove_filter
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_file_chooser_remove_filter" file-chooser-remove-filter)
    :void
 #+liber-documentation
 "@version{#2023-8-22}
  @argument[chooser]{a @class{gtk:file-chooser} widget}
  @argument[filter]{a @class{gtk:file-filter} object}
  @begin{short}
    Removes a filter from the list of filters that the user can select between.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:file-chooser} implementation is deprecated since 4.10. Use
    the @class{gtk:file-dialog} object instead.
  @end{dictionary}
  @see-class{gtk:file-chooser}
  @see-class{gtk:file-filter}
  @see-class{gtk:file-dialog}
  @see-function{gtk:file-chooser-add-filter}"
  (chooser (g:object file-chooser))
  (filter (g:object file-filter)))

(export 'file-chooser-remove-filter)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_add_shortcut_folder
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_file_chooser_add_shortcut_folder"
               %file-chooser-add-shortcut-folder) :boolean
  (chooser (g:object file-chooser))
  (folder g:file-as-namestring)
  (err :pointer))

(defun file-chooser-add-shortcut-folder (chooser folder)
 #+liber-documentation
 "@version{#2023-8-22}
  @argument[chooser]{a @class{gtk:file-chooser} widget}
  @argument[folder]{a namestring with a filename of the folder to add}
  @begin{return}
    @em{True} if the folder could be added successfully, @em{false} otherwise.
  @end{return}
  @begin{short}
    Adds a folder to be displayed with the shortcut folders in a file chooser.
  @end{short}
  Note that shortcut folders do not get saved, as they are provided by the
  application. For example, you can use this to add a
  @file{\"/usr/share/mydrawprogram/Clipart\"} folder to the volume list.
  @begin[Warning]{dictionary}
    The @class{gtk:file-chooser} implementation is deprecated since 4.10. Use
    the @class{gtk:file-dialog} object instead.
  @end{dictionary}
  @see-class{gtk:file-chooser}
  @see-class{gtk:file-dialog}
  @see-function{gtk:file-chooser-remove-shortcut-folder}"
  (glib:with-g-error (err)
    (%file-chooser-add-shortcut-folder chooser folder err)))

(export 'file-chooser-add-shortcut-folder)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_remove_shortcut_folder
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_file_chooser_remove_shortcut_folder"
               %file-chooser-remove-shortcut-folder) :boolean
  (chooser (g:object file-chooser))
  (folder g:file-as-namestring)
  (err :pointer))

(defun file-chooser-remove-shortcut-folder (chooser folder)
 #+liber-documentation
 "@version{#2023-8-22}
  @argument[chooser]{a @class{gtk:file-chooser} widget}
  @argument[folder]{a namestring with the filename of the folder to remove}
  @begin{return}
    @em{True} if the operation succeeds, @em{false} otherwise.
  @end{return}
  @begin{short}
    Removes a folder from a file chooser's list of shortcut folders.
  @end{short}
  See also the @fun{gtk:file-chooser-add-shortcut-folder} function.
  @begin[Warning]{dictionary}
    The @class{gtk:file-chooser} implementation is deprecated since 4.10. Use
    the @class{gtk:file-dialog} object instead.
  @end{dictionary}
  @see-class{gtk:file-chooser}
  @see-class{gtk:file-dialog}
  @see-function{gtk:file-chooser-add-shortcut-folder}"
  (glib:with-g-error (err)
    (%file-chooser-remove-shortcut-folder chooser folder err)))

(export 'file-chooser-remove-shortcut-folder)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_add_choice ()
;;;
;;; void
;;; gtk_file_chooser_add_choice (GtkFileChooser *chooser,
;;;                              const char *id,
;;;                              const char *label,
;;;                              const char **options,
;;;                              const char **option_labels);
;;;
;;; Adds a 'choice' to the file chooser. This is typically implemented as a
;;; combobox or, for boolean choices, as a checkbutton. You can select a value
;;; using gtk_file_chooser_set_choice() before the dialog is shown, and you can
;;; obtain the user-selected value in the ::response signal handler using
;;; gtk_file_chooser_get_choice().
;;;
;;; chooser :
;;;     a GtkFileChooser
;;;
;;; id :
;;;     id for the added choice
;;;
;;; label :
;;;     user-visible label for the added choice
;;;
;;; options :
;;;     ids for the options of the choice, or NULL for a boolean choice.
;;;
;;; option_labels :
;;;     user-visible labels for the options, must be the same length as options
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_remove_choice ()
;;;
;;; void
;;; gtk_file_chooser_remove_choice (GtkFileChooser *chooser,
;;;                                 const char *id);
;;;
;;; Removes a 'choice' that has been added with gtk_file_chooser_add_choice().
;;;
;;; chooser :
;;;     a GtkFileChooser
;;;
;;; id :
;;;     the ID of the choice to remove
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_set_choice ()
;;;
;;; void
;;; gtk_file_chooser_set_choice (GtkFileChooser *chooser,
;;;                              const char *id,
;;;                              const char *option);
;;;
;;; Selects an option in a 'choice' that has been added with
;;; gtk_file_chooser_add_choice(). For a boolean choice, the possible options
;;; are "true" and "false".
;;;
;;; chooser :
;;;     a GtkFileChooser
;;;
;;; id :
;;;     the ID of the choice to set
;;;
;;; option :
;;;     the ID of the option to select
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_get_choice ()
;;;
;;; const char *
;;; gtk_file_chooser_get_choice (GtkFileChooser *chooser,
;;;                              const char *id);
;;;
;;; Gets the currently selected option in the 'choice' with the given ID.
;;;
;;; chooser :
;;;     a GtkFileChooser
;;;
;;; id :
;;;     the ID of the choice to get
;;;
;;; Returns :
;;;     the ID of the currently selected option
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk4.file-chooser.lisp -------------------------------------

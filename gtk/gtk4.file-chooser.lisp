;;; ----------------------------------------------------------------------------
;;; gtk.file-chooser.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2012 - 2022 Dieter Kaiser
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
;;; GtkFileChooser
;;;
;;;     File chooser interface used by GtkFileChooserWidget and
;;;     GtkFileChooserDialog
;;;
;;; Types and Values
;;;
;;;     GtkFileChooser
;;;
;;;     GtkFileChooserAction
;;;     GTK_FILE_CHOOSER_ERROR
;;;     GtkFileChooserError
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
;;; enum GtkFileChooserAction
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkFileChooserAction" file-chooser-action
  (:export t
   :type-initializer "gtk_file_chooser_action_get_type")
  (:open 0)
  (:save 1)
  (:select-folder 2))

#+liber-documentation
(setf (liber:alias-for-symbol 'file-chooser-action)
      "GEnum"
      (liber:symbol-documentation 'file-chooser-action)
 "@version{#2021-1-28}
  @begin{short}
    Describes whether a @class{gtk:file-chooser} widget is being used to
    open existing files or to save to a possibly new file.
  @end{short}
  @begin{pre}
(define-g-enum \"GtkFileChooserAction\" gtk:file-chooser-action
  (:export t
   :type-initializer \"gtk_file_chooser_action_get_type\")
  (:open 0)
  (:save 1)
  (:select-folder 2))
  @end{pre}
  @begin[code]{table}
    @entry[:open]{Indicates Open mode. The file chooser will only let the user
      pick an existing file.}
    @entry[:save]{Indicates Save mode. The file chooser will let the user pick
      an existing file, or type in a new filename.}
    @entry[:select-folder]{Indicates an Open mode for selecting folders. The
      file chooser will let the user pick an existing folder.}
  @end{table}
  @see-class{gtk:file-chooser}")

;;; ----------------------------------------------------------------------------
;;; GTK_FILE_CHOOSER_ERROR
;;;
;;; #define GTK_FILE_CHOOSER_ERROR (gtk_file_chooser_error_quark ())
;;;
;;; Used to get the GError quark for GtkFileChooser errors.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GtkFileChooserError
;;;
;;; These identify the various errors that can occur while calling
;;; GtkFileChooser functions.
;;;
;;; GTK_FILE_CHOOSER_ERROR_NONEXISTENT
;;;     Indicates that a file does not exist.
;;;
;;; GTK_FILE_CHOOSER_ERROR_BAD_FILENAME
;;;     Indicates a malformed filename.
;;;
;;; GTK_FILE_CHOOSER_ERROR_ALREADY_EXISTS
;;;     Indicates a duplicate path (e.g. when adding a bookmark).
;;;
;;; GTK_FILE_CHOOSER_ERROR_INCOMPLETE_HOSTNAME
;;;     Indicates an incomplete hostname (e.g. "http://foo" without a slash
;;;     after that).
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkFileChooser
;;; ----------------------------------------------------------------------------

(define-g-interface "GtkFileChooser" file-chooser
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

#+liber-documentation
(setf (liber:alias-for-class 'file-chooser)
      "Interface"
      (documentation 'file-chooser 'type)
 "@version{#2021-2-4}
  @begin{short}
    The @sym{gtk:file-chooser} interface is an interface that can be
    implemented by file selection widgets.
  @end{short}
  The main widgets that implement this interface are the
  @class{gtk:file-chooser-widget} and @class{gtk:file-chooser-dialog} widgets.
  You do not need to write a widget that implements the @sym{gtk:file-chooser}
  interface unless you are trying to adapt an existing file selector to expose
  a standard programming interface.

  The @sym{gtk:file-chooser} interface allows for shortcuts to various places
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
      abstraction, e.g. both the gnome-vfs and the Windows filesystems provide
      \"Desktop\" shortcuts. Shortcuts cannot be modified by the user.
    @end{entry}
    @begin[Volumes]{entry}
      are provided by the underlying filesystem abstraction. Volumes are the
      \"roots\" of the filesystem.
    @end{entry}
  @end{table}
  @subheading{File Names and Encodings}
    When the user is finished selecting files in a @sym{gtk:file-chooser}
    widget, the program can get the selected filenames as @class{g:file}
    objects.

  @subheading{Adding options}
    You can add extra widgets to a file chooser to provide options that are not
    present in the default design, by using the
    @fun{gtk:file-chooser-add-choice} function. Each choice has an identifier
    and a user visible label. Additionally, each choice can have multiple
    options. If a choice has no option, it will be rendered as a check button
    with the given label. If a choice has options, it will be rendered as a
    combo box.

    @b{Example:} Sample Usage
    @begin{pre}
(defun create-file-chooser-preview ()
  (let ((response nil)
        (preview-width 256)
        (preview-height 256)
        (chooser (gtk:file-chooser-dialog-new \"Example File Chooser Preview\"
                                              nil
                                              :open
                                              \"gtk-open\" :accept
                                              \"gtk-cancel\" :cancel))
        (preview (make-instance 'gtk:image
                                :margin 24)))
    ;; Handler for the signal \"upadate-preview\"
    (g:signal-connect chooser \"update-preview\"
        (lambda (chooser)
          (let* ((filename (gtk:file-chooser-preview-filename chooser))
                 (pixbuf (when filename
                           (gdk-pixbuf:pixbuf-new-from-file-at-size filename
                                                             preview-width
                                                             preview-height))))
            (if pixbuf
                (progn
                  (gtk:image-set-from-pixbuf preview pixbuf)
                  (setf (gtk:file-chooser-preview-widget-active chooser) t))
                (setf (gtk:file-chooser-preview-widget-active chooser) nil)))))
    ;; Set the preview widget
    (setf (gtk:file-chooser-preview-widget chooser) preview)
    ;; Run the file chooser dialog
    (when (eq :accept
              (setf response
                    (gtk:dialog-run chooser)))
      (format t \"Save to file ~A~%\"
                (gtk:file-chooser-filename chooser)))
    (gtk:widget-destroy chooser)
    response))
    @end{pre}
    @b{Example:} Sample Usage
    @begin{pre}
(defun create-file-chooser-widget ()
  (let ((response nil)
        (chooser (gtk:file-chooser-dialog-new \"Example File Chooser Widget\"
                                              nil
                                              :open
                                              \"gtk-open\" :accept
                                              \"gtk-cancel\" :cancel))
        (extra-widget (make-instance 'gtk:box
                                     :orientation :horizontal
                                     :spacing 12))
        (local-only (gtk:check-button-new-with-label \"Local only\"))
        (select-multiple (gtk:check-button-new-with-label \"Select Multiple\"))
        (show-hidden (gtk:check-button-new-with-label \"Show hidden\")))
    ;; Connect signal handlers to the toggle buttons
    (g:signal-connect local-only \"toggled\"
                      (lambda (button)
                        (setf (gtk:file-chooser-local-only chooser)
                              (gtk:toggle-button-active button))))
    (g:signal-connect select-multiple \"toggled\"
                      (lambda (button)
                        (setf (gtk:file-chooser-select-multiple chooser)
                              (gtk:toggle-button-active button))))
    (g:signal-connect show-hidden \"toggled\"
                      (lambda (button)
                        (setf (gtk:file-chooser-show-hidden chooser)
                              (gtk:toggle-button-active button))))
    ;; Put the extra widgets in a box
    (gtk:box-pack-start extra-widget local-only)
    (setf (gtk:toggle-button-active local-only) t) ; default is true
    (gtk:box-pack-start extra-widget select-multiple)
    (gtk:box-pack-start extra-widget show-hidden)
    (setf (gtk:file-chooser-extra-widget chooser) extra-widget)
    ;; Show the extra widgets
    (gtk:widget-show-all extra-widget)
    ;; Run the file chooser dialog
    (when (eq :accept
              (setf response
                    (gtk:dialog-run chooser)))
      (format t \"Open file ~A~%\"
                (gtk:file-chooser-filename chooser)))
    (gtk:widget-destroy chooser)
    response))
    @end{pre}
    If you want to set more than one extra widget in the file chooser, you can
    add a container such as a @class{gtk:box} or a @class{gtk:grid} widget and
    include your widgets in it. Then, set the container as the whole extra
    widget.
  @see-slot{gtk:file-chooser-action}
  @see-slot{gtk:file-chooser-create-folders}
  @see-slot{gtk:file-chooser-filter}
  @see-slot{gtk:file-chooser-filters}
  @see-slot{gtk:file-chooser-select-multiple}
  @see-slot{gtk:file-chooser-shortcut-folders}
  @see-class{gtk:file-chooser-dialog}
  @see-class{gtk:file-chooser-widget}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;: --- file-chooser-action ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "action" 'file-chooser) t)
 "The @code{action} property of type @symbol{gtk:file-chooser-action}
  (Read / Write) @br{}
  The type of operation that the file selector is performing. @br{}
  Default value: @code{:action-open}")

#+liber-documentation
(setf (liber:alias-for-function 'file-chooser-action)
      "Accessor"
      (documentation 'file-chooser-action 'function)
 "@version{#2021-2-5}
  @syntax[]{(gtk:file-chooser-action object) => action}
  @syntax[]{(setf (gtk:file-chooser-action object) action)}
  @argument[object]{a @class{gtk:file-chooser} widget}
  @argument[action]{a value of the @symbol{gtk:file-chooser-action} enumeration}
  @begin{short}
    Accessor of the @slot[gtk:file-chooser]{action} slot of the
    @class{gtk:file-chooser} interface.
  @end{short}

  The @sym{gtk:file-chooser-action} function gets the type of operation that the
  file chooser is performing. The @sym{(setf gtk:file-chooser-action)} function
  sets the type of operation. The user interface is adapted to suit the selected
  action. For example, an option to create a new folder might be shown if the
  action is @code{:save} but not if the action is @code{:open}.
  @see-class{gtk:file-chooser}
  @see-symbol{gtk:file-chooser-action}")

;;; --- file-chooser-create-folders ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "create-folders"
                                               'file-chooser) t)
 "The @code{create-folders} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether a file chooser not in @code{:action-open} mode will offer the user to
  create new folders. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'file-chooser-create-folders)
      "Accessor"
      (documentation 'file-chooser-create-folders 'function)
 "@version{#2021-2-5}
  @syntax[]{(gtk:file-chooser-create-folders object) => create-folders}
  @syntax[]{(setf (gtk:file-chooser-create-folders object) create-folders)}
  @argument[object]{a @class{gtk:file-chooser} widget}
  @argument[create-folders]{@em{true} if the New Folder button should be
    displayed}
  @begin{short}
    Accessor of the @slot[gtk:file-chooser]{create-folders} slot of the
    @class{gtk:file-chooser} interface.
  @end{short}

  The @sym{gtk:file-chooser-create-folders} function gets whether the file
  chooser will offer to create new folders. The
  @sym{(setf gtk:file-chooser-create-folders)} function sets whether the file
  chooser will offer to create new folders. This is only relevant if the action
  of the file chooser is not set to be @code{:open}.
  @see-class{gtk:file-chooser}
  @see-symbol{gtk:file-chooser-action}")

;;; --- file-chooser-filter ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "filter" 'file-chooser) t)
 "The @code{filter} property of type @class{gtk:file-filter} (Read / Write)
  @br{}
  The current filter for selecting which files are displayed.")

#+liber-documentation
(setf (liber:alias-for-function 'file-chooser-filter)
      "Accessor"
      (documentation 'file-chooser-filter 'function)
 "@version{#2021-1-28}
  @syntax[]{(gtk:file-chooser-filter object) => filter}
  @syntax[]{(setf (gtk:file-chooser-filter object) filter)}
  @argument[object]{a @class{gtk:file-chooser} widget}
  @argument[filter]{a @class{gtk:file-filter} object}
  @begin{short}
    Accessor of the @slot[gtk:file-chooser]{filter} slot of the
    @class{gtk:file-chooser} interface.
  @end{short}

  The @sym{gtk:file-chooser-filter} function gets the current filter. The
  @sym{(setf gtk:file-chooser-filter)} function sets the current filter. Only
  the files that pass the filter will be displayed.

  If the user-selectable list of filters is non-empty, then the filter should be
  one of the filters in that list. Setting the current filter when the list of
  filters is empty is useful if you want to restrict the displayed set of files
  without letting the user change it.
  @see-class{gtk:file-chooser}
  @see-class{gtk:file-filter}")

;;; --- file-chooser-filters -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "filter" 'file-chooser) t)
 "The @code{filters} property of type @class{g:list-model} (Read) @br{}
  A @class{g:list-model} object containing the filters that have been added with
  the @fun{gtk:file-chooser-add-filter} function. The returned object should not
  be modified. It may or may not be updated for later changes.")

#+liber-documentation
(setf (liber:alias-for-function 'file-chooser-filters)
      "Accessor"
      (documentation 'file-chooser-filters 'function)
 "@version{#2022-6-19}
  @syntax[]{(gtk:file-chooser-filters object) => filters}
  @argument[object]{a @class{gtk:file-chooser} widget}
  @argument[filters]{a @class{g:list-model} object containing the current set
    of user-selectable filters}
  @begin{short}
    Accessor of the @slot[gtk:file-chooser]{filters} slot of the
    @class{gtk:file-chooser} interface.
  @end{short}

  The @sym{gtk:file-chooser-filters} function gets the current set of
  user-selectable filters, as a list model. See the
  @fun{gtk:file-chooser-add-filter}, and @fun{gtk:file-chooser-remove-filter}
  functions. You should not modify the returned list model. Future changes to
  chooser may or may not affect the returned model.
  @see-class{gtk:file-chooser}
  @see-function{gtk:file-chooser-add-filter}
  @see-function{gtk:file-chooser-remove-filter}")

;;; --- file-chooser-select-multiple ---------------------------------------

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
 "@version{#2021-2-5}
  @syntax[]{(gtk:file-chooser-select-multiple object) => select-multiple}
  @syntax[]{(setf (gtk:file-chooser-select-multiple object) select-multiple)}
  @argument[object]{a @class{gtk:file-chooser} widget}
  @argument[select-multiple]{@em{true} if multiple files can be selected}
  @begin{short}
    Accessor of the @slot[gtk:file-chooser]{select-multiple} slot of the
    @class{gtk:file-chooser} interface.
  @end{short}

  The @sym{gtk:file-chooser-select-multiple} function gets whether multiple
  files can be selected in the file selector. The
  @sym{(setf gtk:file-chooser-select-multiple)} function sets whether multiple
  files can be selected. This is only relevant if the action of the file chooser
  is set to be @code{:open} or @code{:select-folder}.
  @see-class{gtk:file-chooser}
  @see-symbol{gtk:file-chooser-action}")

;;; --- file-chooser-shortcut-folders --------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "shortcut-folders"
                                               'file-chooser) t)
 "The @code{shortcut-folders} property of type @class{g:list-model} (Read) @br{}
  A @class{g:list-model} object containing the shortcut folders that have been
  added with the @fun{gtk:file-chooser-add-shortcut-folder} function. The
  returned object should not be modified. It may or may not be updated for later
  changes.")

#+liber-documentation
(setf (liber:alias-for-function 'file-chooser-shortcut-folders)
      "Accessor"
      (documentation 'file-chooser-shortcut-folders 'function)
 "@version{#2022-6-19}
  @syntax[]{(gtk:file-chooser-shortcut-folders object) => folders}
  @argument[object]{a @class{gtk:file-chooser} widget}
  @argument[folders]{a @class{g:list-model} object of @class{g:file} objects}
  @begin{short}
    Accessor of the @slot[gtk:file-chooser]{shortcut-folders} slot of the
    @class{gtk:file-chooser} interface.
  @end{short}

  The @sym{gtk:file-chooser-shortcut-folders} function queries the list of
  shortcut folders in the file chooser, as set by the
  @fun{gtk:file-chooser-add-shortcut-folder} function. You should not modify the
  returned list model. Future changes to chooser may or may not affect the
  returned model.
  @see-class{gtk:file-chooser}
  @see-symbol{gtk:file-chooser-add-shortcut-folder}")

;;; ----------------------------------------------------------------------------
;;;gtk_file_chooser_set_create_folders ()
;;;void
;;;gtk_file_chooser_set_create_folders (GtkFileChooser *chooser,
;;;                                     gboolean create_folders);
;;;Sets whether file chooser will offer to create new folders. This is only relevant if the action is not set to be GTK_FILE_CHOOSER_ACTION_OPEN.

;;;Parameters
;;;chooser

;;;a GtkFileChooser

;;;create_folders

;;;TRUE if the Create Folder button should be displayed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;gtk_file_chooser_get_create_folders ()
;;;gboolean
;;;gtk_file_chooser_get_create_folders (GtkFileChooser *chooser);
;;;Gets whether file chooser will offer to create new folders. See gtk_file_chooser_set_create_folders().

;;;Parameters
;;;chooser

;;;a GtkFileChooser

;;;Returns
;;;TRUE if the Create Folder button should be displayed.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_get_current_name ()
;;; gtk_file_chooser_set_current_name () -> file-chooser-current-name
;;; ----------------------------------------------------------------------------

(defun (setf file-chooser-current-name) (name chooser)
  (foreign-funcall "gtk_file_chooser_set_current_name"
                   (g:object file-chooser) chooser
                   (:string :free-to-foreign t :encoding :utf-8) name
                   :void)
  name)

(defcfun ("gtk_file_chooser_get_current_name" file-chooser-current-name)
    (:string :free-from-foreign t :encoding :utf-8)
 #+liber-documentation
 "@version{#2021-2-5}
  @syntax[]{(gtk:file-chooser-current-name chooser) => name}
  @syntax[]{(setf (gtk:file-chooser-current-name chooser) name)}
  @argument[chooser]{a @class{gtk:file-chooser} widget}
  @argument[name]{a string with the filename to use, as a UTF-8 string}
  @begin{short}
    Accessor of the current name of a file chooser widget.
  @end{short}

  The @sym{gtk:file-chooser-current-name} function gets the current name in the
  file selector, as entered by the user in the text entry for \"Name\". The
  @sym{(gtk:file-chooser-current-name)} function sets the current name.

  This is meant to be used in save dialogs, to get the currently typed filename
  when the file itself does not exist yet. For example, an application that adds
  a custom extra widget to the file chooser for \"file format\" may want to
  change the extension of the typed filename based on the chosen format, say,
  from \".jpg\" to \".png\".

  Note that the name passed in here is a UTF-8 string rather than a filename.
  This function is meant for such uses as a suggested name in a \"Save As...\"
  dialog. You can pass \"Untitled.doc\" or a similarly suitable suggestion for
  the name.

  If you want to preselect a particular existing file, you should use the
  @fun{gtk:file-chooser-filename} or @fun{gtk:file-chooser-uri} functions
  instead. Please see the documentation for those functions for an
  example of using the @sym{gtk:file-chooser-current-name} function as well.
  @see-class{gtk:file-chooser}
  @see-function{gtk:file-chooser-filename}
  @see-function{gtk:file-chooser-uri}"
  (chooser (g:object file-chooser)))

(export 'file-chooser-current-name)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_get_file ()
;;; gtk_file_chooser_set_file () -> file-chooser-file
;;; ----------------------------------------------------------------------------

(defun (setf file-chooser-file) (file chooser)
  (with-g-error (err)
    (foreign-funcall "gtk_file_chooser_set_file"
                     (g:object file-chooser) chooser
                     (g:object g:file) file
                     :pointer err
                     :boolean)
     file))

(defcfun ("gtk_file_chooser_get_file" file-chooser-file) (g:object g:file)
 #+liber-documentation
 "@version{#2021-2-5}
  @syntax[]{(gtk:file-chooser-file chooser) => file}
  @syntax[]{(setf (gtk:file-chooser-file chooser) file)}
  @argument[chooser]{a @class{gtk:file-chooser} widget}
  @argument[file]{a @class{g:file} object for the file}
  @begin{short}
    Accessor of the file of the file chooser.
  @end{short}

  The @sym{gtk:file-choose-file} function gets the @class{g:file} object for
  the currently selected file in the file selector. If multiple files are
  selected, one of the files will be returned at random. If the file chooser
  is in folder mode, this function returns the selected folder.

  The @sym{gtk:file-chooser-file} function sets @arg{file} as the current
  filename for the file chooser, by changing to the file's parent folder and
  actually selecting the file in list. If the chooser is in @code{:save} mode,
  the file's base name will also appear in the dialog's file name entry.

  If the file name is not in the current folder of the file chooser, then the
  current folder of the file chooser will be changed to the folder containing
  @arg{file}. This is equivalent to a sequence of the
  @fun{gtk:file-chooser-unselect-all} function followed by the
  @fun{gtk:file-chooser-select-filename} function.

  Note that the file must exist, or nothing will be done except for the
  directory change.

  If you are implementing a File/Save As... dialog, you should use this
  function if you already have a file name to which the user may save. For
  example, when the user opens an existing file and then does File/Save As...
  on it. If you do not have a file name already - for example, if the user just
  created a new file and is saving it for the first time, do not call this
  function. Instead, use something similar to this:
  @begin{pre}
(if document-is-new
    (progn
      ;; the user just created a new document
      (setf (gtk:file-chooser-current-folder-file chooser)
            default-file-for-saving)
      (setf (gtk:file-chooser-current-name chooser \"Untitled document\")))
      (progn
        ;; the user edited an existing document
        (setf (gtk:file-chooser-file chooser) existing-file)))
  @end{pre}
  @see-class{gtk:file-chooser}
  @see-class{g:file}
  @see-function{gtk:file-chooser-unselect-all}
  @see-function{gtk:file-chooser-select-filename}
  @see-function{gtk:file-chooser-current-folder-file}"
  (chooser (g:object file-chooser)))

(export 'file-chooser-file)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_get_files () -> file-chooser-files
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_file_chooser_get_files" file-chooser-files)
    (g:slist-t (g:object g:file))
 #+liber-documentation
 "@version{#2021-2-5}
  @argument[chooser]{a @class{gtk:file-chooser} widget}
  @begin{return}
    A list containing a @class{g:file} object for each selected file and
    subfolder in the current folder.
  @end{return}
  @begin{short}
    Lists all the selected files and subfolders in the current folder of
    the file chooser as a list of @class{g:file} objects.
  @end{short}
  This is an internal function, see the @fun{gtk:file-chooser-uris} function.
  @see-class{gtk:file-chooser}
  @see-class{g:file}
  @see-function{gtk:file-chooser-uris}"
  (chooser (g:object file-chooser)))

(export 'file-chooser-files)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_get_current_folder ()
;;; gtk_file_chooser_set_current_folder () -> file-chooser-current-folder
;;; ----------------------------------------------------------------------------

(defun (setf file-chooser-current-folder) (filename chooser)
  (foreign-funcall "gtk_file_chooser_set_current_folder"
                   (g:object file-chooser) chooser
                   :string (if filename filename (null-pointer))
                   :boolean)
  filename)

(defcfun ("gtk_file_chooser_get_current_folder" file-chooser-current-folder)
    :string
 #+liber-documentation
 "@version{#2021-2-5}
  @syntax[]{(gtk:file-chooser-current-folder chooser) => filename}
  @syntax[]{(setf (gtk:file-chooser-current-folder chooser) filename)}
  @argument[chooser]{a @class{gtk:file-chooser} widget}
  @argument[filename]{a string with the full path of the new current folder}
  @begin{short}
    Accessor of the current folder of the file chooser.
  @end{short}

  The @sym{gtk:file-chooser-current-folder} function gets the current folder
  of the file chooser as a local filename. The
  @sym{(setf gtk:file-chooser-current-folder)} function sets the current folder.

  The user will be shown the full contents of the current folder, plus user
  interface elements for navigating to other folders.

  Note that this is the folder that the file chooser is currently displaying,
  e.g. @file{/home/username/Documents}, which is not the same as the
  currently selected folder if the chooser is in @code{:select-folder} mode,
  e.g. @file{/home/username/Documents/selected-folder/}. To get the
  currently selected folder in that mode, use the @fun{gtk:file-chooser-uri}
  function as the usual way to get the selection.

  In general, you should not use this function. See the section on setting up
  a file chooser dialog for the rationale behind this.
  @see-class{gtk:file-chooser}
  @see-function{gtk:file-chooser-uri}"
  (chooser (g:object file-chooser)))

(export 'file-chooser-current-folder)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_add_filter ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_file_chooser_add_filter" file-chooser-add-filter) :void
 #+liber-documentation
 "@version{#2021-2-5}
  @argument[chooser]{a @class{gtk:file-chooser} widget}
  @argument[filter]{a @class{gtk:file-filter} object}
  @begin{short}
    Adds a filter to the list of filters that the user can select between.
  @end{short}
  When a filter is selected, only files that are passed by that filter are
  displayed.
  @see-class{gtk:file-chooser}
  @see-class{gtk:file-filter}
  @see-function{gtk:file-chooser-remove-filter}"
  (chooser (g:object file-chooser))
  (filter (g:object file-filter)))

(export 'file-chooser-add-filter)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_remove_filter ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_file_chooser_remove_filter" file-chooser-remove-filter) :void
 #+liber-documentation
 "@version{#2021-2-5}
  @argument[chooser]{a @class{gtk:file-chooser} widget}
  @argument[filter]{a @class{gtk:file-filter} object}
  @begin{short}
    Removes a filter from the list of filters that the user can select between.
  @end{short}
  @see-class{gtk:file-chooser}
  @see-class{gtk:file-filter}
  @see-function{gtk:file-chooser-add-filter}"
  (chooser (g:object file-chooser))
  (filter (g:object file-filter)))

(export 'file-chooser-remove-filter)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_add_shortcut_folder ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_file_chooser_add_shortcut_folder"
          %file-chooser-add-shortcut-folder) :boolean
  (chooser (g:object file-chooser))
  (folder :string)
  (error :pointer))

(defun file-chooser-add-shortcut-folder (chooser folder)
 #+liber-documentation
 "@version{#2021-2-5}
  @argument[chooser]{a @class{gtk:file-chooser} widget}
  @argument[folder]{a string with a filename of the folder to add}
  @begin{return}
    @em{True} if the folder could be added successfully, @em{false} otherwise.
  @end{return}
  @begin{short}
    Adds a folder to be displayed with the shortcut folders in a file chooser.
  @end{short}
  Note that shortcut folders do not get saved, as they are provided by the
  application. For example, you can use this to add a
  \"/usr/share/mydrawprogram/Clipart\" folder to the volume list.
  @see-class{gtk:file-chooser}
  @see-function{gtk:file-chooser-remove-shortcut-folder}"
  (with-g-error (err)
    (%file-chooser-add-shortcut-folder chooser folder err)))

(export 'file-chooser-add-shortcut-folder)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_remove_shortcut_folder ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_file_chooser_remove_shortcut_folder"
          %file-chooser-remove-shortcut-folder) :boolean
  (chooser (g:object file-chooser))
  (folder :string)
  (error :pointer))

(defun file-chooser-remove-shortcut-folder (chooser folder)
 #+liber-documentation
 "@version{#2021-2-5}
  @argument[chooser]{a @class{gtk:file-chooser} widget}
  @argument[folder]{a string with the filename of the folder to remove}
  @begin{return}
    @em{True} if the operation succeeds, @em{false} otherwise.
  @end{return}
  @begin{short}
    Removes a folder from a file chooser's list of shortcut folders.
  @end{short}
  See also the @fun{gtk:file-chooser-add-shortcut-folder} function.
  @see-class{gtk:file-chooser}
  @see-function{gtk:file-chooser-add-shortcut-folder}"
  (with-g-error (err)
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

;;; --- End of file gtk.file-chooser.lisp --------------------------------------

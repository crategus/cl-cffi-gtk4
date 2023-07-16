;;; ----------------------------------------------------------------------------
;;; gtk4.file-chooser-dialog.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2023 Dieter Kaiser
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
;;; GtkFileChooserDialog
;;;
;;;     A file chooser dialog, suitable for “File Open” or “File Save” commands
;;;
;;; Types and Values
;;;
;;;     GtkFileChooserDialog
;;;
;;; Functions
;;;
;;;     gtk_file_chooser_dialog_new
;;;
;;; Actions
;;;
;;;     response.activate
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkWindow
;;;                 ╰── GtkDialog
;;;                     ╰── GtkFileChooserDialog
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;;     GtkNative
;;;     GtkRoot
;;;     GtkShortcutManager
;;;     GtkFileChooser
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkFileChooserDialog
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkFileChooserDialog" file-chooser-dialog
  (:superclass dialog
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget"
                "GtkNative"
                "GtkRoot"
                "GtkShortcutManager"
                "GtkFileChooser")
   :type-initializer "gtk_file_chooser_dialog_get_type")
  nil)

#+liber-documentation
(setf (documentation 'file-chooser-dialog 'type)
 "@version{#2021-2-4}
  @begin{short}
    The @sym{gtk:file-chooser-dialog} widget is a dialog box suitable for use
    with \"File/Open\" or \"File/Save as\" commands.
  @end{short}
  This widget works by putting a @class{gtk:file-chooser-widget} widget inside
  a @class{gtk:dialog} widget. It exposes the @class{gtk:file-chooser}
  interface, so you can use all of the @class{gtk:file-chooser} functions on
  the file chooser dialog as well as those for the @class{gtk:dialog} widget.

  @image[file-chooser-dialog]{Figure: GtkFileChooserDialog}

  Note that the @sym{gtk:file-chooser-dialog} widget does not have any methods
  of its own. Instead, you should use the functions that work on a
  @class{gtk:file-chooser} interface.

  If you want to integrate well with the platform you should use the
  @class{gtk:file-chooser-native} API, which will use a platform-specific dialog
  if available and fall back to the @class{gtk:file-chooser-dialog} widget
  otherwise.

  @b{Example:} Typical usage @br{}
  In the simplest of cases, you can the following code to use the
  @sym{gtk:file-chooser-dialog} widget to select a file for opening:
  @begin{pre}
(defun create-file-chooser-dialog-open (window)
  (let ((dialog (gtk:file-chooser-dialog-new \"Open File\"
                                             window
                                             :open
                                             \"gtk-cancel\" :cancel
                                             \"gtk-open\" :accept)))
    (if (eq :accept (gtk:dialog-run dialog))
      (let ((filename (gtk:file-chooser-filename dialog)))
        ...
      ))
    (gtk:widget-destroy dialog)))
  @end{pre}
  To use a dialog for saving, you can use this:
  @begin{pre}
(defun create-file-chooser-dialog-save (window filename)
  (let ((dialog (gtk:file-chooser-dialog-new \"Save File\"
                                             window
                                             :save
                                             \"gtk-cancel\" :cancel
                                             \"gtk-save\" :accept)))
    (setf (gtk:file-chooser-do-overwrite-confirmation dialog) t)
    (if filename
        (setf (gtk:file-chooser-filename dialog) filename)
        (setf (gtk:file-chooser-current-name dialog) \"Untitled document\"))
    (if (eq :accept (gtk:dialog-run dialog))
      (let ((filename (gtk:file-chooser-filename dialog)))
        ...
      ))
    (gtk:widget-destroy dialog)))
  @end{pre}
  @subheading{Setting up a file chooser dialog}
    There are various cases in which you may need to use a
    @sym{gtk:file-chooser-dialog} widget:
    @begin{itemize}
      @begin{item}
        To select a file for opening, as for a File/Open command. Use
        @code{:open}.
      @end{item}
      @begin{item}
        To save a file for the first time, as for a File/Save command. Use
        @code{:save}, and suggest a name such as \"Untitled\" with the function
        @fun{gtk:file-chooser-current-name}.
      @end{item}
      @begin{item}
        To save a file under a different name, as for a File/Save As command.
        Use @code{:save}, and set the existing filename with the function
        @fun{gtk:file-chooser-filename}.
      @end{item}
      @begin{item}
        To choose a folder instead of a file. Use @code{:select-folder}.
      @end{item}
    @end{itemize}
  @subheading{Note}
    Old versions of the file chooser's documentation suggested using the
    function @fun{gtk:file-chooser-current-folder} in various situations,
    with the intention of letting the application suggest a reasonable default
    folder. This is no longer considered to be a good policy, as now the file
    chooser is able to make good suggestions on its own. In general, you should
    only cause the file chooser to show a specific folder when it is appropriate
    to use the function @fun{gtk:file-chooser-filename}, i.e. when you are
    doing a File/Save As command and you already have a file saved somewhere.

  @subheading{Response Codes}
    The @sym{gtk:file-chooser-dialog} widget inherits from the
    @class{gtk:dialog} widget, so buttons that go in its action area have
    response codes such as @code{:accept} and @code{:cancel}. For example, you
    could call the function @fun{gtk:file-chooser-dialog-new} as follows:
    @begin{pre}
 (let ((dialog (gtk:file-chooser-dialog-new \"Open File\"
                                            parent-window
                                            :open
                                            \"gtk-cancel\" :cancel
                                            \"gtk-open\" :accept)))
   ... )
    @end{pre}
    This will create buttons for \"Cancel\" and \"Open\" that use stock
    response identifiers from the @symbol{gtk:response-type} enumeration. For
    most dialog boxes you can use your own custom response codes rather than
    the ones in the @symbol{gtk:response-type} enumeration, but the
    @sym{gtk:file-chooser-dialog} widget assumes that its \"accept\"-type
    action, e.g. an \"Open\" or \"Save\" button, will have one of the following
    response codes:
    @begin{pre}
     @code{:accept}
     @code{:ok}
     @code{:yes}
     @code{:apply}
    @end{pre}
    This is because the @sym{gtk:file-chooser-dialog} widget must intercept
    responses and switch to folders if appropriate, rather than letting the
    dialog terminate - the implementation uses these known response codes to
    know which responses can be blocked if appropriate.

  @subheading{Note}
    To summarize, make sure you use a stock response code when you use the
    @sym{gtk:file-chooser-dialog} widget to ensure proper operation.

  @begin[Action Details]{dictionary}
    @subheading{The \"response.activate\" action}
      Activate the default response of the dialog.
  @end{dictionary}

  @see-class{gtk:dialog}
  @see-class{gtk:file-chooser}
  @see-class{gtk:file-chooser-widget}
  @see-class{gtk:file-chooser-native}
  @see-function{gtk:file-chooser-current-name}
  @see-function{gtk:file-chooser-filename}
  @see-function{gtk:file-chooser-dialog-new}")

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_dialog_new ()
;;; ----------------------------------------------------------------------------

(defun file-chooser-dialog-new (title parent action &rest buttons)
 #+liber-documentation
 "@version{#2021-2-4}
  @argument[title]{a string with title of the dialog, or @code{nil}}
  @argument[parent]{a @class{gtk:window} transient parent of the dialog,
    or @code{nil}}
  @argument[action]{a value of the @symbol{gtk:file-chooser-action} enumeration}
  @argument[buttons]{pairs with a button text or stock ID and the response ID
    of type @symbol{gtk:response-type} for the button}
  @return{A new @class{gtk:file-chooser-dialog} widget.}
  @begin{short}
    Creates a new file chooser dialog.
  @end{short}
  This function is analogous to the function @fun{gtk:dialog-new-with-buttons}.
  @see-class{gtk:file-chooser-dialog}
  @see-function{gtk:dialog-new-with-buttons}"
  (let ((dialog (make-instance 'file-chooser-dialog
                               :title title
                               :action action)))
    (when parent
      (setf (window-transient-for dialog) parent))
    (when buttons
      (apply #'dialog-add-buttons (cons dialog buttons)))
    dialog))

(export 'file-chooser-dialog-new)

;;; --- End of file gtk4.file-chooser-dialog.lisp ------------------------------

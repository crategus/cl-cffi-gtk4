;;; ----------------------------------------------------------------------------
;;; gtk4.file-chooser-dialog.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.12 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2025 Dieter Kaiser
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
;;; GtkFileChooserDialog
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkFileChooserDialog" file-chooser-dialog
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

#+(and gtk-4-10 gtk-warn-deprecated)
(defmethod initialize-instance :after ((obj file-chooser-dialog) &key)
  (when gtk-init:*warn-deprecated*
    (warn "GTK:FILE-CHOOSER-DIALOG is deprecated since 4.10")))

#+liber-documentation
(setf (documentation 'file-chooser-dialog 'type)
 "@version{2025-09-21}
  @begin{short}
    The @class{gtk:file-chooser-dialog} widget is a dialog suitable for use
    with \"File/Open\" or \"File/Save as\" commands.
  @end{short}
  This widget works by putting a @class{gtk:file-chooser-widget} widget inside
  a @class{gtk:dialog} widget. It exposes the @class{gtk:file-chooser}
  interface, so you can use all of the @class{gtk:file-chooser} functions on
  the file chooser dialog as well as those for the @class{gtk:dialog} widget.

  @image[file-chooser-dialog]{Figure: GtkFileChooserDialog}

  Note that the @class{gtk:file-chooser-dialog} widget does not have any
  methods of its own. Instead, you should use the functions that work on a
  @class{gtk:file-chooser} interface.

  If you want to integrate well with the platform you should use the
  @class{gtk:file-chooser-native} API, which will use a platform-specific
  dialog if available and fall back to the @class{gtk:file-chooser-dialog}
  widget otherwise.
  @begin[Setting up a file chooser dialog]{dictionary}
    There are various cases in which you may need to use a
    @class{gtk:file-chooser-dialog} widget:
    @begin{itemize}
      @begin{item}
        To select a file for opening, as for a File/Open command. Use
        @val[gtk:file-chooser-action]{:open}.
      @end{item}
      @begin{item}
        To save a file for the first time, as for a File/Save command. Use
        @val[gtk:file-chooser-action]{:save}, and suggest a name such as
        \"Untitled\" with the @fun{gtk:file-chooser-current-name} function.
      @end{item}
      @begin{item}
        To save a file under a different name, as for a File/Save As command.
        Use @val[gtk:file-chooser-action]{:save}, and set the existing filename
        with the @fun{gtk:file-chooser-file} function.
      @end{item}
      @begin{item}
        To choose a folder instead of a file. Use
        @val[gtk:file-chooser-action]{:select-folder}.
      @end{item}
    @end{itemize}
    In general, you should only cause the file chooser to show a specific folder
    when it is appropriate to use the @fun{gtk:file-chooser-file} function,
    that is, when you are doing a \"Save As\" command and you already have a
    file saved somewhere.
  @end{dictionary}
  @begin[Response Codes]{dictionary}
    The @class{gtk:file-chooser-dialog} widget inherits from the
    @class{gtk:dialog} widget, so buttons that go in its action area have
    response codes such as @code{:accept} and @code{:cancel}. For example, you
    could call the @fun{gtk:file-chooser-dialog-new} function as follows:
    @begin{pre}
(let ((dialog (gtk:file-chooser-dialog-new \"Open File\"
                                           parent-window
                                           :open
                                           \"Cancel\" :cancel
                                           \"Open\" :accept)))
  ... )
    @end{pre}
    This will create buttons for \"Cancel\" and \"Open\" that  identifiers from
    the @sym{gtk:response-type} enumeration. For most dialogs you can use your
    own custom response codes rather than the ones in the
    @sym{gtk:response-type} enumeration, but the @class{gtk:file-chooser-dialog}
    widget assumes that its \"accept\"-type action, for example, an \"Open\" or
    \"Save\" button, will have one of the following response codes:
    @begin{pre}
  @code{:accept}
  @code{:ok}
  @code{:yes}
  @code{:apply}
    @end{pre}
    This is because the @class{gtk:file-chooser-dialog} widget must intercept
    responses and switch to folders if appropriate, rather than letting the
    dialog terminate - the implementation uses these known response codes to
    know which responses can be blocked if appropriate.

    To summarize, make sure you use a predefined response code when you use
    the @class{gtk:file-chooser-dialog} widget to ensure proper operation.
  @end{dictionary}
  @begin[Examples]{dictionary}
    Typical usage: In the simplest of cases, you can the following code to use
    the @class{gtk:file-chooser-dialog} widget to select a file for opening:
    @begin{pre}
(defun create-file-chooser-dialog-open (window)
  (let ((dialog (gtk:file-chooser-dialog-new \"Open File\"
                                             window
                                             :open
                                             \"Cancel\" :cancel
                                             \"Open\" :accept)))
    (if (eq :accept (gtk:dialog-run dialog))
      (let ((filename (gtk:file-chooser-filename dialog)))
        ...
      ))
    (gtk:window-destroy dialog)))
    @end{pre}
    To use a dialog for saving, you can use this:
    @begin{pre}
(defun create-file-chooser-dialog-save (window filename)
  (let ((dialog (gtk:file-chooser-dialog-new \"Save File\"
                                             window
                                             :save
                                             \"Cancel\" :cancel
                                             \"Save\" :accept)))
    (setf (gtk:file-chooser-do-overwrite-confirmation dialog) t)
    (if filename
        (setf (gtk:file-chooser-filename dialog) filename)
        (setf (gtk:file-chooser-current-name dialog) \"Untitled document\"))
    (if (eq :accept (gtk:dialog-run dialog))
      (let ((filename (gtk:file-chooser-filename dialog)))
        ...
      ))
    (gtk:window-destroy dialog)))
    @end{pre}
  @end{dictionary}
  @begin[CSS nodes]{dictionary}
    The @class{gtk:file-chooser-dialog} implementation has a single CSS node
    with the name @code{window} and @code{.filechooser} style class.
  @end{dictionary}
  @begin[Warning]{dictionary}
    The @class{gtk:file-chooser-dialog} implementation is deprecated since
    4.10. Use the @class{gtk:file-dialog} object instead.
  @end{dictionary}
  @see-constructor{gtk:file-chooser-dialog-new}
  @see-class{gtk:dialog}
  @see-class{gtk:file-chooser}
  @see-class{gtk:file-chooser-widget}
  @see-class{gtk:file-chooser-native}")

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_dialog_new
;;; ----------------------------------------------------------------------------

(defun file-chooser-dialog-new (title parent action &rest buttons)
 #+liber-documentation
 "@version{2025-07-27}
  @argument[title]{a string for the title of the dialog, or @code{nil}}
  @argument[parent]{a @class{gtk:window} transient parent window of the dialog,
    or @code{nil}}
  @argument[action]{a @sym{gtk:file-chooser-action} value}
  @argument[buttons]{pairs for a button text and the response ID of type
    @sym{gtk:response-type} for the button}
  @return{The new @class{gtk:file-chooser-dialog} widget.}
  @begin{short}
    Creates a new file chooser dialog.
  @end{short}
  This function is analogous to the @fun{gtk:dialog-new-with-buttons} function.
  @begin[Warning]{dictionary}
    The @class{gtk:file-chooser-dialog} implementation is deprecated since 4.10.
    Use the @class{gtk:file-dialog} object instead.
  @end{dictionary}
  @see-class{gtk:file-chooser-dialog}
  @see-class{gtk:window}
  @see-class{gtk:file-dialog}
  @see-symbol{gtk:file-chooser-action}
  @see-symbol{gtk:response-type}
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

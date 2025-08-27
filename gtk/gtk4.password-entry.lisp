;;; ----------------------------------------------------------------------------
;;; gtk4.password-entry.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.18 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 - 2025 Dieter Kaiser
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
;;; GtkPasswordEntry
;;;
;;;     An entry for secrets
;;;
;;; Types and Values
;;;
;;;     GtkPasswordEntry
;;;     GtkPasswordEntryBuffer                             Since 4.4
;;;
;;; Accessors
;;;
;;;     gtk_password_entry_set_extra_menu
;;;     gtk_password_entry_get_extra_menu
;;;     gtk_password_entry_set_show_peek_icon
;;;     gtk_password_entry_get_show_peek_icon
;;;
;;; Functions
;;;
;;;     gtk_password_entry_new
;;;
;;;     gtk_password_entry_buffer_new                      Since 4.4
;;;
;;; Properties
;;;
;;;     activates-default
;;;     extra-menu
;;;     placeholder-text
;;;     show-peek-icon
;;;
;;; Signals
;;;
;;;     activate
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkPasswordEntry
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;;     GtkEditable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkPasswordEntryBuffer
;;; ----------------------------------------------------------------------------

#+gtk-4-4
(gobject:define-gobject "GtkPasswordEntryBuffer" password-entry-buffer
  (:superclass entry-buffer
   :export t
   :interfaces ()
   :type-initializer "gtk_password_entry_buffer_get_type")
  nil)

#+(and gtk-4-4 liber-documentation)
(setf (documentation 'password-entry-buffer 'type)
 "@version{2024-12-05}
  @begin{short}
    The @class{gtk:entry-buffer} object that locks the underlying memory to
    prevent it from being swapped to disk.
  @end{short}
  The @class{gtk:password-entry} widget uses the
  @class{gtk:password-entry-buffer} object.

  Since 4.4
  @see-constructor{gtk:password-entry-buffer-new}
  @see-class{gtk:password-entry}
  @see-class{gtk:entry-buffer}")

;;; ----------------------------------------------------------------------------
;;; gtk_password_entry_buffer_new
;;; ----------------------------------------------------------------------------

#+gtk-4-4
(declaim (inline password-entry-buffer))

#+gtk-4-4
(defun password-entry-buffer-new ()
 #+liber-documentation
 "@version{2024-12-05}
  @return{The newly created @class{gtk:password-entry-buffer} object.}
  @begin{short}
    Creates a new @class{gtk:entry-buffer} object using secure memory
    allocations.
  @end{short}
  @see-class{gtk:password-entry-buffer}
  @see-class{gtk:entry-buffer}"
  (make-instance 'password-entry-buffer))

#+gtk-4-4
(export 'password-entry-buffer-new)

;;; ----------------------------------------------------------------------------
;;; GtkPasswordEntry
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkPasswordEntry" password-entry
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget"
                "GtkEditable")
   :type-initializer "gtk_password_entry_get_type")
  ((activates-default
    password-entry-activates-default
    "activates-default" "gboolean" t t)
   (extra-menu
    password-entry-extra-menu
    "extra-menu" "GMenuModel" t t)
   (placeholder-text
    password-entry-placeholder-text
    "placeholder-text" "gchararray" t t)
   (show-peek-icon
    password-entry-show-peek-icon
    "show-peek-icon" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'password-entry 'type)
 "@version{2024-12-05}
  @begin{short}
    The @class{gtk:password-entry} class is a text entry that has been tailored
    for entering secrets.
  @end{short}

  @image[password-entry]{Figure: GtkPasswordEntry}

  It does not show its contents in clear text, does not allow to copy it to the
  clipboard, and it shows a warning when the @kbd{Caps Lock} key is engaged. If
  the underlying platform allows it, the @class{gtk:password-entry} widget will
  also place the text in a non-pageable memory area, to avoid it being written
  out to disk by the operating system. Optionally, it can offer a way to reveal
  the contents in clear text.

  The @class{gtk:password-entry} widget provides only minimal API and should be
  used with the @class{gtk:editable} API.
  @begin[CSS Nodes]{dictionary}
    @begin{pre}
entry.password
╰── text
    ├── image.caps-lock-indicator
    ┊
    @end{pre}
    The @class{gtk:password-entry} implementation has a single CSS node with
    name @code{entry} that carries a @code{.passwordstyle} style class. The
    @code{text} CSS node below it has a child with name @code{image} and
    @code{.caps-lock-indicator} style class for the @kbd{Caps Lock} icon, and
    possibly other children.
  @end{dictionary}
  @begin[Accessibility]{dictionary}
    The @class{gtk:password-entry} implementation uses the
    @val[gtk:accessible-role]{:text-box} role of the @sym{gtk:accessible-role}
    enumeration.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @begin[password-entry::activate]{signal}
      @begin{pre}
lambda (entry)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[entry]{The @class{gtk:password-entry} widget on which the signal
          is emitted.}
      @end{simple-table}
      A keybinding signal which gets emitted when the user activates the
      password entry. Applications should not connect to it, but may emit it
      with the @fun{g:signal-emit} function if they need to control activation
      programmatically. The default bindings for this signal are all forms of
      the @kbd{Enter} key.
    @end{signal}
  @end{dictionary}
  @see-constructor{gtk:password-entry-new}
  @see-slot{gtk:password-entry-activates-default}
  @see-slot{gtk:password-entry-extra-menu}
  @see-slot{gtk:password-entry-placeholder-text}
  @see-slot{gtk:password-entry-show-peek-icon}
  @see-class{gtk:editable}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:password-entry-activates-default -----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "activates-default"
                                               'password-entry) t)
 "The @code{activates-default} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether to activate the default widget, such as the default button in a
  dialog, when the @kbd{Enter} key is pressed. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'password-entry-activates-default)
      "Accessor"
      (documentation 'password-entry-activates-default 'function)
 "@version{2025-08-20}
  @syntax{(gtk:password-entry-activates-default object) => setting}
  @syntax{(setf (gtk:password-entry-activates-default object) setting)}
  @argument[object]{a @class{gtk:password-entry} widget}
  @argument[setting]{@em{true} to activate the default widget of the window on
    @kbd{Enter} keypress}
  @begin{short}
    The accessor for the @slot[gtk:password-entry]{activates-default} slot of
    the @class{gtk:password-entry} class gets or sets whether to activate the
    default widget, when the @kbd{Enter} key is pressed.
  @end{short}

  If the @arg{setting} argument is @em{true}, pressing the @kbd{Enter} key in
  the password entry will activate the default widget for the window containing
  the password entry. This usually means that the dialog containing the password
  entry will be closed, since the default widget is usually one of the dialog
  buttons.
  @see-class{gtk:password-entry}")

;;; --- gtk:password-entry-extra-menu ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "extra-menu" 'password-entry) t)
 "The @code{extra-menu} property of type @class{g:menu-model} (Read / Write)
  @br{}
  The menu model whose contents will be appended to the context menu.")

#+liber-documentation
(setf (liber:alias-for-function 'password-entry-extra-menu)
      "Accessor"
      (documentation 'password-entry-extra-menu 'function)
 "@version{2025-08-20}
  @syntax{(gtk:password-entry-extra-menu object) => menu}
  @syntax{(setf (gtk:password-entry-extra-menu object) menu)}
  @argument[object]{a @class{gtk:password-entry} widget}
  @argument[menu]{a @class{g:menu-model} object}
  @begin{short}
    The accessor for the @slot[gtk:password-entry]{extra-menu} slot of the
    @class{gtk:password-entry} class gets or sets the menu model whose contents
    will be appended to the context menu for the password entry.
  @end{short}
  @see-class{gtk:password-entry}
  @see-class{g:menu-model}")

;;; --- gtk:password-entry-placeholder-text ------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "placeholder-text"
                                               'password-entry) t)
 "The @code{placeholder-text} property of type @code{:string} (Read / Write)
  @br{}
  The text that will be displayed in the password entry when it is empty and
  unfocused. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'password-entry-placeholder-text)
      "Accessor"
      (documentation 'password-entry-placeholder-text 'function)
 "@version{2025-08-20}
  @syntax{(gtk:password-entry-placeholder-text object) => text}
  @syntax{(setf (gtk:password-entry-placeholder-text object) text)}
  @argument[object]{a @class{gtk:password-entry} widget}
  @argument[text]{a string to be displayed when @arg{entry} is empty and
    unfocused, or @code{nil}}
  @begin{short}
    The accessor for the @slot[gtk:password-entry]{placeholder-text} slot of the
    @class{gtk:password-entry} class gets or sets the text that will be
    displayed when the password entry is empty and unfocused.
  @end{short}
  This can be used to give a visual hint of the expected contents of the
  password entry.

  Note that since the placeholder text gets removed when the password entry
  received focus, using this feature is a bit problematic if the password entry
  is given the initial focus in a window. Sometimes this can be worked around
  by delaying the initial focus setting until the first key event arrives.
  @see-class{gtk:password-entry}")

;;; --- gtk:password-entry-show-peek-icon --------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "show-peek-icon"
                                               'password-entry) t)
 "The @code{show-peek-icon} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether to show an icon for revealing the content. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'password-entry-show-peek-icon)
      "Accessor"
      (documentation 'password-entry-show-peek-icon 'function)
 "@version{2025-08-20}
  @syntax{(gtk:password-entry-show-peek-icon object) => setting}
  @syntax{(setf (gtk:password-entry-show-peek-icon object) setting)}
  @argument[object]{a @class{gtk:password-entry} widget}
  @argument[setting]{a boolean whether to show the peek icon}
  @begin{short}
    The accessor for the @slot[gtk:password-entry]{show-peek-icon} slot of the
    @class{gtk:password-entry} class gets or sets whether the password entry is
    showing a clickable icon to reveal the contents of the password entry in
    clear text.
  @end{short}
  Setting this to @em{false} also hides the text again.
  @see-class{gtk:password-entry}")

;;; ----------------------------------------------------------------------------
;;; gtk_password_entry_new
;;; ----------------------------------------------------------------------------

(declaim (inline password-entry-new))

(defun password-entry-new ()
 #+liber-documentation
 "@version{2024-12-05}
  @return{The new @class{gtk:password-entry} widget.}
  @short{Creates a password entry.}
  @see-class{gtk:password-entry}"
  (make-instance 'password-entry))

(export 'password-entry-new)

;;; --- End of file gtk4.password-entry.lisp -----------------------------------

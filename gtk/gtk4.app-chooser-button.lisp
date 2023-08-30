;;; ----------------------------------------------------------------------------
;;; gtk4.app-chooser-button.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.12 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2013 - 2023 Dieter Kaiser
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
;;; GtkAppChooserButton
;;;
;;;     A button to launch an application chooser dialog
;;;
;;; Types and Values
;;;
;;;     GtkAppChooserButton
;;;
;;; Accessors
;;;
;;;     gtk_app_chooser_button_get_heading
;;;     gtk_app_chooser_button_set_heading
;;;     gtk_app_chooser_button_get_modal
;;;     gtk_app_chooser_button_set_modal
;;;     gtk_app_chooser_button_get_show_default_item
;;;     gtk_app_chooser_button_set_show_default_item
;;;     gtk_app_chooser_button_get_show_dialog_item
;;;     gtk_app_chooser_button_set_show_dialog_item
;;;
;;; Functions
;;;
;;;     gtk_app_chooser_button_new
;;;     gtk_app_chooser_button_append_custom_item
;;;     gtk_app_chooser_button_append_separator
;;;     gtk_app_chooser_button_set_active_custom_item
;;;
;;; Properties
;;;
;;;     heading
;;;     modal
;;;     show-default-item
;;;     show-dialog-item
;;;
;;; Signals
;;;
;;;     activate                                           Since 4.4
;;;     changed
;;;     custom-item-activated
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkAppChooserButton
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;;     GtkAppChooser
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkAppChooserButton
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkAppChooserButton" app-chooser-button
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget"
                "GtkAppChooser")
   :type-initializer "gtk_app_chooser_button_get_type")
  ((heading
    app-chooser-button-heading
    "heading" "gchararray" t t)
   (modal
    app-chooser-button-modal
    "modal" "gboolean" t t)
   (show-default-item
    app-chooser-button-show-default-item
    "show-default-item" "gboolean" t t)
   (show-dialog-item
    app-chooser-button-show-dialog-item
    "show-dialog-item" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'app-chooser-button 'type)
 "@version{2023-8-29}
  @begin{short}
    The @class{gtk:app-chooser-button} widget is a widget that lets the user
    select an application.
  @end{short}
  It implements the @class{gtk:app-chooser} interface.

  @image[app-chooser-button]{Figure: GtkAppChooserButton}

  Initially, a @class{gtk:app-chooser-button} widget selects the first
  application in its list, which will either be the most-recently used
  application or, if the @code{show-default-item} property is @em{true}, the
  default application.

  The list of applications shown in a @class{gtk:app-chooser-button} widget
  includes the recommended applications for the given content type. When the
  @slot[gtk:app-chooser-button]{show-default-item} property is set, the default
  application is also included. To let the user chooser other applications, you
  can set the @slot[gtk:app-chooser-button]{show-dialog-item} property, which
  allows to open a full @class{gtk:app-chooser-dialog} widget.

  It is possible to add custom items to the list, using the
  @fun{gtk:app-chooser-button-append-custom-item} function. These items cause
  the \"custom-item-activated\" signal to be emitted when they are selected.

  To track changes in the selected application, use the \"changed\" signal.
  @begin[CSS nodes]{dictionary}
    The @class{gtk:app-chooser-button} implementation has a single CSS node
    with the name @code{appchooserbutton}.
  @end{dictionary}
  @begin[Warning]{dictionary}
    The @class{gtk:app-chooser-button} implementation is deprecated since 4.10.
    The application selection widgets should be implemented according to the
    design of each platform and/or application requiring them.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"activate\" signal}
      @begin{pre}
lambda (button)    :run-first
      @end{pre}
      Emitted to when the button is activated. The signal is an action signal
      and emitting it causes the button to pop up its dialog.
      @begin[code]{table}
        @entry[button]{The @class{gtk:app-chooser-button} widget which received
          the signal.}
      @end{table}
    @subheading{The \"changed\" signal}
      @begin{pre}
lambda (button)    :run-last
      @end{pre}
      Emitted when the active application on the @class{gtk:app-chooser-button}
      widget changes.
      @begin[code]{table}
        @entry[button]{The @class{gtk:app-chooser-button} widget which received
          the signal.}
      @end{table}
    @subheading{The \"custom-item-activated\" signal}
      @begin{pre}
lambda (button item)    :has-details
      @end{pre}
      Emitted when a custom item, previously added with the
      @fun{gtk:app-chooser-button-append-custom-item} function, is activated
      from the dropdown menu.
      @begin[code]{table}
        @entry[button]{The @class{gtk:app-chooser-button} widget which received
          the signal.}
        @entry[item]{A string with the name of the activated item.}
      @end{table}
  @end{dictionary}
  @see-constructor{gtk:app-chooser-button-new}
  @see-slot{gtk:app-chooser-button-heading}
  @see-slot{gtk:app-chooser-button-modal}
  @see-slot{gtk:app-chooser-button-show-default-item}
  @see-slot{gtk:app-chooser-button-show-dialog-item}
  @see-class{gtk:app-chooser}
  @see-class{gtk:app-chooser-dialog}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- app-chooser-button-heading ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "heading"
                                               'app-chooser-button) t)
 "The @code{heading} property of @code{:string} (Read / Write) @br{}
  The text to show at the top of the dialog that can be opened from the
  button. The string may contain Pango markup. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'app-chooser-button-heading)
      "Accessor"
      (documentation 'app-chooser-button-heading 'function)
 "@version{2023-8-29}
  @syntax[]{(gtk:app-chooser-button-heading object) => heading}
  @syntax[]{(setf (gtk:app-chooser-button-heading object) heading)}
  @argument[object]{a @class{gtk:app-chooser-button} widget}
  @argument[heading]{a string containing Pango markup}
  @begin{short}
    Accessor of the @slot[gtk:app-chooser-button]{heading} slot of the
    @class{gtk:app-chooser-button} class.
  @end{short}
  The @fun{gtk:app-chooser-button-heading} function returns the text to display
  at the top of the dialog. The @sym{(setf gtk:app-chooser-button-heading)}
  function sets the text to display at the top of the dialog.

  If the heading is not set, the dialog displays a default text.
  @begin[Warning]{dictionary}
    The @class{gtk:app-chooser-button} implementation is deprecated since 4.10.
  @end{dictionary}
  @see-class{gtk:app-chooser-button}")

;;; --- app-chooser-button-modal -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "modal"
                                               'app-chooser-button) t)
 "The @code{modal} property of @code{:boolean} (Read / Write / Construct) @br{}
  Whether the dialog should be modal. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'app-chooser-button-modal)
      "Accessor"
      (documentation 'app-chooser-button-modal 'function)
 "@version{2023-8-29}
  @syntax[]{(gtk:app-chooser-button-modal object) => setting}
  @syntax[]{(setf (gtk:app-chooser-button-modal object) setting)}
  @argument[object]{a @class{gtk:app-chooser-button} widget}
  @argument[setting]{a boolean whether the dialog is modal}
  @begin{short}
    Accessor of the @slot[gtk:app-chooser-button]{modal} slot of the
    @class{gtk:app-chooser-button} class.
  @end{short}
  The @fun{gtk:app-chooser-button-modal} function gets whether the dialog is
  modal. The @sym{(setf gtk:app-chooser-button-modal)} function sets whether
  the dialog should be modal.
  @begin[Warning]{dictionary}
    The @class{gtk:app-chooser-button} implementation is deprecated since 4.10.
  @end{dictionary}
  @see-class{gtk:app-chooser-button}")

;;; --- app-chooser-button-show-default-item -----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "show-default-item"
                                               'app-chooser-button) t)
 "The @code{show-default-item} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  Whether the dropdown menu should show the default application on top for the
  provided content type. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'app-chooser-button-show-default-item)
      "Accessor"
      (documentation 'app-chooser-button-show-default-item 'function)
 "@version{2023-8-29}
  @syntax[]{(gtk:app-chooser-button-show-default-item object) => setting}
  @syntax[]{(setf (gtk:app-chooser-button-show-default-item object) setting)}
  @argument[object]{a @class{gtk:app-chooser-button} widget}
  @argument[setting]{a boolean whether the dropdown menu should show the default
    application}
  @begin{short}
    Accessor of the @slot[gtk:app-chooser-button]{show-default-item} slot of
    the @class{gtk:app-chooser-button} class.
  @end{short}
  The @fun{gtk:app-chooser-button-show-default-item} function returns whether
  the dropdown menu of the button should show the default application. The
  @sym{(setf gtk:app-chooser-button-show-default-item)} function sets whether
  the dropdown menu of the button should show the default application for the
  given content type at top.
  @begin[Warning]{dictionary}
    The @class{gtk:app-chooser-button} implementation is deprecated since 4.10.
  @end{dictionary}
  @see-class{gtk:app-chooser-button}")

;;; --- app-chooser-button-show-dialog-item ------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "show-dialog-item"
                                               'app-chooser-button) t)
 "The @code{show-dialog-item} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  Whether the dropdown menu should show an item that triggers a
  @class{gtk:app-chooser-dialog} widget when clicked. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'app-chooser-button-show-dialog-item)
      "Accessor"
      (documentation 'app-chooser-button-show-dialog-item 'function)
 "@version{2023-8-29}
  @syntax[]{(gtk:app-chooser-button-show-dialog-item object) => setting}
  @syntax[]{(setf (gtk:app-chooser-button-show-dialog-item object) setting)}
  @argument[object]{a @class{gtk:app-chooser-button} widget}
  @argument[setting]{a boolean whether the dropdown menu shoult show a
    @class{gtk:app-chooser-dialog} widget}
  @begin{short}
    Accessor of the @slot[gtk:app-chooser-button]{show-dialog-item} slot of
    the @class{gtk:app-chooser-button} class.
  @end{short}
  The @fun{gtk:app-chooser-button-show-dialog-item} function returns whether
  the dropdown menu of the button should show an entry to trigger a
  @class{gtk:app-chooser-dialog} widget. The
  @sym{(setf gtk:app-chooser-button-show-dialog-item)} function sets the
  property.
  @begin[Warning]{dictionary}
    The @class{gtk:app-chooser-button} implementation is deprecated since 4.10.
  @end{dictionary}
  @see-class{gtk:app-chooser-button}")

;;; ----------------------------------------------------------------------------
;;; gtk_app_chooser_button_new ()
;;; ----------------------------------------------------------------------------

(defun app-chooser-button-new (content-type)
 #+liber-documentation
 "@version{2023-8-29}
  @argument[content-type]{a string with the content type to show applications
    for}
  @return{A newly created @class{gtk:app-chooser-button} widget.}
  @begin{short}
    Creates a new application chooser button for applications that can handle
    content of the given type.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:app-chooser-button} implementation is deprecated since 4.10.
  @end{dictionary}
  @see-class{gtk:app-chooser-button}"
  (make-instance 'app-chooser-button
                 :content-type (if content-type
                                   content-type
                                   (cffi:null-pointer))))

(export 'app-chooser-button-new)

;;; ----------------------------------------------------------------------------
;;; gtk_app_chooser_button_append_custom_item ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_app_chooser_button_append_custom_item"
               app-chooser-button-append-custom-item) :void
 #+liber-documentation
 "@version{#2020-5-21}
  @argument[widget]{a @class{gtk:app-chooser-button} widget}
  @argument[name]{a string with the name of the custom item}
  @argument[label]{a string with the label for the custom item}
  @argument[icon]{the icon of type @class{g:icon} for the custom item}
  @begin{short}
    Appends a custom item to the list of applications that is shown in the
    popup.
  @end{short}
  The item name must be unique per-widget. Clients can use the provided name as
  a detail for the \"custom-item-activated\" signal, to add a callback for the
  activation of a particular custom item in the list. See also the
  @fun{gtk:app-chooser-button-append-separator} function.
  @begin[Warning]{dictionary}
    The @class{gtk:app-chooser-button} implementation is deprecated since 4.10.
  @end{dictionary}
  @see-class{gtk:app-chooser-button}
  @see-class{g:icon}
  @see-function{gtk:app-chooser-button-append-separator}"
  (widget (g:object app-chooser-button))
  (name :string)
  (label :string)
  (icon (g:object g:icon)))

(export 'app-chooser-button-append-custom-item)

;;; ----------------------------------------------------------------------------
;;; gtk_app_chooser_button_append_separator ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_app_chooser_button_append_separator"
               app-chooser-button-append-separator) :void
 #+liber-documentation
 "@version{#2020-5-21}
  @argument[widget]{a @class{gtk:app-chooser-button} widget}
  @begin{short}
    Appends a separator to the list of applications that is shown in the popup.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:app-chooser-button} implementation is deprecated since 4.10.
  @end{dictionary}
  @see-class{gtk:app-chooser-button}"
  (widget (g:object app-chooser-button)))

(export 'app-chooser-button-append-separator)

;;; ----------------------------------------------------------------------------
;;; gtk_app_chooser_button_set_active_custom_item ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_app_chooser_button_set_active_custom_item"
               app-chooser-button-set-active-custom-item) :void
 #+liber-documentation
 "@version{#2020-5-21}
  @argument[widget]{a @class{gtk:app-chooser-button} widget}
  @argument[name]{a string with the name of the custom item}
  @begin{short}
    Selects a custom item previously added with the
    @fun{gtk:app-chooser-button-append-custom-item} function.
  @end{short}
  Use the @fun{gtk:app-chooser-refresh} function to bring the selection to its
  initial state.
  @begin[Warning]{dictionary}
    The @class{gtk:app-chooser-button} implementation is deprecated since 4.10.
  @end{dictionary}
  @see-class{gtk:app-chooser-button}
  @see-function{gtk:app-chooser-refresh}
  @see-function{gtk:app-chooser-button-append-custom-item}"
  (widget (g:object app-chooser-button))
  (name :string))

(export 'app-chooser-button-set-active-custom-item)

;;; --- End of file gtk4.app-chooser-button.lisp -------------------------------

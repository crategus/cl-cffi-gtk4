;;; ----------------------------------------------------------------------------
;;; gtk4.native-dialog.lisp
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
;;; GtkNativeDialog
;;;
;;;     Integrate with native dialogs
;;;
;;; Types and Values
;;;
;;;     GtkNativeDialog
;;;
;;; Accessor
;;;
;;;     gtk_native_dialog_set_modal
;;;     gtk_native_dialog_get_modal
;;;     gtk_native_dialog_set_title
;;;     gtk_native_dialog_get_title
;;;     gtk_native_dialog_set_transient_for
;;;     gtk_native_dialog_get_transient_for
;;;     gtk_native_dialog_get_visible
;;;
;;; Functions
;;;
;;;     gtk_native_dialog_show
;;;     gtk_native_dialog_hide
;;;     gtk_native_dialog_destroy
;;;
;;; Properties
;;;
;;;     modal
;;;     title
;;;     transient-for
;;;     visible
;;;
;;; Signals
;;;
;;;     response
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkNativeDialog
;;;         ╰── GtkFileChooserNative
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkNativeDialog
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkNativeDialog" native-dialog
  (:superclass g:object
   :export t
   :interfaces ()
   :type-initializer "gtk_native_dialog_get_type")
  ((modal
    native-dialog-modal
    "modal" "gboolean" t t)
   (title
    native-dialog-title
    "title" "gchararray" t t)
   (transient-for
    native-dialog-transient-for
    "transient-for" "GtkWindow" t t)
   (visible
    native-dialog-visible
    "visible" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'native-dialog 'type)
 "@version{2025-06-22}
  @begin{short}
    Native dialogs are platform dialogs that do not use the @class{gtk:dialog}
    or @class{gtk:window} classes.
  @end{short}
  They are used in order to integrate better with a platform, by looking the
  same as other native applications and supporting platform specific features.

  The @class{gtk:dialog} functions cannot be used on such objects, but we need
  a similar API in order to drive them. The @class{gtk:native-dialog} object is
  an API that allows you to do this. It allows you to set various common
  properties on the dialog, as well as show and hide it and get a
  @sig[gtk:native-dialog]{response} signal when the user finished with the
  dialog.

  Note that unlike the @class{gtk:dialog} widget, @class{gtk:native-dialog}
  objects are not toplevel widgets, and GTK does not keep them alive. It is
  your responsibility to keep a reference until you are done with the object.
  @begin[Signal Details]{dictionary}
    @begin[native-dialog::response]{signal}
      @begin{pre}
lambda (dialog response)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[dialog]{The @class{gtk:native-dialog} object on which the signal
          is emitted.}
        @entry[response]{The integer for the response ID.}
      @end{simple-table}
      Emitted when the user responds to the dialog. When this is called the
      dialog has been hidden. If you call the @fun{gtk:native-dialog-hide}
      function before the user responds to the dialog this signal will not be
      emitted.
    @end{signal}
  @end{dictionary}
  @see-slot{gtk:native-dialog-modal}
  @see-slot{gtk:native-dialog-title}
  @see-slot{gtk:native-dialog-transient-for}
  @see-slot{gtk:native-dialog-visible}
  @see-class{gtk:dialog}
  @see-class{gtk:file-chooser-native}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:native-dialog-modal ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "modal" 'native-dialog) t)
 "The @code{modal} property of type @code{:boolean} (Read / Write) @br{}
  Whether the window should be modal with respect to its transient parent. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'native-dialog-modal)
      "Accessor"
      (documentation 'native-dialog-modal 'function)
 "@version{2025-03-24}
  @syntax{(gtk:native-dialog-modal object) => modal}
  @syntax{(setf (gtk:native-dialog-modal object) modal)}
  @argument[object]{a @class{gtk:native-dialog} object}
  @argument[modal]{@em{true} if the dialog is modal}
  @begin{short}
    Accessor of the @slot[gtk:native-dialog]{modal} slot of the
    @class{gtk:native-dialog} class.
  @end{short}
  The @fun{gtk:native-dialog-modal} function returns whether the dialog is
  modal. The @setf{gtk:native-dialog-modal} function sets a dialog modal or
  non-modal.

  Modal dialogs prevent interaction with other windows in the same application.
  To keep modal dialogs on top of main application windows, use the
  @fun{gtk:native-dialog-transient-for} function to make the dialog transient
  for the parent. Most window managers will then disallow lowering the dialog
  below the parent.
  @see-class{gtk:native-dialog}
  @see-function{gtk:native-dialog-transient-for}")

;;; --- gtk:native-dialog-title ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "title" 'native-dialog) t)
 "The @code{title} property of type @code{:string} (Read / Write) @br{}
  The title of the dialog window. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'native-dialog-title)
      "Accessor"
      (documentation 'native-dialog-title 'function)
 "@version{2025-03-24}
  @syntax{(gtk:native-dialog-title object) => title}
  @syntax{(setf (gtk:native-dialog-title object) title)}
  @argument[object]{a @class{gtk:native-dialog} object}
  @argument[title]{a string for the title of the dialog, or @code{nil} if
    none has been set explicitly}
  @begin{short}
    Accessor of the @slot[gtk:native-dialog]{title} slot of the
    @class{gtk:native-dialog} class.
  @end{short}
  The @fun{gtk:native-dialog-title} function gets the title of the dialog. The
  @setf{gtk:native-dialog-title} function sets the title.
  @see-class{gtk:native-dialog}")

;;; --- gtk:native-dialog-transient-for ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "transient-for"
                                               'native-dialog) t)
 "The @code{transient-for} property of type @class{gtk:window}
  (Read / Write / Construct) @br{}
  The transient parent of the dialog, or @code{nil} for none.")

#+liber-documentation
(setf (liber:alias-for-function 'native-dialog-transient-for)
      "Accessor"
      (documentation 'native-dialog-transient-for 'function)
 "@version{2025-03-24}
  @syntax{(gtk:native-dialog-transient-for object) => parent}
  @syntax{(setf (gtk:native-dialog-transient-for object) parent)}
  @argument[object]{a @class{gtk:native-dialog} object}
  @argument[parent]{a @class{gtk:window} parent window}
  @begin{short}
    Accessor of the @slot[gtk:native-dialog]{transient-for} slot of the
    @class{gtk:native-dialog} class.
  @end{short}
  The @fun{gtk:native-dialog-transient-for} function fetches the transient
  parent for the dialog. The @setf{gtk:native-dialog-transient-for} function
  sets the parent.

  Dialog windows should be set transient for the main application window they
  were spawned from. This allows window managers, for example, to keep the
  dialog on top of the main window, or center the dialog over the main window.

  Passing @code{nil} for parent unsets the current transient window.
  @see-class{gtk:native-dialog}
  @see-class{gtk:window}")

;;; --- gtk:native-dialog-visible ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "visible" 'native-dialog) t)
 "The @code{visible} property of type @code{:boolean} (Read / Write) @br{}
  Whether the window is currently visible. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'native-dialog-visible)
      "Accessor"
      (documentation 'native-dialog-visible 'function)
 "@version{2025-03-24}
  @syntax{(gtk:native-dialog-visible object) => visible}
  @syntax{(setf (gtk:native-dialog-visible object) visible)}
  @argument[object]{a @class{gtk:native-dialog} object}
  @argument[visible]{@em{true} if the dialog is visible}
  @begin{short}
    Accessor of the @slot[gtk:native-dialog]{visible} slot of the
    @class{gtk:native-dialog} class.
  @end{short}
  Determines whether the dialog is visible.
  @see-class{gtk:native-dialog}")

;;; ----------------------------------------------------------------------------
;;; gtk_native_dialog_show
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_native_dialog_show" native-dialog-show) :void
 #+liber-documentation
 "@version{2025-06-22}
  @argument[dialog]{a @class{gtk:native-dialog} object}
  @begin{short}
    Shows the dialog on the display, allowing the user to interact with it.
  @end{short}
  When the user accepts the state of the dialog the dialog will be automatically
  hidden and the @sig[gtk:native-dialog]{response} signal will be emitted.
  Multiple calls while the dialog is visible will be ignored.
  @see-class{gtk:native-dialog}"
  (dialog (g:object native-dialog)))

(export 'native-dialog-show)

;;; ----------------------------------------------------------------------------
;;; gtk_native_dialog_hide
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_native_dialog_hide" native-dialog-hide) :void
 #+liber-documentation
 "@version{#2025-06-22}
  @argument[dialog]{a @class{gtk:native-dialog} object}
  @begin{short}
    Hides the dialog if it is visilbe, aborting any interaction.
  @end{short}
  Once this is called the @sig[gtk:native-dialog]{response} signal will not be
  emitted until after the next call to the @fun{gtk:native-dialog-show}
  function.

  If the dialog is not visible this does nothing.
  @see-class{gtk:native-dialog}
  @see-function{gtk:native-dialog-show}"
  (dialog (g:object native-dialog)))

(export 'native-dialog-hide)

;;; ----------------------------------------------------------------------------
;;; gtk_native_dialog_destroy
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_native_dialog_destroy" native-dialog-destroy) :void
 #+liber-documentation
 "@version{2025-03-24}
  @argument[dialog]{a @class{gtk:native-dialog} object}
  @begin{short}
    Destroys a dialog.
  @end{short}
  When a dialog is destroyed, it will break any references it holds to other
  objects. If it is visible it will be hidden and any underlying window system
  resources will be destroyed.

  Note that this does not release any reference to the object, as opposed to
  destroying a @class{gtk:window} widget, because there is no reference from
  the windowing system to the @class{gtk:native-dialog} object.
  @see-class{gtk:native-dialog}
  @see-class{gtk:window}"
  (dialog (g:object native-dialog)))

(export 'native-dialog-destroy)

;;; --- End of file gtk4.native-dialog.lisp ------------------------------------

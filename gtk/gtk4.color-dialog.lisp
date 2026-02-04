;;; ----------------------------------------------------------------------------
;;; gtk4.color-dialog.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.20 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2023 - 2026 Dieter Kaiser
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
;;;     GtkColorDialog
;;;
;;; Accessors
;;;
;;;     gtk_color_dialog_get_modal
;;;     gtk_color_dialog_set_modal
;;;     gtk_color_dialog_get_title
;;;     gtk_color_dialog_set_title
;;;     gtk_color_dialog_get_with-alpha
;;;     gtk_color_dialog_set_with-alpha
;;;
;;; Functions
;;;
;;;     gtk_color_dialog_new
;;;     gtk_color_dialog_choose_rgba
;;;     gtk_color_dialog_choose_rgba_finish
;;;
;;; Properties
;;;
;;;     modal
;;;     title
;;;     with-alpha
;;;
;;; Hierachy
;;;
;;;     GObject
;;;     ╰── GtkColorDialog
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkColorDialog
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkColorDialog" color-dialog
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gtk_color_dialog_get_type")
  ((modal
    color-dialog-modal
    "modal" "gboolean" t t)
   (title
    color-dialog-title
    "title" "gchararray" t t)
   (with-alpha
    color-dialog-with-alpha
    "with-alpha" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'color-dialog 'type)
 "@version{2025-08-06}
  @begin{short}
    The @class{gtk:color-dialog} object collects the arguments that are needed
    to present a color chooser dialog to the user, such as a title for the
    dialog and whether it should be modal.
  @end{short}

  The dialog is shown with the @fun{gtk:color-dialog-choose-rgba} function. This
  API follows the GIO async pattern, and the result can be obtained by calling
  the @fun{gtk:color-dialog-choose-rgba-finish} function.

  See the @class{gtk:color-dialog-button} widget for a convenient control that
  uses the @class{gtk:color-dialog} object and presents the results.

  Since 4.10
  @see-constructor{gtk:color-dialog-new}
  @see-slot{gtk:color-dialog-modal}
  @see-slot{gtk:color-dialog-title}
  @see-slot{gtk:color-dialog-with-alpha}
  @see-class{gtk:color-dialog-button}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:color-dialog-modal -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "modal" 'color-dialog) t)
 "The @code{modal} property of type @code{:boolean} (Read / Write) @br{}
  Whether the color chooser dialog is modal. @br{}
  Default value: @em{true}")

#+ liber-documentation
(setf (liber:alias-for-function 'color-dialog-modal)
      "Accessor"
      (documentation 'color-dialog-modal 'function)
 "@version{2025-08-06}
  @syntax{(gtk:color-dialog-modal object) => modal)}
  @syntax{(setf (gtk:color-dialog-modal object) modal)}
  @argument[object]{a @class{gtk:color-dialog} object}
  @argument[modal]{a boolean whether the color chooser dialog is modal}
  @begin{short}
    The accessor for the @slot[gtk:color-dialog]{modal} slot of the
    @class{gtk:color-dialog} class gets or sets whether the color chooser dialog
    blocks interaction with the parent window while it is presented.
  @end{short}

  Since 4.10
  @see-class{gtk:color-dialog}")

;;; --- gtk:color-dialog-title -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "title" 'color-dialog) t)
 "The @code{title} property of type @code{:string} (Read / Write) @br{}
  The title that may be shown on the color chooser dialog that is presented by
  the @fun{gtk:color-dialog-choose-rgba} function. @br{}
  Default value: @code{nil}")

#+ liber-documentation
(setf (liber:alias-for-function 'color-dialog-title)
      "Accessor"
      (documentation 'color-dialog-title 'function)
 "@version{2025-08-06}
  @syntax{(gtk:color-dialog-title object) => title)}
  @syntax{(setf (gtk:color-dialog-title object) title)}
  @argument[object]{a @class{gtk:color-dialog} object}
  @argument[title]{a string for the title}
  @begin{short}
    The accessor for the @slot[gtk:color-dialog]{title} slot of the
    @class{gtk:color-dialog} class gets or sets the title that will be shown on
    the color chooser dialog.
  @end{short}

  Since 4.10
  @see-class{gtk:color-dialog}")

;;; --- gtk:color-dialog-with-alpha --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "with-alpha" 'color-dialog) t)
 "The @code{with-alpha} property of type @code{:boolean} (Read / Write) @br{}
  Whether colors may have alpha (translucency). If @em{false}, the color that is
  selected will be forced to have @code{alpha = 1}. @br{}
  Default value: @em{true}")

#+ liber-documentation
(setf (liber:alias-for-function 'color-dialog-with-alpha)
      "Accessor"
      (documentation 'color-dialog-with-alpha 'function)
 "@version{2025-08-06}
  @syntax{(gtk:color-dialog-with-alpha object) => setting)}
  @syntax{(setf (gtk:color-dialog-with-alpha object) setting)}
  @argument[object]{a @class{gtk:color-dialog} object}
  @argument[setting]{a boolean whether colors may have alpha}
  @begin{short}
    The accessor for the @slot[gtk:color-dialog]{with-alpha} slot of the
    @class{gtk:color-dialog} class get or sets whether colors may have alpha.
  @end{short}

  Since 4.10
  @see-class{gtk:color-dialog}")

;;; ----------------------------------------------------------------------------
;;; gtk_color_dialog_new
;;; ----------------------------------------------------------------------------

(declaim (inline color-dialog-new))

(defun color-dialog-new ()
 #+liber-documentation
 "@version{2025-08-06}
  @return{The new @class{gtk:color-dialog} object.}
  @short{Creates a new @class{gtk:color-dialog} object.}

  Since 4.10
  @see-class{gtk:color-dialog}"
  (make-instance 'color-dialog))

(export 'color-dialog-new)

;;; ----------------------------------------------------------------------------
;;; gtk_color_dialog_choose_rgba
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_color_dialog_choose_rgba" %color-dialog-choose-rgba) :void
  (dialog (g:object color-dialog))
  (parent (g:object window))
  (color (g:boxed gdk:rgba))
  (cancellable (g:object g:cancellable))
  (func :pointer)
  (data :pointer))

(defun color-dialog-choose-rgba (dialog parent color cancellable func)
 #+liber-documentation
 "@version{#2025-08-06}
  @argument[dialog]{a @class{gtk:color-dialog} object}
  @argument[parent]{a parent @class{gtk:window} widget}
  @argument[color]{a @class{gdk:rgba} instance for the color to select
    initially}
  @argument[cancellable]{a @class{g:cancellable} object to cancel the operation}
  @argument[func]{a @sym{g:async-ready-callback} callback function to call when
    the operation is complete}
  @begin{short}
    This function initiates a color choice operation by presenting a color
    chooser dialog to the user.
  @end{short}
  The callback function will be called when the dialog is dismissed. It should
  call the @fun{gtk:color-dialog-choose-rgba-finish} function to obtain the
  result.

  Since 4.10
  @see-class{gtk:color-dialog}
  @see-class{gtk:window}
  @see-class{gdk:rgba}
  @see-class{g:cancellable}
  @see-symbol{g:async-ready-callback}
  @see-function{gtk:color-dialog-choose-rgba-finish}"
  (let ((ptr (glib:allocate-stable-pointer func)))
    (%color-dialog-choose-rgba dialog
                               parent
                               color
                               cancellable
                               (cffi:callback g:async-ready-callback)
                               ptr)))

(export 'color-dialog-choose-rgba)

;;; ----------------------------------------------------------------------------
;;; gtk_color_dialog_choose_rgba_finish
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_color_dialog_choose_rgba_finish"
               %color-dialog-choose-rgba-finish) (g:boxed gdk:rgba)
  (dialog (g:object color-dialog))
  (result (g:object g:async-result))
  (err :pointer))

(defun color-dialog-choose-rgba-finish (dialog result)
 #+liber-documentation
 "@version{#2025-08-06}
  @argument[dialog]{a @class{gtk:color-dialog} object}
  @argument[result]{a @class{g:async-result} instance}
  @return{The @class{gdk:rgba} instance for the selected color, or @code{nil}.}
  @begin{short}
    Finishes the @fun{gtk:color-dialog-choose-rgba} function call and returns
    the resulting color.
  @end{short}

  Since 4.10
  @see-class{gtk:color-dialog}
  @see-class{g:async-result}
  @see-class{gdk:rgba}
  @see-function{gtk:color-dialog-choose-rgba}"
  (glib:with-ignore-error (err)
    (%color-dialog-choose-rgba-finish dialog result err)))

(export 'color-dialog-choose-rgba-finish)

;;; --- End of file gtk4.color-dialog.lisp -------------------------------------

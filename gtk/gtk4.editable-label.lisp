;;; ----------------------------------------------------------------------------
;;; gtk4.editable-label.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.12 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 - 2023 Dieter Kaiser
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
;;; GtkEditableLabel
;;;
;;;     A label that can be edited
;;;
;;; Types and Values
;;;
;;;     GtkEditableLabel
;;;
;;; Accessors
;;;
;;;     gtk_editable_label_get_editing
;;;
;;; Functions
;;;
;;;     gtk_editable_label_new
;;;     gtk_editable_label_start_editing
;;;     gtk_editable_label_stop_editing
;;;
;;; Properties
;;;
;;;     editing
;;;
;;; Actions
;;;
;;;     editing.stop
;;;     editing.start
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkEditableLabel
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
;;; struct GtkEditableLabel
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkEditableLabel" editable-label
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget"
                "GtkEditable")
   :type-initializer "gtk_editable_label_get_type")
  ((editing
    editable-label-editing
    "editing" "gboolean" t nil)))

#+liber-documentation
(setf (documentation 'editable-label 'type)
 "@version{#2022-6-12}
  @begin{short}
    A @class{gtk:editable-label} widget is a @class{gtk:label} widget that allows
    users to edit the text by switching the widget to an \"edit mode\".
  @end{short}

  @image[editable-label]{Figure: GtkEditableLabel}

  The @class{gtk:editable-label} widget does not have API of its own, but it
  implements the @class{gtk:editable} interface.

  The default bindings for activating the edit mode is to click or press the
  @kbd{Enter} key. The default bindings for leaving the edit mode are the
  @kbd{Enter} key, to save the results, or the @kbd{Escape} key, to cancel the
  editing.
  @begin[CSS Nodes]{dictionary}
    @begin{pre}
editablelabel[.editing]
╰── stack
    ├── label
    ╰── text
    @end{pre}
    The @class{gtk:editable-label} implementation has a main node with the name
    @code{editablelabel}. When the entry is in editing mode, it gets the
    @code{.editing} style class. For all the subnodes added to the text node in
    various situations, see the @class{gtk:text} documentation.
  @end{dictionary}
  @begin[Action Details]{dictionary}
    @subheading{The \"editing.stop\" action}
      Switch the widget out of editing mode. If @arg{commit} is @em{true}, then
      the results of the editing are taken as the new value of the
      @slot[gtk:editable]{text} property. The default binding for this action
      is the @kbd{Escape} key. This action is disabled when the @code{editing}
      property is @em{false}.
      @begin[code]{table}
        @entry[commit]{A boolean whether to make changes permanent.}
      @end{table}
    @subheading{The \"editing.start\" action}
      Switch the widget into editing mode, so that the user can make changes to
      the text. The default bindings for this action are clicking on the widget
      and the @kbd{Enter} key. This action is disabled when the @code{editing}
      property is @em{false}.
  @end{dictionary}
  @see-constructor{gtk:editable-label-new}
  @see-slot{gtk:editable-label-editing}
  @see-class{gtk:editable}
  @see-class{gtk:label}
  @see-class{gtk:entry}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- editable-label-editing -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "editing" 'editable-label) t)
 "The @code{editing} property of type @code{:boolean} (Read) @br{}
  The property is @em{true} while the widget is in edit mode. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'editable-label-editing)
      "Accessor"
      (documentation 'editable-label-editing 'function)
 "@version{#2022-6-12}
  @syntax[]{(gtk:editable-label-editing object) => setting}
  @syntax[]{(setf (gtk:editable-label-editing object) setting)}
  @argument[object]{a @class{gtk:editable-label} widget}
  @argument[setting]{a boolean whether the label is currently in editing mode}
  @begin{short}
    Accessor of the @slot[gtk:editable-label]{editing} slot of the
    @class{gtk:editable-label} class.
  @end{short}
  The @fun{gtk:editable-label-editing} function returns whether the label is
  currently in editing mode.
  @see-class{gtk:editable-label}")

;;; ----------------------------------------------------------------------------
;;; gtk_editable_label_new ()
;;; ----------------------------------------------------------------------------

(defun editable-label-new (text)
 "@version{#2022-6-12}
  @argument[text]{a string with the text for the label}
  @return{The new @class{gtk:editable-label} widget.}
  @short{Creates a new editable label.}
  @see-class{gtk:editable-label}"
  (make-instance 'editable-label
                 :text text))

(export 'editable-label-new)

;;; ----------------------------------------------------------------------------
;;; gtk_editable_label_start_editing ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_editable_label_start_editing" editable-label-start-editing)
    :void
 #+liber-documentation
 "@version{#2023-5-4}
  @argument[label]{a @class{gtk:editable-label} widget}
  @short{Switches the label into \"editing mode\".}
  @see-class{gtk:editable-label}"
  (label (g:object editable-label)))

(export 'editable-label-start-editing)

;;; ----------------------------------------------------------------------------
;;; gtk_editable_label_stop_editing ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_editable_label_stop_editing" editable-label-stop-editing)
    :void
 #+liber-documentation
 "@version{#2023-5-4}
  @argument[label]{a @class{gtk:editable-label} widget}
  @argument[commit]{a boolean whether to set the edited text on the label}
  @begin{short}
    Switches the label out of “editing mode”.
  @end{short}
  If commit is @em{true}, the resulting text is kept as the
  @slot[gtk:editable]{text} property value, otherwise the resulting text is
  discarded and the label will keep its previous @slot[gtk:editable]{text}
  property value.
  @see-class{gtk:editable-label}
  @see-function{gtk:editable-text}"
  (label (g:object editable-label))
  (commit :boolean))

(export 'editable-label-stop-editing)

;;; --- End of file gtk4.editable-label.lisp -----------------------------------

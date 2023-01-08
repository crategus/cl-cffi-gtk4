;;; ----------------------------------------------------------------------------
;;; gtk.editable-label.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 Dieter Kaiser
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

(define-g-object-class "GtkEditableLabel" editable-label
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
    A @sym{gtk:editable-label} widget is a @class{gtk:label} widget that allows
    users to edit the text by switching the widget to an \"edit mode\".
  @end{short}

  @image[editable-label]{Figure: GtkEditableLabel}

  The @sym{gtk:editable-label} widget does not have API of its own, but it
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
    The @sym{gtk:editable-label} implementation has a main node with the name
    @code{editablelabel}. When the entry is in editing mode, it gets the
    @code{.editing} style class.

    For all the subnodes added to the text node in various situations, see
    the @class{gtk:text} documentation.
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
  @see-slot{gtk:editable-label}
  @see-constructor{gtk:editable-label-new}
  @see-class{gtk:editable}
  @see-class{gtk:label}
  @see-class{gtk:entry}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- editable-label-editing ---------------------------------------------

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

  The @sym{gtk:editable-label-editing} function returns whether the label is
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
;;;
;;; void
;;; gtk_editable_label_start_editing (GtkEditableLabel *self);
;;;
;;; Switches the label into “editing mode”.
;;;
;;; label:
;;;     a GtkEditableLabel
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_editable_label_start_editing" editable-label-start-editing)
    :void
  (label (g:object editable-label)))

(export 'editable-label-start-editing)

;;; ----------------------------------------------------------------------------
;;; gtk_editable_label_stop_editing ()
;;;
;;; void
;;; gtk_editable_label_stop_editing (GtkEditableLabel *self,
;;;                                  gboolean commit);
;;;
;;; Switches the label out of “editing mode”. If commit is TRUE, the resulting
;;; text is kept as the “text” property value, otherwise the resulting text is
;;; discarded and the label will keep its previous “text” property value.
;;;
;;; self :
;;;     a GtkEditableLabel
;;;
;;; commit :
;;;     whether to set the edited text on the label
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_editable_label_stop_editing" editable-label-stop-editing)
    :void
  (label (g:object editable-label))
  (commit :boolean))

(export 'editable-label-stop-editing)

;;; --- End of file gtk.editable-label.lisp ------------------------------------

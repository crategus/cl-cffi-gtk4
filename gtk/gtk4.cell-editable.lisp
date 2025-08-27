;;; ----------------------------------------------------------------------------
;;; gtk4.cell-editable.lisp
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
;;; GtkCellEditable
;;;
;;;     Interface for widgets that can be used for editing cells
;;;
;;; Types and Values
;;;
;;;     GtkCellEditable
;;;
;;; Functions
;;;
;;;     gtk_cell_editable_start_editing
;;;     gtk_cell_editable_editing_done
;;;     gtk_cell_editable_remove_widget
;;;
;;; Properties
;;;
;;;     editing-canceled
;;;
;;; Signals
;;;
;;;     editing-done
;;;     remove-widget
;;;
;;; Object Hierarchy
;;;
;;;     GInterface
;;;     ╰── GtkCellEditable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkCellEditable
;;; ----------------------------------------------------------------------------

(gobject:define-ginterface "GtkCellEditable" cell-editable
  (:export t
   :type-initializer "gtk_cell_editable_get_type")
  ((editing-canceled
    cell-editable-editing-canceled
    "editing-canceled" "gboolean" t t)))

#+liber-documentation
(setf (liber:alias-for-class 'cell-editable)
      "Interface"
      (documentation 'cell-editable 'type)
 "@version{2025-07-22}
  @begin{short}
    The @class{gtk:cell-editable} interface must be implemented for widgets to
    be usable when editing the contents of a @class{gtk:tree-view} cell widget.
  @end{short}
  It provides a way to specify how temporary widgets should be configured for
  editing, get the new value, and so on.
  @begin[Warning]{dictionary}
    The @class{gtk:cell-editable} implementation is deprecated since 4.10,
    List views use widgets for displaying their contents. See the
    @class{gtk:editable} interface for editable text widgets.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @begin[cell-editable::editing-done]{signal}
      @begin{pre}
lambda (editable)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[editable]{The @class{gtk:cell-editable} object on which the
          signal was emitted.}
      @end{simple-table}
      The signal is a sign for the cell renderer to update its value from the
      @arg{editable} argument. Implementations of the @class{gtk:cell-editable}
      class are responsible for emitting the signal when they are done editing,
      for example, the @class{gtk:entry} widget is emitting it when the user
      presses the @kbd{Enter} key. The @fun{gtk:cell-editable-editing-done}
      function is a convenience method for emitting the
      @sig[gtk:cell-editable]{editing-done} signal.
    @end{signal}
    @begin[cell-editable::remove-widget]{signal}
      @begin{pre}
lambda (editable)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[editable]{The @class{gtk:cell-editable} object on which the
          signal was emitted.}
      @end{simple-table}
      The signal is meant to indicate that the cell is finished editing, and
      the widget may now be destroyed. Implementations of the
      @class{gtk:cell-editable} class are responsible for emitting the signal
      when they are done editing. It must be emitted after the
      @sig[gtk:cell-editable]{editing-done} signal, to give the cell renderer a
      chance to update the value of the cell before the widget is removed. The
      @fun{gtk:cell-editable-remove-widget} function is a convenience method
      for emitting the @sig[gtk:cell-editable]{remove-widget} signal.
    @end{signal}
  @end{dictionary}
  @see-slot{gtk:cell-editable-editing-canceled}
  @see-class{gtk:cell-renderer}
  @see-function{gtk:cell-editable-editing-done}
  @see-function{gtk:cell-editable-remove-widget}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "editing-canceled"
                                               'cell-editable) t)
 "The @code{editing-canceled} property of type @code{:boolean} (Read / Write)
  @br{}
  Indicates whether editing on the cell has been canceled. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'cell-editable-editing-canceled)
      "Accessor"
      (documentation 'cell-editable-editing-canceled 'function)
 "@version{#2025-08-17}
  @syntax{(gtk:cell-editable-editing-canceled object) => canceled}
  @syntax{(setf (gtk:cell-editable-editing-canceled object) canceled)}
  @argument[object]{a @class{gtk:cell-editable} object}
  @argument[canceled]{a boolean whether editing on the cell has been canceled}
  @begin{short}
    The accessor for the @slot[gtk:cell-editable]{editing-canceled} slot of the
    @class{gtk:cell-editable} class gets or sets whether editing on the cell has
    been canceled.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-editable} implementation is deprecated since 4.10,
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-editable}")

;;; ----------------------------------------------------------------------------
;;; gtk_cell_editable_start_editing
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_editable_start_editing" cell-editable-start-editing)
    :void
 #+liber-documentation
 "@version{#2025-07-22}
  @argument[editable]{a @class{gtk:cell-editable} object}
  @argument[event]{a @class{gdk:event} instance that began the editing process,
    or @code{nil} if editing was initiated programmatically}
  @begin{short}
    Begins editing on a cell editable.
  @end{short}
  The @class{gtk:cell-renderer} object for the cell creates and returns a
  @class{gtk:cell-editable} object from the
  @fun{gtk:cell-renderer-start-editing} function, configured for the
  @class{gtk:cell-renderer} type.

  The @fun{gtk:cell-editable-start-editing} function can then set up the
  @arg{editable} argument suitably for editing a cell, for example, making the
  @kbd{Esc} key emit the @sig[gtk:cell-editable]{editing-done} signal.

  Note that the @arg{editable} argument is created on-demand for the current
  edit. Its lifetime is temporary and does not persist across other edits
  and/or cells.
  @begin[Warning]{dictionary}
    The @class{gtk:cell-editable} implementation is deprecated since 4.10,
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-editable}
  @see-class{gtk:cell-renderer}
  @see-class{gdk:event}
  @see-function{gtk:cell-renderer-start-editing}"
  (editable (g:object cell-editable))
  (event gdk:event))

(export 'cell-editable-start-editing)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_editable_editing_done
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_editable_editing_done" cell-editable-editing-done)
    :void
 #+liber-documentation
 "@version{#2025-07-22}
  @argument[editable]{a @class{gtk:cell-editable} object}
  @begin{short}
    Emits the @sig[gtk:cell-editable]{editing-done} signal.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-editable} implementation is deprecated since 4.10,
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-editable}"
  (editable (g:object cell-editable)))

(export 'cell-editable-editing-done)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_editable_remove_widget
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_editable_remove_widget" cell-editable-remove-widget)
    :void
 #+liber-documentation
 "@version{#2025-07-22}
  @argument[editable]{a @class{gtk:cell-editable} object}
  @begin{short}
    Emits the @sig[gtk:cell-editable]{remove-widget} signal.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-editable} implementation is deprecated since 4.10,
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-editable}"
  (editable (g:object cell-editable)))

(export 'cell-editable-remove-widget)

;;; --- End of file gtk4.cell-editable.lisp ------------------------------------

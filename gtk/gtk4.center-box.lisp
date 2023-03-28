;;; ----------------------------------------------------------------------------
;;; gtk4.center-box.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.6 and modified to document the Lisp binding to the GTK library.
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
;;; GtkCenterBox
;;;
;;;     A centering container
;;;
;;; Types and Values
;;;
;;;     GtkCenterBox
;;;
;;; Accessors
;;;
;;;     gtk_center_box_set_baseline_position
;;;     gtk_center_box_get_baseline_position
;;;
;;; Functions
;;;
;;;     gtk_center_box_new
;;;     gtk_center_box_set_start_widget
;;;     gtk_center_box_set_center_widget
;;;     gtk_center_box_set_end_widget
;;;     gtk_center_box_get_start_widget
;;;     gtk_center_box_get_center_widget
;;;     gtk_center_box_get_end_widget
;;;
;;; Properties
;;;
;;;     baseline-position
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;;            ╰── GtkCenterBox
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;;     GtkOrientable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkCenterBox
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkCenterBox" center-box
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget"
                "GtkOrientable")
   :type-initializer "gtk_center_box_get_type")
  ((baseline-position
    center-box-baseline-position
    "baseline-position" "GtkBaselinePosition" t t)))

#+liber-documentation
(setf (documentation 'center-box 'type)
 "@version{#2022-2-2}
  @begin{short}
    The @sym{gtk:center-box} widget arranges three children in a horizontal or
    vertical arrangement, keeping the middle child centered as well as possible.
  @end{short}

  @image[centerbox]{Figure: GtkCenterBox}

  To add children to the @sym{gtk:center-box} widget, use the
  @fun{gtk:center-box-start-widget}, @fun{gtk:center-box-center-widget} and
  @fun{gtk:center-box-end-widget} functions.

  The sizing and positioning of children can be influenced with the align and
  expand properties of the children.
  @begin[GtkCenterBox as GtkBuildable]{dictionary}
    The @sym{gtk:center-box} implementation of the @class{gtk:buildable}
    interface supports placing children in the 3 positions by specifying
    @code{start}, @code{center} or @code{end} as the @code{type} attribute of a
    @code{<child>} element.
  @end{dictionary}
  @begin[CSS nodes]{dictionary}
    The @sym{gtk:center-box} implementation uses a single CSS node with the
    name @code{box}.

    The first child of the @sym{gtk:center-box} widet will be allocated
    depending on the text direction, i.e. in left-to-right layouts it will be
    allocated on the left and in right-to-left layouts on the right. In vertical
    orientation, the nodes of the children are arranged from top to bottom.
  @end{dictionary}
  @begin[Accessibility]{dictionary}
    The @sym{gtk:center-box} implementation uses the @code{:group} value of the
    @symbol{gtk:accessible-role} enumeration.
  @end{dictionary}
  @see-slot{gtk:center-box-baseline-position}
  @see-class{gtk:box}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- center-box-baseline-position ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "baseline-position"
                                               'center-box) t)
 "The @code{baseline-position} property of type @symbol{gtk:baseline-position}
  (Read / Write) @br{}
  The position of the baseline aligned widgets if extra space is available.@br{}
  Default value: @code{:center}")

#+liber-documentation
(setf (liber:alias-for-function 'center-box-baseline-position)
      "Accessor"
      (documentation 'center-box-baseline-position 'function)
 "@version{#2022-2-22}
  @syntax[]{(gtk:center-box-baseline-position object) => position}
  @syntax[]{(setf (gtk:center-box-baseline-position object) position)}
  @argument[object]{a @class{gtk:center-box} widget}
  @argument[position]{a value of the @symbol{gtk:baseline-position} enumeration}
  @begin{short}
    Accessor of the @slot[gtk:box]{baseline-position} slot of the
    @class{gtk:center-box} class.
  @end{short}
  The @sym{gtk:center-box-baseline-position} function gets the baseline position
  of a center box. The @sym{(setf gtk:center-box-baseline-position)} function
  sets the baseline position.

  This affects only horizontal boxes with at least one baseline aligned child
  widget. If there is more vertical space available than requested, and the
  baseline is not allocated by the parent widget then the @arg{position} value
  is used to allocate the baseline with respect to the extra space available.
  @see-class{gtk:center-box}
  @see-symbol{gtk:baseline-position}")

;;; ----------------------------------------------------------------------------
;;; gtk_center_box_new
;;; ----------------------------------------------------------------------------

(defun center-box-new ()
 #+liber-documentation
 "@version{#2022-2-2}
  @return{The new @class{gtk:center-box} widget.}
  @short{Creates a new @class{gtk:center-box} widget.}
  @see-class{gtk:center-box}"
  (make-instance 'center-box))

(export 'center-box-new)

;;; ----------------------------------------------------------------------------
;;;gtk_center_box_set_start_widget
;;;gtk_center_box_get_start_widget -> center-box-start-widget
;;; ----------------------------------------------------------------------------

(defun (setf center-box-start-widget) (value box)
  (cffi:foreign-funcall "gtk_center_box_set_start_widget"
                        (g:object center-box) box
                        (g:object widget) value
                        :void)
  value)

(defcfun ("gtk_center_box_get_start_widget" center-box-start-widget)
    (g:object widget)
 #+liber-documentation
 "@version{#2022-2-3}
  @syntax[]{(gtk:center-box-start-widget box) => child}
  @syntax[]{(setf (gtk:center-box-start-widget box) child)}
  @argument[box]{a @class{gtk:center-box} widget}
  @argument[child]{a @class{gtk:widget} start widget}
  @begin{short}
    Accessor function of the start widget of the center widget.
  @end{short}

  The @sym{gtk:center-box-start-widget} function gets the start widget, or
  @code{nil} if there is none. The @sym{(setf gtk:center-box-start-widget)}
  function sets the start widget. To remove the existing start widget, pass
  @code{nil}.
  @see-class{gtk:center-box}"
  (box (g:object center-box)))

(export 'center-box-start-widget)

;;; ----------------------------------------------------------------------------
;;;gtk_center_box_set_center_widget
;;;gtk_center_box_get_center_widget -> center-box-center-widget
;;; ----------------------------------------------------------------------------

(defun (setf center-box-center-widget) (value box)
  (cffi:foreign-funcall "gtk_center_box_set_center_widget"
                        (g:object center-box) box
                        (g:object widget) value
                        :void)
  value)

(defcfun ("gtk_center_box_get_center_widget" center-box-center-widget)
    (g:object widget)
 #+liber-documentation
 "@version{#2022-2-3}
  @syntax[]{(gtk:center-box-center-widget box) => child}
  @syntax[]{(setf (gtk:center-box-center-widget box) child)}
  @argument[box]{a @class{gtk:center-box} widget}
  @argument[child]{a @class{gtk:widget} center widget}
  @begin{short}
    Accessor function of the center widget of the center widget.
  @end{short}

  The @sym{gtk:center-box-center-widget} function gets the center widget, or
  @code{nil} if there is none. The @sym{(setf gtk:center-box-center-widget)}
  function sets the center widget. To remove the existing center widget, pass
  @code{nil}.
  @see-class{gtk:center-box}"
  (box (g:object center-box)))

(export 'center-box-center-widget)

;;; ----------------------------------------------------------------------------
;;;gtk_center_box_set_end_widget
;;;gtk_center_box_get_end_widget -> center-box-end-widget
;;; ----------------------------------------------------------------------------

(defun (setf center-box-end-widget) (value box)
  (cffi:foreign-funcall "gtk_center_box_set_end_widget"
                        (g:object center-box) box
                        (g:object widget) value
                        :void)
  value)

(defcfun ("gtk_center_box_get_end_widget" center-box-end-widget)
    (g:object widget)
 #+liber-documentation
 "@version{#2022-2-3}
  @syntax[]{(gtk:center-box-end-widget box) => child}
  @syntax[]{(setf (gtk:center-box-end-widget box) child)}
  @argument[box]{a @class{gtk:center-box} widget}
  @argument[child]{a @class{gtk:widget} end widget}
  @begin{short}
    Accessor function of the end widget of the center widget.
  @end{short}

  The @sym{gtk:center-box-end-widget} function gets the end widget, or
  @code{nil} if there is none. The @sym{(setf gtk:center-box-end-widget)}
  function sets the end widget. To remove the existing end widget, pass
  @code{nil}.
  @see-class{gtk:center-box}"
  (box (g:object center-box)))

(export 'center-box-end-widget)

;;; --- End of file gtk4.center-box.lisp ---------------------------------------

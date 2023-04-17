;;; ----------------------------------------------------------------------------
;;; gtk4.fixed-layout.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.10 and modified to document the Lisp binding to the GTK library.
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
;;; GtkFixedLayout
;;;
;;;     A layout manager that allows positioning at fixed coordinates
;;;
;;; Types and Values
;;;
;;;     GtkFixedLayout
;;;     GtkFixedLayoutChild
;;;
;;; Functions
;;;
;;;     gtk_fixed_layout_new
;;;
;;;     gtk_fixed_layout_child_set_transform
;;;     gtk_fixed_layout_child_get_transform
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ├── GtkLayoutChild
;;;     │   ╰── GtkFixedLayoutChild
;;;     ╰── GtkLayoutManager
;;;         ╰── GtkFixedLayout
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkFixedLayoutChild
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkFixedLayoutChild" fixed-layout-child
  (:superclass layout-child
   :export t
   :interfaces ()
   :type-initializer "gtk_fixed_layout_child_get_type")
  ((transform
    fixed-layout-child-transform
    "transform" "GskTransform" t t)))

#+liber-documentation
(setf (documentation 'fixed-layout-child 'type)
 "@version{#2023-4-16}
  @begin{short}
    The @sym{gtk:layout-child} subclass for children in a
    @class{gtk:fixed-layout} class.
  @end{short}
  @see-class{gtk:fixed-layout}
  @see-class{gtk:layout-child}
  @see-class{gsk:transform}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "transform"
                                               'fixed-layout-child) t)
 "The @code{transform} property of type @class{gsk:transform} (Read / Write)
  @br{}
  The transform of the child widget.")

#+liber-documentation
(setf (liber:alias-for-function 'fixed-layout-child-transform)
      "Accessor"
      (documentation 'fixed-layout-child-transform 'function)
 "@version{#2023-4-16}
  @syntax[]{(gtk:fixed-layout-child-transform object) => transform}
  @syntax[]{(setf (gtk:fixed-layout-child-transform object) transform)}
  @argument[object]{a @class{gtk:fixed-layout-child} widget}
  @argument[transform]{a @class{gsk:transform} instance}
  @begin{short}
    Accessor of the @slot[gtk:fixed-layout-child]{transform} slot of the
    @class{gtk:fixed-layout-child} class.
  @end{short}
  The @sym{gtk:fixed-layout-child-transform} function retrieves the
  transformation of the child widget of a @class{gtk:fixed-layout} object.
  The @sym{(setf gtk:fixed-layout-child-transform)} function sets the transform.
  @see-class{gtk:fixed-layout-child}
  @see-class{gsk:transform}")

;;; ----------------------------------------------------------------------------
;;; GtkFixedLayout
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkFixedLayout" fixed-layout
  (:superclass layout-manager
   :export t
   :interfaces ()
   :type-initializer "gtk_fixed_layout_get_type")
  nil)

#+liber-documentation
(setf (documentation 'fixed-layout 'type)
 "@version{#2023-4-16}
  @begin{short}
    The @sym{gtk:fixed-layout} object is a layout manager which can place child
    widgets at fixed positions, and with fixed sizes.
  @end{short}

  Most applications should never use this layout manager. Fixed positioning and
  sizing requires constant recalculations on where children need to be
  positioned and sized. Other layout managers perform this kind of work
  internally so that application developers do not need to do it. Specifically,
  widgets positioned in a fixed layout manager will need to take into account:
  @begin{itemize}
    @begin{item}
      Themes, which may change widget sizes.
    @end{item}
    @begin{item}
      Fonts other than the one you used to write the app will of course change
      the size of widgets containing text; keep in mind that users may use a
      larger font because of difficulty reading the default, or they may be
      using a different OS that provides different fonts.
    @end{item}
    @begin{item}
      Translation of text into other languages changes its size. Also, display
      of non-English text will use a different font in many cases.
    @end{item}
  @end{itemize}
  In addition, the @sym{gtk:fixed-layout} object does not pay attention to text
  direction and thus may produce unwanted results if your app is run under
  right-to-left languages such as Hebrew or Arabic. That is: normally GTK will
  order containers appropriately depending on the text direction, e.g. to put
  labels to the right of the thing they label when using an RTL language. The
  @sym{gtk:fixed-layout} object will not be able to do that for you.

  Finally, fixed positioning makes it kind of annoying to add/remove GUI
  elements, since you have to reposition all the other elements. This is a
  long-term maintenance problem for your application.
  @see-class{gtk:layout-manager}
  @see-class{gtk:fixed-layout-child}")

;;; ----------------------------------------------------------------------------
;;; gtk_fixed_layout_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline fixed-layout-new))

(defun fixed-layout-new ()
 #+liber-documentation
 "@version{#2023-4-16}
  @return{A newly created @class{gtk:fixed-layout} object.}
  @begin{short}
    Creates a new @class{gtk:fixed-layout} object.
  @end{short}
  @see-class{gtk:fixed-layout}"
  (make-instance 'fixed-layout))

(export 'fixed-layout-new)

;;; --- End of file gtk4.fixed-layout.lisp -------------------------------------

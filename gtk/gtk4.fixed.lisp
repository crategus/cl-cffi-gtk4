;;; ----------------------------------------------------------------------------
;;; gtk4.fixed.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2023 Dieter Kaiser
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
;;; GtkFixed
;;;
;;;     A container which allows you to position widgets at fixed coordinates
;;;
;;; Types and Values
;;;
;;;     GtkFixed
;;;
;;; Functions
;;;
;;;     gtk_fixed_new
;;;     gtk_fixed_put
;;;     gtk_fixed_remove
;;;     gtk_fixed_move
;;;     gtk_fixed_get_child_position
;;;     gtk_fixed_get_child_transform
;;;     gtk_fixed_set_child_transform
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkFixed
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkFixed
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkFixed" fixed
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget")
   :type-initializer "gtk_fixed_get_type")
  nil)

#+liber-documentation
(setf (documentation 'fixed 'type)
 "@version{#2022-2-6}
  @begin{short}
    The @sym{gtk:fixed} widget is a container which can place child widgets at
    fixed positions and with fixed sizes, given in pixels.
  @end{short}
  The fixed widget performs no automatic layout management.

  For most applications, you should not use this container. It keeps you from
  having to learn about the other GTK containers, but it results in broken
  applications. With the @sym{gtk:fixed} widget, the following things will
  result in truncated text, overlapping widgets, and other display bugs:
  @begin{itemize}
    @begin{item}
      Themes, which may change widget sizes.
    @end{item}
    @begin{item}
      Fonts other than the one you used to write the application will of course
      change the size of widgets containing text. Keep in mind that users may
      use a larger font because of difficulty reading the default, or they may
      be using a different OS that provides different fonts.
    @end{item}
    @begin{item}
      Translation of text into other languages changes its size. Also,
      display of non-English text will use a different font in many cases.
    @end{item}
  @end{itemize}
  In addition, the fixed widget cannot properly be mirrored in right-to-left
  languages such as Hebrew and Arabic. That is, normally GTK will order
  containers appropriately for the text direction, e.g. to put labels to the
  right of the thing they label when using an RTL language, but it cannot do
  that with the @sym{gtk:fixed} widget. So if you need to reorder widgets
  depending on the text direction, you would need to manually detect it and
  adjust child positions accordingly.

  Finally, fixed positioning makes it kind of annoying to add/remove GUI
  elements, since you have to reposition all the other elements. This is a
  long-term maintenance problem for your application.

  If you know none of these things are an issue for your application, and
  prefer the simplicity of the @sym{gtk:fixed} widget, by all means use the
  fixed widget. But you should be aware of the tradeoffs.")

;;; ----------------------------------------------------------------------------
;;; gtk_fixed_new
;;; ----------------------------------------------------------------------------

(declaim (inline fixed-new))

(defun fixed-new ()
 #+liber-documentation
 "@version{#2022-2-12}
  @return{A new @class{gtk:fixed} widget.}
  @begin{short}
    Creates a new fixed widget.
  @end{short}
  @see-class{gtk:fixed}"
  (make-instance 'fixed))

(export 'fixed-new)

;;; ----------------------------------------------------------------------------
;;; gtk_fixed_put
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_fixed_put" %fixed-put) :void
  (fixed (g:object fixed))
  (child (g:object widget))
  (x :double)
  (y :double))

(defun fixed-put (fixed child x y)
 #+liber-documentation
 "@version{#2022-2-12}
  @argument[fixed]{a @class{gtk:fixed} widget}
  @argument[child]{a @class{gtk:widget} child widget to add}
  @argument[x]{a number which is coerced to a double float with the horizontal
    position to place the child widget at}
  @argument[y]{a number which is coerced to a double float with the vertical
    position to place the child widget at}
  @begin{short}
    Adds a child widget to a fixed container and assigns a translation
    transformation to the given @arg{x} and @arg{y} coordinates to it.
  @end{short}
  @see-class{gtk:fixed}
  @see-class{gtk:widget}
  @see-function{gtk:fixed-move}"
  (%fixed-put fixed
              child
              (coerce x 'double-float)
              (coerce y 'double-float)))

(export 'fixed-put)

;;; ----------------------------------------------------------------------------
;;; gtk_fixed_remove
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_fixed_remove" fixed-remove) :void
 #+liber-documentation
 "@version{#2022-2-12}
  @argument[fixed]{a @class{gtk:fixed} widget}
  @argument[child]{a @class{gtk:widget} child widget to remove}
  @begin{short}
    Removes a child widget from the fixed widget, after it has been added with
    the @fun{gtk:fixed-put} function.
  @end{short}
  @see-class{gtk:fixed}
  @see-function{gtk:fixed-put}"
  (fixed (g:object fixed))
  (child (g:object widget)))

(export 'fixed-remove)

;;; ----------------------------------------------------------------------------
;;; gtk_fixed_move
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_fixed_move" %fixed-move) :void
  (fixed (g:object fixed))
  (child (g:object widget))
  (x :double)
  (y :double))

(defun fixed-move (fixed child x y)
 #+liber-documentation
 "@version{#2022-2-12}
  @argument[fixed]{a @class{gtk:fixed} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @argument[x]{a number which is coerced to a double float with the horizontal
    position to move the child widget to}
  @argument[y]{a number which is coerced to a double float with the vertical
    position to move the child widget to}
  @begin{short}
    Sets a translation transformation to the given @arg{x} and @arg{y}
    coordinates to the child widget of the given fixed container.
  @end{short}
  @see-class{gtk:fixed}
  @see-class{gtk:widget}
  @see-function{gtk:fixed-put}"
  (%fixed-move fixed
               child
               (coerce x 'double-float)
               (coerce y 'double-float)))

(export 'fixed-move)

;;; ----------------------------------------------------------------------------
;;;gtk_fixed_get_child_position ()
;;;void
;;;gtk_fixed_get_child_position (GtkFixed *fixed,
;;;                              GtkWidget *widget,
;;;                              double *x,
;;;                              double *y);
;;;Retrieves the translation transformation of the given child GtkWidget in the given GtkFixed container.

;;;See also: gtk_fixed_get_child_transform().

;;;Parameters
;;;fixed

;;;a GtkFixed

;;;widget

;;;a child of fixed

;;;x

;;;the horizontal position of the widget .

;;;[out]
;;;y

;;;the vertical position of the widget .

;;;[out]
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_fixed_get_child_position" %fixed-child-position) :void
  (fixed (g:object fixed))
  (child (g:object widget))
  (x (:pointer :double))
  (y (:pointer :double)))

(defun fixed-child-position (fixed child)
  (with-foreign-objects ((x :double) (y :double))
    (%fixed-child-position fixed child x y)
    (values (cffi:mem-ref x :double)
            (cffi:mem-ref y :double))))

(export 'fixed-child-position)

;;; ----------------------------------------------------------------------------
;;;gtk_fixed_get_child_transform ()
;;;GskTransform *
;;;gtk_fixed_get_child_transform (GtkFixed *fixed,
;;;                               GtkWidget *widget);
;;;Retrieves the transformation for widget set using gtk_fixed_set_child_transform().

;;;Parameters
;;;fixed

;;;a GtkFixed

;;;widget

;;;a GtkWidget, child of fixed

;;;Returns
;;;a GskTransform or NULL in case no transform has been set on widget .

;;;[transfer none][nullable]
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;gtk_fixed_set_child_transform ()
;;;void
;;;gtk_fixed_set_child_transform (GtkFixed *fixed,
;;;                               GtkWidget *widget,
;;;                               GskTransform *transform);
;;;Sets the transformation for widget .

;;;This is a convenience function that retrieves the GtkFixedLayoutChild instance associated to widget and calls gtk_fixed_layout_child_set_transform().

;;;Parameters
;;;fixed

;;;a GtkFixed

;;;widget

;;;a GtkWidget, child of fixed

;;;transform

;;;the transformation assigned to widget or NULL to reset widget 's transform.
;;; ----------------------------------------------------------------------------

(defun (setf fixed-child-transform) (value fixed child)
  (cffi:foreign-funcall "gtk_fixed_set_child_transform"
                        (g:object fixed) fixed
                        (g:object widget) child
                        (g:boxed gsk:transform) value)
  value)

(defcfun ("gtk_fixed_get_child_transform" fixed-child-transform)
    (g:boxed gsk:transform)
  (fixed (g:object fixed))
  (child (g:object widget)))

(export 'fixed-child-transform)

;;; --- End of file gtk4.fixed.lisp --------------------------------------------

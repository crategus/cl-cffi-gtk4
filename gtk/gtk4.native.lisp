;;; ----------------------------------------------------------------------------
;;; gtk4.native.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.20 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 - 2026 Dieter Kaiser
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
;;; GtkNative
;;;
;;;     Interface for widgets having surfaces
;;;
;;; Types and Values
;;;
;;;     GtkNative
;;;
;;; Functions
;;;
;;;     gtk_native_get_for_surface
;;;     gtk_native_get_surface
;;;     gtk_native_get_renderer
;;;     gtk_native_get_surface_transform
;;;     gtk_native_realize
;;;     gtk_native_unrealize
;;;
;;; Hierarchy
;;;
;;;     GInterface
;;;     ╰── GtkNative
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkNative
;;; ----------------------------------------------------------------------------

(gobject:define-ginterface "GtkNative" native
  (:superclass g:object
   :export t
   :type-initializer "gtk_native_get_type")
  nil)

#+liber-documentation
(setf (liber:alias-for-class 'native)
      "Interface"
      (documentation 'native 'type)
 "@version{2025-12-08}
  @begin{short}
    The @class{gtk:native} interface is the interface implemented by all widgets
    that can provide a @class{gdk:surface} object for widgets to render on.
  @end{short}
  The obvious example is the @class{gtk:window} widget.

  Every widget that is not itself a @class{gtk:native} widget is contained in
  one, and you can get it with the @fun{gtk:widget-native} function. To get the
  surface of a @class{gtk:native} widget, use the @fun{gtk:native-surface}
  function. It is also possible to find the @class{gtk:native} widget to which
  a surface belongs, with the @fun{gtk:native-for-surface} function.

  In addition to a @class{gdk:surface} object, a @class{gtk:native} widget also
  provides a @class{gsk:renderer} object for rendering on that surface. To get
  the renderer, use the @fun{gtk:native-renderer} function.
  @see-class{gdk:surface}
  @see-class{gtk:window}
  @see-class{gsk:renderer}")

;;; ----------------------------------------------------------------------------
;;;gtk_native_get_for_surface
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_native_get_for_surface" native-for-surface)
    (g:object native)
 #+liber-documentation
 "@version{2025-05-15}
  @argument[surface]{a @class{gdk:surface} object}
  @return{The @class{gtk:native} widget that is associated with @arg{surface}.}
  @begin{short}
    Finds the native widget associated with the surface.
  @end{short}
  @see-class{gtk:native}
  @see-class{gdk:surface}"
  (surface (g:object gdk:surface)))

(export 'native-for-surface)

;;; ----------------------------------------------------------------------------
;;; gtk_native_get_surface
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_native_get_surface" native-surface) (g:object gdk:surface)
 #+liber-documentation
 "@version{2025-07-12}
  @argument[native]{a @class{gtk:native} widget}
  @return{The @class{gdk:surface} object for @arg{native}.}
  @begin{short}
    Returns the surface of the native widget.
  @end{short}
  @see-class{gtk:native}
  @see-class{gdk:surface}"
  (native (g:object native)))

(export 'native-surface)

;;; ----------------------------------------------------------------------------
;;; gtk_native_get_renderer
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_native_get_renderer" native-renderer)
    (g:object gsk:renderer)
 #+liber-documentation
 "@version{2025-07-12}
  @argument[native]{a @class{gtk:native} widget}
  @return{The @class{gsk:renderer} object for @arg{native}.}
  @begin{short}
    Returns the renderer that is used for the native widget.
  @end{short}
  @see-class{gtk:native}
  @see-class{gsk:renderer}"
  (native (g:object native)))

(export 'native-renderer)

;;; ----------------------------------------------------------------------------
;;; gtk_native_get_surface_transform
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_native_get_surface_transform" %native-surface-transform)
    :void
  (native (g:object native))
  (x (:pointer :double))
  (y (:pointer :double)))

(defun native-surface-transform (native)
 #+liber-documentation
 "@version{2025-05-15}
  @syntax{(gtk:native-surface-transform native) => x, y}
  @argument[native]{a @class{gtk:native} widget}
  @argument[x]{a double float for the x coordinate}
  @argument[y]{a double float for the y coordinate}
  @begin{short}
    Retrieves the surface transform of the native widget.
  @end{short}
  This is the translation from the surface coordinates into the coordinates of
  the native widget.
  @see-class{gtk:native}"
  (cffi:with-foreign-objects ((x :double) (y :double))
    (%native-surface-transform native x y)
    (values (cffi:mem-ref x :double)
            (cffi:mem-ref y :double))))

(export 'native-surface-transform)

;;; ----------------------------------------------------------------------------
;;; gtk_native_realize
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_native_realize" native-realize) :void
 #+liber-documentation
 "@version{#2025-05-15}
  @argument[native]{a @class{gtk:native} widget}
  @begin{short}
    Realizes the native widget.
  @end{short}
  @see-class{gtk:native}"
  (native (g:object native)))

(export 'native-realize)

;;; ----------------------------------------------------------------------------
;;; gtk_native_unrealize
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_native_unrealize" native-unrealize) :void
 #+liber-documentation
 "@version{#2025-05-15}
  @argument[native]{a @class{gtk:native} widget}
  @begin{short}
    Unrealizes the native widget.
  @end{short}
  @see-class{gtk:native}"
  (native (g:object native)))

(export 'native-unrealize)

;;; --- End of file gtk4.native.lisp -------------------------------------------

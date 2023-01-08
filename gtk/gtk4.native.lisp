;;; ----------------------------------------------------------------------------
;;; gtk.native.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.6 and modified to document the Lisp binding to the GTK library.
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

(define-g-interface "GtkNative" native
  (:export t
   :type-initializer "gtk_native_get_type")
  nil)

#+liber-documentation
(setf (liber:alias-for-class 'native)
      "Interface"
      (documentation 'native 'type)
 "@version{#2022-7-11}
  @begin{short}
    The @sym{gtk:native} interface is the interface implemented by all widgets
    that can provide a @class{gdk-surface} object for widgets to render on.
  @end{short}
  The obvious example of a @sym{gtk:native} widget is the @class{gtk:window}
  widget.
  @see-class{gdk-surface}
  @see-class{gtk:window}")

;;; ----------------------------------------------------------------------------
;;;gtk_native_get_for_surface -> native-for-surface
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_native_get_for_surface" native-for-surface)
    (g:object native)
 #+liber-documentation
 "@version{#2022-7-11}
  @argument[surface]{a @class{gdk-surface} object}
  @return{The @class{gtk:native} widget that is associated with @arg{surface}.}
  @begin{short}
    Finds the native widget associated with the surface.
  @end{short}
  @see-class{gtk:native}
  @see-class{gdk-surface}"
  (surface (g:object gdk-surface)))

(export 'native-for-surface)

;;; ----------------------------------------------------------------------------
;;; gtk_native_get_surface -> native-surface
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_native_get_surface" native-surface) (g:object gdk-surface)
 #+liber-documentation
 "@version{#2022-7-11}
  @argument[native]{a @class{gtk:native} widget}
  @return{The @class{gdk-surface} object of @arg{native}.}
  @begin{short}
    Returns the surface of the native widget.
  @end{short}
  @see-class{gtk:native}
  @see-class{gdk-surface}"
  (native (g:object native)))

(export 'native-surface)

;;; ----------------------------------------------------------------------------
;;; gtk_native_get_renderer -> native-renderer
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_native_get_renderer" native-renderer)
    (g:object gsk:renderer)
 #+liber-documentation
 "@version{#2022-7-11}
  @argument[native]{a @class{gtk:native} widget}
  @return{The @class{gsk:renderer} object of @arg{native}.}
  @begin{short}
    Returns the renderer that is used for the native widget.
  @end{short}
  @see-class{gtk:native}
  @see-class{gsk:renderer}"
  (native (g:object native)))

(export 'native-renderer)

;;; ----------------------------------------------------------------------------
;;; gtk_native_get_surface_transform -> native-surface-transform
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_native_get_surface_transform" %native-surface-transform)
    :void
  (native (g:object native))
  (x (:pointer :double))
  (y (:pointer :double)))

(defun native-surface-transform (native)
 #+liber-documentation
 "@version{#2022-7-11}
  @argument[native]{a @class{gtk:native} widget}
  @begin{return}
    @arg{x} -- a double float with the x coordinate @br{}
    @arg{y} -- a double float with the y coordinate
  @end{return}
  @begin{short}
    Retrieves the surface transform of the native widget.
  @end{short}
  This is the translation from the surface coordinates into the coordinates of
  the native widget.
  @see-class{gtk:native}"
  (with-foreign-objects ((x :double) (y :double))
    (%native-surface-transform native x y)
    (values (mem-ref x :double)
            (mem-ref y :double))))

(export 'native-surface-transform)

;;; ----------------------------------------------------------------------------
;;; gtk_native_realize
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_native_realize" native-realize) :void
 #+liber-documentation
 "@version{#2022-7-11}
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

(defcfun ("gtk_native_unrealize" native-unrealize) :void
 #+liber-documentation
 "@version{#2022-7-11}
  @argument[native]{a @class{gtk:native} widget}
  @begin{short}
    Unrealizes the native widget.
  @end{short}
  @see-class{gtk:native}"
  (native (g:object native)))

(export 'native-unrealize)

;;; --- End of file gtk.native.lisp --------------------------------------------

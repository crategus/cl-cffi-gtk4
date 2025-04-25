;;; ----------------------------------------------------------------------------
;;; gtk4.buildable.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.18 and modified to document the Lisp binding to the GTK library,
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
;;; GtkBuildable
;;;
;;;     Interface for objects that can be built by GtkBuilder
;;;
;;; Types and Values
;;;
;;;     GtkBuildable
;;;
;;; Functions
;;;
;;;     gtk_buildable_get_buildable_id
;;;
;;; Virtual functions
;;;
;;;     gtk_buildable_add_child
;;;     gtk_buildable_construct_child
;;;     gtk_buildable_custom_finished
;;;     gtk_buildable_custom_tag_end
;;;     gtk_buildable_custom_tag_start
;;;     gtk_buildable_get_id
;;;     gtk_buildable_set_id
;;;     gtk_buildable_get_internal_child
;;;     gtk_buildable_parser_finished
;;;     gtk_buildable_set_buildable_property
;;;
;;; Hierarchy
;;;
;;;     GInterface
;;;     ╰── GtkBuildable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkBuildable
;;; ----------------------------------------------------------------------------

(gobject:define-ginterface "GtkBuildable" buildable
  (:export t
   :type-initializer "gtk_buildable_get_type")
  nil)

#+liber-documentation
(setf (liber:alias-for-class 'buildable)
      "Interface"
      (documentation 'buildable 'type)
 "@version{2025-4-17}
  @begin{short}
    The @class{gtk:buildable} interface allows objects to extend and customize
    their deserialization from @class{gtk:builder} UI descriptions.
  @end{short}
  The interface includes methods for setting names and properties of objects,
  parsing custom tags and constructing child objects.

  The @class{gtk:buildable} interface is implemented by all widgets and many of
  the non-widget objects that are provided by GTK. The main user of this
  interface is the @class{gtk:builder} class. There should be very little need
  for applications to call any of these functions directly.

  An object only needs to implement this interface if it needs to extend the
  @class{gtk:builder} format or run any extra routines at deserialization time.
  @see-class{gtk:builder}")

;;; ----------------------------------------------------------------------------
;;; gtk_buildable_get_buildable_id
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_buildable_get_buildable_id" buildable-buildable-id) :string
 #+liber-documentation
 "@version{2025-4-17}
  @argument[buildable]{a @class{gtk:buildable} object}
  @return{The string with the ID of the buildable object.}
  @begin{short}
    Gets the ID of the buildable object.
  @end{short}
  The @class{gtk:builder} object sets the name based on the ID attribute of the
  tag used to construct the buildable object.
  @see-class{gtk:buildable}"
  (buildable (g:object buildable)))

(export 'buildable-buildable-id)

;;; --- End of file gtk4.buildable.lisp ----------------------------------------

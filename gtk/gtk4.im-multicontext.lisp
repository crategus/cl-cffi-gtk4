;;; ----------------------------------------------------------------------------
;;; gtk4.im-multicontext.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.20 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2026 Dieter Kaiser
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
;;; GtkIMMulticontext
;;;
;;;     An input method context supporting multiple, loadable input methods
;;;
;;; Types and Values
;;;
;;;     GtkIMMulticontext
;;;
;;; Functions
;;;
;;;     gtk_im_multicontext_new
;;;     gtk_im_multicontext_get_context_id
;;;     gtk_im_multicontext_set_context_id
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkIMContext
;;;         ╰── GtkIMMulticontext
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkIMMulticontext
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkIMMulticontext" im-multicontext
  (:superclass im-context
   :export t
   :interfaces nil
   :type-initializer "gtk_im_multicontext_get_type")
  nil)

#+liber-documentation
(setf (documentation 'im-multicontext 'type)
 "@version{2026-01-31}
  @begin{short}
    The @class{gtk:im-multicontext} class is an input method context supporting
    multiple, switchable input methods.
  @end{short}
  Text widgets such as @class{gtk:text} or @class{gtk:text-view} widgets use a
  @class{gtk:im-multicontext} object to implement their @code{im-module}
  property for switching between different input methods.
  @see-class{gtk:im-context}
  @see-class{gtk:text}
  @see-class{gtk:text-view}")

;;; ----------------------------------------------------------------------------
;;; gtk_im_multicontext_new
;;; ----------------------------------------------------------------------------

(defun im-multicontext-new ()
 #+liber-documentation
 "@version{2026-01-31}
  @return{The new @class{gtk:im-multicontext} object.}
  @begin{short}
    Creates a new input method context supporting multiple, switchable
    input methods.
  @end{short}
  @see-class{gtk:im-multicontext}"
  (make-instance 'im-multicontext))

(export 'im-multicontext-new)

;;; ----------------------------------------------------------------------------
;;; gtk_im_multicontext_get_context_id
;;; gtk_im_multicontext_set_context_id
;;; ----------------------------------------------------------------------------

(defun (setf im-multicontext-context-id) (id context)
  (cffi:foreign-funcall "gtk_im_multicontext_set_context_id"
                        (g:object im-multicontext) context
                        :string id
                        :void)
  id)

(cffi:defcfun ("gtk_im_multicontext_get_context_id"
               im-multicontext-context-id) :string
 #+liber-documentation
 "@version{2026-01-31}
  @syntax{(gtk:im-multicontext-context-id context) => id}
  @syntax{(setf (gtk:im-multicontext-context-id content) id)}
  @argument[context]{a @class{gtk:im-multicontext} object}
  @argument[id]{a string for the context ID}
  @begin{short}
    Gets or sets the ID of the currently active delegate of the context.
  @end{short}
  Setting the context causes the currently active delegate of @arg{context} to
  be replaced by the delegate corresponding to the new context ID.
  @see-class{gtk:im-multicontext}"
  (context (g:object im-multicontext)))

(export 'im-multicontext-context-id)

;;; --- End of file gtk4.im-multicontext.lisp ----------------------------------

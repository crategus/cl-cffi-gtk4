;;; ----------------------------------------------------------------------------
;;; gtk4.im-multicontext.lisp
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
;;; struct GtkIMMulticontext
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkIMMulticontext" im-multicontext
  (:superclass im-context
   :export t
   :interfaces nil
   :type-initializer "gtk_im_multicontext_get_type")
  nil)

#+liber-documentation
(setf (documentation 'im-multicontext 'type)
 "@version{#2022-7-10}
  @begin{short}
    The @sym{gtk:im-multicontext} object is an input method context supporting
    multiple, switchable input methods.
  @end{short}
  Text widgets such as @class{gtk:text} or @class{gtk:text-view} widgets use a
  @sym{gtk:im-multicontext} object to implement their @code{im-module} property
  for switching between different input methods.
  @see-class{gtk:im-context}
  @see-class{gtk:text}
  @see-class{gtk:text-view}")

;;; ----------------------------------------------------------------------------
;;; gtk_im_multicontext_new ()
;;; ----------------------------------------------------------------------------

(defun im-multicontext-new ()
 #+liber-documentation
 "@version{#2022-7-10}
  @return{A new @class{gtk:im-multicontext} object.}
  @short{Creates a new input method context supporting multiple, switchable
    input methods.}
  @see-class{gtk:im-multicontext}"
  (make-instance 'im-multicontext))

(export 'im-multicontext-new)

;;; ----------------------------------------------------------------------------
;;; gtk_im_multicontext_get_context_id ()
;;;
;;; const char *
;;; gtk_im_multicontext_get_context_id (GtkIMMulticontext *context);
;;;
;;; Gets the id of the currently active delegate of the context .
;;;
;;; context :
;;;     a GtkIMMulticontext
;;;
;;; Returns :
;;;     the id of the currently active delegate
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_im_multicontext_set_context_id ()
;;;
;;; void
;;; gtk_im_multicontext_set_context_id (GtkIMMulticontext *context,
;;;                                     const char *context_id);
;;;
;;; Sets the context id for context .
;;;
;;; This causes the currently active delegate of context to be replaced by the
;;; delegate corresponding to the new context id.
;;;
;;; context :
;;;     a GtkIMMulticontext
;;;
;;; context_id :
;;;     the id to use
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk4.im-multicontext.lisp ----------------------------------

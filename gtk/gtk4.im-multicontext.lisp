;;; ----------------------------------------------------------------------------
;;; gtk.im-multicontext.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2022 Dieter Kaiser
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

(define-g-object-class "GtkIMMulticontext" im-multicontext
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

;;; --- End of file gtk.im-multicontext.lisp -----------------------------------

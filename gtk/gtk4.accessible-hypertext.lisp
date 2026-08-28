;;; ----------------------------------------------------------------------------
;;; gtk4.accessible-hypertext.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.22 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2026 Dieter Kaiser
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
;;; Types and Values
;;;
;;;     GtkAccessibleHypertext
;;;
;;; Virtual Methods
;;;
;;;     get_link
;;;     get_link_at
;;;     get_n_links
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkAccessibleHypertext
;;; ----------------------------------------------------------------------------

(gobject:define-ginterface "GtkAccessibleHypertext" accessible-hypertext
  (:superclass accessible
   :export t
   :type-initializer "gtk_accessible_hypertext_get_type")
  nil)

#+liber-documentation
(setf (liber:alias-for-class 'accessible-hypertext)
      "Interface"
      (documentation 'accessible-hypertext 'type)
 "@version{2026-05-23}
  @begin{short}
    The @class{gtk:accessible-hypertext} interface is an interface for
    accessible objects containing links.
  @end{short}
  This interface is meant to be implemented by accessible objects that contain
  links. Those links do not necessarily have to be part of text, they can be
  associated with images and other things.
  @begin[Prerequisite]{dictionary}
    In order to implement the @class{gtk:accessible-hypertext} interface, your
    type must inherit from the @class{gtk:accessible} interface.
  @end{dictionary}

  Since 4.22
  @see-class{gtk:accessible}")

;;; ----------------------------------------------------------------------------
;;; gtk_accessible_hypertext_get_link                       Since 4.22
;;; ----------------------------------------------------------------------------

#+nil
(cffi:defcfun ("gtk_accessible_hypertext_get_link" accessible-hypertext-link)
    (g:object accessible-hyperlink)
  (hypertext (g:object accessible-hypertext))
  (index :uint))

#+nil
(export 'accessible-hypertext-link)

;;; ----------------------------------------------------------------------------
;;; gtk_accessible_hypertext_get_link_at                    Since 4.22
;;; ----------------------------------------------------------------------------

#+nil
(cffi:defcfun ("gtk_accessible_hypertext_get_link_at"
               accessible-hypertext-link-at) :uint
  (hypertext (g:object accessible-hypertext))
  (offset :uint))

#+nil
(export 'accessible-hypertext-link-at)

;;; ----------------------------------------------------------------------------
;;; gtk_accessible_hypertext_get_n_links                    Since 4.22
;;; ----------------------------------------------------------------------------

#+nil
(cffi:defcfun ("gtk_accessible_hypertext_get_n_links"
               accessible-hypertext-n-links) :uint
  (hypertext (g:object accessible-hypertext)))

#+nil
(export 'accessible-hypertext-n-links)

;;; --- End of file gtk4.accessible-hypertext.lisp -----------------------------

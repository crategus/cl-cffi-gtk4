;;; ----------------------------------------------------------------------------
;;; gtk4.accessible-hyperlink.lisp
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
;;;     GtkAccessibleHyperlink
;;;
;;; Functions
;;;
;;;     gtk_accessible_hyperlink_new
;;;     gtk_accessible_hyperlink_set_platform_state
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkAccessibleHyperlink
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkAccessibleHyperlink" accessible-hyperlink
  (:superclass g:object
   :export t
   :interfaces ("GtkAccessible")
   :type-initializer "gtk_accessible_hyperlink_get_type")
  nil)

#+liber-documentation
(setf (documentation 'accessible-hyperlink 'type)
 "@version{2026-05-23}
  @begin{short}
    The @class{gtk:accessible-hyperlink} class represents a link, that is
    an URI.
  @end{short}
  A widget that contains one or more links should implement the
  the @class{gtk:accessible-hypertext} interface and return
  @class{gtk:accessible-hyperlink} objects for each of the links.

  Since 4.22
  @see-constructor{gtk:accessible-hyperlink-new}
  @see-class{gtk:accessible-hypertext}")

;;; ----------------------------------------------------------------------------
;;; gtk_accessible_hyperlink_new
;;; ----------------------------------------------------------------------------

;; TODO: We need the implementation of GtkAccessibleTextRange for internal use

(cffi:defcfun ("gtk_accessible_hyperlink_new" %accessible-hyperlink-new)
    (g:object accessible-hyperlink)
  (parent (g:object accessible-hypertext))
  (index :uint)
  (uri :string)
  (bounds :pointer))

(export 'accessible-hyerlink-new)

;;; ----------------------------------------------------------------------------
;;; gtk_accessible_hyperlink_set_platform_state
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_accessible_hyperlink_set_platform_state"
               accessible-hyperlink-set-platform-state) :void
  (hyperlink (g:object accessible-hyperlink))
  (state accessible-platform-state)
  (enabled :boolean))

(export 'accessible-hyperlink-set-platfom-state)

;;; --- End of file gtk4.accessible-hyperlink.lisp -----------------------------

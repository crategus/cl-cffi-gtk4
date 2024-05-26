;;; ----------------------------------------------------------------------------
;;; gtk4.accessible-text.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.14 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2024 Dieter Kaiser
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
;;;     GtkAccessibleText
;;;
;;; Functions
;;;
;;;     gtk_accessible_text_get_attributes
;;;     gtk_accessible_text_get_caret_position
;;;     gtk_accessible_text_get_contents
;;;     gtk_accessible_text_get_contents_at
;;;     gtk_accessible_text_get_default_attributes
;;;     gtk_accessible_text_get_extents                     Since 4.16
;;;     gtk_accessible_text_get_offset                      Since 4.16
;;;     gtk_accessible_text_get_selection
;;;
;;;     gtk_accessible_text_update_caret_position
;;;     gtk_accessible_text_update_contents
;;;     gtk_accessible_text_update_selection_bound
;;; ----------------------------------------------------------------------------

(in-package :gtk)

(gobject:define-g-interface "GtkAccessibleText" accessible-text
  (:superclass accessible
   :export t
   :type-initializer "gtk_accessible_text_get_type")
  nil)

#+liber-documentation
(setf (liber:alias-for-class 'accessible-text)
      "Interface"
      (documentation 'accessible-text 'type)
 "@version{2024-5-26}
  @begin{short}
    An interface for accessible objects containing formatted text.
  @end{short}
  The @class{gtk:accessible-text} interfaces is meant to be implemented by
  accessible objects that have text formatted with attributes, or non-trivial
  text contents.

  You should use the @code{:label} or the @code{:description} values of the
  @symbol{gtk:accessible-property} enumeration for accessible objects containing
  simple, unformatted text.

  Since 4.14
  @see-class{gtk:accessible}
  @see-symbol{gtk:accessible-property}")

;;; ----------------------------------------------------------------------------
;;; Gtk.AccessibleText.get_attributes
;;;
;;; Retrieves the text attributes inside the accessible object.
;;;
;;; Since 4.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; Gtk.AccessibleText.get_caret_position
;;;
;;; Retrieves the position of the caret inside the accessible object.
;;;
;;; Since 4.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; Gtk.AccessibleText.get_contents
;;;
;;; Retrieve the current contents of the accessible object within the given
;;; range.
;;;
;;; Since 4.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; Gtk.AccessibleText.get_contents_at
;;;
;;; Retrieve the current contents of the accessible object starting from the
;;; given offset, and using the given granularity.
;;;
;;; Since 4.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; Gtk.AccessibleText.get_default_attributes
;;;
;;; Retrieves the default text attributes inside the accessible object.
;;;
;;; Since: 4.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; Gtk.AccessibleText.get_extents
;;;
;;; Obtains the extents of a range of text, in widget coordinates.
;;;
;;; Since 4.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; Gtk.AccessibleText.get_offset
;;;
;;; Gets the text offset at a given point.
;;;
;;; Since 4.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; Gtk.AccessibleText.get_selection
;;;
;;; Retrieves the selection ranges in the accessible object.
;;;
;;; Since 4.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accessible_text_update_caret_position
;;;
;;; Updates the position of the caret.
;;;
;;; Since 4.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accessible_text_update_contents
;;;
;;; Notifies assistive technologies of a change in contents.
;;;
;;; Since 4.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accessible_text_update_selection_bound
;;;
;;; Updates the boundary of the selection.
;;;
;;; Since 4.14
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk4.accessible-text.lisp ----------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk4.custom-sorter.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2023 Dieter Kaiser
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
;;; GtkCustomSorter
;;;
;;;     Sorting with a callbacks
;;;
;;; Types and Values
;;;
;;;     GtkCustomSorter
;;;
;;; Functions
;;;
;;;     gtk_custom_sorter_new
;;;     gtk_custom_sorter_set_sort_func
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkSorter
;;;         ╰── GtkCustomSorter
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkCustomSorter
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkCustomSorter" custom-sorter
  (:superclass sorter
   :export t
   :interfaces ()
   :type-initializer "gtk_custom_sorter_get_type")
  nil)



;;;Description
;;;GtkCustomSorter is a GtkSorter implementation that sorts via a traditional GCompareDataFunc callback.

;;;Functions
;;;gtk_custom_sorter_new ()
;;;GtkCustomSorter *
;;;gtk_custom_sorter_new (GCompareDataFunc sort_func,
;;;                       gpointer user_data,
;;;                       GDestroyNotify user_destroy);
;;;Creates a new GtkSorter that works by calling sort_func to compare items.

;;;If sort_func is NULL, all items are considered equal.

;;;Parameters
;;;sort_func

;;;the GCompareDataFunc to use for sorting.

;;;[nullable]
;;;user_data

;;;user data to pass to sort_func .

;;;[nullable]
;;;user_destroy

;;;destroy notify for user_data .

;;;[nullable]
;;;Returns
;;;a new GtkCustomSorter

;;;gtk_custom_sorter_set_sort_func ()
;;;void
;;;gtk_custom_sorter_set_sort_func (GtkCustomSorter *self,
;;;                                 GCompareDataFunc sort_func,
;;;                                 gpointer user_data,
;;;                                 GDestroyNotify user_destroy);
;;;Sets (or unsets) the function used for sorting items.

;;;If sort_func is NULL, all items are considered equal.

;;;If the sort func changes its sorting behavior, gtk_sorter_changed() needs to be called.

;;;If a previous function was set, its user_destroy will be called now.

;;;Parameters
;;;self

;;;a GtkCustomSorter

;;;sort_func

;;;function to sort items.

;;;[nullable]
;;;user_data

;;;user data to pass to match_func .

;;;[nullable]
;;;user_destroy

;;;destroy notify for user_data


;;; --- End of file gtk4.custom-sorter.lisp ------------------------------------

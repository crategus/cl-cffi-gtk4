;;; ----------------------------------------------------------------------------
;;; gtk4.custom-filter.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.12 and modified to document the Lisp binding to the GTK library.
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
;;; GtkCustomFilter
;;;
;;;     Filtering with callbacks
;;;
;;; Types and Values
;;;
;;;     GtkCustomFilter
;;;
;;; Functions
;;;
;;;     GtkCustomFilterFunc
;;;     gtk_custom_filter_new
;;;     gtk_custom_filter_set_filter_func
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkFilter
;;;         ╰── GtkCustomFilter
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkCustomFilter
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkCustomFilter" custom-filter
  (:superclass filter
   :export t
   :interfaces nil
   :type-initializer "gtk_custom_filter_get_type")
  nil)

#+liber-documentation
(setf (documentation 'custom-filter 'type)
 "@version{2023-8-16}
  @begin{short}
    The @class{gtk:custom-filter} object is a @class{gtk:filter} object that
    uses a callback to determine whether to include an item or not.
  @end{short}
  @see-constructor{gtk:custom-filter-new}
  @see-class{gtk:filter}")

;;; ----------------------------------------------------------------------------
;;; GtkCustomFilterFunc ()
;;;
;;; gboolean
;;; (*GtkCustomFilterFunc) (gpointer item,
;;;                         gpointer user_data);
;;;
;;; User function that is called to determine if the item should be matched. If
;;; the filter matches the item, this function must return TRUE. If the item
;;; should be filtered out, FALSE must be returned.
;;;
;;; item :
;;;     The item to be matched.
;;;
;;; user_data :
;;;     user data
;;;
;;; Returns :
;;;     TRUE to keep the item around
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_custom_filter_new ()
;;;
;;; GtkCustomFilter *
;;; gtk_custom_filter_new (GtkCustomFilterFunc match_func,
;;;                        gpointer user_data,
;;;                        GDestroyNotify user_destroy);
;;;
;;; Creates a new filter using the given match_func to filter items.
;;;
;;; If match_func is NULL, the filter matches all items.
;;;
;;; If the filter func changes its filtering behavior, gtk_filter_changed()
;;; needs to be called.
;;;
;;; match_func :
;;;     function to filter items.
;;;
;;; user_data :
;;;     user data to pass to match_func .
;;;
;;; user_destroy :
;;;     destroy notify for user_data
;;;
;;; Returns :
;;;     a new GtkCustomFilter
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_custom_filter_set_filter_func ()
;;;
;;; void
;;; gtk_custom_filter_set_filter_func (GtkCustomFilter *self,
;;;                                    GtkCustomFilterFunc match_func,
;;;                                    gpointer user_data,
;;;                                    GDestroyNotify user_destroy);
;;;
;;; Sets (or unsets) the function used for filtering items.
;;;
;;; If match_func is NULL, the filter matches all items.
;;;
;;; If the filter func changes its filtering behavior, gtk_filter_changed()
;;; needs to be called.
;;;
;;; If a previous function was set, its user_destroy will be called now.
;;;
;;; self :
;;;     a GtkCustomFilter
;;;
;;; match_func :
;;;     function to filter items.
;;;
;;; user_data :
;;;     user data to pass to match_func .
;;;
;;; user_destroy :
;;;     destroy notify for user_data
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk4.custom-filter.lisp ------------------------------------

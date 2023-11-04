;;; ----------------------------------------------------------------------------
;;; gtk4.scroll-info.lisp
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

(in-package :gtk)

;;;Struct
;;;GtkScrollInfo
;;;since: 4.12

;;;[−]
;;;Description
;;;[src]
;;;struct GtkScrollInfo {
;;;  /* No available fields */
;;;}

;;;The GtkScrollInfo can be used to provide more accurate data on how a scroll operation should be performed.

;;;Scrolling functions usually allow passing a NULL scroll info which will cause the default values to be used and just scroll the element into view.

;;;Available since: 4.12


(cffi:defcfun ("gtk_scroll_info_new" %scroll-info-new) :pointer)

(glib:define-g-boxed-opaque scroll-info "GtkScrollInfo"
  :export t
  :type-initializer "gtk_scroll_info_get_type"
  :alloc (%scroll-info-new))




;;;[−]
;;;Constructors
;;;gtk_scroll_info_new
;;;Creates a new scroll info for scrolling an element into view.

;;;since: 4.12

;;;[−]
;;;Instance methods
;;;gtk_scroll_info_get_enable_horizontal
;;;Checks if horizontal scrolling is enabled.

;;;since: 4.12

;;;gtk_scroll_info_get_enable_vertical
;;;Checks if vertical scrolling is enabled.

;;;since: 4.12

;;;gtk_scroll_info_ref
;;;Increases the reference count of a GtkScrollInfo by one.

;;;since: 4.12

;;;gtk_scroll_info_set_enable_horizontal
;;;Turns horizontal scrolling on or off.

;;;since: 4.12

;;;gtk_scroll_info_set_enable_vertical
;;;Turns vertical scrolling on or off.

;;;since: 4.12

;;;gtk_scroll_info_unref
;;;Decreases the reference count of a GtkScrollInfo by one.

;;;since: 4.12

;;; --- End of file gtk4.scroll-info.lisp --------------------------------------

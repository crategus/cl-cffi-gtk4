;;; ----------------------------------------------------------------------------
;;; gtk4.scroll-info.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.16 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2023 - 2024 Dieter Kaiser
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
;;;     GtkScrollInfo
;;;
;;; Functions
;;;
;;;     gtk_scroll_info_new
;;;     gtk_scroll_info_ref
;;;     gtk_scroll_info_unref
;;;     gtk_scroll_info_get_enable_horizontal
;;;     gtk_scroll_info_set_enable_horizontal
;;;     gtk_scroll_info_get_enable_vertical
;;;     gtk_scroll_info_set_enable_vertical
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkScrollInfo
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_scroll_info_new" %scroll-info-new) :pointer)

(glib:define-gboxed-opaque scroll-info "GtkScrollInfo"
  :export t
  :type-initializer "gtk_scroll_info_get_type"
  :alloc (%scroll-info-new))

#+liber-documentation
(setf (liber:alias-for-class 'scroll-info)
      "GBoxed"
      (documentation 'scroll-info 'type)
 "@version{2023-11-16}
  @begin{short}
    The @class{gtk:scroll-info} structure can be used to provide more accurate
    data on how a scroll operation should be performed.
  @end{short}
  Scrolling functions usually allow passing a @code{nil} scroll info which will
  cause the default values to be used and just scroll the element into view.

  Since 4.12
  @see-constructor{gtk:scroll-info-new}
  @see-class{gtk:list-view}
  @see-class{gtk:grid-view}")

;;; ----------------------------------------------------------------------------
;;; gtk_scroll_info_new
;;; ----------------------------------------------------------------------------

(defun scroll-info-new ()
 #+liber-documentation
 "@version{2023-11-16}
  @return{The new @class{gtk:scroll-info} instance.}
  @begin{short}
    Creates a new scroll info for scrolling an element into view.
  @end{short}

  Since 4.12
  @see-class{gtk:scroll-info}
  @see-class{gtk:list-view}
  @see-class{gtk:grid-view}"
  (make-instance 'scroll-info))

(export 'scroll-info-new)

;;; ----------------------------------------------------------------------------
;;; gtk_scroll_info_ref
;;;
;;; Increases the reference count of a GtkScrollInfo by one.
;;;
;;; Since 4.12
;;; ----------------------------------------------------------------------------

;; not needed

;;; ----------------------------------------------------------------------------
;;; gtk_scroll_info_unref
;;;
;;; Decreases the reference count of a GtkScrollInfo by one.
;;;
;;; Since 4.12
;;; ----------------------------------------------------------------------------

;; not needed

;;; ----------------------------------------------------------------------------
;;; gtk_scroll_info_get_enable_horizontal
;;; gtk_scroll_info_set_enable_horizontal
;;; ----------------------------------------------------------------------------

(defun (setf scroll-info-enable-horizontal) (value scrollinfo)
  (cffi:foreign-funcall "gtk_scroll_info_set_enable_horizontal"
                        (g:boxed scroll-info) scrollinfo
                        :boolean value
                        :void)
  value)

(cffi:defcfun ("gtk_scroll_info_get_enable_horizontal"
               scroll-info-enable-horizontal) :boolean
 #+liber-documentation
 "@version{2023-11-16}
  @syntax{(gtk:scroll-info-enable-horizontal scrollinfo) => enable}
  @syntax{(setf (gtk:scroll-info-enable-horizontal scrollinfo) enable)}
  @argument[scrollinfo]{a @class{gtk:scroll-info} instance}
  @argument[enable]{a boolean whether scrolling in the horizontal direction
    should happen}
  @begin{short}
    Turns horizontal scrolling on or off.
  @end{short}

  Since 4.12
  @see-class{gtk:scroll-info}
  @see-function{gtk:scroll-info-enable-vertical}"
  (scrollinfo (g:boxed scroll-info)))

(export 'scroll-info-enable-horizontal)

;;; ----------------------------------------------------------------------------
;;; gtk_scroll_info_get_enable_vertical
;;; gtk_scroll_info_set_enable_vertical
;;; ----------------------------------------------------------------------------

(defun (setf scroll-info-enable-vertical) (value scrollinfo)
  (cffi:foreign-funcall "gtk_scroll_info_set_enable_vertical"
                        (g:boxed scroll-info) scrollinfo
                        :boolean value
                        :void)
  value)

(cffi:defcfun ("gtk_scroll_info_get_enable_vertical"
               scroll-info-enable-vertical) :boolean
 #+liber-documentation
 "@version{2023-11-16}
  @syntax{(gtk:scroll-info-enable-vertical scrollinfo) => enable}
  @syntax{(setf (gtk:scroll-info-enable-vertical scrollinfo) enable)}
  @argument[scrollinfo]{a @class{gtk:scroll-info} instance}
  @argument[enable]{a boolean whether scrolling in the vertical direction
    should happen}
  @begin{short}
    Turns vertical scrolling on or off.
  @end{short}

  Since 4.12
  @see-class{gtk:scroll-info}
  @see-function{gtk:scroll-info-enable-vertical}"
  (scrollinfo (g:boxed scroll-info)))

(export 'scroll-info-enable-vertical)

;;; --- End of file gtk4.scroll-info.lisp --------------------------------------

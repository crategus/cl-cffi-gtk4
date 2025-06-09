;;; ----------------------------------------------------------------------------
;;; gtk4.custom-filter.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.18 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2023 - 2025 Dieter Kaiser
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

(gobject:define-gobject "GtkCustomFilter" custom-filter
  (:superclass filter
   :export t
   :interfaces nil
   :type-initializer "gtk_custom_filter_get_type")
  nil)

#+liber-documentation
(setf (documentation 'custom-filter 'type)
 "@version{2024-09-27}
  @begin{short}
    The @class{gtk:custom-filter} object is a @class{gtk:filter} object that
    uses a callback to determine whether to include an item or not.
  @end{short}
  @see-constructor{gtk:custom-filter-new}
  @see-class{gtk:filter}")

;;; ----------------------------------------------------------------------------
;;; gtk_custom_filter_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_custom_filter_new" %custom-filter-new)
    (g:object custom-filter :return)
  (func :pointer)
  (data :pointer)
  (notify :pointer))

(defun custom-filter-new (&optional func)
 #+liber-documentation
 "@version{2024-09-27}
  @argument[func]{an optional @symbol{gtk:custom-filter-func} callback function
    to filter items, the default value is @code{nil}}
  @return{The new @class{gtk:custom-filter} object.}
  @begin{short}
    Creates a new custom filter using the given @arg{func} callback function to
    filter items.
  @end{short}
  If the @arg{func} argument is @code{nil}, the custom filter matches all items.
  If the custom filter function changes its filtering behavior, the
  @fun{gtk:filter-changed} function needs to be called.
  @see-class{gtk:custom-filter}
  @see-symbol{gtk:custom-filter-func}
  @see-function{gtk:filter-changed}"
  (if func
      (%custom-filter-new (cffi:callback custom-filter-func)
                          (glib:allocate-stable-pointer func)
                          (cffi:callback glib:stable-pointer-destroy-notify))
      (make-instance 'custom-filter)))

(export 'custom-filter-new)

;;; ----------------------------------------------------------------------------
;;; GtkCustomFilterFunc
;;; ----------------------------------------------------------------------------

(cffi:defcallback custom-filter-func :boolean
    ((item :pointer)
     (data :pointer))
  (let ((func (glib:get-stable-pointer-value data))
        (object (cffi:convert-from-foreign item 'g:object)))
    (funcall func object)))

#+liber-documentation
(setf (liber:alias-for-symbol 'custom-filter-func)
      "Callback"
      (liber:symbol-documentation 'custom-filter-func)
 "@version{2025-06-06}
  @syntax{lambda (item) => result}
  @argument[item]{a @class{g:object} instance for the item to be matched}
  @argument[result]{@em{true} to keep the item around}
  @begin{short}
    User function that is called to determine if the item should be matched.
  @end{short}
  If the filter matches the item, this function must return @em{true}. If the
  item should be filtered out, @em{false} must be returned.
  @see-class{gtk:custom-filter}")

(export 'custom-filter-func)

;;; ----------------------------------------------------------------------------
;;; gtk_custom_filter_set_filter_func
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_custom_filter_set_filter_func"
               %custom-filter-set-filter-func) :void
  (filter (g:object custom-filter))
  (func :pointer)
  (data :pointer)
  (notify :pointer))

(defun custom-filter-set-filter-func (filter &optional func)
 #+liber-documentation
 "@version{2025-06-06}
  @argument[filter]{a @class{gtk:custom-filter} object}
  @argument[func]{an optional @symbol{gtk:custom-filter-func} callback function
    to filter items, or the default value @code{nil} to unset the function}
  @begin{short}
    Sets (or unsets) the function used for filtering items.
  @end{short}
  If @arg{func} is @code{nil}, the custom filter matches all items. If the
  custom filter function changes its filtering behavior, the
  @fun{gtk:filter-changed} function needs to be called.
  @see-class{gtk:custom-filter}
  @see-symbol{gtk:custom-filter-func}"
  (if func
      (%custom-filter-set-filter-func
              filter
              (cffi:callback custom-filter-func)
              (glib:allocate-stable-pointer func)
              (cffi:callback glib:stable-pointer-destroy-notify))
      (%custom-filter-set-filter-func
              filter
              (cffi:null-pointer)
              (cffi:null-pointer)
              (cffi:null-pointer))))

(export 'custom-filter-set-filter-func)

;;; --- End of file gtk4.custom-filter.lisp ------------------------------------

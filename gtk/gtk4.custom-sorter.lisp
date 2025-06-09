;;; ----------------------------------------------------------------------------
;;; gtk4.custom-sorter.lisp
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
;;; GtkCustomSorter
;;;
;;;     Sorting with a callback function
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

(gobject:define-gobject "GtkCustomSorter" custom-sorter
  (:superclass sorter
   :export t
   :interfaces ()
   :type-initializer "gtk_custom_sorter_get_type")
  nil)

#+liber-documentation
(setf (documentation 'custom-sorter 'type)
 "@version{2025-03-14}
  @begin{short}
    The @class{gtk:custom-sorter} object is a @class{gtk:sorter} implementation
    that sorts via a @symbol{g:compare-data-func} callback function.
  @end{short}
  @see-constructor{gtk:custom-sorter-new}
  @see-class{gtk:sorter}")

;;; ----------------------------------------------------------------------------
;;; gtk_custom_sorter_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_custom_sorter_new" %custom-sorter-new)
    (g:object custom-sorter :return)
  (func :pointer)
  (data :pointer)
  (notify :pointer))

(defun custom-sorter-new (&optional func)
 #+liber-documentation
 "@version{2025-03-14}
  @argument[func]{a @symbol{g:compare-data-func} callback function to use for
    sorting}
  @return{The new @class{gtk:custom-sorter} object.}
  @begin{short}
    Creates a new custom sorter that works by calling the @arg{func} callback
    function to compare objects.
  @end{short}
  If @arg{func} is @code{nil}, all objects are considered equal.
  @see-class{gtk:custom-sorter}"
  (if func
      (%custom-sorter-new (cffi:callback g:compare-data-func)
                          (glib:allocate-stable-pointer func)
                          (cffi:callback glib:stable-pointer-destroy-notify))
      (%custom-sorter-new (cffi:null-pointer)
                          (cffi:null-pointer)
                          (cffi:null-pointer))))

(export 'custom-sorter-new)

;;; ----------------------------------------------------------------------------
;;; gtk_custom_sorter_set_sort_func
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_custom_sorter_set_sort_func" %custom-sorter-set-sort-func)
    :void
  (sorter (g:object custom-sorter))
  (func :pointer)
  (data :pointer)
  (notify :pointer))

(defun custom-sorter-set-sort-func (sorter func)
 #+liber-documentation
 "@version{2025-03-14}
  @argument[sorter]{a @class{gtk:custom-sorter} object}
  @argument[func]{a @symbol{g:compare-data-func} callback function}
  @begin{short}
    Sets (or unsets) the callback function used for sorting items.
  @end{short}
  If @arg{func} is @code{nil}, all items are considered equal. If the sort
  function changes its sorting behavior, the @fun{gtk:sorter-changed} function
  needs to be called.
  @see-class{gtk:custom-sorter}
  @see-symbol{g:compare-data-func}
  @see-function{gtk:sorter-changed}"
  (if func
      (%custom-sorter-set-sort-func
              sorter
              (cffi:callback g:compare-data-func)
              (glib:allocate-stable-pointer func)
              (cffi:callback glib:stable-pointer-destroy-notify))
      (%custom-sorter-set-sort-func
              sorter
              (cffi:null-pointer)
              (cffi:null-pointer)
              (cffi:null-pointer))))

(export 'custom-sorter-set-sort-func)

;;; --- End of file gtk4.custom-sorter.lisp ------------------------------------

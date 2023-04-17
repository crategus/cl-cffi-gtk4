;;; ----------------------------------------------------------------------------
;;; gtk4.bin-layout.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.10 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 - 2023 Dieter Kaiser
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
;;; GtkBinLayout
;;;
;;;     A layout manager for bin-like widgets
;;;
;;; Types and Values
;;;
;;;     GtkBinLayout
;;;
;;; Functions
;;;
;;;      gtk_bin_layout_new
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkLayoutManager
;;;         ╰── GtkBinLayout
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkBinLayout
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkBinLayout" bin-layout
  (:superclass layout-manager
   :export t
   :interfaces ()
   :type-initializer "gtk_bin_layout_get_type")
  nil)

#+liber-documentation
(setf (documentation 'bin-layout 'type)
 "@version{#2023-4-17}
  @begin{short}
    The @sym{gtk:bin-layout} class is a @class{gtk:layout-manager} subclass
    useful for create \"bins\" of widgets.
  @end{short}
  The @sym{gtk:bin-layout} object will stack each child of a widget on top of
  each other, using the @slot[gtk:widget]{hexpand}, @slot[gtk:widget]{vexpand},
  @slot[gtk:widget]{halign}, and @slot[gtk:widget]{valign} properties of each
  child widget to determine where they should be positioned.
  @see-class{layout-manager}
  @see-class{gtk:widget}")

;;; ----------------------------------------------------------------------------
;;; gtk_bin_layout_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline bin-layout-new))

(defun bin-layout-new ()
 #+liber-documentation
 "@version{#2023-4-17}
  @return{The newly created @class{gtk:bin-layout} object.}
  @short{Creates a new @class{gtk:bin-layout} object.}
  @see-class{gtk:bin-layout}"
  (make-instance 'bin-layout))

(export 'bin-layout-new)

;;; --- End of file gtk4.bin-layout.lisp ---------------------------------------

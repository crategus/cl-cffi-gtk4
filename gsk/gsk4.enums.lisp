;;; ----------------------------------------------------------------------------
;;; gsk.enums.lisp
;;;
;;; The documentation in this file is taken from the GSK 4 Reference Manual
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
;;;     GskIsolation                                        Since 4.22
;;;     GskPorterDuff                                       Since 4.22
;;; ----------------------------------------------------------------------------

(in-package :gsk)

;;; ----------------------------------------------------------------------------
;;; GskIsolation                                            Since 4.22
;;; ----------------------------------------------------------------------------

#+gtk-4-22
(gobject:define-gflags "GskIsolation" isolation
  (:export t
   :type-initializer "gsk_isolation_get_type")
  (:none 0)
  (:background #.(ash 1 0))
  (:copy-paste #.(ash 1 1))
  (:all -1))

#+(and gtk-4-22 liber-documentation)
(setf (liber:alias-for-symbol 'isolation)
      "GFlags"
      (liber:symbol-documentation 'isolation)
 "@version{2026-08-30}
  @begin{declaration}
(gobject:define-gflags \"GskIsolation\" isolation
  (:export t
   :type-initializer \"gsk_isolation_get_type\")
  (:none 0)
  (:background #.(ash 1 0))
  (:copy-paste #.(ash 1 1))
  (:all -1))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:none]{No isolation is defined.}
      @entry[:background]{If the background should be made available. If the
        background is not available, future operations will be rendered to a
        transparent background and added to the existing background later.}
      @entry[:copy-paste]{If copies should be available to paste nodes. If
        copies are not available, paste nodes can only paste from copies that
        are made inside the isolated contents.}
      @entry[:all]{Isolate everything. This will include features that are
        added in the future.}
    @end{simple-table}
  @end{values}
  @begin{short}
    These flags describe the types of isolations possible with a
    @symbol{gsk:isolation-node} node.
  @end{short}
  More isolation options may be added in the future.

  Since 4.22
  @see-symbol{gsk:isolation-node}")

;;; ----------------------------------------------------------------------------
;;; GskPorterDuff                                           Since 4.22
;;; ----------------------------------------------------------------------------

#+gtk-4-22
(gobject:define-genum "GskPorterDuff" porter-duff
  (:export t
   :type-initializer "gsk_porter_duff_get_type")
  (:source 0)
  (:dest 1)
  (:over-dest 2)
  (:over-source 3)
  (:in-dest 4)
  (:in-source 5)
  (:source-out-dest 6)
  (:dest-out-source 7)
  (:source-atop-dest 8)
  (:dest-atop-source 9)
  (:xor 10)
  (:clear 11))

#+(and gtk-4-22 liber-documentation)
(setf (liber:alias-for-symbol 'porter-duff)
      "GEnum"
      (liber:symbol-documentation 'porter-duff)
 "@version{2026-08-30}
  @begin{declaration}
(gobject:define-genum \"GskPorterDuff\" porter-duff
  (:export t
   :type-initializer \"gsk_porter_duff_get_type\")
  (:source 0)
  (:dest 1)
  (:over-dest 2)
  (:over-source 3)
  (:in-dest 4)
  (:in-source 5)
  (:source-out-dest 6)
  (:dest-out-source 7)
  (:source-atop-dest 8)
  (:dest-atop-source 9)
  (:xor 10)
  (:clear 11))
  @end{declaration}
  @begin{values}
    @begin{simple-table}
      @entry[:source]{No description available.}
      @entry[:dest]{No description available.}
      @entry[:over-dest]{No description available.}
      @entry[:in-dest]{No description available.}
      @entry[:in-source]{No description available.}
      @entry[:source-out-dest]{No description available.}
      @entry[:dest-out-source]{No description available.}
      @entry[:source-atop-dest]{No description available.}
      @entry[:dest-atop-source]{No description available.}
      @entry[:xor]{No description available.}
      @entry[:clear]{No description available.}
    @end{simple-table}
  @end{values}
  @begin{short}
    The 12 compositing modes defined by the seminal paper by Thomas Porter and
    Tom Duff.
  @end{short}
  They are used in SVG, PDF and in Cairo with the @symbol{cairo:operator-t}
  enumeration.

  Since 4.22
  @see-symbol{cairo:operator-t}")

;;; --- End of file gsk4.enums.lisp --------------------------------------------

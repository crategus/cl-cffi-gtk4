;;; ----------------------------------------------------------------------------
;;; gdk4.dmabuf-formats.lisp
;;;
;;; The documentation of this file is taken from the GDK 4 Reference Manual
;;; Version 4.14 and modified to document the Lisp binding to the GDK library.
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
;;;     GdkDmabufFormats
;;;
;;; Functions
;;;
;;;     gdk_dmabuf-formats-contains
;;;     gdk_dmabuf-formats-equal
;;;     gdk_dmabuf-formats-get_format
;;;     gdk_dmabuf-formats-get_n_formats
;;;     gdk_dmabuf-formats-ref
;;;     gdk_dmabuf-formats-unref
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkDmabufFormats
;;; ----------------------------------------------------------------------------

(glib:define-g-boxed-opaque dmabuf-formats "GdkDmabufFormats"
  :export t
  :type-initializer "gdk_dmabuf_formats_get_type"
  :alloc (error "GdkDmabufFormats cannot be created from the Lisp side"))

#+liber-documentation
(setf (liber:alias-for-class 'dmabuf-formats)
      "GBoxed"
      (documentation 'dmabuf-formats 'type)
 "@version{2024-5-26}
  @begin{declaration}
(glib:define-g-boxed-opaque dmabuf-formats \"GdkDmabufFormats\"
  :export t
  :type-initializer \"gdk_dmabuf_formats_get_type\"
  :alloc (error \"GdkDmabufFormats cannot be created from the Lisp side\"))
  @end{declaration}
  @begin{short}
    The @class{gdk:dmabuf-formats} structure provides information about
    supported DMA buffer formats.
  @end{short}
  You can query whether a given format is supported with the
  @fun{gdk:dmabuf-formats-contains} function and you can iterate over the list
  of all supported formats with the @fun{gdk:dmabuf-formats-n-formats}
  and @fun{gdk:dmabuf-formats-format} function.

  The list of supported formats is sorted by preference, with the best formats
  coming first. The list may contain (format, modifier) pairs where the modifier
  is @code{DMA_FORMAT_MOD_INVALID}, indicating that implicit modifiers may be
  used with this format.

  See the @class{gdk:dmabuf-texture-builder} documentation for more information
  about DMA buffers. Note that DMA buffers only exist on Linux.

  Since 4.14
  @see-class{gdk:dmabuf-texture-builder}")

#|
gdk_dmabuf_formats_contains
Returns whether a given format is contained in formats.

since: 4.14

gdk_dmabuf_formats_equal
Returns whether formats1 and formats2 contain the same dmabuf formats, in the
same order.

since: 4.14

gdk_dmabuf_formats_get_format
Gets the fourcc code and modifier for a format that is contained in formats.

since: 4.14

gdk_dmabuf_formats_get_n_formats
Returns the number of formats that the formats object contains.

since: 4.14

gdk_dmabuf_formats_ref
Increases the reference count of formats.

since: 4.14

gdk_dmabuf_formats_unref
Decreases the reference count of formats.

since: 4.14
|#

;;; --- End of file gdk4.dmabuf-formats.lisp -----------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk4.dmabuf-formats.lisp
;;;
;;; The documentation in this file is taken from the GDK 4 Reference Manual
;;; version 4.18 and modified to document the Lisp binding to the GDK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2024 - 2025 Dieter Kaiser
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

(glib:define-gboxed-opaque dmabuf-formats "GdkDmabufFormats"
  :export t
  :type-initializer "gdk_dmabuf_formats_get_type"
  :alloc (error "GdkDmabufFormats cannot be created from the Lisp side"))

#+liber-documentation
(setf (liber:alias-for-class 'dmabuf-formats)
      "GBoxed"
      (documentation 'dmabuf-formats 'type)
 "@version{2024-05-26}
  @begin{declaration}
(glib:define-gboxed-opaque dmabuf-formats \"GdkDmabufFormats\"
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

;;; ----------------------------------------------------------------------------
;;; gdk_dmabuf_formats_contains
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_dmabuf_formats_contains" dmabuf-formats-contains) :boolean
 #+liber-documentation
 "@version{2025-08-02}
  @argument[formats]{a @class{gdk:dmabuf-formats} instance}
  @argument[fourcc]{an integer for the format code}
  @argument[modifier]{an integer for the format modifier}
  @begin{return}
    @em{True} if the format specified by the arguments is part of @arg{formats}.
  @end{return}
  @begin{short}
    Returns whether a given format is contained in @arg{formats}.
  @end{short}

  Since 4.14
  @see-class{gdk:dmabuf-formats}"
  (formats (g:boxed dmabuf-formats))
  (fourcc :uint32)
  (modifier :uint64))

(export 'dmabuf-formats-contains)

;;; ----------------------------------------------------------------------------
;;; gdk_dmabuf_formats_equal
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_dmabuf_formats_equal" dmabuf-formats-equal) :boolean
 #+liber-documentation
 "@version{2024-07-10}
  @argument[formats1]{a @class{gdk:dmabuf-formats} instance}
  @argument[formats2]{another @class{gdk:dmabuf-formats} instance}
  @return{@em{True} if @arg{formats1} and @arg{formats2} are equal.}
  @begin{short}
    Returns whether @arg{formats1} and @arg{formats2} contain the same dmabuf
    formats, in the same order.
  @end{short}

  Since 4.14
  @see-class{gdk:dmabuf-formats}"
  (formats1 (g:boxed dmabuf-formats))
  (formats2 (g:boxed dmabuf-formats)))

(export 'dmabuf-formats-equal)

;;; ----------------------------------------------------------------------------
;;; gdk_dmabuf_formats_get_format
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_dmabuf_formats_get_format" %dmabuf-formats-format) :void
  (formats (g:boxed dmabuf-formats))
  (idx :size)
  (fourcc (:pointer :uint32))
  (modifier (:pointer :uint64)))

(defun dmabuf-formats-format (formats idx)
 #+liber-documentation
 "@version{2025-08-02}
  @syntax{(gdk:dmabuf-formats-format formats idx) => fourcc, modifier}
  @argument[formats]{a @class{gdk:dmabuf-formats} instance}
  @argument[idx]{an integer for the index of the format to return}
  @argument[fourcc]{an integer for the format code}
  @argument[modifier]{an integer for the format modifier}
  @begin{short}
    Gets the fourcc code and modifier for a format that is contained in
    @arg{formats}.
  @end{short}

  Since 4.14
  @see-class{gdk:dmabuf-formats}"
  (cffi:with-foreign-objects ((fourcc :uint32) (modifier :uint64))
    (%dmabuf-formats-format formats idx fourcc modifier)
    (values (cffi:mem-ref fourcc :uint32)
            (cffi:mem-ref modifier :uint64))))

(export 'dmabuf-formats-format)

;;; ----------------------------------------------------------------------------
;;; gdk_dmabuf_formats_get_n_formats
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_dmabuf_formats_get_n_formats" dmabuf-formats-n-formats)
    :size
 #+liber-documentation
 "@version{2025-08-04}
  @argument[formats]{a @class{gdk:dmabuf-formats} instance}
  @return{The integer for the number of formats.}
  @begin{short}
    Returns the number of formats that the formats object contains.
  @end{short}
  Note that DMA buffers are a Linux concept, so on other platforms,
  the @fun{gdk:dmabuf-formats-n-formats} function will always return zero.

  Since 4.14
  @see-class{gdk:dmabuf-formats}"
  (formats (g:boxed dmabuf-formats)))

(export 'dmabuf-formats-n-formats)

;;; ----------------------------------------------------------------------------
;;; gdk_dmabuf_formats_ref                                  not needed
;;;
;;; Increases the reference count of formats.
;;;
;;; Since: 4.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_dmabuf_formats_unref                                not needed
;;;
;;; Decreases the reference count of formats.
;;;
;;; Since 4.14
;;; ----------------------------------------------------------------------------

;;; --- End of file gdk4.dmabuf-formats.lisp -----------------------------------

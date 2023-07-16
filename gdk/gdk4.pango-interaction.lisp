;;; ----------------------------------------------------------------------------
;;; gdk4.pango-interaction.lisp
;;;
;;; The documentation of this file is taken from the GDK 4 Reference Manual
;;; Version 4.10 and modified to document the Lisp binding to the GDK library.
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
;;; Pango Interaction
;;;
;;;     Using Pango in GDK
;;;
;;; Functions
;;;
;;;     gdk_pango_layout_get_clip_region
;;;     gdk_pango_layout_line_get_clip_region
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; gdk_pango_layout_get_clip_region ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_pango_layout_get_clip_region" %pango-layout-clip-region)
    (:pointer (:struct cairo:region-t))
  (layout (g:object pango:layout))
  (xorigin :int)
  (yorigin :int)
  (ranges-ar :pointer)
  (n-ranges :int))

(defun pango-layout-clip-region (layout xorigin yorigin ranges)
 #+liber-documentation
 "@version{#2023-4-14}
  @argument[layout]{a @class{pango:layout} object}
  @argument[xorigin]{an integer with the x pixel where you intend to draw the
    layout with this clip }
  @argument[yorigin]{an integer with the y pixel where you intend to draw the
    layout with this clip }
  @argument[ranges]{a list of integer with the byte indexes into the layout,
    where even members of the list are start indexes and elements end indexes}
  @return{A @symbol{cairo:region-t} instance with the clip region containing
    the given ranges.}
  @begin{short}
    Obtains a clip region which contains the areas where the given ranges of
    text would be drawn.
  @end{short}
  The @arg{xorigin} and @arg{yorigin} arguments are the top left point to center
  the layout. The @arg{ranges} argument should contain ranges of bytes in the
  text of the layout.

  Note that the regions returned correspond to logical extents of the text
  ranges, not ink extents. So the drawn layout may in fact touch areas out of
  the clip region. The clip region is mainly useful for highlightling parts of
  text, such as when text is selected.
  @see-class{pango:layout}
  @see-symbol{cairo:region-t}"
  (let ((n (length ranges)))
    (cffi:with-foreign-object (ranges-ar :int n)
      (iter (for i from 0 below n)
            (for range in ranges)
            (setf (cffi:mem-aref ranges-ar :int i) range))
      (%pango-layout-clip-region layout xorigin yorigin ranges-ar n))))

(export 'pango-layout-clip-region)

;;; ----------------------------------------------------------------------------
;;; gdk_pango_layout_line_get_clip_region ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_pango_layout_line_clip_region" %pango-layout-line-clip-region)
    (:pointer (:struct cairo:region-t))
  (line (g:boxed pango:layout-line))
  (xorigin :int)
  (yorigin :int)
  (ranges-ar :pointer)
  (n-ranges :int))

(defun pango-layout-line-clip-region (line xorigin yorigin ranges)
 #+liber-documentation
 "@version{#2023-4-14}
  @argument[line]{a @class{pango:layout-line} instance}
  @argument[xorigin]{an integer with the x pixel where you intend to draw the
    layout line with this clip }
  @argument[yorigin]{an integer with the y pixel where you intend to draw the
    layout line with this clip }
  @argument[ranges]{a list of integer with the byte indexes into the layout,
    where even members of the list are start indexes and elements end indexes}
  @return{A @symbol{cairo:region-t} instance with the clip region containing
    the given ranges.}
  @begin{short}
    Obtains a clip region which contains the areas where the given ranges of
    text would be drawn.
  @end{short}
  The @arg{xorigin} and @arg{yorigin} arguments are the top left position of
  the layout. The @arg{ranges} argument should contain ranges of bytes in the
  text of the layout. The clip region will include space to the left or right
  of the line (to the layout bounding box) if you have indexes above or below
  the indexes contained inside the line. This is to draw the selection all the
  way to the side of the layout. However, the clip region is in line
  coordinates, not layout coordinates.

  Note that the regions returned correspond to logical extents of the text
  ranges, not ink extents. So the drawn layout may in fact touch areas out of
  the clip region. The clip region is mainly useful for highlightling parts of
  text, such as when text is selected.
  @see-class{pango:layout}
  @see-symbol{cairo:region-t}"
  (let ((n (length ranges)))
    (cffi:with-foreign-object (ranges-ar :int n)
      (iter (for i from 0 below n)
            (for range in ranges)
            (setf (cffi:mem-aref ranges-ar :int i) range))
      (%pango-layout-line-clip-region line xorigin yorigin ranges-ar n))))

(export 'pango-layout-line-clip-region)

;;; --- End of file gdk4.pango-interaction.lisp --------------------------------

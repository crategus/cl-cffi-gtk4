;;; ----------------------------------------------------------------------------
;;; gdk4.content-formats-builder.lisp
;;;
;;; The documentation in this file is taken from the GDK 4 Reference Manual
;;; version 4.20 and modified to document the Lisp binding to the GDK library,
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
;;; Content Formats Builder
;;;
;;; Types and Values
;;;
;;;     GdkContentFormatsBuilder
;;;
;;; Functions
;;;
;;;     gdk_content_formats_builder_new
;;;     gdk_content_formats_builder_free_to_formats         not needed
;;;     gdk_content_formats_builder_add_formats
;;;     gdk_content_formats_builder_add_gtype
;;;     gdk_content_formats_builder_add_mime_type
;;;     gdk_content_formats_builder_ref                     not needed
;;;     gdk_content_formats_builder_unref                   not needed
;;;     gdk_content_formats_builder_to_formats
;;;
;;; Object Hierarchy
;;;
;;;     GBoxed
;;;     ╰── GdkContentFormatsBuilder
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkContentFormatsBuilder
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_content_formats_builder_new" %content-formats-builder-new)
  :pointer)

(glib:define-gboxed-opaque content-formats-builder "GdkContentFormatsBuilder"
  :export t
  :type-initializer "gdk_content_formats_builder_get_type"
  :alloc (%content-formats-builder-new))

#+liber-documentation
(setf (liber:alias-for-class 'content-formats-builder)
      "GBoxed"
      (documentation 'content-formats-builder 'type)
 "@version{2026-04-03}
  @begin{declaration}
(glib:define-gboxed-opaque content-formats \"GdkContentFormatsBuilder\"
  :export t
  :type-initializer \"gdk_content_formats_builder_get_type\"
  :alloc (%content-formats-builder-new))
  @end{declaration}
  @begin{short}
    The @class{gdk:content-formats-builder} structure is an opaque structure.
  @end{short}
  It is meant to not be kept around and only be used to create new
  @class{gdk:content-formats} instances.
  @see-constructor{gdk:content-formats-builder-new}
  @see-class{gdk:content-formats}")

;;; ----------------------------------------------------------------------------
;;; gdk_content_formats_builder_new
;;; ----------------------------------------------------------------------------

(defun content-formats-builder-new ()
 #+liber-documentation
 "@version{2026-04-03}
  @return{The new @class{gdk:content-formats-builder} instance.}
  @begin{short}
    Creates a new @class{gdk:content-formats-builder} instance.
  @end{short}
  The resulting builder would create an empty @class{gdk:content-formats}
  instance. Use addition functions to add types to it.
  @see-class{gdk:content-formats-builder}"
  (make-instance 'content-formats-builder))

(export 'content-formats-builder-new)

;;; ----------------------------------------------------------------------------
;;; gdk_content_formats_builder_free_to_formats             not needed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_formats_builder_add_formats
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_content_formats_builder_add_formats"
               content-formats-builder-add-formats) :void
 #+liber-documentation
 "@version{2026-04-03}
  @argument[builder]{a @class{gdk:content-formats-builder} instance}
  @argument[formats]{a @class{gdk:content-formats} instance for the formats
    to add}
  @begin{short}
    Appends all formats from @arg{formats} to @arg{builder}, skipping those
    that already exist.
  @end{short}
  @see-class{gdk:content-formats-builder}
  @see-class{gdk:content-formats}"
  (builder (g:boxed content-formats-builder))
  (formats (g:boxed content-formats)))

(export 'content-formats-builder-add-formats)

;;; ----------------------------------------------------------------------------
;;; gdk_content_formats_builder_add_gtype
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_content_formats_builder_add_gtype"
               content-formats-builder-add-gtype) :void
 #+liber-documentation
 "@version{2026-04-03}
  @argument[builder]{a @class{gdk:content-formats-builder} instance}
  @argument[gtype]{a @class{g:type-t} type ID}
  @begin{short}
    Appends @arg{gtype} to @arg{builder} if it has not already been added.
  @end{short}
  @see-class{gdk:content-formats-builder}
  @see-class{g:type-t}"
  (builder (g:boxed content-formats-builder))
  (gtype g:type-t))

(export 'content-formats-builder-add-gtype)

;;; ----------------------------------------------------------------------------
;;; gdk_content_formats_builder_add_mime_type
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_content_formats_builder_add_mime_type"
               content-formats-builder-add-mime-type) :void
 #+liber-documentation
 "@version{2026-04-03}
  @argument[builder]{a @class{gdk:content-formats-builder} instance}
  @argument[mtype]{a string for the MIME type}
  @begin{short}
    Appends @arg{mtype} to @arg{builder} if it has not already been added.
  @end{short}
  @see-class{gdk:content-formats-builder}"
  (builder (g:boxed content-formats-builder))
  (mtype :string))

(export 'content-formats-builder-add-mime-type)

;;; ----------------------------------------------------------------------------
;;; gdk_content_formats_builder_ref                         not needed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_formats_builder_unref                       not needed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_formats_builder_to_formats
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_content_formats_builder_to_formats"
               content-formats-builder-to-formats)
    (g:boxed content-formats :return)
 #+liber-documentation
 "@version{2026-04-03}
  @argument[builder]{a @class{gdk:content-formats-builder} instance}
  @begin{return}
    The newly created @class{gdk:content-formats} instance with all the formats
    added to @arg{builder}.
  @end{return}
  @begin{short}
    Creates a new @class{gdk:content-formats} instance from the given
    @arg{builder}.
  @end{short}
  The given @arg{builder} is reset once this function returns. You cannot call
  this function multiple times on the same builder instance.
  @see-class{gdk:content-formats-builder}
  @see-class{gdk:content-formats}"
  (builder (g:boxed content-formats-builder)))

(export 'content-formats-builder-to-formats)

;;; --- End of file gdk4.content-formats-builder -------------------------------

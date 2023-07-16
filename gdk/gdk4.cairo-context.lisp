;;; ----------------------------------------------------------------------------
;;; gdk4.cairo-context.lisp
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
;;; GdkCairoContext
;;;
;;;     Cairo draw context
;;;
;;; Types and Values
;;;
;;;     GdkCairoContext
;;;
;;; Functions
;;;
;;;     gdk_cairo_context_cairo_create
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkCairoContext
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GdkCairoContext" cairo-context
  (:superclass draw-context
   :export t
   :interfaces nil
   :type-initializer "gdk_cairo_context_get_type")
  nil)

#+liber-documentation
(setf (documentation 'cairo-context 'type)
 "@version{2023-4-7}
  @begin{short}
    The @sym{gdk:cairo-context} object is an object representing the platform
    specific draw context.
  @end{short}
  The @sym{gdk:cairo-context} objects are created for a @class{gdk:display}
  object using the @fun{gdk:surface-create-cairo-context} function, and the
  context can then be used to draw on that @class{gdk:surface} object.
  @see-class{gdk:display}
  @see-class{gdk:surface}
  @see-function{gdk:surface-create-cairo-context}")

;;; ----------------------------------------------------------------------------
;;; gdk_cairo_context_cairo_create ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_cairo_context_cairo_create" cairo-context-cairo-create)
    (:pointer (:struct cairo:context-t))
 #+liber-documentation
 "@version{2023-4-7}
  @argument[context]{a @class{gdk:cairo-context} object that is currently
    drawing}
  @return{A @symbol{cairo:context-t} instance with the Cairo context to be used
    to draw the contents of the @class{gdk:surface} object. @code{NULL} is
    returned when @arg{context} is not drawing.}
  @begin{short}
    Retrieves a Cairo context to be used to draw on the @class{gdk:surface}
    object of @arg{context}.
  @end{short}
  A call to the @fun{gdk:draw-context-begin-frame} function with this context
  must have been done or this function will return @code{NULL}. The returned
  context is guaranteed to be valid until the @fun{gdk:draw-context-end-frame}
  function is called.
  @see-class{gdk:cairo-context}
  @see-symbol{cairo:context-t}
  @see-function{gdk:draw-context-begin-frame}
  @see-function{gdk:draw-context-end-frame}"
  (context (g:object cairo-context)))

(export 'cairo-context-cairo-create)

;;; --- End of file gdk4.cairo-context.lisp ------------------------------------

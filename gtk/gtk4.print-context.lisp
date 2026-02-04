;;; ----------------------------------------------------------------------------
;;; gtk4.print-context.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.20 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2026 Dieter Kaiser
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
;;; GtkPrintContext
;;;
;;;     Encapsulates context for drawing pages
;;;
;;; Types and Values
;;;
;;;     GtkPrintContext
;;;
;;; Functions
;;;
;;;     gtk_print_context_get_cairo_context
;;;     gtk_print_context_set_cairo_context
;;;     gtk_print_context_get_page_setup
;;;     gtk_print_context_get_width
;;;     gtk_print_context_get_height
;;;     gtk_print_context_get_dpi_x
;;;     gtk_print_context_get_dpi_y
;;;     gtk_print_context_get_pango_fontmap
;;;     gtk_print_context_create_pango_context
;;;     gtk_print_context_create_pango_layout
;;;     gtk_print_context_get_hard_margins
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkPrintContext
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkPrintContext
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkPrintContext" print-context
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gtk_print_context_get_type")
  nil)

#+liber-documentation
(setf (documentation 'print-context 'type)
 "@version{2025-07-24}
  @begin{short}
    The @class{gtk:print-context} object encapsulates context information that
    is required when drawing pages for printing, such as the Cairo context and
    important parameters like page size and resolution.
  @end{short}
  It also lets you create @class{pango:layout} and @class{pango:context} objects
  that match the font metrics of the Cairo surface.

  The @class{gtk:print-context} object gets passed to the
  @sig[gtk:print-operation]{begin-print}, @sig[gtk:print-operation]{end-print},
  @sig[gtk:print-operation]{request-page-setup} and
  @sig[gtk:print-operation]{draw-page} signal handlers on the print operation.
  @begin[Examples]{dictionary}
    Using the @class{gtk:print-context} object in a @code{draw-page} callback
    function.
    @begin{pre}
(defun draw-page (operation context pagenr)
  (declare (ignore operation pagenr))
  (let ((cr (gtk:print-context-cairo-context context))
        (layout (gtk:print-context-create-pango-layout context)))
    ;; Draw a red rectangle, as wide as the paper (inside the margins)
    (cairo:set-source-rgb cr 1.0 0 0)
    (cairo:rectangle cr 0 0 (gtk:print-context-width context) 50)
    (cairo:fill cr)
    ;; Draw some lines
    (cairo:move-to cr 20 10)
    (cairo:line-to cr 40 20)
    (cairo:arc cr 60 60 20 0 3.14)
    (cairo:line-to cr 80 20)
    (cairo:set-source-rgb cr 0 0 0)
    (cairo:set-line-width cr 5)
    (cairo:set-line-cap cr :round)
    (cairo:set-line-join cr :round)
    (cairo:stroke cr)
    ;; Draw some text
    (setf (pango:layout-text layout) \"Hello World! Printing is easy\")
    (setf (pango:layout-font-description layout)
          (pango:font-description-from-string \"sans 28\"))
    (cairo:move-to cr 30 20)
    (pango:cairo-layout-path cr layout)
    ;; Font Outline
    (cairo:set-source-rgb cr 0.93 1.0 0.47)
    (cairo:set-line-width cr 0.5)
    (cairo:stroke-preserve cr)
    ;; Font Fill
    (cairo:set-source-rgb cr 0 0.0 1.0)
    (cairo:fill cr)))
    @end{pre}
  @end{dictionary}
  @see-class{gtk:print-operation}")

;;; ----------------------------------------------------------------------------
;;; gtk_print_context_get_cairo_context
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_print_context_get_cairo_context"
               print-context-cairo-context) (:pointer (:struct cairo:context-t))
 #+liber-documentation
 "@version{#2025-07-26}
  @argument[context]{a @class{gtk:print-context} object}
  @begin{return}
    The @sym{cairo:context-t} instance for the Cairo context of @arg{context}.
  @end{return}
  @begin{short}
    Obtains the Cairo context that is associated with the print context.
  @end{short}
  @see-class{gtk:print-context}
  @see-symbol{cairo:context-t}
  @see-function{gtk:print-context-set-cairo-context}"
  (context (g:object print-context)))

(export 'print-context-cairo-context)

;;; ----------------------------------------------------------------------------
;;; gtk_print_context_set_cairo_context
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_print_context_set_cairo_context"
               %print-context-set-cairo-context) :void
  (context (g:object print-context))
  (cr (:pointer (:struct cairo:context-t)))
  (xdpi :double)
  (ydpi :double))

(defun print-context-set-cairo-context (context cr xdpi ydpi)
 #+liber-documentation
 "@version{#2025-07-26}
  @argument[context]{a @class{gtk:print-context} object}
  @argument[cr]{a @sym{cairo:context-t} instance for the Cairo context}
  @argument[xdpi]{a number coerced to a double float for the horizontal
    resolution to use with @arg{cr}}
  @argument[ydpi]{a number coerced to a double float for the vertical
    resolution to use with @arg{cr}}
  @begin{short}
    Sets a new Cairo context on a print context.
  @end{short}
  This function is intended to be used when implementing an internal print
  preview, it is not needed for printing, since GTK itself creates a suitable
  Cairo context in that case.
  @see-class{gtk:print-context}
  @see-symbol{cairo:context-t}
  @see-function{gtk:print-context-cairo-context}"
  (%print-context-set-cairo-context context
                                    cr
                                    (coerce xdpi 'double-float)
                                    (coerce ydpi 'double-float)))

(export 'print-context-set-cairo-context)

;;; ----------------------------------------------------------------------------
;;; gtk_print_context_get_page_setup
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_print_context_get_page_setup" print-context-page-setup)
    (g:object page-setup)
 #+liber-documentation
 "@version{#2025-07-26}
  @argument[context]{a @class{gtk:print-context} object}
  @begin{return}
    The @class{gtk:page-setup} object for the page setup of the print context.
  @end{return}
  @begin{short}
    Obtains the page setup that determines the page dimensions of the print
    context.
  @end{short}
  @see-class{gtk:print-context}
  @see-class{gtk:page-setup}"
  (context (g:object print-context)))

(export 'print-context-page-setup)

;;; ----------------------------------------------------------------------------
;;; gtk_print_context_get_width
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_print_context_get_width" print-context-width) :double
 #+liber-documentation
 "@version{2025-07-26}
  @argument[context]{a @class{gtk:print-context} object}
  @return{The double float for the width of @arg{context}.}
  @begin{short}
    Obtains the width of the print context, in pixels.
  @end{short}
  @see-class{gtk:print-context}
  @see-function{gtk:print-context-height}"
  (context (g:object print-context)))

(export 'print-context-width)

;;; ----------------------------------------------------------------------------
;;; gtk_print_context_get_height
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_print_context_get_height" print-context-height) :double
 #+liber-documentation
 "@version{2025-07-26}
  @argument[context]{a @class{gtk:print-context} object}
  @return{The double float for the height of @arg{context}.}
  @begin{short}
    Obtains the height of the print context, in pixels.
  @end{short}
  @see-class{gtk:print-context}
  @see-function{gtk:print-context-width}"
  (context (g:object print-context)))

(export 'print-context-height)

;;; ----------------------------------------------------------------------------
;;; gtk_print_context_get_dpi_x
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_print_context_get_dpi_x" print-context-dpi-x) :double
 #+liber-documentation
 "@version{#2025-07-26}
  @argument[context]{a @class{gtk:print-context} object}
  @return{The double float for the horizontal resolution of @arg{context}.}
  @begin{short}
    Obtains the horizontal resolution of the print context, in dots per inch.
  @end{short}
  @see-class{gtk:print-context}
  @see-function{gtk:print-context-dpi-y}"
  (context (g:object print-context)))

(export 'print-context-dpi-x)

;;; ----------------------------------------------------------------------------
;;; gtk_print_context_get_dpi_y
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_print_context_get_dpi_y" print-context-dpi-y) :double
 #+liber-documentation
 "@version{#2025-07-26}
  @argument[context]{a @class{gtk:print-context} object}
  @return{The double float for the vertical resolution of @arg{context}.}
  @begin{short}
    Obtains the vertical resolution of the print context, in dots per inch.
  @end{short}
  @see-class{gtk:print-context}
  @see-function{gtk:print-context-dpi-x}"
  (context (g:object print-context)))

(export 'print-context-dpi-y)

;;; ----------------------------------------------------------------------------
;;; gtk_print_context_get_pango_fontmap
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_print_context_get_pango_fontmap"
               print-context-pango-fontmap) (g:object pango:font-map)
 #+liber-documentation
 "@version{#2025-07-27}
  @argument[context]{a @class{gtk:print-context} object}
  @return{The @class{pango:font-map} object for the font map of @arg{context}.}
  @begin{short}
    Returns a font map that is suitable for use with the print context.
  @end{short}
  @see-class{gtk:print-context}
  @see-class{pango:font-map}"
  (context (g:object print-context)))

(export 'print-context-pango-fontmap)

;;; ----------------------------------------------------------------------------
;;; gtk_print_context_create_pango_context
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_print_context_create_pango_context"
               print-context-create-pango-context) (g:object pango:context)
 #+liber-documentation
 "@version{#2025-07-26}
  @argument[context]{a @class{gtk:print-context} object}
  @begin{return}
    The new @class{pango:context} object with the Pango context for
    @arg{context}.
  @end{return}
  @begin{short}
    Creates a new Pango context that can be used with the print context.
  @end{short}
  @see-class{gtk:print-context}
  @see-class{pango:context}"
  (context (g:object print-context)))

(export 'print-context-create-pango-context)

;;; ----------------------------------------------------------------------------
;;; gtk_print_context_create_pango_layout
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_print_context_create_pango_layout"
               print-context-create-pango-layout) (g:object pango:layout)
 #+liber-documentation
 "@version{2024-02-16}
  @argument[context]{a @class{gtk:print-context} object}
  @return{The new @class{pango:layout} object for @arg{context}.}
  @begin{short}
    Creates a new Pango layout that is suitable for use with the print context.
  @end{short}
  @see-class{gtk:print-context}
  @see-class{pango:layout}"
  (context (g:object print-context)))

(export 'print-context-create-pango-layout)

;;; ----------------------------------------------------------------------------
;;; gtk_print_context_get_hard_margins
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_print_context_get_hard_margins"
               %print-context-hard-margins) :boolean
  (context (g:object print-context))
  (top (:pointer :int))
  (bottom (:pointer :int))
  (left (:pointer :int))
  (right (:pointer :int)))

(defun print-context-hard-margins (context)
 #+liber-documentation
 "@version{#2025-08-04}
  @syntax{(gtk:print-context-hard-margins context) => top, bottom, left, right}
  @argument[context]{a @class{gtk:print-context} object}
  @argument[top]{an integer for the top hardware printer margin}
  @argument[bottom]{an integer for the bottom hardware printer margin}
  @argument[left]{an integer for the left hardware printer margin}
  @argument[right]{an integer for the right hardware printer margin}
  @begin{short}
    Obtains the hardware printer margins of the print context, in units.
  @end{short}
  @see-class{gtk:print-context}"
  (cffi:with-foreign-objects ((top :int) (bottom :int) (left :int) (right :int))
    (%print-context-hard-margins context top bottom left right)
    (values (cffi:mem-ref top :int)
            (cffi:mem-ref bottom :int)
            (cffi:mem-ref left :int)
            (cffi:mem-ref right :int))))

(export 'print-context-hard-margins)

;;; --- End of file gtk4.print-context.lisp ------------------------------------

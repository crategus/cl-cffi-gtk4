;;; ----------------------------------------------------------------------------
;;; gtk4.print-operation-preview.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.20 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2025 Dieter Kaiser
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
;;;     GtkPrintOperationPreview
;;;
;;; Functions
;;;
;;;     gtk_print_operation_preview_end_preview
;;;     gtk_print_operation_preview_is_selected
;;;     gtk_print_operation_preview_render_page
;;;
;;; Signals
;;;
;;;     got-page-size
;;;     ready
;;;
;;; Object Hierarchy
;;;
;;;     GInterface
;;;     ╰── GtkPrintOperationPreview
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkPrintOperationPreview
;;; ----------------------------------------------------------------------------

(gobject:define-ginterface "GtkPrintOperationPreview" print-operation-preview
  (:export t
   :type-initializer "gtk_print_operation_preview_get_type")
  nil)

#+liber-documentation
(setf (liber:alias-for-class 'print-operation-preview)
      "Interface"
      (documentation 'print-operation-preview 'type)
 "@version{2026-01-11}
  @begin{short}
    The @class{gtk:print-operation-preview} interface is implemented by the
    @class{gtk:print-operation} class.
  @end{short}

  By default the @class{gtk:print-operation} object uses an external
  application to do print preview. To implement a custom print preview, an
  application must connect to the @sig[gtk:print-operation]{preview} signal.
  The @fun{gtk:print-operation-preview-render-page},
  @fun{gtk:print-operation-preview-end-preview} and
  @fun{gtk:print-operation-preview-is-selected} functions are useful when
  implementing a print preview.
  @begin[Signal Details]{dictionary}
    @begin[print-operation-preview::got-page-size]{signal}
      @begin{pre}
lambda (preview context pagesetup)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[preview]{The @class{gtk:print-operation-preview} object on which
          the signal is emitted.}
        @entry[context]{The current @class{gtk:print-context} object.}
        @entry[pagesetup]{The @class{gtk:page-setup} object for the current
          page.}
      @end{simple-table}
      The signal is emitted once for each page that gets rendered to the
      preview. A handler for this signal should update the context according to
      the @arg{pagesetup} argument and set up a suitable Cairo context, using
      the @fun{gtk:print-context-set-cairo-context} function.
    @end{signal}
    @begin[print-operation-preview::ready]{signal}
      @begin{pre}
lambda (preview context)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[preview]{The @class{gtk:print-operation-preview} object on which
          the signal is emitted.}
        @entry[context]{The current @class{gtk:print-context} object.}
      @end{simple-table}
      The signal gets emitted once per preview operation, before the first page
      is rendered. A handler for this signal can be used for setup tasks.
    @end{signal}
  @end{dictionary}
  @see-class{gtk:print-operation}
  @see-class{gtk:print-context}
  @see-class{gtk:print-unix-dialog}")

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_preview_end_preview
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_print_operation_preview_end_preview"
               print-operation-preview-end-preview) :void
 #+liber-documentation
 "@version{#2026-01-11}
  @argument[preview]{a @class{gtk:print-operation-preview} object}
  @begin{short}
    Ends a preview.
  @end{short}
  This function must be called to finish a custom print preview.
  @see-class{gtk:print-operation}
  @see-class{gtk:print-operation-preview}"
  (preview (g:object print-operation-preview)))

(export 'print-operation-preview-end-preview)

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_preview_is_selected
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_print_operation_preview_is_selected"
               print-operation-preview-is-selected) :boolean
 #+liber-documentation
 "@version{#2026-01-11}
  @argument[preview]{a @class{gtk:print-operation-preview} object}
  @argument[pagenr]{an integer for the page number}
  @return{@em{True} if the page has been selected for printing.}
  @begin{short}
    Returns whether the given page is included in the set of pages that have
    been selected for printing.
  @end{short}
  @see-class{gtk:print-operation}
  @see-class{gtk:print-operation-preview}"
  (preview (g:object print-operation-preview))
  (pagenr :int))

(export 'print-operation-preview-is-selected)

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_preview_render_page
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_print_operation_preview_render_page"
               print-operation-preview-render-page) :void
 #+liber-documentation
 "@version{#2026-01-11}
  @argument[preview]{a @class{gtk:print-operation-preview} object}
  @argument[pagenr]{an integer for the page to render}
  @begin{short}
    Renders a page to the preview, using the print context that was passed to
    the @sig[gtk:print-operation]{preview} signal handler together with
    @arg{preview}.
  @end{short}
  A custom print preview should use this function to render the currently
  selected page. Note that this function requires a suitable Cairo context to
  be associated with the print context.
  @see-class{gtk:print-operation}
  @see-class{gtk:print-operation-preview-render-page}"
  (preview (g:object print-operation-preview))
  (page-nr :int))

(export 'print-operation-preview-render-page)

;;; --- End of file gtk4.print-operation-preview.lisp --------------------------

;;; ----------------------------------------------------------------------------
;;; gtk4.section-model.lisp
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
;;; Types and Valus
;;;
;;;     GtkSectionModel
;;;
;;; Functions
;;;
;;;     gtk_section_model_get_section
;;;     gtk_section_model_sections_changed
;;;
;;; Signals
;;;
;;;     sections-changed
;;;
;;; Prerequisite
;;;
;;;     GListModel
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkSectionModel
;;; ----------------------------------------------------------------------------

(gobject:define-ginterface "GtkSectionModel" section-model
  (:export t
   :type-initializer "gtk_section_model_get_type")
  nil)

#+liber-documentation
(setf (liber:alias-for-class 'section-model)
      "Interface"
      (documentation 'section-model 'type)
 "@version{2024-11-29}
  @begin{short}
    The @class{gtk:section-model} interface is an interface that adds support
    for sections to list models.
  @end{short}
  A @class{gtk:section-model} object groups successive items into so-called
  sections. List widgets like the @class{gtk:list-view} and
  @class{gtk:grid-view} widget then allow displaying section headers for these
  sections by installing a header factory.

  Many GTK list models support sections inherently, or they pass through the
  sections of a model they are wrapping.

  When the section groupings of a model change, the model will emit the
  @code{\"sections-changed\"} signal by calling the
  @fun{gtk:section-model-sections-changed} function. All sections in the given
  range then need to be queried again. The @code{\"items-changed\"} signal has
  the same effect, all sections in that range are invalidated, too.

  Since 4.12
  @begin[Signals]{dictionary}
    @subheading{The \"sections-changed\" signal}
      @begin{pre}
lambda (model pos n-items)    :run-last
      @end{pre}
      @begin[code]{table}
        @entry[model]{The @class{gtk:section-model} object that emitted the
          signal.}
        @entry[pos]{The unsigned integer with the first item that may have
          changed.}
        @entry[n-items]{The unsigned integer with the number of items with
          changes.}
      @end{table}
      Emitted when the start-of-section state of some of the items in
      @arg{model} changes. Note that this signal does not specify the new
      section state of the items, they need to be queried manually. It is also
      not necessary for a model to change the section state of any of the items
      in the section model, though it would be rather useless to emit such a
      signal. The @code{\"items-changed\"} signal of the @class{g:list-model}
      class implies the effect of the @code{\"sections-changed\"} signal for
      all the items it covers.
  @end{dictionary}
  @see-class{gtk:list-view}
  @see-class{gtk:grid-view}")

;;; ----------------------------------------------------------------------------
;;; gtk_section_model_get_section
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_section_model_get_section" %section-model-section) :void
  (model (g:object section-model))
  (pos :uint)
  (start (:pointer :uint))
  (end (:pointer :uint)))

(defun section-model-section (model pos)
 #+liber-documentation
 "@version{2025-03-17}
  @syntax{(gtk:section-model-section model pos) => start, end}
  @argument[model]{a @class{gtk:section-model} object}
  @argument[pos]{an unsigned integer for the position of the item to query}
  @argument[start]{an unsigned integer for the position of the first item
    in the section}
  @argument[end]{an unsigned integer for the position of the first item not
    part of the section anymore}
  @begin{short}
    Query the section that covers the given position.
  @end{short}
  The number of items in the section can be computed by @arg{end} - @arg{start}.

  If the position is larger than the number of items, a single range from
  @code{n-items} to @code{G_MAXUINT} will be returned.

  Since 4.12
  @see-class{gtk:section-model}"
  (cffi:with-foreign-objects ((start :uint) (end :uint))
    (%section-model-section model pos start end)
    (values (cffi:mem-ref start :uint)
            (cffi:mem-ref end :uint))))

(export 'section-model-section)

;;; ----------------------------------------------------------------------------
;;; gtk_section_model_sections_changed
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_section_model_sections_changed"
               section-model-sections-changed) :void
 #+liber-documentation
 "@version{2025-03-17}
  @argument[model]{a @class{gtk:section-model} object}
  @argument[pos]{an unsigned integer for the first item hat may have
    changed}
  @argument[n-items]{an unsigned integer for the number of items with changes}
  @begin{short}
    Emits the @code{\"sections-changed\"} signal on @arg{model}.
  @end{short}

  Since 4.12
  @see-class{gtk:section-model}"
  (model (g:object section-model))
  (pos :uint)
  (n-items :uint))

(export 'section-model-sections-changed)

;;; --- End of file gtk4.section-model.lisp ------------------------------------

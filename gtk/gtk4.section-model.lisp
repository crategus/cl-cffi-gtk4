;;; ----------------------------------------------------------------------------
;;; gtk4.section-model.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.12 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2023 Dieter Kaiser
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

(in-package :gtk)

;;;Interface
;;;GtkSectionModel
;;;since: 4.12

;;;[−]
;;;Description
;;;[src]
;;;interface Gtk.SectionModel : Gio.ListModel
;;;GtkSectionModel is an interface that adds support for sections to list models.

;;;A GtkSectionModel groups successive items into so-called sections. List widgets like GtkListView and GtkGridView then allow displaying section headers for these sections by installing a header factory.

;;;Many GTK list models support sections inherently, or they pass through the sections of a model they are wrapping.

;;;When the section groupings of a model change, the model will emit the GtkSectionModel::sections-changed signal by calling the gtk_section_model_sections_changed() function. All sections in the given range then need to be queried again. The GListModel::items-changed signal has the same effect, all sections in that range are invalidated, too.

;;;Available since: 4.12

;;;[−]
;;;Prerequisite 
;;;In order to implement SectionModel, your type must inherit fromGListModel.

;;;[+]
;;;Implementations
;;;[−]

(gobject:define-g-interface "GtkSectionModel" section-model
  (:export t
   :type-initializer "gtk_section_model_get_type")
  nil)

#+liber-documentation
(setf (liber:alias-for-class 'section-model)
      "Interface"
      (documentation 'section-model 'type)
 "@version{#2023-11-4}
  @begin{short}

  @end{short}
")




;;;Instance methods 
;;;gtk_section_model_get_section
;;;Query the section that covers the given position. The number of items in the section can be computed by out_end - out_start.

;;;since: 4.12

;;;gtk_section_model_sections_changed
;;;No description available.
;;;[−]
;;;Signals 
;;;Gtk.SectionModel::sections-changed
;;;Emitted when the start-of-section state of some of the items in model changes.

;;;since: 4.12

;;;[+]
;;;Interface structure
;;;[−]
;;;Virtual methods 
;;;Gtk.SectionModel.get_section
;;;Query the section that covers the given position. The number of items in the section can be computed by out_end - out_start.

;;;since: 4.12

;;; --- End of file gtk4.section-model.lisp ------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk4.inscripton.lisp
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
;;;
;;; Types and Values
;;;
;;;     GtkInscription
;;;     GtkInscriptionOverflow
;;;
;;; Functions
;;;
;;;     gtk_inscription_new
;;;
;;; Properties
;;;
;;;     attributes
;;;     markup
;;;     min-chars
;;;     min-lines
;;;     nat-chars
;;;     nat-lines
;;;     text
;;;     text-overflow
;;;     wrap-mode
;;;     xalign
;;;     yalign
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkInscription
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; enum GtkInscriptionOverflow
;;;
;;;
;;; Since 4.8
;;;
;;; GTK_INSCRIPTION_OVERFLOW_CLIP
;;;     
;;;
;;; GTK_INSCRIPTION_OVERFLOW_ELLIPSIZE_START
;;;     
;;;
;;; GTK_INSCRIPTION_OVERFLOW_ELLIPSIZE_MIDDLE
;;;     
;;;
;;; GTK_INSCRIPTION_OVERFLOW_ELLIPSIZE_END
;;;     
;;; ----------------------------------------------------------------------------

(gobject:define-g-enum "GtkInscriptionOverflow" inscription-overflow
  (:export t
   :type-initializer "gtk_inscription_overflow_get_type")
  (:clip 0)
  (:ellipsize-start 1)
  (:ellipsize-middle 2)
  (:ellipsize-end 3))

#+liber-documentation
(setf (liber:alias-for-symbol 'inscription-overflow)
      "GEnum"
      (liber:symbol-documentation 'inscription-overflow)
 "@version{2023-10-13}
  @begin{short}
    The different methods to handle text in the @class{gtk:inscription} widget 
    when it does not fit the available space.
  @end{short}
  @begin{pre}
(gobject:define-g-enum \"GtkInscriptionOverflow\" inscription-overflow
  (:export t
   :type-initializer \"gtk_inscription_overflow_get_type\")
  (:clip 0)
  (:ellipsize-start 1)
  (:ellipsize-middle 2)
  (:ellipsize-end 3))
  @end{pre}
  @begin[code]{table}
    @entry[:clip]{Clip the remaining text.}
    @entry[:ellipsize-start]{Omit characters at the start of the text.}
    @entry[:ellipsize-middle]{Omit characters at the middle of the text.}
    @entry[:ellipsize-end]{Omit characters at the end of the text.}
  @end{table}
  Since 4.8
  @see-class{gtk:message-dialog}")

;;; ----------------------------------------------------------------------------
;;; Class GtkInscription
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkInscription" inscription
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget")
   :type-initializer "gtk_inscription_get_type")
  ((attributes
    inscription-attributes
    "attributes" "PangoAttrList" t t)
   (markup
    inscription-markup
    "markup" "gchararray" nil t)
   (min-chars
    inscription-min-chars
    "min-chars" "guint" t t)
   (min-lines
    inscription-min-lines
    "min-lines" "guint" t t)
   (nat-chars
    inscription-nat-chars
    "nat-chars" "guint" t t)
   (nat-lines
    inscription-nat-lines
    "nat-lines" "guint" t t)
   (text
    inscription-text
    "text" "gchararray" t t)
   (text-overflow
    inscription-text-over-flow
    "text-overflow" "GtkInscriptionOverflow" t t)
   (wrap-mode
    inscription-wrap-mode
    "wrap-mode" "PangoWrapMode" t t)
   (xalign
    inscription-xalign
    "xalign" "gfloat" t t)
   (yalign
    inscription-yalign
    "yalign" "gfloat" t t)))




;;;Description

;;;final class Gtk.Inscription : Gtk.Widget
;;;  implements Gtk.Accessible, Gtk.Buildable, Gtk.ConstraintTarget {
;;;  /* No available fields */
;;;}
;;;GtkInscription is a widget to show text in a predefined area.

;;;You likely want to use GtkLabel instead as this widget is intended only for a small subset of use cases. The main scenario envisaged is inside lists such as GtkColumnView.

;;;While a GtkLabel sizes itself depending on the text that is displayed, GtkInscription is given a size and inscribes the given text into that space as well as it can.

;;;Users of this widget should take care to plan behaviour for the common case where the text doesn’t fit exactly in the allocated space.

;;;Available since: 4.8


;;; ----------------------------------------------------------------------------
;;; gtk_inscription_new
;;;
;;; Creates a new GtkInscription with the given text.
;;;
;;; Since 4.8
;;; ----------------------------------------------------------------------------

(declaim (inline inscription-new))

(defun inscription-new (text)
  (make-instance 'inscription
                 :text (if text text (cffi:null-pointer))))

(export 'inscription-new)

;;;Instance methods
;;;gtk_inscription_get_attributes
;;;Gets the inscription’s attribute list.

;;;since: 4.8

;;;gtk_inscription_get_min_chars
;;;Gets the min-chars of the inscription.

;;;since: 4.8

;;;gtk_inscription_get_min_lines
;;;Gets the min-lines of the inscription.

;;;since: 4.8

;;;gtk_inscription_get_nat_chars
;;;Gets the nat-chars of the inscription.

;;;since: 4.8

;;;gtk_inscription_get_nat_lines
;;;Gets the nat-lines of the inscription.

;;;since: 4.8

;;;gtk_inscription_get_text
;;;Gets the text that is displayed.

;;;since: 4.8

;;;gtk_inscription_get_text_overflow
;;;Gets the inscription’s overflow method.

;;;since: 4.8

;;;gtk_inscription_get_wrap_mode
;;;Returns line wrap mode used by the inscription.

;;;since: 4.8

;;;gtk_inscription_get_xalign
;;;Gets the xalign of the inscription.

;;;since: 4.8

;;;gtk_inscription_get_yalign
;;;Gets the yalign of the inscription.

;;;since: 4.8

;;;gtk_inscription_set_attributes
;;;Apply attributes to the inscription text.

;;;since: 4.8

;;;gtk_inscription_set_markup
;;;Utility function to set the text and attributes to be displayed.

;;;since: 4.8

;;;gtk_inscription_set_min_chars
;;;Sets the min-chars of the inscription.

;;;since: 4.8

;;;gtk_inscription_set_min_lines
;;;Sets the min-lines of the inscription.

;;;since: 4.8

;;;gtk_inscription_set_nat_chars
;;;Sets the nat-chars of the inscription.

;;;since: 4.8

;;;gtk_inscription_set_nat_lines
;;;Sets the nat-lines of the inscription.

;;;since: 4.8

;;;gtk_inscription_set_text
;;;Sets the text to be displayed.

;;;since: 4.8

;;;gtk_inscription_set_text_overflow
;;;Sets what to do when the text doesn’t fit.

;;;since: 4.8

;;;gtk_inscription_set_wrap_mode
;;;Controls how line wrapping is done.

;;;since: 4.8

;;;gtk_inscription_set_xalign
;;;Sets the xalign of the inscription.

;;;since: 4.8

;;;gtk_inscription_set_yalign
;;;Sets the yalign of the inscription.

;;;since: 4.8

;;;[+]
;;;Methods inherited from GtkWidget (162)
;;;[+]
;;;Methods inherited from GObject (43)
;;;[+]
;;;Methods inherited from GtkAccessible (18)
;;;[+]
;;;Methods inherited from GtkBuildable (1)
;;;[−]
;;;Properties
;;;Gtk.Inscription:attributes
;;;A list of style attributes to apply to the text of the inscription.

;;;since: 4.8

;;;Gtk.Inscription:markup
;;;Utility property that sets both the GtkInscription:text and GtkInscription:attributes properties, mainly intended for use in GtkBuilder ui files to ease translation support and bindings.

;;;since: 4.8

;;;Gtk.Inscription:min-chars
;;;The number of characters that should fit into the inscription at minimum.

;;;since: 4.8

;;;Gtk.Inscription:min-lines
;;;The number of lines that should fit into the inscription at minimum.

;;;since: 4.8

;;;Gtk.Inscription:nat-chars
;;;The number of characters that should ideally fit into the inscription.

;;;since: 4.8

;;;Gtk.Inscription:nat-lines
;;;The number of lines that should ideally fit into the inscription.

;;;since: 4.8

;;;Gtk.Inscription:text
;;;The displayed text.

;;;since: 4.8

;;;Gtk.Inscription:text-overflow
;;;The overflow method to use for the text.

;;;since: 4.8

;;;Gtk.Inscription:wrap-mode
;;;Controls how the line wrapping is done.

;;;since: 4.8

;;;Gtk.Inscription:xalign
;;;The horizontal alignment of the text inside the allocated size.

;;;since: 4.8

;;;Gtk.Inscription:yalign
;;;The vertical alignment of the text inside the allocated size.

;;;since: 4.8

;;; --- gtk4.inscription.lisp --------------------------------------------------

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
  @see-class{gtk:inscription}")

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
    inscription-text-overflow
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

#+liber-documentation
(setf (documentation 'inscription 'type)
 "@version{2023-10-18}
  @begin{short}
    The @class{gtk:inscription} widget is a widget to show text in a predefined
    area.
  @end{short}
  You likely want to use the @class{gtk:label} widget instead as this widget is
  intended only for a small subset of use cases. The main scenario envisaged is
  inside lists such as the @class{gtk:column-view} widget.

  While a @class{gtk:label} widget sizes itself depending on the text that is
  displayed, the @class{gtk:inscription} widget is given a size and inscribes
  the given text into that space as well as it can.

  Users of this widget should take care to plan behaviour for the common case
  where the text does not fit exactly in the allocated space.

  Since 4.8
  @see-constructor{gtk:inscription-new}
  @see-slot{gtk:inscription-attributes}
  @see-slot{gtk:inscription-markup}
  @see-slot{gtk:inscription-min-chars}
  @see-slot{gtk:inscription-min-lines}
  @see-slot{gtk:inscription-nat-chars}
  @see-slot{gtk:inscription-nat-lines}
  @see-slot{gtk:inscription-text}
  @see-slot{gtk:inscription-text-overflow}
  @see-slot{gtk:inscription-wrap-mode}
  @see-slot{gtk:inscription-xalign}
  @see-slot{gtk:inscription-yalign}
  @see-class{gtk:label}
  @see-class{gtk:column-view}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- inscription-attributes -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "attributes" 'inscription) t)
 "The @code{attributes} property of type @class{pango:attr-list} (Read / Write)
  @br{}
  A list of style attributes to apply to the text of the inscription.")

#+liber-documentation
(setf (liber:alias-for-function 'inscription-attributes)
      "Accessor"
      (documentation 'inscription-attributes 'function)
 "@version{2023-10-18}
  @syntax[]{(gtk:inscription-attributes object) => attrs}
  @syntax[]{(setf (gtk:inscription-attributes object) attrs)}
  @argument[object]{a @class{gtk:inscription} widget}
  @argument[attrs]{a @class{pango:attr-list} instance}
  @begin{short}
    Accessor of the @slot[gtk:inscription]{attributes} slot of the
    @class{gtk:inscription} class.
  @end{short}
  The @fun{gtk:inscription-attributes} function gets the attribute list of the
  inscription. The @setf{gtk:inscription-attributes} function applies attributes
  to the inscription text. These attributes will not be evaluated for sizing the
  inscription.

  Since 4.8
  @see-class{gtk:inscription}
  @see-class{pango:attr-list}")

;;; --- inscription-markup -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "markup" 'inscription) t)
 "The @code{markup} property of type @code{:string} (Write) @br{}
  Utility property that sets both the @slot[gtk:inscription]{text} and
  @slot[gtk:inscription]{attributes} properties, mainly intended for use in
  @class{gtk:builder} UI files to ease translation support and bindings. This
  function uses the @fun{pango:parse-markup} function to parse the markup into
  text and attributes. The markup must be valid. If you cannot ensure that,
  consider using the @fun{pango:parse-markup} function and setting the two
  properties yourself.")

#+liber-documentation
(setf (liber:alias-for-function 'inscription-markup)
      "Accessor"
      (documentation 'inscription-markup 'function)
 "@version{2023-10-18}
  @syntax[]{(setf (gtk:inscription-markup object) attrs)}
  @argument[object]{a @class{gtk:inscription} widget}
  @argument[markup]{a string with the markup to display}
  @begin{short}
    Accessor of the @slot[gtk:inscription]{markup} slot of the
    @class{gtk:inscription} class.
  @end{short}
  The @setf{gtk:inscription-markup} function is an utility function to set the
  text and attributes to be displayed.

  Since 4.8
  @see-class{gtk:inscription}")

;;; --- inscription-min-chars --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "min-chars" 'inscription) t)
 "The @code{min-chars} property of type @code{:uint} (Read / Write) @br{}
  The number of characters that should fit into the inscription at minimum. This
  influences the requested width, not the width actually given to the widget,
  which might turn out to be larger. Note that this is an approximate character
  width, so some characters might be wider and some might be thinner, so do not
  expect the number of characters to exactly match. If you set this property to
  0, the inscription will not request any width at all and its width will be
  determined entirely by its surroundings. @br{}
  Default value: 3")

#+liber-documentation
(setf (liber:alias-for-function 'inscription-min-chars)
      "Accessor"
      (documentation 'inscription-min-chars 'function)
 "@version{2023-10-18}
  @syntax[]{(gtk:inscription-min-chars object) => chars}
  @syntax[]{(setf (gtk:inscription-min-chars object) chars)}
  @argument[object]{a @class{gtk:inscription} widget}
  @argument[chars]{an integer with the minimum number of characters that should
    fit, approximately}
  @begin{short}
    Accessor of the @slot[gtk:inscription]{min-chars} slot of the
    @class{gtk:inscription} class.
  @end{short}

  Since 4.8
  @see-class{gtk:inscription}")

;;; --- inscription-min-lines --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "min-lines" 'inscription) t)
 "The @code{min-lines} property of type @code{:uint} (Read / Write) @br{}
  The number of lines that should fit into the inscription at minimum. This
  influences the requested height, not the height actually given to the widget,
  which might turn out to be larger. Note that this is an approximate line
  height, so if the text uses things like fancy Unicode or attribute that
  influence the height, the text might not fit. If you set this property to 0,
  the inscription will not request any height at all and its height will be
  determined entirely by its surroundings. @br{}
  Default value: 1")

#+liber-documentation
(setf (liber:alias-for-function 'inscription-min-lines)
      "Accessor"
      (documentation 'inscription-min-lines 'function)
 "@version{2023-10-18}
  @syntax[]{(gtk:inscription-min-lines object) => lines}
  @syntax[]{(setf (gtk:inscription-min-lines object) lines)}
  @argument[object]{a @class{gtk:inscription} widget}
  @argument[lines]{an integer with the minimum number of lines that should fit,
    approximately}
  @begin{short}
    Accessor of the @slot[gtk:inscription]{min-lines} slot of the
    @class{gtk:inscription} class.
  @end{short}

  Since 4.8
  @see-class{gtk:inscription}")

;;; --- inscription-nat-chars --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "nat-chars" 'inscription) t)
 "The @code{nat-chars} property of type @code{:uint} (Read / Write) @br{}
  The number of characters that should ideally fit into the inscription. This
  influences the requested width, not the width actually given to the widget.
  The widget might turn out larger as well as smaller. If this property is set
  to a value smaller than the @slot[gtk:inscription]{min-chars} property, that
  value will be used. In particular, for the default value of 0, this will
  always be the case. @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'inscription-nat-chars)
      "Accessor"
      (documentation 'inscription-nat-chars 'function)
 "@version{2023-10-18}
  @syntax[]{(gtk:inscription-nat-chars object) => chars}
  @syntax[]{(setf (gtk:inscription-nat-chars object) chars)}
  @argument[object]{a @class{gtk:inscription} widget}
  @argument[chars]{an integer with the number of characters that should ideally
    fit}
  @begin{short}
    Accessor of the @slot[gtk:inscription]{nat-chars} slot of the
    @class{gtk:inscription} class.
  @end{short}

  Since 4.8
  @see-class{gtk:inscription}")

;;; --- inscription-nat-lines --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "nat-lines" 'inscription) t)
 "The @code{nat-lines} property of type @code{:uint} (Read / Write) @br{}
  The number of lines that should ideally fit into the inscription. This
  influences the requested height, not the height actually given to the widget.
  The widget might turn out larger as well as smaller. If this property is set
  to a value smaller than the @slot[gtk:inscription]{min-lines} property, that
  value will be used. In particular, for the default value of 0, this will
  always be the case. @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'inscription-nat-lines)
      "Accessor"
      (documentation 'inscription-nat-lines 'function)
 "@version{2023-10-18}
  @syntax[]{(gtk:inscription-nat-lines object) => lines}
  @syntax[]{(setf (gtk:inscription-nat-lines object) lines)}
  @argument[object]{a @class{gtk:inscription} widget}
  @argument[lines]{an integer with the number of lines that should ideally fit}
  @begin{short}
    Accessor of the @slot[gtk:inscription]{nat-lines} slot of the
    @class{gtk:inscription} class.
  @end{short}

  Since 4.8
  @see-class{gtk:inscription}")

;;; --- inscription-text -------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "text" 'inscription) t)
 "The @code{text} property of type @code{:string} (Read / Write) @br{}
  The displayed text. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'inscription-text)
      "Accessor"
      (documentation 'inscription-text 'function)
 "@version{2023-10-18}
  @syntax[]{(gtk:inscription-text object) => text}
  @syntax[]{(setf (gtk:inscription-text object) text)}
  @argument[object]{a @class{gtk:inscription} widget}
  @argument[lines]{a string with the text to display}
  @begin{short}
    Accessor of the @slot[gtk:inscription]{text} slot of the
    @class{gtk:inscription} class.
  @end{short}

  Since 4.8
  @see-class{gtk:inscription}")

;;; --- inscription-text-overflow ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "text-overflow" 'inscription) t)
 "The @code{text-overflow} property of type @symbol{gtk:inscription-overflow}
  (Read / Write) @br{}
  The overflow method to use for the text. @br{}
  Default value: @code{:clip}")

#+liber-documentation
(setf (liber:alias-for-function 'inscription-text-overflow)
      "Accessor"
      (documentation 'inscription-text-overflow 'function)
 "@version{2023-10-18}
  @syntax[]{(gtk:inscription-text-overflow object) => setting}
  @syntax[]{(setf (gtk:inscription-text-overflow object) setting)}
  @argument[object]{a @class{gtk:inscription} widget}
  @argument[setting]{a @symbol{gtk:inscription-overflow} value}
  @begin{short}
    Accessor of the @slot[gtk:inscription]{text-overflow} slot of the
    @class{gtk:inscription} class.
  @end{short}

  Since 4.8
  @see-class{gtk:inscription}
  @see-symbol{gtk:inscription-overflow}")

;;; --- inscription-wrap-mode --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "wrap-mode" 'inscription) t)
 "The @code{wrap-mode} property of type @symbol{pango:wrap-mode} (Read / Write)
  @br{}
  Controls how the line wrapping is done. Note that unlike the @class{gtk:label}
  widget, the default here is the @code{:word-char} value. @br{}
  Default value: @code{:word-char}")

#+liber-documentation
(setf (liber:alias-for-function 'inscription-wrap-mode)
      "Accessor"
      (documentation 'inscription-wrap-mode 'function)
 "@version{2023-10-18}
  @syntax[]{(gtk:inscription-wrap-mode object) => mode}
  @syntax[]{(setf (gtk:inscription-wrap-mode object) mode)}
  @argument[object]{a @class{gtk:inscription} widget}
  @argument[mode]{a @symbol{pango:wrap-mode} value}
  @begin{short}
    Accessor of the @slot[gtk:inscription]{wrap-mode} slot of the
    @class{gtk:inscription} class.
  @end{short}

  Since 4.8
  @see-class{gtk:inscription}
  @see-symbol{pango:wrap-mode}")

;;; --- inscription-xalign -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "xalign" 'inscription) t)
 "The @code{xalign} property of type @code{:float} (Read / Write) @br{}
  The horizontal alignment of the text inside the allocated size. Compare this
  to the @slot[gtk:widget]{halign} property, which determines how the
  size allocation of the inscription is positioned in the available space. @br{}
  Default value: 0.0")

#+liber-documentation
(setf (liber:alias-for-function 'inscription-xalign)
      "Accessor"
      (documentation 'inscription-xalign 'function)
 "@version{2023-10-18}
  @syntax[]{(gtk:inscription-xalign object) => xalign}
  @syntax[]{(setf (gtk:inscription-xalign object) xalign)}
  @argument[object]{a @class{gtk:inscription} widget}
  @argument[xalign]{a single float with the xalign value, between 0 and 1}
  @begin{short}
    Accessor of the @slot[gtk:inscription]{xalign} slot of the
    @class{gtk:inscription} class.
  @end{short}

  Since 4.8
  @see-class{gtk:inscription}")

;;; --- inscription-yalign -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "yalign" 'inscription) t)
 "The @code{yalign} property of type @code{:float} (Read / Write) @br{}
  The vertical alignment of the text inside the allocated size. Compare this
  to the @slot[gtk:widget]{valign} property, which determines how the
  size allocation of the inscription is positioned in the available space. @br{}
  Default value: 0.5")

#+liber-documentation
(setf (liber:alias-for-function 'inscription-yalign)
      "Accessor"
      (documentation 'inscription-yalign 'function)
 "@version{2023-10-18}
  @syntax[]{(gtk:inscription-yalign object) => yalign}
  @syntax[]{(setf (gtk:inscription-yalign object) yalign)}
  @argument[object]{a @class{gtk:inscription} widget}
  @argument[yalign]{a single float with the yalign value, between 0 and 1}
  @begin{short}
    Accessor of the @slot[gtk:inscription]{yalign} slot of the
    @class{gtk:inscription} class.
  @end{short}

  Since 4.8
  @see-class{gtk:inscription}")

;;; ----------------------------------------------------------------------------
;;; gtk_inscription_new
;;; ----------------------------------------------------------------------------

(declaim (inline inscription-new))

(defun inscription-new (text)
 #+liber-documentation
 "@version{2023-10-18}
  @argument[text]{a string with the text to display}
  @return{A new @class{gtk:inscription} widget.}
  @begin{short}
    Creates a new @class{gtk:inscription} widget with the given text.
  @end{short}

  Since 4.8
  @see-class{gtk:inscription}"
  (make-instance 'inscription
                 :text (if text text (cffi:null-pointer))))

(export 'inscription-new)

;;; --- End of file gtk4.inscription.lisp --------------------------------------

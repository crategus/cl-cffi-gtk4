;;; ----------------------------------------------------------------------------
;;; gtk.link-button.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2022 Dieter Kaiser
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License for Lisp
;;; as published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version and with a preamble to
;;; the GNU Lesser General Public License that clarifies the terms for use
;;; with Lisp programs and is referred as the LLGPL.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program and the preamble to the Gnu Lesser
;;; General Public License.  If not, see <http://www.gnu.org/licenses/>
;;; and <http://opensource.franz.com/preamble.html>.
;;; ----------------------------------------------------------------------------
;;;
;;; GtkLinkButton
;;;
;;;     Create buttons bound to a URL
;;;
;;; Types and Values
;;;
;;;     GtkLinkButton
;;;
;;; Accessors
;;;
;;;     gtk_link_button_get_uri
;;;     gtk_link_button_set_uri
;;;     gtk_link_button_get_visited
;;;     gtk_link_button_set_visited
;;;
;;; Functions
;;;
;;;     gtk_link_button_new
;;;     gtk_link_button_new_with_label
;;;
;;; Properties
;;;
;;;     uri
;;;     visited
;;;
;;; Signals
;;;
;;;     activate-link
;;;
;;; Actions
;;;
;;;     menu.popup
;;;     clipboard.copy
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkButton
;;;                 ╰── GtkLinkButton
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;;     GtkActionable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkLinkButton
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkLinkButton" link-button
  (:superclass button
    :export t
    :interfaces ("GtkAccessible"
                 "GtkBuildable"
                 "GtkConstraintTarget"
                 "GtkActionable")
    :type-initializer "gtk_link_button_get_type")
  ((uri
    link-button-uri
    "uri" "gchararray" t t)
   (visited
    link-button-visited
    "visited" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'link-button 'type)
 "@version{#2021-12-23}
  @begin{short}
    A @sym{gtk:link-button} widget is a @class{gtk:button} widget with a
    hyperlink, similar to the one used by web browsers, which triggers an
    action when clicked. It is useful to show quick links to resources.
  @end{short}
  A link button is created by calling either the @fun{gtk:link-button-new} or
  @fun{gtk:link-button-new-with-label} functions. If using the former, the URI
  you pass to the constructor is used as a label for the widget.

  @image[link-button]{Figure: GtkLinkButton}

  The URI bound to a @sym{gtk:link-button} widget can be set specifically or
  retrieved using the @fun{gtk:link-button-uri} function.

  By default, the @sym{gtk:link-button} widget calls the @fun{gtk:show-uri}
  function when the button is clicked. This behaviour can be overridden by
  connecting to the \"activate-link\" signal and returning @em{true} from the
  signal handler.
  @begin[CSS nodes]{dictionary}
    The @sym{gtk:link-button} implemenation has a single CSS node with name
    @code{button}. To differentiate it from a plain @class{gtk:button} widget,
    it gets the @code{.link} style class.
  @end{dictionary}
  @begin[Accessibility]{dictionary}
    The @class{gtk:link-button} implementation uses the @code{:link} role of
    the @symbol{gtk:accessible-role} enumeration.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"activate-link\" signal}
      @begin{pre}
lambda (button)    :run-last
      @end{pre}
      The signal is emitted each time the link button has been clicked. The
      default handler will call the @fun{gtk:show-uri} function with the URI
      stored inside the @code{uri} property. To override the default behavior,
      you can connect to the \"activate-link\" signal and stop the propagation
      of the signal by returning @em{true} from your handler.
      @begin[code]{table}
        @entry[button]{The @sym{gtk:link-button} widget that emitted the
          signal.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk:link-button-uri}
  @see-slot{gtk:link-button-visited}
  @see-class{gtk:button}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- link-button-uri ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "uri" 'link-button) t)
 "The @code{uri} property of type @code{:string} (Read / Write) @br{}
  The URI bound to this button. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'link-button-uri)
      "Accessor"
      (documentation 'link-button-uri 'function)
 "@version{#2021-12-23}
  @syntax[]{(gtk:link-button-uri object) => uri}
  @syntax[]{(setf (gtk:link-button-uri object) uri)}
  @argument[object]{a @class{gtk:link-button} widget}
  @argument[uri]{a string with a valid URI}
  @begin{short}
    Accessor of the @slot[gtk:link-button]{uri} slot of the
    @class{gtk:link-button} class.
  @end{short}

  The @sym{gtk:link-button-uri} function retrieves the URI. The
  @sym{gtk:link-button-uri} function sets @arg{uri} as the URI where the link
  button points. As a side-effect this unsets the visited state of the button.
  @see-class{gtk:link-button}")

;;; --- link-button-visited ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "visited" 'link-button) t)
 "The @code{visited} property of type @code{:boolean} (Read / Write) @br{}
  The visited state of this button. A visited link is drawn in a different
  color. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'link-button-visited)
      "Accessor"
      (documentation 'link-button-visited 'function)
 "@version{#2021-12-23}
  @syntax[]{(gtk:link-button-visited object) => visited}
  @syntax[]{(setf (gtk:link-button-visited object) visited)}
  @argument[object]{a @class{gtk:link-button} widget}
  @argument[visited]{a boolean with the \"visited\" state}
  @begin{short}
    Accessor of the @slot[gtk:link-button]{visited} slot of the
    @class{gtk:link-button} class.
  @end{short}

  The @sym{gtk:link-button-visited} function retrieves the \"visited\" state of
  the URI where the link button points. The @sym{(setf gtk:link-button-visited)}
  function sets the \"visited\" state of the URI.

  The button becomes visited when it is clicked. If the URI is changed on the
  button, the visited state is unset again.
  @see-class{gtk:link-button}")

;;; ----------------------------------------------------------------------------
;;; gtk_link_button_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline link-button-new))

(defun link-button-new (uri)
 #+liber-documentation
 "@version{#2021-12-23}
  @argument[uri]{a string with a valid URI}
  @return{A new @class{gtk:link-button} widget.}
  @short{Creates a new link button with the URI as its text.}
  @see-class{gtk:link-button}
  @see-function{gtk:link-button-new-with-label}"
  (make-instance 'link-button
                 :uri uri
                 :label uri))

(export 'link-button-new)

;;; ----------------------------------------------------------------------------
;;; gtk_link_button_new_with_label ()
;;; ----------------------------------------------------------------------------

(declaim (inline link-button-new-with-label))

(defun link-button-new-with-label (uri label)
 #+liber-documentation
 "@version{#2021-12-23}
  @argument[uri]{a string with a valid URI}
  @argument[label]{a string with the text of the button}
  @return{A new @class{gtk:link-button} widget.}
  @short{Creates a new link button containing a label.}
  @see-class{gtk:link-button}
  @see-function{gtk:link-button-new}"
  (make-instance 'link-button
                 :uri uri
                 :label label))

(export 'link-button-new-with-label)

;;; --- End of file gtk.link-button.lisp ---------------------------------------

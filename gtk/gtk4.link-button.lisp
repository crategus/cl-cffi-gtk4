;;; ----------------------------------------------------------------------------
;;; gtk4.link-button.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; Version 4.16 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation of the Lisp binding is
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
;;; GtkLinkButton
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkLinkButton" link-button
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
 "@version{2025-2-22}
  @begin{short}
    The @class{gtk:link-button} widget is a @class{gtk:button} widget with a
    hyperlink, similar to the one used by web browsers, which triggers an
    action when clicked. It is useful to show quick links to resources.
  @end{short}
  A link button is created by calling either the @fun{gtk:link-button-new} or
  @fun{gtk:link-button-new-with-label} function. If using the former, the URI
  you pass to the constructor is used as a label for the widget.

  @image[gtk-link-button]{Figure: GtkLinkButton}

  The URI bound to a @class{gtk:link-button} widget can be set specifically or
  retrieved using the @fun{gtk:link-button-uri} function.

  By default, the @class{gtk:link-button} widget calls the
  @fun{gtk:file-launcher-launch} function when the button is clicked. This
  behaviour can be overridden by connecting to the @code{\"activate-link\"}
  signal and returning @em{true} from the signal handler.
  @begin[CSS nodes]{dictionary}
    The @class{gtk:link-button} implementation has a single CSS node with name
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
      @begin[code]{table}
        @entry[button]{The @class{gtk:link-button} widget that emitted the
          signal.}
      @end{table}
      The signal is emitted each time the link button has been clicked. The
      default handler will call the @fun{gtk:file-launcher-launch} function
      with the URI stored inside the @slot[gtk:link-button]{uri} property.
      To override the default behavior, you can connect to the
      @code{\"activate-link\"} signal and stop the propagation of the signal by
      returning @em{true} from your handler.
  @end{dictionary}
  @see-constructor{gtk:link-button-new}
  @see-constructor{gtk:link-button-new-with-label}
  @see-slot{gtk:link-button-uri}
  @see-slot{gtk:link-button-visited}
  @see-class{gtk:button}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:link-button-uri ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "uri" 'link-button) t)
 "The @code{uri} property of type @code{:string} (Read / Write) @br{}
  The URI bound to this button. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'link-button-uri)
      "Accessor"
      (documentation 'link-button-uri 'function)
 "@version{2025-2-22}
  @syntax{(gtk:link-button-uri object) => uri}
  @syntax{(setf (gtk:link-button-uri object) uri)}
  @argument[object]{a @class{gtk:link-button} widget}
  @argument[uri]{a string for a valid URI}
  @begin{short}
    Accessor of the @slot[gtk:link-button]{uri} slot of the
    @class{gtk:link-button} class.
  @end{short}
  The @fun{gtk:link-button-uri} function retrieves the URI. The
  @setf{gtk:link-button-uri} function sets @slot[gtk:link-button]{uri} as the
  URI where the link button points. As a side-effect this unsets the visited
  state of the button.
  @see-class{gtk:link-button}")

;;; --- gtk:link-button-visited ------------------------------------------------

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
 "@version{2025-2-22}
  @syntax{(gtk:link-button-visited object) => visited}
  @syntax{(setf (gtk:link-button-visited object) visited)}
  @argument[object]{a @class{gtk:link-button} widget}
  @argument[visited]{a boolean for the \"visited\" state}
  @begin{short}
    Accessor of the @slot[gtk:link-button]{visited} slot of the
    @class{gtk:link-button} class.
  @end{short}
  The @fun{gtk:link-button-visited} function retrieves the \"visited\" state of
  the URI where the link button points. The @setf{gtk:link-button-visited}
  function sets the \"visited\" state of the URI.

  The button becomes visited when it is clicked. If the URI is changed on the
  button, the visited state is unset again.
  @see-class{gtk:link-button}")

;;; ----------------------------------------------------------------------------
;;; gtk_link_button_new
;;; ----------------------------------------------------------------------------

(declaim (inline link-button-new))

(defun link-button-new (uri)
 #+liber-documentation
 "@version{2025-2-22}
  @argument[uri]{a string for a valid URI}
  @return{The new @class{gtk:link-button} widget.}
  @short{Creates a new link button with the URI as its text.}
  @see-class{gtk:link-button}
  @see-function{gtk:link-button-new-with-label}"
  (make-instance 'link-button
                 :uri uri
                 :label uri))

(export 'link-button-new)

;;; ----------------------------------------------------------------------------
;;; gtk_link_button_new_with_label
;;; ----------------------------------------------------------------------------

(declaim (inline link-button-new-with-label))

(defun link-button-new-with-label (uri label)
 #+liber-documentation
 "@version{2025-2-22}
  @argument[uri]{a string for a valid URI}
  @argument[label]{a string for the text of the button}
  @return{The new @class{gtk:link-button} widget.}
  @short{Creates a new link button containing a label.}
  @see-class{gtk:link-button}
  @see-function{gtk:link-button-new}"
  (make-instance 'link-button
                 :uri uri
                 :label label))

(export 'link-button-new-with-label)

;;; --- End of file gtk4.link-button.lisp --------------------------------------

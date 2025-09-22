;;; ----------------------------------------------------------------------------
;;; gtk4.style-provider.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.18 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2013 - 2025 Dieter Kaiser
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
;;; GtkStyleProvider
;;;
;;;     Interface to provide style information to GtkStyleContext
;;;
;;; Types and Values
;;;
;;;     GtkStyleProvider
;;;
;;;     GTK_STYLE_PROVIDER_PRIORITY_FALLBACK
;;;     GTK_STYLE_PROVIDER_PRIORITY_THEME
;;;     GTK_STYLE_PROVIDER_PRIORITY_SETTINGS
;;;     GTK_STYLE_PROVIDER_PRIORITY_APPLICATION
;;;     GTK_STYLE_PROVIDER_PRIORITY_USER
;;;
;;; Functions
;;;
;;; Signals
;;;
;;;     gtk-private-changed
;;;
;;; Hierarchy
;;;
;;;     GInterface
;;;     ╰── GtkStyleProvider
;;;
;;; Implementations
;;;
;;;     GtkCssProvider
;;;     GtkSettings
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_PROVIDER_PRIORITY_FALLBACK
;;; ----------------------------------------------------------------------------

(defconstant +priority-fallback+ 1)

#+liber-documentation
(setf (liber:alias-for-variable '+priority-fallback+)
      "Constant"
      (documentation '+priority-fallback+ 'variable)
 "@version{2025-09-22}
  @variable-value{1}
  @begin{short}
    The priority used for the default style information that is used in the
    absence of themes.
  @end{short}
  Note that this is not very useful for providing default styling for custom
  style classes, as themes are likely to override any styling provided at this
  priority using catch-all @code{* {...@}} rules.
  @see-class{gtk:style-provider}")

(export '+priority-fallback+)

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_PROVIDER_PRIORITY_THEME
;;; ----------------------------------------------------------------------------

(defconstant +priority-theme+ 200)

#+liber-documentation
(setf (liber:alias-for-variable '+priority-theme+)
      "Constant"
      (documentation '+priority-theme+ 'variable)
 "@version{2024-04-01}
  @variable-value{200}
  @begin{short}
    The priority used for style information provided by themes.
  @end{short}
  @see-class{gtk:style-provider}")

(export '+priority-theme+)

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_PROVIDER_PRIORITY_SETTINGS
;;; ----------------------------------------------------------------------------

(defconstant +priority-settings+ 400)

#+liber-documentation
(setf (liber:alias-for-variable '+priority-settings+)
      "Constant"
      (documentation '+priority-settings+ 'variable)
 "@version{2025-09-22}
  @variable-value{400}
  @begin{short}
    The priority used for style information provided with the
    @class{gtk:settings} object.
  @end{short}
  This priority is higher than the @var{gtk:+priority-theme+} value, enabling
  settings to override themes.
  @see-class{gtk:style-provider}
  @see-class{gtk:settings}
  @see-variable{gtk:+priority-theme+}")

(export '+priority-settings+)

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_PROVIDER_PRIORITY_APPLICATION
;;; ----------------------------------------------------------------------------

(defconstant +priority-application+ 600)

#+liber-documentation
(setf (liber:alias-for-variable '+priority-application+)
      "Constant"
      (documentation '+priority-application+ 'variable)
 "@version{2024-04-01}
  @variable-value{600}
  @begin{short}
    A priority that can be used when adding a @class{gtk:style-provider} object
    for application specific style information.
  @end{short}
  @see-class{gtk:style-provider}")

(export '+priority-application+)

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_PROVIDER_PRIORITY_USER
;;; ----------------------------------------------------------------------------

(defconstant +priority-user+ 800)

#+liber-documentation
(setf (liber:alias-for-variable '+priority-user+)
      "Constant"
      (documentation '+priority-user+ 'variable)
 "@version{2024-04-01}
  @variable-value{800}
  @begin{short}
    The priority used for the style information from the
    @file{$XDG_CONFIG_HOME/gtk-4.0/gtk.css} file.
  @end{short}
  You should not use priorities higher than this, to give the user the last
  word.
  @see-class{gtk:style-provider}")

(export '+priority-user+)

;;; ----------------------------------------------------------------------------
;;; GtkStyleProvider
;;; ----------------------------------------------------------------------------

(gobject:define-ginterface "GtkStyleProvider" style-provider
  (:export t
   :type-initializer "gtk_style_provider_get_type")
  nil)

#+liber-documentation
(setf (liber:alias-for-class 'style-provider)
      "Interface"
      (documentation 'style-provider 'type)
 "@version{2025-09-22}
  @begin{short}
    The @class{gtk:style-provider} interface is an interface used to provide
    style information to a @class{gtk:style-context} object.
  @end{short}
  See the @fun{gtk:style-context-add-provider-for-display} function for adding
  style providers to a style context and the @fun{gtk:widget-add-provider}
  function to add a style provider to the display of a widget.

  GTK uses the @class{gtk:style-provider} implementation for CSS in the
  @class{gtk:css-provider} implementation.
  @begin[Signal Details]{dictionary}
    @begin[style-provider::gtk-private-changed]{signal}
      @begin{pre}
lambda (provider)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[provider]{The @class{gtk:style-provider} object that received
          the signal.}
      @end{simple-table}
    @end{signal}
  @end{dictionary}
  @see-class{gtk:style-context}
  @see-class{gtk:css-provider}
  @see-function{gtk:style-context-add-provider-for-display}
  @see-function{gtk:widget-add-provider}")

;;; --- End of file gtk4.style-provider.lisp -----------------------------------

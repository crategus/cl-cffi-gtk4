;;; ----------------------------------------------------------------------------
;;; gtk.style-provider.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.6 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2013 - 2022 Dieter Kaiser
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
;;; GtkStyleProvider
;;; ----------------------------------------------------------------------------

(define-g-interface "GtkStyleProvider" style-provider
  (:export t
   :type-initializer "gtk_style_provider_get_type")
  nil)

#+liber-documentation
(setf (liber:alias-for-class 'style-provider)
      "Interface"
      (documentation 'style-provider 'type)
 "@version{#2022-1-9}
  @begin{short}
    The @sym{gtk:style-provider} interface is an interface used to provide
    style information to a @class{gtk:style-context} object.
  @end{short}
  See the @fun{gtk:style-context-add-provider} and
  @fun{gtk:style-context-add-provider-for-display} functions for adding style
  providers.
  @begin[Signal Details]{dictionary}
    @subheading{The \"gtk:private-changed\" signal}
      @begin{pre}
lambda (provider)    :run-last
      @end{pre}
      No documentation available.
      @begin[code]{table}
        @entry[provider]{The @sym{gtk:style-provider} object which received the
          signal.}
      @end{table}
  @end{dictionary}
  @see-class{gtk:style-context}
  @see-class{gtk:css-provider}
  @see-function{gtk:style-context-add-provider}
  @see-function{gtk:style-context-add-provider-for-display}")

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_PROVIDER_PRIORITY_FALLBACK
;;; ----------------------------------------------------------------------------

(defconstant +gtk-priority-fallback+ 1)

#+liber-documentation
(setf (liber:alias-for-variable '+gtk-priority-fallback+)
      "Constant"
      (documentation '+gtk-priority-fallback+ 'variable)
 "@version{#2022-1-9}
  @variable-value{1}
  @begin{short}
    The priority used for default style information that is used in the absence
    of themes.
  @end{short}
  Note that this is not very useful for providing default styling for custom
  style classes - themes are likely to override styling provided at this
  priority with catch-all * {...@} rules.
  @see-class{gtk:style-provider}")

(export '+gtk-priority-fallback+)

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_PROVIDER_PRIORITY_THEME
;;; ----------------------------------------------------------------------------

(defconstant +gtk-priority-theme+ 200)

#+liber-documentation
(setf (liber:alias-for-variable '+gtk-priority-theme+)
      "Constant"
      (documentation '+gtk-priority-theme+ 'variable)
 "@version{#2022-1-9}
  @variable-value{200}
  @begin{short}
    The priority used for style information provided by themes.
  @end{short}
  @see-class{gtk:style-provider}")

(export '+gtk-priority-theme+)

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_PROVIDER_PRIORITY_SETTINGS
;;; ----------------------------------------------------------------------------

(defconstant +gtk-priority-settings+ 400)

#+liber-documentation
(setf (liber:alias-for-variable '+gtk-priority-settings+)
      "Constant"
      (documentation '+gtk-priority-settings+ 'variable)
 "@version{#2011-1-9}
  @variable-value{400}
  @begin{short}
    The priority used for style information provided via the
    @class{gtk:settings} object.
  @end{short}
  This priority is higher than the @var{+gtk-priority-theme+} value to let
  settings override themes.
  @see-class{gtk:style-provider}
  @see-class{gtk:settings}
  @see-variable{+gtk-priority-theme+}")

(export '+gtk-priority-settings+)

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_PROVIDER_PRIORITY_APPLICATION
;;; ----------------------------------------------------------------------------

(defconstant +gtk-priority-application+ 600)

#+liber-documentation
(setf (liber:alias-for-variable '+gtk-priority-application+)
      "Constant"
      (documentation '+gtk-priority-application+ 'variable)
 "@version{#2022-1-9}
  @variable-value{600}
  @begin{short}
    A priority that can be used when adding a @class{gtk:style-provider} object
    for application specific style information.
  @end{short}
  @see-class{gtk:style-provider}")

(export '+gtk-priority-application+)

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_PROVIDER_PRIORITY_USER
;;; ----------------------------------------------------------------------------

(defconstant +gtk-priority-user+ 800)

#+liber-documentation
(setf (liber:alias-for-variable '+gtk-priority-user+)
      "Constant"
      (documentation '+gtk-priority-user+ 'variable)
 "@version{2022-11-25}
  @variable-value{800}
  @begin{short}
    The priority used for the style information from the
    @file{$XDG_CONFIG_HOME/gtk-4.0/gtk.css} file.
  @end{short}
  You should not use priorities higher than this, to give the user the last
  word.
  @see-class{gtk:style-provider}")

(export '+gtk-priority-user+)

;;; --- End of file gtk-style-provider.lisp ------------------------------------

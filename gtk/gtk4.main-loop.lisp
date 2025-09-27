;;; ----------------------------------------------------------------------------
;;; gtk4.main-loop.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.18 and modified to document the Lisp binding to the GTK library,
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
;;; Library initialization and main loop
;;;
;;; Functions
;;;
;;;     gtk_init
;;;     gtk_init_check
;;;     gtk_init_is_initialized
;;;
;;;     gtk_disable_setlocale
;;;     gtk_get_default_language
;;;     gtk_get_locale_direction
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; gtk_init
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_init" init) :void
 #+liber-documentation
 "@version{2025-09-23}
  @begin{short}
    Call this function before using any other GTK functions in your GUI
    applications.
  @end{short}
  It will initialize everything needed to operate the toolkit.

  If you are using the @class{gtk:application} class, you do not have to call
  the @fun{gtk:init} or @fun{gtk:init-check} functions. The
  @sig[g:application]{startup} handler does it for you.

  This function will terminate your program if it was unable to initialize the
  windowing system for some reason. If you want your program to fall back to a
  textual interface you want to call the @fun{gtk:init-check} function instead.

  GTK calls @code{signal(SIGPIPE, SIG_IGN)} during initialization, to ignore
  @code{SIGPIPE} signals, since these are almost never wanted in graphical
  applications. If you do need to handle @code{SIGPIPE} for some reason, reset
  the handler after the @fun{gtk:init} call, but notice that other libraries,
  for example, @code{libdbus} or @code{gvfs}, might do similar things.
  @see-class{gtk:application}
  @see-function{gtk:init-check}")

(export 'init)

;;; ----------------------------------------------------------------------------
;;; gtk_init_check
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_init_check" init-check) :boolean
 #+liber-documentation
 "@version{2025-07-26}
  @begin{return}
    @em{True} if the windowing system has been successfully initialized,
    @em{false} otherwise.
  @end{return}
  @begin{short}
    This function does the same work as the @fun{gtk:init} function with only a
    single change.
  @end{short}
  It does not terminate the program if the windowing system cannot be
  initialized. Instead it returns @em{false} on failure.

  This way the application can fall back to some other means of communication
  with the user - for example a curses or command line interface.
  @see-function{gtk:init}")

(export 'init-check)

;;; ----------------------------------------------------------------------------
;;; gtk_is_initialized
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_is_initialized" is-initialized) :boolean
 #+liber-documentation
 "@version{2025-07-27}
  @return{The boolean for the initialization status.}
  @begin{short}
    Use this function to check if GTK has been initialized with the
    @fun{gtk:init} or @fun{gtk:init-check} functions.
  @end{short}
  @see-function{gtk:init}
  @see-function{gtk:init-check}")

(export 'is-initialized)

;;; ----------------------------------------------------------------------------
;;; gtk_disable_setlocale
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_disable_setlocale" disable-setlocale) :void
 #+liber-documentation
 "@version{#2024-11-05}
  @begin{short}
    Prevents the @fun{gtk:init} and @fun{gtk:init-check} functions from
    automatically calling @code{setlocale(LC_ALL, \"\")}.
  @end{short}
  You would want to use this function if you wanted to set the locale for your
  program to something other than the locale of the user, or if you wanted to
  set different values for different locale categories.

  Most programs should not need to call this function.
  @see-function{gtk:init}
  @see-function{gtk:init-check}")

(export 'disable-setlocale)

;;; ----------------------------------------------------------------------------
;;; gtk_get_default_language
;;; ----------------------------------------------------------------------------

(defun default-language ()
 #+liber-documentation
 "@version{2024-11-05}
  @return{The default language as a @class{pango:language} instance.}
  @begin{short}
    Returns the Pango language instance for the default language currently in
    effect.
  @end{short}
  Note that this can change over the life of an application. The default
  language is derived from the current locale. It determines, for example,
  whether GTK uses the right-to-left or left-to-right text direction.

  This function is equivalent to the @fun{pango:language-default} function. See
  that function for details.
  @begin[Examples]{dictionary}
    @begin{pre}
(setq lang (gtk:default-language))
=> #<PANGO-LANGUAGE {C7B3C51@}>
(pango:language-to-string lang)
=> \"de-de\"
    @end{pre}
  @end{dictionary}
  @see-class{pango:language}
  @see-function{pango:language-default}"
  (pango:language-default))

(export 'default-language)

;;; ----------------------------------------------------------------------------
;;; gtk_get_locale_direction
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_get_locale_direction" locale-direction) text-direction
 #+liber-documentation
 "@version{2025-09-23}
  @return{The @sym{gtk:text-direction} value for the current locale.}
  @begin{short}
    Gets the direction of the current locale.
  @end{short}
  This is the expected reading direction for text and UI. This function depends
  on the current locale being set with the @code{setlocale()} function and will
  default to setting the @val[gtk:text-direction]{:ltr} direction otherwise. The
  @val[gtk:text-direction]{:none} direction will never be returned.

  GTK sets the default text direction according to the locale during the
  @fun{gtk:init} function, and you should normally use the
  @fun{gtk:widget-direction} or @fun{gtk:widget-default-direction} functions to
  obtain the current direction.

  This function is only needed rare cases when the locale is changed after GTK
  has already been initialized.
  @begin[Examples]{dictionary}
    You can use the @fun{gtk:locale-direction} function to update the default
    text direction as follows:
    @begin{pre}
(setf (gtk:widget-default-direction) (gtk:locale-direction))
=> :LTR
    @end{pre}
  @end{dictionary}
  @see-symbol{gtk:text-direction}
  @see-function{gtk:widget-direction}
  @see-function{gtk:widget-default-direction}")

(export 'locale-direction)

;;; --- End of file gtk4.main.loop.lisp ----------------------------------------

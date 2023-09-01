;;; ----------------------------------------------------------------------------
;;; gtk4.main-loop.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.12 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2023 Dieter Kaiser
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
;;;     Initialization, exit, mainloop and miscellaneous routines
;;;
;;; Types and Values
;;;
;;;     GtkDebugFlags                                      not implemented
;;;
;;;     GTK_PRIORITY_RESIZE                                not implemented
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
;;;
;;;     gtk_get_debug_flags                                not implemented
;;;     gtk_set_debug_flags                                not implemented
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GTK_PRIORITY_RESIZE
;;;
;;; #define GTK_PRIORITY_RESIZE (G_PRIORITY_HIGH_IDLE + 10)
;;;
;;; Use this priority for functionality related to size allocation.
;;;
;;; It is used internally by GTK to compute the sizes of widgets. This priority
;;; is higher than GDK_PRIORITY_REDRAW to avoid resizing a widget which was
;;; just redrawn.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkDebugFlags
;;;
;;; Flags to use with gtk_set_debug_flags().
;;;
;;; Settings these flags causes GTK to print out different types of debugging
;;; information. Some of these flags are only available when GTK has been
;;; configured with -Ddebug=true.
;;;
;;; GTK_DEBUG_TEXT
;;;     Information about GtkTextView.
;;;
;;; GTK_DEBUG_TREE
;;;     Information about GtkTreeView.
;;;
;;; GTK_DEBUG_KEYBINDINGS
;;;    Information about keyboard shortcuts.
;;;
;;; GTK_DEBUG_MODULES
;;;     Information about modules and extensions.
;;;
;;; GTK_DEBUG_GEOMETRY
;;;     Information about size allocation.
;;;
;;; GTK_DEBUG_ICONTHEME
;;;     Information about icon themes.
;;;
;;; GTK_DEBUG_PRINTING
;;;     Information about printing.
;;;
;;; GTK_DEBUG_BUILDER
;;;     Trace GtkBuilder operation.
;;;
;;; GTK_DEBUG_SIZE_REQUEST
;;;     Information about size requests.
;;;
;;; GTK_DEBUG_NO_CSS_CACHE
;;;     Disable the style property cache.
;;;
;;; GTK_DEBUG_INTERACTIVE
;;;     Open the GTK inspector.
;;;
;;; GTK_DEBUG_TOUCHSCREEN
;;;     Pretend the pointer is a touchscreen.
;;;
;;; GTK_DEBUG_ACTIONS
;;;     Information about actions and menu models.
;;;
;;; GTK_DEBUG_LAYOUT
;;;     Information from layout managers.
;;;
;;; GTK_DEBUG_SNAPSHOT
;;;     Include debug render nodes in the generated snapshots.
;;;
;;; GTK_DEBUG_CONSTRAINTS
;;;     Information from the constraints solver.
;;;
;;; GTK_DEBUG_BUILDER_OBJECTS
;;;     Log unused GtkBuilder objects.
;;;
;;; GTK_DEBUG_A11Y
;;;     Information about accessibility state changes.
;;;
;;; GTK_DEBUG_ICONFALLBACK
;;;     Information about icon fallback. Since: 4.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_init
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_init" init) :void
 #+liber-documentation
 "@version{#2022-1-4}
  @begin{short}
    Call this function before using any other GTK functions in your GUI
    applications.
  @end{short}
  It will initialize everything needed to operate the toolkit.

  If you are using the @class{gtk:application} class, you do not have to call
  the @sym{gtk:init} or @fun{gtk:init-check} functions. The
  \"GApplication::startup\" handler does it for you.

  This function will terminate your program if it was unable to initialize the
  windowing system for some reason. If you want your program to fall back to a
  textual interface you want to call the @fun{gtk:init-check} function instead.

  GTK calls @code{signal(SIGPIPE, SIG_IGN)} during initialization, to ignore
  @code{SIGPIPE} signals, since these are almost never wanted in graphical
  applications. If you do need to handle @code{SIGPIPE} for some reason, reset
  the handler after the @sym{gtk:init} call, but notice that other libraries,
  e.g. libdbus or gvfs, might do similar things.
  @see-class{gtk:application}
  @see-function{gtk:init-check}")

(export 'init)

;;; ----------------------------------------------------------------------------
;;; gtk_init_check
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_init_check" init-check) :boolean
 #+liber-documentation
 "@version{#2022-1-4}
  @begin{return}
    @em{True} if the windowing system has been successfully initialized,
    @em{false} otherwise.
  @end{return}
  @begin{short}
    This function does the same work as the @fun{gtk:init} function with only a
    single change:
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
 "@version{2023-8-30}
  @return{A boolean with the initialization status.}
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
 "@version{#2023-8-30}
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
;;; gtk_get_default_language -> default-language
;;; ----------------------------------------------------------------------------

(defun default-language ()
 #+liber-documentation
 "@version{2023-8-30}
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
  @begin[Example]{dictionary}
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
;;; gtk_get_locale_direction -> locale-direction
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_get_locale_direction" locale-direction) text-direction
 #+liber-documentation
 "@version{2023-8-30}
  @return{The @symbol{gtk:text-direction} value of the current locale.}
  @begin{short}
    Gets the direction of the current locale.
  @end{short}
  This is the expected reading direction for text and UI. This function depends
  on the current locale being set with the @code{setlocale()} function and will
  default to setting the @code{:ltr} direction otherwise. The @code{:none}
  direction will never be returned.

  GTK sets the default text direction according to the locale during the
  @fun{gtk:init} function, and you should normally use the
  @fun{gtk:widget-direction} or @fun{gtk:widget-default-direction} functions to
  obtain the current direction.

  This function is only needed rare cases when the locale is changed after GTK
  has already been initialized.
  @begin[Example]{dictionary}
    You can use the @sym{gtk:locale-direction} function to update the default
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

;;; ----------------------------------------------------------------------------
;;; gtk_get_debug_flags ()
;;;
;;; GtkDebugFlags
;;; gtk_get_debug_flags (void);
;;;
;;; Returns the GTK debug flags that are currently active.
;;;
;;; This function is intended for GTK modules that want to adjust their debug
;;; output based on GTK debug flags.
;;;
;;; Returns :
;;;     the GTK debug flags.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_set_debug_flags ()
;;;
;;; void
;;; gtk_set_debug_flags (GtkDebugFlags flags);
;;;
;;; Sets the GTK debug flags.
;;;
;;; flags :
;;;     the debug flags to set
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk4.main.loop.lisp ----------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk4.display-manager.lisp
;;;
;;; The documentation of this file is taken from the GDK 4 Reference Manual
;;; Version 4.10 and modified to document the Lisp binding to the GDK library.
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
;;; GdkDisplayManager
;;;
;;;     Maintains a list of all open GdkDisplays
;;;
;;; Types and Values
;;;
;;;     GdkDisplayManager
;;;
;;; Accessors
;;;
;;;     gdk_display_manager_get_default_display
;;;     gdk_display_manager_set_default_display
;;;
;;; Functions
;;;
;;;     gdk_display_manager_get
;;;     gdk_display_manager_list_displays
;;;     gdk_display_manager_open_display
;;;     gdk_set_allowed_backends
;;;
;;; Properties
;;;
;;;     default-display
;;;
;;; Signals
;;;
;;;     display-opened
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GdkDisplayManager
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkDisplayManager
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GdkDisplayManager" display-manager
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gdk_display_manager_get_type")
  ((default-display
    display-manager-default-display
    "default-display" "GdkDisplay" t t)))

#+liber-documentation
(setf (documentation 'display-manager 'type)
 "@version{2023-4-14}
  @begin{short}
    The purpose of the @sym{gdk:display-manager} singleton object is to offer
    notification when displays appear or disappear or the default display
    changes.
  @end{short}

  You can use the @fun{gdk:display-manager-get} function to obtain the
  @sym{gdk:display-manager} singleton, but that should be rarely necessary.
  Typically, initializing GTK opens a display that you can work with without
  ever accessing the @sym{gdk:display-manager} object.

  The GDK library can be built with support for multiple backends. The
  @sym{gdk:display-manager} object determines which backend is used at runtime.

  When writing backend specific code that is supposed to work with multiple
  GDK backends, you have to consider both compile time and runtime. At compile
  time, use the @code{GDK_WINDOWING_X11}, @code{GDK_WINDOWING_WIN32} macros,
  etc. to find out which backends are present in the GDK library you are
  building your application against. At runtime, use type check macros like
  @code{GDK_IS_X11_DISPLAY()} to find out which backend is in use:
  @begin[Example]{dictionary}
    Backend specific code
    @begin{pre}
#ifdef GDK_WINDOWING_X11
  if (GDK_IS_X11_DISPLAY (display))
    {
      /* make X11-specific calls here */
    @}
  else
#endif
#ifdef GDK_WINDOWING_QUARTZ
  if (GDK_IS_QUARTZ_DISPLAY (display))
    {
      /* make Quartz-specific calls here */
    @}
  else
#endif
  g_error (\"Unsupported GDK backend\");
    @end{pre}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"display-opened\" signal}
      @begin{pre}
lambda (manager display)    :run-last
      @end{pre}
      The signal is emitted when a display is opened.
      @begin[code]{table}
        @entry[manager]{The @sym{gdk:display-manager} object on which the
          signal is emitted.}
        @entry[display]{The opened @class{gdk:display} object.}
      @end{table}
  @end{dictionary}
  @see-slot{gdk:display-manager-default-display}
  @see-class{gdk:display}
  @see-function{gdk:display-manager-get}")

;;; ----------------------------------------------------------------------------
;;; Property and Accesor Details
;;; ----------------------------------------------------------------------------

;;; --- display-manager-default-display ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "default-display"
                                               'display-manager) t)
 "The @code{default-display} property of type @class{gdk:display} (Read / Write)
  @br{}
  The default display for GDK.")

#+liber-documentation
(setf (liber:alias-for-function 'display-manager-default-display)
      "Accessor"
      (documentation 'display-manager-default-display 'function)
 "@version{2023-4-14}
  @syntax[]{(gdk:display-manager-default-display object) => display}
  @syntax[]{(setf (gdk:display-manager-default-display object) display)}
  @argument[object]{a @class{gdk:display-manager} object}
  @argument[display]{a default @class{gdk:display} object}
  @begin{short}
    Accessor of the @slot[gdk:display-manager]{default-display} slot of the
    @class{gdk:display-manager} class.
  @end{short}
  The @sym{gdk:display-manager-default-display} function gets the default
  @class{gdk:display} object, or @code{nil} if there is no default display. The
  @sym{(setf gdk:display-manager-default-display)} function sets @arg{display}
  as the default display.
  @begin[Example]{dictionary}
    @begin{pre}
(gdk:display-manager-default-display (gdk:display-manager-get))
=> #<GDK:DISPLAY {1001F9A233@}>
    @end{pre}
  @end{dictionary}
  @see-class{gdk:display}
  @see-class{gdk:display-manager}")

;;; ----------------------------------------------------------------------------
;;; gdk_display_manager_get ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_manager_get" display-manager-get)
    (g:object display-manager)
 #+liber-documentation
 "@version{2023-4-14}
  @return{The global @class{gdk:display-manager} singleton object.}
  @begin{short}
    Gets the @class{gdk:display-manager} singleton object.
  @end{short}
  When called for the first time, this function consults the @code{GDK_BACKEND}
  environment variable to find out which of the supported GDK backends to use
  in case GDK has been compiled with multiple backends.
  @begin[Example]{dictionary}
    @begin{pre}
(gdk:display-manager-get)
=> #<GDK:DISPLAY-MANAGER {1001CFF103@}>
    @end{pre}
  @end{dictionary}
  @see-class{gdk:display-manager}")

(export 'display-manager-get)

;;; ----------------------------------------------------------------------------
;;; gdk_display_manager_list_displays ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_manager_list_displays" display-manager-list-displays)
    (g:slist-t (g:object display))
 #+liber-documentation
 "@version{2023-4-14}
  @argument[manager]{a @class{gdk:display-manager} object}
  @return{A list of @class{gdk:display} objects.}
  @short{List all currently open displays.}
  @see-class{gdk:display}
  @see-class{gdk:display-manager}"
  (manager (g:object display-manager)))

(export 'display-manager-list-displays)

;;; ----------------------------------------------------------------------------
;;; gdk_display_manager_open_display ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_manager_open_display" display-manager-open-display)
    (g:object display)
 #+liber-documentation
 "@version{2023-4-14}
  @argument[manager]{a @class{gdk:display-manager} object}
  @argument[name]{a string with the name of the display to open}
  @begin{return}
    A @class{gdk:display} object, or @code{nil} if the display could not be
    opened.
  @end{return}
  @short{Opens a display.}
  @see-class{gdk:display}
  @see-class{gdk:display-manager}"
  (manager (g:object display-manager))
  (name :string))

(export 'display-manager-open-display)

;;; ----------------------------------------------------------------------------
;;; gdk_set_allowed_backends ()
;;;
;;; void
;;; gdk_set_allowed_backends (const char *backends);
;;;
;;; Sets a list of backends that GDK should try to use.
;;;
;;; This can be useful if your application does not work with certain GDK
;;; backends.
;;;
;;; By default, GDK tries all included backends.
;;;
;;; For example,
;;;
;;; gdk_set_allowed_backends ("wayland,quartz,*");
;;;
;;; instructs GDK to try the Wayland backend first, followed by the Quartz
;;; backend, and then all others.
;;;
;;; If the GDK_BACKEND environment variable is set, it determines what backends
;;; are tried in what order, while still respecting the set of allowed backends
;;; that are specified by this function.
;;;
;;; The possible backend names are x11, win32, quartz, broadway, wayland. You
;;; can also include a * in the list to try all remaining backends.
;;;
;;; This call must happen prior to gdk_display_open(), gtk_init(), or
;;; gtk_init_check() in order to take effect.
;;;
;;; backends :
;;;     a comma-separated list of backends
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_set_allowed_backends" set-allowed-backends) :void
  (backends :string))

(export 'set-allowed-backends)

;;; -- End of file gdk4.display-manager.lisp -----------------------------------

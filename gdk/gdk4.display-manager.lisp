;;; ----------------------------------------------------------------------------
;;; gdk4.display-manager.lisp
;;;
;;; The documentation in this file is taken from the GDK 4 Reference Manual
;;; version 4.18 and modified to document the Lisp binding to the GDK library,
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

(gobject:define-gobject "GdkDisplayManager" display-manager
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gdk_display_manager_get_type")
  ((default-display
    display-manager-default-display
    "default-display" "GdkDisplay" t t)))

#+liber-documentation
(setf (documentation 'display-manager 'type)
 "@version{2025-06-29}
  @begin{short}
    The purpose of the @class{gdk:display-manager} singleton object is to offer
    notification when displays appear or disappear or the default display
    changes.
  @end{short}
  You can use the @fun{gdk:display-manager-get} function to obtain the
  @class{gdk:display-manager} singleton, but that should be rarely necessary.
  Typically, initializing GTK opens a display that you can work with without
  ever accessing the @class{gdk:display-manager} object.

  The GDK library can be built with support for multiple backends. The
  @class{gdk:display-manager} object determines which backend is used at
  runtime.
  @begin[Signal Details]{dictionary}
    @begin[display-manager::display-opened]{signal}
      @begin{pre}
lambda (manager display)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[manager]{The @class{gdk:display-manager} object on which the
          signal is emitted.}
        @entry[display]{The opened @class{gdk:display} object.}
      @end{simple-table}
      The signal is emitted when a display is opened.
    @end{signal}
  @end{dictionary}
  @see-slot{gdk:display-manager-default-display}
  @see-class{gdk:display}
  @see-function{gdk:display-manager-get}")

;;; ----------------------------------------------------------------------------
;;; Property and Accesor Details
;;; ----------------------------------------------------------------------------

;;; --- gdk:display-manager-default-display ------------------------------------

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
 "@version{2025-09-29}
  @syntax{(gdk:display-manager-default-display object) => display}
  @syntax{(setf (gdk:display-manager-default-display object) display)}
  @argument[object]{a @class{gdk:display-manager} object}
  @argument[display]{a default @class{gdk:display} object}
  @begin{short}
    The accessor for the @slot[gdk:display-manager]{default-display} slot of the
    @class{gdk:display-manager} class gets or sets the default
    @class{gdk:display} object.
  @end{short}
  Returns @code{nil} if there is no default display.
  @begin[Examples]{dictionary}
    @begin{pre}
(gdk:display-manager-default-display (gdk:display-manager-get))
=> #<GDK:DISPLAY {1001F9A233@}>
    @end{pre}
  @end{dictionary}
  @see-class{gdk:display}
  @see-class{gdk:display-manager}")

;;; ----------------------------------------------------------------------------
;;; gdk_display_manager_get
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_manager_get" display-manager-get)
    (g:object display-manager)
 #+liber-documentation
 "@version{2024-07-10}
  @return{The global @class{gdk:display-manager} singleton object.}
  @begin{short}
    Gets the @class{gdk:display-manager} singleton object.
  @end{short}
  When called for the first time, this function consults the @code{GDK_BACKEND}
  environment variable to find out which of the supported GDK backends to use
  in case GDK has been compiled with multiple backends.
  @begin[Examples]{dictionary}
    @begin{pre}
(gdk:display-manager-get)
=> #<GDK:DISPLAY-MANAGER {1001CFF103@}>
    @end{pre}
  @end{dictionary}
  @see-class{gdk:display-manager}")

(export 'display-manager-get)

;;; ----------------------------------------------------------------------------
;;; gdk_display_manager_list_displays
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_manager_list_displays"
               display-manager-list-displays) (g:slist-t (g:object display))
 #+liber-documentation
 "@version{2024-07-10}
  @argument[manager]{a @class{gdk:display-manager} object}
  @return{The list of @class{gdk:display} objects.}
  @short{List all currently open displays.}
  @see-class{gdk:display}
  @see-class{gdk:display-manager}"
  (manager (g:object display-manager)))

(export 'display-manager-list-displays)

;;; ----------------------------------------------------------------------------
;;; gdk_display_manager_open_display
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_manager_open_display" display-manager-open-display)
    (g:object display)
 #+liber-documentation
 "@version{2025-08-02}
  @argument[manager]{a @class{gdk:display-manager} object}
  @argument[name]{a string for the name of the display to open}
  @begin{return}
    The @class{gdk:display} object, or @code{nil} if the display could not be
    opened.
  @end{return}
  @short{Opens a display.}
  @see-class{gdk:display}
  @see-class{gdk:display-manager}"
  (manager (g:object display-manager))
  (name :string))

(export 'display-manager-open-display)

;;; ----------------------------------------------------------------------------
;;; gdk_set_allowed_backends
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_set_allowed_backends" set-allowed-backends) :void
 #+liber-documentation
 "@version{#2025-08-02}
  @argument[backends]{a string for a comma-separated list of backends}
  @begin{short}
    Sets a list of backends that GDK should try to use.
  @end{short}
  This can be useful if your application does not work with certain GDK
  backends. By default, GDK tries all included backends. For example,
  @begin{pre}
(gdk:set-allowed-backends \"wayland,quartz,*\")
  @end{pre}
  instructs GDK to try the Wayland backend first, followed by the Quartz
  backend, and then all others.

  If the @code{GDK_BACKEND} environment variable is set, it determines what
  backends are tried in what order, while still respecting the set of allowed
  backends that are specified by this function.

  The possible backend names are @code{x11}, @code{win32}, @code{quartz},
  @code{broadway}, @code{wayland}. You can also include a @code{*} in the list
  to try all remaining backends.

  This call must happen prior to the @fun{gdk:display-open},
  @fun{gtk:init}, or @fun{gtk:init-check} functions in order to take effect.
  @see-class{gdk:display-manager}
  @see-function{gdk:display-open}
  @see-function{gtk:init}
  @see-function{gtk:init-check}"
  (backends :string))

(export 'set-allowed-backends)

;;; -- End of file gdk4.display-manager.lisp -----------------------------------

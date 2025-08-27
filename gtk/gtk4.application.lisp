;;; ----------------------------------------------------------------------------
;;; gtk4.application.lisp
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
;;; GtkApplication
;;;
;;;     Application class
;;;
;;; Types and Values
;;;
;;;     GtkApplication
;;;     GtkApplicationInhibitFlags
;;;
;;; Accessors
;;;
;;;     gtk_application_get_active_window
;;;     gtk_application_get_menubar
;;;     gtk_application_set_menubar

;;; Functions
;;;
;;;     gtk_application_new
;;;     gtk_application_add_window
;;;     gtk_application_remove_window
;;;     gtk_application_get_windows
;;;     gtk_application_get_window_by_id
;;;     gtk_application_inhibit
;;;     gtk_application_uninhibit
;;;     gtk_application_get_menu_by_id
;;;     gtk_application_list_action_descriptions
;;;     gtk_application_get_accels_for_action
;;;     gtk_application_set_accels_for_action
;;;     gtk_application_get_actions_for_accel
;;;
;;; Properties
;;;
;;;     active-window
;;;     menubar
;;;     register-session
;;;     screensaver-active
;;;
;;; Signals
;;;
;;;     query-end
;;;     window-added
;;;     window-removed
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GApplication
;;;         ╰── GtkApplication
;;;
;;; Implemented Interfaces
;;;
;;;     GActionGroup
;;;     GActionMap
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkApplicationInhibitFlags
;;; ----------------------------------------------------------------------------

(gobject:define-gflags "GtkApplicationInhibitFlags" application-inhibit-flags
  (:export t
   :type-initializer "gtk_application_inhibit_flags_get_type")
  (:logout  #.(ash 1 0))
  (:switch  #.(ash 1 1))
  (:suspend #.(ash 1 2))
  (:idle    #.(ash 1 3)))

#+liber-documentation
(setf (liber:alias-for-symbol 'application-inhibit-flags)
      "GFlags"
      (liber:symbol-documentation 'application-inhibit-flags)
 "@version{2025-05-12}
  @begin{declaration}
(gobject:define-gflags \"GtkApplicationInhibitFlags\" application-inhibit-flags
  (:export t
   :type-initializer \"gtk_application_inhibit_flags_get_type\")
  (:logout  #.(ash 1 0))
  (:switch  #.(ash 1 1))
  (:suspend #.(ash 1 2))
  (:idle    #.(ash 1 3)))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:logout]{Inhibit ending the user session by logging out or by
        shutting down the computer.}
      @entry[:switch]{Inhibit user switching.}
      @entry[:suspend]{Inhibit suspending the session or computer.}
      @entry[:idle]{Inhibit the session being marked as idle and possibly
        locked.}
    @end{simple-table}
  @end{values}
  @begin{short}
    Types of user actions that may be blocked by a @class{gtk:application}
    instance.
  @end{short}
  See the @fun{gtk:application-inhibit} function.
  @see-class{gtk:application}
  @see-function{gtk:application-inhibit}")

;;; ----------------------------------------------------------------------------
;;; GtkApplication
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkApplication" application
  (:superclass gio:application
   :export t
   :interfaces ("GActionGroup"
                "GActionMap")
   :type-initializer "gtk_application_get_type")
   ((active-window
     application-active-window
     "active-window" "GtkWindow" t nil)
    (menubar
     application-menubar
     "menubar" "GMenuModel" t t)
    (register-session
     application-register-session
     "register-session" "gboolean" t t)
    (screensaver-active
     application-screensaver-active
     "screensaver-active" "gboolean" t nil)))

#+liber-documentation
(setf (documentation 'application 'type)
 "@version{2025-07-11}
  @begin{short}
    The @class{gtk:application} class is a high-level API for writing
    applications.
  @end{short}
  It supports many aspects of a GTK application in a convenient fashion,
  without enforcing a one-size-fits-all application model.

  Currently, the @class{gtk:application} class handles GTK initialization,
  application uniqueness, session management, provides some basic scriptability
  and desktop shell integration by exporting actions and menus and manages a
  list of toplevel windows whose life cycle is automatically tied to the
  life cycle of the application.

  While the @class{gtk:application} class works fine with plain
  @class{gtk:window} widgets, it is recommended to use it together with
  @class{gtk:application-window} widgets.

  @subheading{Automatic resources}
  The @class{gtk:application} instance will automatically load menus from the
  @class{gtk:builder} resource located at @file{\"gtk/menus.ui\"}, relative to
  the resource base path of the application, see the
  @fun{g:application-resource-base-path} function. The menu with the ID
  @code{\"menubar\"} is taken as the menubar of the application. Additional
  menus, most interesting submenus, can be named and accessed via the
  @fun{gtk:application-menu-by-id} function which allows for dynamic population
  of a part of the menu structure. It is also possible to provide the menubar
  manually using the @fun{gtk:application-menubar} function.

  Note that automatic resource loading uses the resource base path that is set
  at construction time and will not work if the resource base path is changed
  at a later time.

  The @class{gtk:application} instance will also automatically setup an icon
  search path for the default icon theme by appending @file{\"icons\"} to the
  resource base path. This allows your application to easily store its icons as
  resources. See the @fun{gtk:icon-theme-add-resource-path} function for more
  information.

  If there is a resource located at @file{\"gtk/help-overlay.ui\"} which
  defines a @class{gtk:shortcuts-window} widget with ID @code{\"help_overlay\"}
  then the @class{gtk:application} instance associates an instance of this
  shortcuts window with each @class{gtk:application-window} widget and sets up
  keyboard accelerators, @kbd{Control-F1} and @kbd{Control-?}, to open it. To
  create a menu item that displays the shortcuts window, associate the menu
  item with the @code{\"win.show-help-overlay\"} action.

  The @class{gtk:application} instance will also automatically set the
  application ID as the default window icon. Use the
  @fun{gtk:window-default-icon-name} function or the
  @slot[gtk:window]{icon-name} property to override that behavior.

  The @class{gtk:application} instance optionally registers with a session
  manager of the users session, if you set the
  @slot[gtk:application]{register-session} property, and offers various
  functionality related to the session life cycle.

  An application can block various ways to end the session with the
  @fun{gtk:application-inhibit} function. Typical use cases for this kind of
  inhibiting are long-running, uninterruptible operations, such as burning a CD
  or performing a disk backup. The session manager may not honor the inhibitor,
  but it can be expected to inform the user about the negative consequences of
  ending the session while inhibitors are present.
  @begin[Examples]{dictionary}
    A simple application:
    @begin{pre}
(defun application-simple (&rest argv)
  (let (;; Create an application
        (app (make-instance 'gtk:application
                            :flags :default-flags
                            :application-id \"com.crategus.application-simple\")))
    ;; Connect signal \"activate\" to the application
    (g:signal-connect app \"activate\"
        (lambda (application)
          ;; Create an application window
          (let ((window (make-instance 'gtk:application-window
                                       :application application
                                       :title \"Simple Application\"
                                       :default-width 480
                                       :default-height 300)))
            ;; Present the application window
            (gtk:window-present window))))
  ;; Run the application
  (g:application-run app argv)))
    @end{pre}
    A resource definition with menus, a shortcuts window and an icon for
    automatically loading resources:
    @begin{pre}
<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<gresources>
  <gresource prefix=\"/com/crategus/application/gtk\">
    <file>menus.ui</file>
    <file>help-overlay.ui</file>
  </gresource>
  <gresource prefix=\"/com/crategus/application/icons\">
    <file>my-gtk-logo.png</file>
  </gresource>
</gresources>
    @end{pre}
    The application that automatically loads the resources:
    @begin{pre}
(defun application-resources (&rest argv)
  ;; Register the resources
  (g:with-resource (resource (glib-sys:check-and-create-resources
                                     \"gtk4-application.xml\"
                                     :package \"gtk4-application\"
                                     :sourcedir \"resource/\"))
    (let (;; Create an application
          (app (make-instance 'gtk:application
                               :flags :default-flags
                               :application-id
                               \"com.crategus.application\")))
      ;; Connect \"activate\" signal
      (g:signal-connect app \"activate\"
          (lambda (application)
            (g:application-hold application)
            ;; Create an application window
            (let (;; Define action entries for the menu items
                  (entries '((\"about\")))
                  (accels '(\"win-show-help-overlay\" \"F1\"
                            \"win.about\" \"<Control>A\"))
                  (window (make-instance 'gtk:application-window
                                         :application application
                                         :title \"Application Resources\"
                                         :show-menubar t
                                         :icon-name \"gtk-logo\"
                                         :default-width 480
                                         :default-height 300)))
              ;; Add accelerators for the application
              (iter (for (name accel) on accels by #'cddr)
                    (setf (gtk:application-accels-for-action application name)
                          accel))
              ;; Add the action entries to the application window
              (g:action-map-add-action-entries window entries)
              ;; Present the window
              (gtk:window-present window)
              (g:application-release application))))
      ;; Run the application
      (g:application-run app argv))))
    @end{pre}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @begin[application::query-end]{signal}
      @begin{pre}
lambda (application)    :run-first
      @end{pre}
      @begin[code]{simple-table}
        @entry[application]{The @class{gtk:application} instance that emitted
          the signal.}
      @end{simple-table}
      Emitted when the session manager is about to end the session. This signal
      is only emitted if the @slot[gtk:application]{register-session} property
      is @em{true}. Applications can connect to this signal and call the
      @fun{gtk:application-inhibit} function with the
      @val[gtk:application-inhibit-flags]{:logout} value to delay the end of
      the session until the state has been saved.
    @end{signal}
    @begin[application::window-added]{signal}
      @begin{pre}
lambda (application window)    :run-first
      @end{pre}
      @begin[code]{simple-table}
        @entry[application]{The @class{gtk:application} instance that emitted
          the signal.}
        @entry[window]{The newly added @class{gtk:window} widget.}
      @end{simple-table}
      Emitted when a @class{gtk:window} widget is added to the application
      through the @fun{gtk:application-add-window} function.
    @end{signal}
    @begin[application::window-removed]{signal}
      @begin{pre}
lambda (application window)    :run-first
      @end{pre}
      @begin[code]{simple-table}
        @entry[application]{The @class{gtk:application} instance that emitted
          the signal.}
        @entry[window]{The @class{gtk:window} widget that is being removed.}
      @end{simple-table}
      Emitted when a @class{gtk:window} widget is removed from the application.
      This can happen as a side-effect of the window being destroyed or
      explicitly through the @fun{gtk:application-remove-window} function.
    @end{signal}
  @end{dictionary}
  @see-slot{gtk:application-active-window}
  @see-slot{gtk:application-menubar}
  @see-slot{gtk:application-register-session}
  @see-slot{gtk:application-screensaver-active}
  @see-constructor{gtk:application-new}
  @see-class{g:application}
  @see-class{gtk:application-window}
  @see-class{gtk:builder}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:application-active-window ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "active-window" 'application) t)
 "The @code{active-window} property of type @class{gtk:window} (Read) @br{}
  The currently focused window of the application.")

#+liber-documentation
(setf (liber:alias-for-function 'application-active-window)
      "Accessor"
      (documentation 'application-active-window 'function)
 "@version{2025-07-28}
  @syntax{(gtk:application-active-window object) => window}
  @argument[object]{a @class{gtk:application} instance}
  @argument[window]{a @class{gtk:window} widget}
  @begin{short}
    The accessor for the @slot[gtk:application]{active-window} slot of the
    @class{gtk:application} class returns the active window of the application.
  @end{short}
  The active window is the one that was most recently focused within the
  application. This window may not have the focus at the moment if another
  application has it - this is just the most recently focused window within
  this application.
  @see-class{gtk:application}
  @see-class{gtk:window}")

;;; --- gtk:application-menubar ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "menubar" 'application) t)
 "The @code{menubar} property of type @class{g:menu-model} (Read / Write) @br{}
  The menu model to be used for the menubar of the application.")

#+liber-documentation
(setf (liber:alias-for-function 'application-menubar)
      "Accessor"
      (documentation 'application-menubar 'function)
 "@version{2025-07-28}
  @syntax{(gtk:application-menubar object) => menubar}
  @syntax{(setf (gtk:application-menubar object) menubar)}
  @argument[object]{a @class{gtk:application} instance}
  @argument[menubar]{a @class{g:menu-model} object, or @code{nil}}
  @begin{short}
    The accessor for the @slot[gtk:application]{menubar} slot of the
    @class{gtk:application} class gets or sets the menubar for windows of the
    application.
  @end{short}
  Pass the @code{nil} value to unset the menubar.

  This is a menubar in the traditional sense. This can only be done in the
  primary instance of the application, after it has been registered. The
  handler for the @sig[g:application]{startup} signal is a good place to set
  the menubar.

  Depending on the desktop environment, the menubar may appear at the top of
  each window, or at the top of the screen. In some environments, if both the
  application menu and the menubar are set, the application menu will be
  presented as if it were the first item of the menubar. Other environments
  treat the two as completely separate - for example, the application menu
  may be rendered by the desktop shell while the menubar, if set, remains in
  each individual window.

  Use the base @class{g:action-map} interface to add actions, to respond to the
  user selecting these menu items.
  @see-class{gtk:application}
  @see-class{g:menu-model}
  @see-class{g:action-map}")

;;; --- gtk:application-register-session ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "register-session"
                                               'application) t)
 "The @code{register-session} property of type @code{:boolean} (Read / Write)
  @br{}
  Set the property to @em{true} to register with the session manager. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'application-register-session)
      "Accessor"
      (documentation 'application-register-session 'function)
 "@version{2025-07-29}
  @syntax{(gtk:application-register-session object) => setting}
  @syntax{(setf (gtk:application-register-session object) setting)}
  @argument[object]{a @class{gtk:application} instance}
  @argument[setting]{a boolean whether to register with the session manager}
  @begin{short}
    The accessor for the @slot[gtk:application]{register-session} slot of the
    @class{gtk:application} class gets or sets whether to register with the
    session manager.
  @end{short}

  Set the @slot[gtk:application]{register-session} property to @em{true} to
  register with the session mananger. This will make GTK track the session
  state, such as the @slot[gtk:application]{screensaver-active} property.
  @see-class{gtk:application}
  @see-function{gtk:application-screensaver-active}")

;;; ----- gtk:application-screensaver-active -----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "screensaver-active"
                                               'application) t)
 "The @code{screensaver-active} property of type @code{:boolean} (Read) @br{}
  The property is @em{true} if GTK believes that the screensaver is currently
  active. GTK only tracks session state, when the
  @slot[gtk:application]{register-session} property is set to @em{true}.
  Tracking the screensaver state is supported on Linux. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'application-screensaver-active)
      "Accessor"
      (documentation 'application-screensaver-active 'function)
 "@version{2025-08-21}
  @syntax{(gtk:application-screensaver-active object) => active}
  @argument[object]{a @class{gtk:application} instance}
  @argument[active]{a boolean whether the screensaver is active}
  @begin{short}
    The accessor for the @slot[gtk:application]{screensaver-active} slot of the
    @class{gtk:application} class returns whether the screensaver is active.
  @end{short}

  The @slot[gtk:application]{screensaver-active} property is @em{true} if GTK
  believes that the screensaver is currently active. GTK only tracks session
  state, including this, when the @slot[gtk:application]{register-session}
  property is set to @em{true}. Tracking the screensaver state is supported on
  Linux.
  @see-class{gtk:application}
  @see-function{gtk:application-register-session}")

;;; ----------------------------------------------------------------------------
;;; gtk_application_new
;;; ----------------------------------------------------------------------------

(declaim (inline application-new))

(defun application-new (id flags)
 "@version{2025-07-25}
  @argument[id]{a string for the application ID, or @code{nil} for no
    application ID}
  @argument[flags]{a @sym{g:application-flags} value for the application flags}
  @return{The new @class{gtk:application} instance.}
  @begin{short}
    Creates a new application.
  @end{short}
  When using the @class{gtk:application} class, it is not necessary to call the
  @fun{gtk:init} function manually. It is called as soon as the application
  gets registered as the primary instance.

  Concretely, the @fun{gtk:init} function is called in the default handler for
  the @sig[g:application]{startup} signal. Therefore, @class{gtk:application}
  subclasses should always chain up in their @sig[g:application]{startup}
  handler before using any GTK API. Note that commandline arguments are not
  passed to the @fun{gtk:init} function.

  The application ID must be valid. See the @fun{g:application-id-is-valid}
  function. If no application ID is given then some features, most notably
  application uniqueness, will be disabled.
  @see-class{gtk:application}
  @see-symbol{g:application-flags}
  @see-function{gtk:init}
  @see-function{g:application-id-is-valid}"
  (make-instance 'application
                 :application-id (or id (cffi:null-pointer))
                 :flags flags))

(export 'application-new)

;;; ----------------------------------------------------------------------------
;;; gtk_application_add_window
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_application_add_window" application-add-window) :void
 #+liber-documentation
 "@version{2025-06-22}
  @argument[application]{a @class{gtk:application} instance}
  @argument[window]{a @class{gtk:window} widget}
  @begin{short}
    Adds a window to the application.
  @end{short}
  This call can only happen after the application has started. Typically, you
  should add new application windows in response to the emission of the
  @sig[g:application]{activate} signal. This call is equivalent to setting
  the @slot[gtk:window]{application} property of the given @arg{window} to the
  given @arg{application}, that is:
  @begin{pre}
(gtk:application-add-window application window)
== (setf (gtk:window-application window) application)
  @end{pre}
  Normally, the connection between the application and the window will remain
  until the window is destroyed, but you can explicitly remove it with the
  @fun{gtk:application-remove-window} function. GTK will keep the application
  running as long as it has any windows.
  @see-class{gtk:application}
  @see-class{gtk:window}
  @see-function{gtk:application-remove-window}
  @see-function{gtk:window-application}"
  (application (g:object application))
  (window (g:object window)))

(export 'application-add-window)

;;; ----------------------------------------------------------------------------
;;; gtk_application_remove_window
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_application_remove_window" application-remove-window) :void
 #+liber-documentation
 "@version{2025-05-12}
  @argument[application]{a @class{gtk:application} instance}
  @argument[window]{a @class{gtk:window} widget}
  @begin{short}
    Remove a window from the application.
  @end{short}
  If the @arg{window} argument belongs to the given @arg{application} then this
  call is equivalent to setting the @slot[gtk:window]{application} property of
  @arg{window} to @code{nil}, that is:
  @begin{pre}
(gtk:application-remove-window application window)
== (setf (gtk:window-application window) nil)
  @end{pre}
  The application may stop running as a result of a call to this function, if
  the @arg{window} argument was the last window of the application.
  @see-class{gtk:application}
  @see-class{gtk:window}
  @see-function{gtk:application-add-window}
  @see-function{gtk:window-application}"
  (application (g:object application))
  (window (g:object window)))

(export 'application-remove-window)

;;; ----------------------------------------------------------------------------
;;; gtk_application_get_windows
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_application_get_windows" application-windows)
    (g:list-t (g:object window) :free-from-foreign nil)
 #+liber-documentation
 "@version{2025-05-12}
  @argument[application]{a @class{gtk:application} instance}
  @return{The list of @class{gtk:window} widgets.}
  @begin{short}
    Gets a list of the windows associated with the application.
  @end{short}
  The list is sorted by most recently focused windows, such that the first
  element is the currently focused window. This is useful for choosing a parent
  for a transient window.
  @begin[Examples]{dictionary}
    In this example, the function iterates over the list of windows and destroys
    the windows. After the last window is destroyed, the application quits.
    @begin{pre}
(defun action-quit (application action parameter)
  (declare (ignore action parameter))
  (let ((windows (gtk:application-windows application)))
    (dolist (window windows)
      (gtk:window-destroy window))))
    @end{pre}
  @end{dictionary}
  @see-class{gtk:application}
  @see-class{gtk:window}"
  (application (g:object application)))

(export 'application-windows)

;;; ----------------------------------------------------------------------------
;;; gtk_application_get_window_by_id
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_application_get_window_by_id" application-window-by-id)
    (g:object window)
 #+liber-documentation
 "@version{2025-07-11}
  @argument[application]{a @class{gtk:application} instance}
  @argument[id]{an unsigned integer for the identifier number}
  @begin{return}
    The @class{gtk:application-window} widget for the given @arg{id}, or
    @code{nil} if there is no window with this ID.
  @end{return}
  @begin{short}
    Returns the application window for the given ID.
  @end{short}
  The ID of an application window can be retrieved with the
  @fun{gtk:application-window-id} function.
  @begin[Notes]{dictionary}
    Both @class{gtk:window} and @class{gtk:application-window} widgets can be
    added to an application, but only a @class{gtk:application-window} widget
    has an ID.
  @end{dictionary}
  @see-class{gtk:application}
  @see-class{gtk:application-window}
  @see-class{gtk:window}
  @see-function{gtk:application-window-id}"
  (application (g:object application))
  (id :uint))

(export 'application-window-by-id)

;;; ----------------------------------------------------------------------------
;;; gtk_application_inhibit
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_application_inhibit" application-inhibit) :uint
 #+liber-documentation
 "@version{2025-07-25}
  @argument[application]{a @class{gtk:application} instance}
  @argument[window]{a @class{gtk:window} widget, or @code{nil}}
  @argument[flags]{a @sym{gtk:application-inhibit-flags} value for the types
    of user actions that should be inhibited}
  @argument[reason]{a short, human readable string that explains why these
    operations are inhibited}
  @begin{return}
    The non-zero unsigned integer cookie that is used to uniquely identify this
    request. It should be used as an argument to the
    @fun{gtk:application-uninhibit} function in order to remove the request. If
    the platform does not support inhibiting or the request failed for some
    reason, 0 is returned.
  @end{return}
  @begin{short}
    Inform the session manager that certain types of actions should be
    inhibited.
  @end{short}
  This is not guaranteed to work on all platforms and for all types of actions.

  Applications should invoke this method when they begin an operation that
  should not be interrupted, such as creating a CD or DVD. The types of
  actions that may be blocked are specified by the @arg{flags} argument. When
  the application completes the operation it should call the
  @fun{gtk:application-uninhibit} function to remove the inhibitor. Note that
  an application can have multiple inhibitors, and all of them must be
  individually removed. Inhibitors are also cleared when the application exits.

  Applications should not expect that they will always be able to block the
  action. In most cases, users will be given the option to force the action to
  take place.

  Reasons should be short and to the point.

  If the @arg{window} argument is given, the session manager may point the user
  to this window to find out more about why the action is inhibited.
  @see-class{gtk:application}
  @see-class{gtk:window}
  @see-symbol{gtk:application-inhibit-flags}
  @see-function{gtk:application-uninhibit}"
  (application (g:object application))
  (window (g:object window))
  (flags application-inhibit-flags)
  (reason :string))

(export 'application-inhibit)

;;; ----------------------------------------------------------------------------
;;; gtk_application_uninhibit
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_application_uninhibit" application-uninhibit) :void
 #+liber-documentation
 "@version{2025-06-21}
  @argument[application]{a @class{gtk:application} instance}
  @argument[cookie]{an unsigned integer for the cookie that was returned by the
    @fun{gtk:application-inhibit} function}
  @begin{short}
    Removes an inhibitor that has been established with the
    @fun{gtk:application-inhibit} function.
  @end{short}
  Inhibitors are also cleared when the application exits.
  @see-class{gtk:application}
  @see-function{gtk:application-inhibit}"
  (application (g:object application))
  (cookie :uint))

(export 'application-uninhibit)

;;; ----------------------------------------------------------------------------
;;; gtk_application_get_menu_by_id
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_application_get_menu_by_id" application-menu-by-id)
    (g:object g:menu)
 #+liber-documentation
 "@version{2025-07-11}
  @argument[application]{a @class{gtk:application} instance}
  @argument[id]{a string for the ID of the menu to look up}
  @begin{return}
    Gets the @class{g:menu} object for the given @arg{id} from the
    automatically loaded resources.
  @end{return}
  @begin{short}
    Gets a menu from automatically loaded resources.
  @end{short}
  @see-class{gtk:application}
  @see-class{g:menu}"
  (application (g:object application))
  (id :string))

(export 'application-menu-by-id)

;;; ----------------------------------------------------------------------------
;;; gtk_application_list_action_descriptions
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_application_list_action_descriptions"
               application-list-action-descriptions) g:strv-t
 #+liber-documentation
 "@version{2025-07-11}
  @argument[application]{a @class{gtk:application} instance}
  @return{The list of strings for the detailed action names.}
  @begin{short}
    Lists the detailed action names which have associated accelerators.
  @end{short}
  See the @fun{gtk:application-accels-for-action} function.
  @see-class{gtk:application}
  @see-function{gtk:application-accels-for-action}"
  (application (g:object application)))

(export 'application-list-action-descriptions)

;;; ----------------------------------------------------------------------------
;;; gtk_application_get_accels_for_action
;;; gtk_application_set_accels_for_action
;;; ----------------------------------------------------------------------------

(defun (setf application-accels-for-action) (accels application name)
  (cffi:foreign-funcall "gtk_application_set_accels_for_action"
                        (g:object application) application
                        :string name
                        g:strv-t (glib-sys:mklist accels)
                        :void)
  accels)

(cffi:defcfun ("gtk_application_get_accels_for_action"
               application-accels-for-action) g:strv-t
 #+liber-documentation
 "@version{2025-08-21}
  @syntax{(gtk:application-accels-for-action application name) => accels}
  @syntax{(setf (gtk:application-accels-for-action application name) accels)}
  @argument[application]{a @class{gtk:application} instance}
  @argument[name]{a string for a detailed action name, specifying an action
    and target}
  @argument[accels]{a string or a list of strings for accelerators in the format
    understood by the @fun{gtk:accelerator-parse} function}
  @begin{short}
    Gets or sets the keyboard accelerators that will trigger the given action.
  @end{short}
  The first item in the list of accelerators will be the primary accelerator,
  which may be displayed in the UI. To remove all accelerators for an action,
  use an empty list.

  For the detailed action name, see the @fun{g:action-parse-detailed-name} and
  @fun{g:action-print-detailed-name} functions.
  @see-class{gtk:application}
  @see-function{gtk:accelerator-parse}
  @see-function{g:action-parse-detailed-name}
  @see-function{g:action-print-detailed-name}"
  (application (g:object application))
  (name :string))

(export 'application-accels-for-action)

;;; ----------------------------------------------------------------------------
;;; gtk_application_get_actions_for_accel
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_application_get_actions_for_accel"
               application-actions-for-accel) g:strv-t
 "@version{2025-05-12}
  @argument[application]{a @class{gtk:application} instance}
  @argument[accel]{a string for an accelerator that can be parsed by the
    @fun{gtk:accelerator-parse} function}
  @return{The list of strings of actions for the @arg{accel} argument.}
  @begin{short}
    Returns the list of actions, possibly empty, that the given accelerator
    maps to.
  @end{short}
  Each item in the list is a detailed action name in the usual form.

  This might be useful to discover if an accelerator already exists in order
  to prevent installation of a conflicting accelerator, from an accelerator
  editor or a plugin system, for example. Note that having more than one
  action per accelerator may not be a bad thing and might make sense in cases
  where the actions never appear in the same context.

  In case there are no actions for a given accelerator, an empty list is
  returned.

  It is a programmer error to pass an invalid accelerator string. If you are
  unsure, check it with the @fun{gtk:accelerator-parse} function first.
  @see-class{gtk:application}
  @see-function{gtk:accelerator-parse}"
  (application (g:object application))
  (accel :string))

(export 'application-actions-for-accel)

;;; --- End of file gtk4.application.lisp --------------------------------------

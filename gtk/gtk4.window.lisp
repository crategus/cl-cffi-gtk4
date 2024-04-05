;;; ----------------------------------------------------------------------------
;;; gtk4.window.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.12 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2024 Dieter Kaiser
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
;;; GtkWindow
;;;
;;;     Toplevel which can contain other widgets
;;;
;;; Types and Values
;;;
;;;     GtkWindow
;;;
;;; Accessors
;;;
;;;     gtk_window_get_application
;;;     gtk_window_set_application
;;;     gtk_window_get_child
;;;     gtk_window_set_child
;;;     gtk_window_get_decorated
;;;     gtk_window_set_decorated
;;;     gtk_window_get_default_widget
;;;     gtk_window_set_default_widget
;;;     gtk_window_get_deletable
;;;     gtk_window_set_deletable
;;;     gtk_window_get_destroy_with_parent
;;;     gtk_window_set_destroy_with_parent
;;;     gtk_window_set_display
;;;     gtk_window_get_focus_visible
;;;     gtk_window_set_focus_visible
;;;     gtk_window_get_handle_menubar_accel                Since 4.2
;;;     gtk_window_set_handle_menubar_accel                Since 4.2
;;;     gtk_window_get_hide_on_close
;;;     gtk_window_set_hide_on_close
;;;     gtk_window_get_icon_name
;;;     gtk_window_set_icon_name
;;;     gtk_window_get_mnemonics_visible
;;;     gtk_window_set_mnemonics_visible
;;;     gtk_window_get_modal
;;;     gtk_window_set_modal
;;;     gtk_window_get_resizable
;;;     gtk_window_set_resizable
;;;     gtk_window_set_startup_id
;;;     gtk_window_get_title
;;;     gtk_window_set_title
;;;     gtk_window_get_titlebar
;;;     gtk_window_set_titlebar
;;;     gtk_window_get_transient_for
;;;     gtk_window_set_transient_for
;;;
;;; Functions
;;;
;;;     gtk_window_new
;;;
;;;     gtk_window_destroy
;;;     gtk_window_get_default_size
;;;     gtk_window_set_default_size
;;;     gtk_window_is_active
;;;     gtk_window_is_maximized
;;;     gtk_window_is_fullscreen
;;;     gtk_window_get_toplevels
;;;     gtk_window_list_toplevels
;;;     gtk_window_get_focus
;;;     gtk_window_set_focus
;;;     gtk_window_present
;;;     gtk_window_present_with_time
;;;     gtk_window_close
;;;     gtk_window_minimize
;;;     gtk_window_unminimize
;;;     gtk_window_maximize
;;;     gtk_window_unmaximize
;;;     gtk_window_fullscreen
;;;     gtk_window_fullscreen_on_monitor
;;;     gtk_window_unfullscreen
;;;     gtk_window_get_default_icon_name
;;;     gtk_window_set_default_icon_name
;;;     gtk_window_get_group
;;;     gtk_window_has_group
;;;     gtk_window_set_auto_startup_notification
;;;     gtk_window_set_interactive_debugging
;;;     gtk_window_is_suspended                            Since 4.12
;;;
;;; Properties
;;;
;;;     application
;;;     child
;;;     decorated
;;;     default-height
;;;     default-widget
;;;     default-width
;;;     deletable
;;;     destroy-with-parent
;;;     display
;;;     focus-visible
;;;     focus-widget
;;;     fullscreened
;;;     handle-menubar-accel                               Since 4.2
;;;     hide-on-close
;;;     icon-name
;;;     is-active
;;;     maximized
;;;     mnemonics-visible
;;;     modal
;;;     resizable
;;;     startup-id
;;;     suspended                                          Since 4.12
;;;     title
;;;     titlebar                                           Since 4.6
;;;     transient-for
;;;
;;; Signals
;;;
;;;     activate-default
;;;     activate-focus
;;;     close-request
;;;     enable-debugging
;;;     keys-changed                                       Deprecated 4.10
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkWindow
;;;                 ├── GtkAboutDialog
;;;                 ├── GtkApplicationWindow
;;;                 ├── GtkAssistant
;;;                 ├── GtkDialog
;;;                 ╰── GtkShortcutsWindow
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;;     GtkNative
;;;     GtkRoot
;;;     GtkShortcutsManager
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkWindow
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkWindow" window
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget"
                "GtkNative"
                "GtkRoot"
                "GtkShortcutManager")
   :type-initializer "gtk_window_get_type")
  ((application
    window-application
    "application" "GtkApplication" t t)
   (child
    window-child
    "child" "GtkWidget" t t)
   (decorated
    window-decorated
    "decorated" "gboolean" t t)
   (default-height
    window-default-height
    "default-height" "gint" t t)
   (default-widget
    window-default-widget
    "default-widget" "GtkWidget" t t)
   (default-width
    window-default-width
    "default-width" "gint" t t)
   (deletable
    window-deletable
    "deletable" "gboolean" t t)
   (destroy-with-parent
    window-destroy-with-parent
    "destroy-with-parent" "gboolean" t t)
   (display
    window-display
    "display" "GdkDisplay" t t)
   (focus-visible
    window-focus-visible
    "focus-visible" "gboolean" t t)
   (focus-widget
    window-focus-widget
    "focus-widget" "GtkWidget" t t)
   (fullscreened
    window-fullscreened
    "fullscreened" "gboolean" t t)
   #+gtk-4-2
   (handle-menubar-accel
    window-handle-menubar-accel
    "handle-menubar-accel" "gboolean" t t)
   (hide-on-close
    window-hide-on-close
    "hide-on-close" "gboolean" t t)
   (icon-name
    window-icon-name
    "icon-name" "gchararray" t t)
   (is-active
    window-is-active
    "is-active" "gboolean" t nil)
   (maximized
    window-maximized
    "maximized" "gboolean" t t)
   (mnemonics-visible
    window-mnemonics-visible
    "mnemonics-visible" "gboolean" t t)
   (modal
    window-modal
    "modal" "gboolean" t t)
   (resizable
    window-resizable
    "resizable" "gboolean" t t)
   (startup-id
    window-startup-id
    "startup-id" "gchararray" nil t)
   #+gtk-4-12
   (suspended
    window-suspended
    "suspended" "gboolean" t nil)
   (title
    window-title
    "title" "gchararray"  t t)
   #+gtk-4-6
   (titlebar
    window-titlebar
    "titlebar" "GtkWidget" t t)
   (transient-for
    window-transient-for
    "transient-for" "GtkWindow" t t)))

#+liber-documentation
(setf (documentation 'window 'type)
 "@version{2024-4-1}
  @begin{short}
    The @class{gtk:window} widget is a toplevel window which can contain other
    widgets.
  @end{short}

  @image[window]{Figure: GtkWindow}

  Windows normally have decorations that are under the control of the windowing
  system and allow the user to manipulate the window, e.g. to resize it, move
  it, or close it.
  @begin[GtkWindow as GtkBuildable]{dictionary}
    The @class{gtk:window} implementation of the @class{gtk:buildable} interface
    supports setting a child widget as the titlebar by specifying
    @code{titlebar} as the @code{type} attribute of a @code{<child>} element.
  @end{dictionary}
  @begin[CSS nodes]{dictionary}
    @begin{pre}
window.background [.csd / .solid-csd / .ssd] [.maximized / .fullscreen / .tiled]
├── <child>
╰── <titlebar child>.titlebar [.default-decoration]
    @end{pre}
    The @class{gtk:window} implementation has a main CSS node with name
    @code{window} and @code{.background} style class.

    Style classes that are typically used with the main CSS node are
    @code{.csd}, when client-side decorations are in use, @code{.solid-csd},
    for client-side decorations without invisible borders, @code{.ssd}, used by
    mutter when rendering server-side decorations. The @class{gtk:window}
    implementation also represents window states with the following style
    classes on the main node: @code{.tiled}, @code{.maximized},
    @code{.fullscreen}. Specialized types of window often add their own
    discriminating style classes, such as @code{.popup} or @code{.tooltip}.

    Generally, some CSS properties do not make sense on the toplevel window
    node, such as margins or padding. When client-side decorations without
    invisible borders are in use, i.e. the @code{.solid-csd} style class is
    added to the main window node, the CSS border of the toplevel window is used
    for resize drags. In the @code{.csd} case, the shadow area outside of the
    window can be used to resize it.

    The @class{gtk:window} implementation adds the @code{.titlebar} and
    @code{.default-decoration} style classes to the widget that is added as a
    titlebar child.
  @end{dictionary}
  @begin[Accessibility]{dictionary}
    The @class{gtk:window} implementation uses the @code{:window} role of the
    @symbol{gtk:accessible-role} enumeration.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"activate-default\" signal}
      @begin{pre}
lambda (window)    :action
      @end{pre}
      The signal is a keybinding signal which gets emitted when the user
      activates the default widget of the window.
      @begin[code]{table}
        @entry[window]{The @class{gtk:window} widget which received the signal.}
      @end{table}
    @subheading{The \"activate-focus\" signal}
      @begin{pre}
lambda (window)    :action
      @end{pre}
      The signal is a keybinding signal which gets emitted when the user
      activates the currently focused widget of the window.
      @begin[code]{table}
        @entry[window]{The @class{gtk:window} widget which received the signal.}
      @end{table}
    @subheading{The \"close-request\" signal}
      @begin{pre}
lambda (window)    :run-last
      @end{pre}
      The signal is emitted when the user clicks on the close button of the
      window.
      @begin[code]{table}
        @entry[window]{The @class{gtk:window} widget on which the signal is
          emitted.}
        @entry[Returns]{@em{True} to stop other handlers from being invoked
          for the signal.}
      @end{table}
    @subheading{The \"enable-debugging\" signal}
      @begin{pre}
lambda (window toggle)    :action
      @end{pre}
      The signal is a keybinding signal which gets emitted when the user enables
      or disables interactive debugging. When the @arg{toggle} argument is
      @em{true}, interactive debugging is toggled on or off, when it is
      @em{false}, the debugger will be pointed at the widget under the pointer.
      The default bindings for this signal are the @kbd{Ctrl-Shift-I} and
      @kbd{Ctrl-Shift-D} keys.
      @begin[code]{table}
        @entry[window]{The @class{gtk:window} widget on which the signal is
          emitted.}
        @entry[toggle]{The boolean which toggles the debugger.}
        @entry[Returns]{The boolean which is @em{true} if the key binding was
          handled.}
      @end{table}
    @subheading{The \"keys-changed\" signal}
      @begin{pre}
lambda (window)    :run-first
      @end{pre}
      The signal gets emitted when the set of accelerators or mnemonics that
      are associated with the window changes. @br{}
      @em{Warning:} Deprecated since 4.10. Use @class{gtk:shortcut} and
      @class{gtk:event-controller} objects to implement keyboard shortcuts.
      @begin[code]{table}
        @entry[window]{The @class{gtk:window} widget which received the signal.}
      @end{table}
  @end{dictionary}
  @see-constructor{gtk:window-new}
  @see-slot{gtk:window-application}
  @see-slot{gtk:window-child}
  @see-slot{gtk:window-decorated}
  @see-slot{gtk:window-default-height}
  @see-slot{gtk:window-default-widget}
  @see-slot{gtk:window-default-width}
  @see-slot{gtk:window-deletable}
  @see-slot{gtk:window-destroy-with-parent}
  @see-slot{gtk:window-display}
  @see-slot{gtk:window-focus-visible}
  @see-slot{gtk:window-focus-widget}
  @see-slot{gtk:window-fullscreened}
  @see-slot{gtk:window-handle-menubar-accel}
  @see-slot{gtk:window-hide-on-close}
  @see-slot{gtk:window-icon-name}
  @see-slot{gtk:window-is-active}
  @see-slot{gtk:window-maximized}
  @see-slot{gtk:window-mnemonics-visible}
  @see-slot{gtk:window-modal}
  @see-slot{gtk:window-resizable}
  @see-slot{gtk:window-startup-id}
  @see-slot{gtk:window-suspended}
  @see-slot{gtk:window-title}
  @see-slot{gtk:window-titlebar}
  @see-slot{gtk:window-transient-for}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:window-application -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "application" 'window) t)
 "The @code{application} property of type @class{gtk:application} (Read / Write)
  @br{}
  The application associated with the window. The application will be kept
  alive for at least as long as it has any windows associated with it. See the
  @fun{g:application-hold} function for a way to keep it alive without windows.
  Normally, the connection between the application and the window will remain
  until the window is destroyed, but you can explicitly remove it by setting
  the @code{application} property to @code{nil}.")

#+liber-documentation
(setf (liber:alias-for-function 'window-application)
      "Accessor"
      (documentation 'window-application 'function)
 "@version{2023-9-18}
  @syntax[]{(gtk:window-application object) => application}
  @syntax[]{(setf (gtk:window-application object) application)}
  @argument[object]{a @class{gtk:window} widget}
  @argument[application]{a @class{gtk:application} instance, or @code{nil}}
  @begin{short}
    Accessor of the @slot[gtk:window]{application} slot of the
    @class{gtk:window} class.
  @end{short}
  The @fun{gtk:window-application} function gets the application associated with
  the window. The @setf{gtk:window-application} function sets or unsets the
  application. The application will be kept alive for at least as long as it has
  any windows associated with it. See the @fun{g:application-hold} function for
  a way to keep it alive without windows.

  Normally, the connection between the application and the window will remain
  until the window is destroyed, but you can explicitly remove it by setting the
  @slot[gtk:window]{application} property to @code{nil}. This is equivalent to
  calling the @fun{gtk:application-remove-window} function and/or the
  @fun{gtk:application-add-window} function on the old/new applications as
  relevant.
  @see-class{gtk:window}
  @see-class{gtk:application}
  @see-function{g:application-hold}
  @see-function{gtk:application-add-window}
  @see-function{gtk:application-remove-window}")

;;; --- gtk:window-child -------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "child" 'window) t)
 "The @code{child} property of type @class{gtk:widget} (Read / Write) @br{}
  The child widget.")

#+liber-documentation
(setf (liber:alias-for-function 'window-child)
      "Accessor"
      (documentation 'window-child 'function)
 "@version{2023-9-18}
  @syntax[]{(gtk:window-child object) => child}
  @syntax[]{(setf (gtk:window-child object) child)}
  @argument[object]{a @class{gtk:window} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @begin{short}
    Accessor of the @slot[gtk:window]{child} slot of the @class{gtk:window}
    class.
  @end{short}
  The @fun{gtk:window-child} function gets the child widget of the window. The
  @setf{gtk:window-child} function sets the child widget.
  @see-class{gtk:window}
  @see-class{gtk:widget}")

;;; --- gtk:window-decorated ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "decorated" 'window) t)
 "The @code{decorated} property of type @code{:boolean} (Read / Write) @br{}
  Whether the window should be decorated by the window manager. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'window-decorated)
      "Accessor"
      (documentation 'window-decorated 'function)
 "@version{2023-9-18}
  @syntax[]{(gtk:window-decorated object) => setting}
  @syntax[]{(setf (gtk:window-decorated object) setting)}
  @argument[object]{a @class{gtk:window} widget}
  @argument[setting]{@em{true} to decorate the window}
  @begin{short}
    Accessor of the @slot[gtk:window]{decorated} slot of the @class{gtk:window}
    class.
  @end{short}
  The @fun{gtk:window-decorated} function returns whether the window has been
  set to have decorations. The @setf{gtk:window-decorated} function sets whether
  the window should be decorated.

  By default, windows are decorated with a title bar, resize controls, etc.
  Some window managers allow GTK to disable these decorations, creating a
  borderless window. If you set the @slot[gtk:window]{decorated} property to
  @em{false} using this function, GTK will do its best to convince the window
  manager not to decorate the window. Depending on the system, this function may
  not have any effect when called on a window that is already visible, so you
  should call it before calling the @fun{gtk:widget-visible} function.

  On Windows, this function always works, since there is no window manager
  policy involved.
  @see-class{gtk:window}
  @see-function{gtk:widget-visible}")

;;; --- gtk:window-default-height ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "default-height" 'window) t)
 "The @code{default-height} property of type @code{:int} (Read / Write) @br{}
  The default height of the window. @br{}
  Allowed values: >= -1 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'window-default-height)
      "Accessor"
      (documentation 'window-default-height 'function)
 "@version{2023-8-20}
  @syntax[]{(gtk:window-default-height object) => height}
  @syntax[]{(setf (gtk:window-default-height object) height)}
  @argument[object]{a @class{gtk:window} widget}
  @argument[height]{an integer with the default height}
  @begin{short}
    Accessor of the @slot[gtk:window]{default-height} slot of the
    @class{gtk:window} class.
  @end{short}
  The default height of the window, used when initially showing the window.
  See the @fun{gtk:window-default-size} function.
  @see-class{gtk:window}
  @see-function{gtk:window-default-size}")

;;; --- gtk:window-default-widget ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "default-widget" 'window) t)
 "The @code{default-widget} property of type @class{gtk:widget} (Read / Write)
  @br{}
  The default widget of the window. The default widget is the widget that is
  activated when the user presses the @kbd{Enter} key.")

#+liber-documentation
(setf (liber:alias-for-function 'window-default-widget)
      "Accessor"
      (documentation 'window-default-widget 'function)
 "@version{2023-9-18}
  @syntax[]{(gtk:window-default-widget object) => widget}
  @syntax[]{(setf (gtk:window-default-widget object) widget)}
  @argument[object]{a @class{gtk:window} widget}
  @argument[widget]{a @class{gtk:widget} object to be the default widget, or
    @code{nil} to unset the default widget for the toplevel}
  @begin{short}
    Accessor of the @slot[gtk:window]{default-widget} slot of the
    @class{gtk:window} class.
  @end{short}
  The @fun{gtk:window-default-widget} function returns the default widget for
  the window. The @setf{gtk:window-default-widget} function sets or unsets the
  default widget. The default widget is the widget that is activated when the
  user presses the @kbd{Enter} key in a dialog for example.
  @see-class{gtk:window}
  @see-class{gtk:widget}")

;;; --- gtk:window-default-width -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "default-width" 'window) t)
 "The @code{default-width} property of type @code{:int} (Read / Write) @br{}
  The default width of the window. @br{}
  Allowed values: >= -1 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'window-default-width)
      "Accessor"
      (documentation 'window-default-width 'function)
 "@version{2023-8-20}
  @syntax[]{(gtk:window-default-width object) => width}
  @syntax[]{(setf (gtk:window-default-width object) width)}
  @argument[object]{a @class{gtk:window} widget}
  @argument[width]{an integer with the default width}
  @begin{short}
    Accessor of the @slot[gtk:window]{default-width} slot of the
    @class{gtk:window} class.
  @end{short}
  The default width of the window, used when initially showing the window.
  See the @fun{gtk:window-default-size} function.
  @see-class{gtk:window}
  @see-function{gtk:window-default-size}")

;;; --- gtk:window-deletable ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "deletable" 'window) t)
 "The @code{deletable} property of type @code{:boolean} (Read / Write) @br{}
  Whether the window frame should have a Close button. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'window-deletable)
      "Accessor"
      (documentation 'window-deletable 'function)
 "@version{2023-9-18}
  @syntax[]{(gtk:window-deletable object) => setting}
  @syntax[]{(setf (gtk:window-deletable object) setting)}
  @argument[object]{a @class{gtk:window} widget}
  @argument[setting]{@em{true} to decorate the window as deletable}
  @begin{short}
    Accessor of the @slot[gtk:window]{deletable} slot of the @class{gtk:window}
    class.
  @end{short}
  The @fun{gtk:window-deletable} function returns whether the window has been
  set to have a Close button. The @setf{gtk:window-deletable} function sets
  whether the window should be deletable.

  By default, windows have a Close button in the window frame. Some window
  managers allow GTK to disable this button. If you set the
  @slot[gtk:window]{deletable} property to @em{false} using this function, GTK
  will do its best to convince the window manager not to show a Close button.
  Depending on the system, this function may not have any effect when called on
  a window that is already visible, so you should call it before calling the
  @fun{gtk:widget-visible} function.

  On Windows, this function always works, since there is no window manager
  policy involved.
  @see-class{gtk:window}
  @see-function{gtk:widget-visible}")

;;; --- gtk:window-destroy-with-parent -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "destroy-with-parent"
                                               'window) t)
 "The @code{destroy-with-parent} property of type @code{:boolean} (Read / Write)
  @br{}
  If this window should be destroyed when the parent is destroyed. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'window-destroy-with-parent)
      "Accessor"
      (documentation 'window-destroy-with-parent 'function)
 "@version{2023-9-18}
  @syntax[]{(gtk:window-destroy-with-parent object) => setting}
  @syntax[]{(setf (gtk:window-destroy-with-parent object) setting)}
  @argument[object]{a @class{gtk:window} widget}
  @argument[setting]{a boolean whether to destroy the window with its transient
    parent}
  @begin{short}
    Accessor of the @slot[gtk:window]{destroy-with-parent} slot of the
    @class{gtk:window} class.
  @end{short}
  The @fun{gtk:window-destroy-with-parent} function returns whether the window
  will be destroyed with its transient parent. The
  @setf{gtk:window-destroy-with-parent} function sets whether to destroy the
  window with its transient parent. If the @arg{setting} argument is @em{true},
  then destroying the transient parent of the window will also destroy the
  window itself.

  This is useful for dialogs that should not persist beyond the lifetime of the
  main window they are associated with.
  @see-class{gtk:window}")

;;; --- gtk:window-display -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "display" 'window) t)
 "The @code{display} property of type @class{gdk:display} (Read / Write) @br{}
  The display that will display the window.")

#+liber-documentation
(setf (liber:alias-for-function 'window-display)
      "Accessor"
      (documentation 'window-display 'function)
 "@version{2023-9-18}
  @syntax[]{(gtk:window-display object) => display}
  @syntax[]{(setf (gtk:window-display object) display)}
  @argument[object]{a @class{gtk:window} widget}
  @argument[display]{a @class{gdk:display} object}
  @begin{short}
    Accessor of the @slot[gtk:window]{display} slot of the @class{gtk:window}
    class.
  @end{short}
  The @fun{gtk:window-display} function returns the display where the window is
  displayed. The @setf{gtk:window-display} function sets the display. If the
  window is already mapped, it will be unmapped, and then remapped on the new
  display.
  @see-class{gtk:window}
  @see-class{gdk:display}")

;;; --- gtk:window-focus-visible -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "focus-visible" 'window) t)
 "The @code{focus-visible} property of type @code{:boolean} (Read / Write) @br{}
  Whether \"focus rectangles\" are currently visible in the window. This
  property is maintained by GTK based on user input and should not be set by
  applications. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'window-focus-visible)
      "Accessor"
      (documentation 'window-focus-visible 'function)
 "@version{2023-8-20}
  @syntax[]{(gtk:window-focus-visible object) => setting}
  @syntax[]{(setf (gtk:window-focus-visible object) setting)}
  @argument[object]{a @class{gtk:window} widget}
  @argument[setting]{a boolean whether \"focus-rectangles\" are currently
    visible in the window}
  @begin{short}
    Accessor of the @slot[gtk:window]{focus-visible} slot of the
    @class{gtk:window} class.
  @end{short}
  Whether \"focus rectangles\" are currently visible in the window. This
  property is maintained by GTK based on user input and should not be set by
  applications.
  @see-class{gtk:window}")

;;; --- gtk:window-focus-widget ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "focus-widget" 'window) t)
 "The @code{focus-widget} property of type @class{gtk:widget} (Read / Write)
  @br{}
  The focus widget. That is the widget that would have the focus if the
  toplevel window focused.")

#+liber-documentation
(setf (liber:alias-for-function 'window-focus-widget)
      "Accessor"
      (documentation 'window-focus-widget 'function)
 "@version{2023-9-18}
  @syntax[]{(gtk:window-focus-widget object) => focus}
  @syntax[]{(setf (gtk:window-focus-widget object) focus)}
  @argument[object]{a @class{gtk:window} widget}
  @argument[focus]{a @class{gtk:widget} focus widget}
  @begin{short}
    Accessor of the @slot[gtk:window]{focus-widget} slot of the
    @class{gtk:window} class.
  @end{short}
  The @fun{gtk:window-focus-widget} function retrieves the current focused
  widget within the window. The @setf{gtk:window-focus-widget} function sets the
  focus widget.

  Note that this is the widget that would have the focus if the toplevel window
  focused. If the toplevel window is not focused then the
  @fun{gtk:widget-has-focus} function will not return @em{true} for the widget.

  If the @arg{focus} argument is not the current focus widget, and is focusable,
  sets it as the focus widget for the window. If the @arg{focus} argument is
  @code{nil}, unsets the focus widget for the window. To set the focus to a
  particular widget in the toplevel, it is usually more convenient to use the
  @fun{gtk:widget-grab-focus} function instead of this function.
  @see-class{gtk:window}
  @see-class{gtk:widget}
  @see-function{gtk:widget-has-focus}
  @see-function{gtk:widget-grab-focus}")

;;; --- gtk:window-fullscreened ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "fullscreened" 'window) t)
 "The @code{fullscreened} property of type @code{:boolean} (Read / Write) @br{}
  Whether the window is fullscreen. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'window-fullscreened)
      "Accessor"
      (documentation 'window-fullscreened 'function)
 "@version{2023-9-18}
  @syntax[]{(gtk:window-fullscreened object) => setting}
  @syntax[]{(setf (gtk:window-fullscreened object) setting)}
  @argument[object]{a @class{gtk:window} widget}
  @argument[setting]{a boolean whether the window is fullscreen}
  @begin{short}
    Accessor of the @slot[gtk:window]{fullscreened} slot of the
    @class{gtk:window} class.
  @end{short}
  The @fun{gtk:window-fullscreened} function retrieves the current fullscreen
  state of the window. The @setf{gtk:window-fullscreened} function sets the
  fullscreen state.

  Setting this property is the equivalent of calling the
  @fun{gtk:window-fullscreen} or @fun{gtk:window-unfullscreen} functions. Either
  operation is asynchronous, which means you will need to connect to the
  @code{\"notify\"} signal in order to know whether the operation was
  successful.

  If the window is not yet mapped, the value returned will show whether the
  initial requested state is fullscreen.
  @see-class{gtk:window}
  @see-function{gtk:window-fullscreen}
  @see-function{gtk:window-unfullscreen}")

;;; --- gtk:window-handle-menubar-accel ----------------------------------------

#+(and gtk-4-2 liber-documentation)
(setf (documentation (liber:slot-documentation "handle-menubar-accel"
                                               'window) t)
 "The @code{handle-menubar-accel} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the window frame should handle the @kbd{F10} key for activating
  menubars. Since 4.2 @br{}
  Default value: @em{true}")

#+(and gtk-4-2 liber-documentation)
(setf (liber:alias-for-function 'window-handle-menubar-accel)
      "Accessor"
      (documentation 'window-handle-menubar-accel 'function)
 "@version{2023-9-18}
  @syntax[]{(gtk:window-handle-menubar-accel object) => setting}
  @syntax[]{(setf (gtk:window-handle-menubar-accel object) setting)}
  @argument[object]{a @class{gtk:window} widget}
  @argument[setting]{a boolean whether the window frame should handle the
    @kbd{F10} key for activating menubars}
  @begin{short}
    Accessor of the @slot[gtk:window]{handle-menubar-accel} slot of the
    @class{gtk:window} class.
  @end{short}
  The @fun{gtk:window-handle-menubar-accel} function returns whether the window
  reacts to the @kbd{F10} key presses by activating a menubar it contains. The
  @setf{gtk:window-handle-menubar-accel} function sets the property.

  Since 4.2
  @see-class{gtk:window}")

;;; --- gtk:window-hide-on-close -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "hide-on-close" 'window) t)
 "The @code{hide-on-close} property of type @code{:boolean} (Read / Write) @br{}
  Whether the window should be hidden when the users clicks the Close button.
  @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'window-hide-on-close)
      "Accessor"
      (documentation 'window-hide-on-close 'function)
 "@version{2023-8-20}
  @syntax[]{(gtk:window-hide-on-close object) => setting}
  @syntax[]{(setf (gtk:window-hide-on-close object) setting)}
  @argument[object]{a @class{gtk:window} widget}
  @argument[setting]{a boolean whether the window should be hidden when the
    users clicks the Close button}
  @begin{short}
    Accessor of the @slot[gtk:window]{hide-on-close} slot of the
    @class{gtk:window} class.
  @end{short}
  The @fun{gtk:window-hide-on-close} function returns whether the window will
  be hidden and not destroyed when the close button is clicked. The
  @fun{gtk:window-hide-on-close} function sets the property.
  @see-class{gtk:window}")

;;; --- gtk:window-icon-name ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "icon-name" 'window) t)
 "The @code{icon-name} property of type @code{:string} (Read / Write) @br{}
  Specifies the name of the themed icon to use as the window icon. See the
  @class{gtk:icon-theme} documentation for more details. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'window-icon-name)
      "Accessor"
      (documentation 'window-icon-name 'function)
 "@version{2023-9-18}
  @syntax[]{(gtk:window-icon-name object) => name}
  @syntax[]{(setf (gtk:window-icon-name object) name)}
  @argument[object]{a @class{gtk:window} widget}
  @argument[name]{a string with the name of the themed icon}
  @begin{short}
    Accessor of the @slot[gtk:window]{icon-name} slot of the @class{gtk:window}
    class.
  @end{short}
  The @fun{gtk:window-icon-name} function returns the name of the themed icon
  for the window. The @setf{gtk:window-icon-name} function sets the icon for the
  window from a named themed icon. See the @class{gtk:icon-theme} documentation
  for more details. On some platforms, the window icon is not used at all.

  Note that this has nothing to do with the @code{WM_ICON_NAME} property which
  is mentioned in the Inter-Client Communication Conventions Manual (ICCCM).
  @see-class{gtk:window}
  @see-class{gtk:icon-theme}")

;;; --- gtk:window-is-active ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "is-active" 'window) t)
 "The @code{is-active} property of type @code{:boolean} (Read) @br{}
  Whether the toplevel is the currently active window. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'window-is-active)
      "Accessor"
      (documentation 'window-is-active 'function)
 "@version{2023-8-20}
  @syntax[]{(gtk:window-is-active object) => active}
  @syntax[]{(setf (gtk:window-is-active object) active)}
  @argument[object]{a @class{gtk:window} widget}
  @argument[active]{a boolean whether the toplevel is the currently active
    window}
  @begin{short}
    Accessor of the @slot[gtk:window]{is-active} slot of the @class{gtk:window}
    class.
  @end{short}
  The active toplevel is the window receiving keystrokes. The return value is
  @em{true} if the window is active toplevel itself. You might use this function
  if you wanted to draw a widget differently in an active window from a widget
  in an inactive window.
  @see-class{gtk:window}")

;;; --- gtk:window-maximized ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "maximized" 'window) t)
 "The @code{maximized} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  Whether the window is maximized. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'window-maximized)
      "Accessor"
      (documentation 'window-maximized 'function)
 "@version{2023-8-20}
  @syntax[]{(gtk:window-maximized object) => maximized}
  @syntax[]{(setf (gtk:window-maximized object) maximized)}
  @argument[object]{a @class{gtk:window} widget}
  @argument[maximized]{a boolean whether the window is maximized}
  @begin{short}
    Accessor of the @slot[gtk:window]{maximized} slot of the @class{gtk:window}
    class.
  @end{short}
  Setting this property is the equivalent of calling the
  @fun{gtk:window-maximize} or @fun{gtk:window-unmaximize} functions. Either
  operation is asynchronous, which means you will need to connect to the
  @code{\"notify\"} signal in order to know whether the operation was
  successful.
  @see-class{gtk:window}
  @see-function{gtk:window-maximize}
  @see-function{gtk:window-unmaximize}")

;;; --- gtk:window-mnemonics-visible -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "mnemonics-visible"
                                               'window) t)
 "The @code{mnemonics-visible} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether mnemonics are currently visible in the window. This property is
  maintained by GTK based on user input, and should not be set by applications.
  @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'window-mnemonics-visible)
      "Accessor"
      (documentation 'window-mnemonics-visible 'function)
 "@version{2023-8-20}
  @syntax[]{(gtk:window-mnemonics-visible object) => setting}
  @syntax[]{(setf (gtk:window-mnemonics-visible object) setting)}
  @argument[object]{a @class{gtk:window} widget}
  @argument[setting]{a boolean whether mnemonics are currently visible in the
    window}
  @begin{short}
    Accessor of the @slot[gtk:window]{mnemonics-visible} slot of the
    @class{gtk:window} class.
  @end{short}
  Whether mnemonics are currently visible in the window. This property is
  maintained by GTK based on user input, and should not be set by applications.
  @see-class{gtk:window}")

;;; --- gtk:window-modal -------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "modal" 'window) t)
 "The @code{modal} property of type @code{:boolean} (Read / Write) @br{}
  If @em{true} other windows are not usable while this one is up. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'window-modal)
      "Accessor"
      (documentation 'window-modal 'function)
 "@version{2023-9-18}
  @syntax[]{(gtk:window-modal object) => modal}
  @syntax[]{(setf (gtk:window-modal object) modal)}
  @argument[object]{a @class{gtk:window} widget}
  @argument[modal]{a boolean whether the window is modal}
  @begin{short}
    Accessor of the @slot[gtk:window]{modal} slot of the @class{gtk:window}
    class.
  @end{short}
  The @fun{gtk:window-modal} function returns @em{true} if the window is set to
  be modal. The @setf{gtk:window-modal} function sets a window modal or
  non-modal.

  Modal windows prevent interaction with other windows in the same application.
  To keep modal dialogs on top of main application windows, use the
  @fun{gtk:window-transient-for} function to make the dialog transient for the
  parent. Most window managers will then disallow lowering the dialog below the
  parent.
  @see-class{gtk:window}
  @see-function{gtk:window-transient-for}")

;;; --- gtk:window-resizable ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "resizable" 'window) t)
 "The @code{resizable} property of type @code{:boolean} (Read / Write) @br{}
  If @em{true}, users can resize the window. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'window-resizable)
      "Accessor"
      (documentation 'window-resizable 'function)
 "@version{2023-8-20}
  @syntax[]{(gtk:window-resizable object) => resizable}
  @syntax[]{(setf (gtk:window-resizable object) resizable)}
  @argument[object]{a @class{gtk:window} widget}
  @argument[resizable]{@em{true} if the user can resize this window}
  @begin{short}
    Accessor of the @slot[gtk:window]{resizable} slot of the @class{gtk:window}
    class.
  @end{short}
  Gets or sets whether the user can resize a window. Windows are user resizable
  by default.
  @see-class{gtk:window}")

;;; --- gtk:window-startup-id --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "startup-id" 'window) t)
 "The @code{startup-id} property of type @code{:string} (Write) @br{}
  A write-only property for setting the startup notification identifier of the
  window. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'window-startup-id)
      "Accessor"
      (documentation 'window-startup-id 'function)
 "@version{2023-8-20}
  @syntax[]{(setf (gtk:window-startup-id object) id)}
  @argument[object]{a @class{gtk:window} widget}
  @argument[id]{a string with the startup ID}
  @begin{short}
    Accessor of the @slot[gtk:window]{startup-id} slot of the @class{gtk:window}
    class.
  @end{short}
  The @fun{(setf gtk:window-start-up-id)} function sets a string with the
  startup notification identifier.

  Startup notification identifiers are used by the desktop environment to track
  application startup, to provide user feedback and other features. This
  function changes the corresponding property on the underlying
  @class{gdk:window} object. Normally, startup identifier is managed
  automatically and you should only use this function in special cases like
  transferring focus from other processes. You should use this function before
  calling the @fun{gtk:window-present} function or any equivalent function
  generating a window map event.

  This function is only useful on X11, not with other GTK targets.
  @see-class{gtk:window}
  @see-class{gdk:window}
  @see-function{gtk:window-present}")

;;; --- gtk:window-suspended ---------------------------------------------------

#+(and gtk-4-12 liber-documentation)
(setf (documentation (liber:slot-documentation "suspended" 'window) t)
 "The @code{suspended} property of type @code{:boolean} (Read) @br{}
  Whether the window is suspended. @br{}
  Default value: @em{false}")

#+(and gtk-4-12 liber-documentation)
(setf (liber:alias-for-function 'window-suspended)
      "Accessor"
      (documentation 'window-suspended 'function)
 "@version{#2023-11-16}
  @syntax[]{(gtk:window-suspended object) => suspended}
  @argument[object]{a @class{gtk:window} widget}
  @argument[suspended]{a boolean whether the window is suspended}
  @begin{short}
    Accessor of the @slot[gtk:window]{suspended} slot of the @class{gtk:window}
    class.
  @end{short}
  The @fun{gtk:window-suspended} function retrieves the current suspended state
  of @arg{window}. A window being suspended means it is currently not visible to
  the user, for example by being on an inactive workspace, minimized, or
  obstructed.

  Since 4.12
  @see-class{gtk:window}
  @see-function{gtk:window-is-suspended}")

;;; --- gtk:window-title -------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "title" 'window) t)
 "The @code{title} property of type @code{:string} (Read / Write) @br{}
  The title of the window. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'window-title)
      "Accessor"
      (documentation 'window-title 'function)
 "@version{2023-9-18}
  @syntax[]{(gtk:window-title object) => title}
  @syntax[]{(setf (gtk:window-title object) title)}
  @argument[object]{a @class{gtk:window} widget}
  @argument[title]{a string with the title of the window}
  @begin{short}
    Accessor of the @slot[gtk:window]{title} slot of the @class{gtk:window}
    class.
  @end{short}
  The @fun{gtk:window-title} function retrieves the title of the window. The
  @setf{gtk:window-title} function sets the title.

  The title of a window will be displayed in the title bar. On the X Window
  System, the title bar is rendered by the window manager, so exactly how the
  title appears to users may vary according to the exact configuration. The
  title should help a user distinguish this window from other windows they
  may have open. A good title might include the application name and current
  document filename.
  @see-class{gtk:window}")

;;; --- gtk:window-titlebar ----------------------------------------------------

#+(and gtk-4-6 liber-documentation)
(setf (documentation (liber:slot-documentation "titlebar" 'window) t)
 "The @code{titlebar} property of type @class{gtk:widget} (Read / Write) @br{}
  The titlebar widget. Since 4.6")

#+(and gtk-4-6 liber-documentation)
(setf (liber:alias-for-function 'window-titlebar)
      "Accessor"
      (documentation 'window-titlebar 'function)
 "@version{2023-9-18}
  @syntax[]{(gtk:window-titlebar window) => widget}
  @syntax[]{(setf (gtk:window-titlebar window) widget)}
  @argument[window]{a @class{gtk:window} widget}
  @argument[widget]{a @class{gtk:widget} object to use as titlebar}
  @begin{short}
    Accessor of the @slot[gtk:window]{titlebar} slot of the @class{gtk:window}
    class.
  @end{short}
  The @fun{gtk:window-titlebar} function returns the custom titlebar for the
  window. The @setf{gtk:window-titlebar} function sets a custom titlebar.

  A typical widget used here is the @class{gtk:header-bar} widget, as it
  provides various features expected of a titlebar while allowing the addition
  of child widgets to it.

  If you set a custom titlebar, GTK will do its best to convince the window
  manager not to put its own titlebar on the window. Depending on the system,
  this function may not work for a window that is already visible, so you set
  the titlebar before calling the @fun{gtk:widget-visible} function.

  Since 4.6
  @see-class{gtk:window}
  @see-class{gtk:widget}
  @see-class{gtk:header-bar}
  @see-function{gtk:widget-visible}")

;;; --- gtk:window-transient-for -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "transient-for" 'window) t)
 "The @code{transient-for} property of type @class{gtk:window}
  (Read / Write / Construct) @br{}
  The transient parent of the window.")

#+liber-documentation
(setf (liber:alias-for-function 'window-transient-for)
      "Accessor"
      (documentation 'window-transient-for 'function)
 "@version{2023-9-18}
  @syntax[]{(gtk:window-transient-for object) => parent}
  @syntax[]{(setf (gtk:window-transient-for object) parent)}
  @argument[object]{a @class{gtk:window} widget}
  @argument[parent]{a @class{gtk:window} parent window, or @code{nil}}
  @begin{short}
    Accessor of the @slot[gtk:window]{transient-for} slot of the
    @class{gtk:window} class.
  @end{short}
  The @fun{gtk:window-transient-for} function returns the transient parent for
  the window, or @code{nil} if no transient parent has been set. The
  @setf{gtk:window-transient-for} function sets the parent window.

  Dialog windows should be set transient for the main application window they
  were spawned from. This allows window managers to e.g. keep the dialog on top
  of the main window, or center the dialog over the main window. The
  @fun{gtk:dialog-new-with-buttons} function and other convenience functions
  in GTK will sometimes call the @fun{gtk:window-transient-for} function on
  your behalf.

  Passing @code{nil} for the @arg{parent} argument unsets the current transient
  window.

  On Windows, this function puts the child window on top of the parent, much
  as the window manager would have done on X11.
  @see-class{gtk:window}
  @see-function{gtk:dialog-new-with-buttons}")

;;; ----------------------------------------------------------------------------
;;; gtk_window_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_new" window-new) (g:object widget)
 #+liber-documentation
 "@version{2023-8-20}
  @return{A new @class{gtk:window} widget.}
  @begin{short}
    Creates a new toplevel window, which can contain other widgets.
  @end{short}
  To get an undecorated window, no window borders, use the
  @fun{gtk:window-decorated} function. All top-level windows created by the
  @fun{gtk:window-new} function are stored in an internal top-level window list.
  This list can be obtained from the @fun{gtk:window-list-toplevels} function.

  To delete a window, call the @fun{gtk:window-destroy} function.
  @see-class{gtk:window}
  @see-function{gtk:window-decorated}
  @see-function{gtk:window-list-toplevels}
  @see-function{gtk:window-destroy}")

(export 'window-new)

;;; ----------------------------------------------------------------------------
;;; gtk_window_destroy
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_destroy" window-destroy) :void
 #+liber-documentation
 "@version{2023-9-30}
  @argument[window]{a @class{gtk:window} widget to destroy}
  @short{Drop the internal reference GTK holds on toplevel windows.}
  @begin[Example]{dictionary}
    Signal handler for a cancel button that gets the toplevel window and
    destroys it to quit the window.
    @begin{pre}
(g:signal-connect cancelbutton \"clicked\"
                  (lambda (button)
                    (let ((toplevel (gtk:widget-root button)))
                      (gtk:window-destroy toplevel))))
    @end{pre}
  @end{dictionary}
  @see-class{gtk:window}
  @see-function{gtk:widget-root}"
  (window (g:object window)))

(export 'window-destroy)

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_default_size
;;; gtk_window_set_default_size -> window-default-size
;;; ----------------------------------------------------------------------------

(defun (setf window-default-size) (size window)
  (destructuring-bind (width height) size
     (values (setf (window-default-width window) width)
             (setf (window-default-height window) height))))

(defun window-default-size (window)
 #+liber-documentation
 "@version{2023-9-18}
  @syntax[]{(gtk:window-default-size window) => width, height}
  @syntax[]{(setf (gtk:window-default-size window) (list width height))}
  @argument[window]{a @class{gtk:window} widget}
  @argument[width]{an integer with the default width of the window}
  @argument[height]{an integer with the default height of the window}
  @begin{short}
    Accessor of the default size of a @class{gtk:window} widget.
  @end{short}
  The @fun{gtk:window-default-size} function gets the default size of the
  window. The @setf{gtk:window-default-size} function sets the default size. A
  value of -1 for the width or height indicates that a default size has not been
  explicitly set for that dimension, so the \"natural\" size of the window will
  be used. If the \"natural\" size of the window, its size request, is larger
  than the default, the default will be ignored.

  Unlike the @fun{gtk:widget-size-request} function, which sets a size request
  for a widget and thus would keep users from shrinking the window, this
  function only sets the initial size, just as if the user had resized the
  window themselves. Users can still shrink the window again as they normally
  would. Setting a default size of -1 means to use the \"natural\" default
  size, the size request of the window.

  The default size of a window only affects the first time a window is shown.
  If a window is hidden and re-shown, it will remember the size it had prior
  to hiding, rather than using the default size.

  Windows cannot actually be 0 x 0 in size, they must be at least 1 x 1, but
  passing 0 for width and height is fine, resulting in a 1 x 1 default size.

  If you use this function to reestablish a previously saved window size, note
  that the appropriate size to save is the one returned by the
  @fun{gtk:window-default-size} function. Using the window allocation directly
  will not work in all circumstances and can lead to growing or shrinking
  windows.
  @begin[Example]{dictionary}
   @begin{pre}
(let ((window (make-instance 'gtk:window)))
  (setf (gtk:window-default-size window) '(300 200))
  (gtk:window-default-size window))
=> 300, 200
    @end{pre}
  @end{dictionary}
  @see-class{gtk:window}
  @see-function{gtk:window-default-width}
  @see-function{gtk:window-default-height}
  @see-function{gtk:widget-size-request}"
  (values (window-default-width window)
          (window-default-height window)))

(export 'window-default-size)

;;; ----------------------------------------------------------------------------
;;; gtk_window_is_active
;;; ----------------------------------------------------------------------------

;; Implemented as the window-is-active slot access function

;;; ----------------------------------------------------------------------------
;;; gtk_window_is_maximized
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_is_maximized" window-is-maximized) :boolean
 #+liber-documentation
 "@version{#2023-8-20}
  @argument[window]{a @class{gtk:window} widget}
  @return{A boolean whether the window is maximized.}
  @begin{short}
    Retrieves the current maximized state of the window.
  @end{short}
  Note that since maximization is ultimately handled by the window manager and
  happens asynchronously to an application request, you should not assume the
  return value of this function changing immediately, or at all, as an effect
  of calling the @fun{gtk:window-maximize} or @fun{gtk:window-unmaximize}
  functions.

  If the window is not yet mapped, the value returned will whether the initial
  requested state is maximized.
  @see-class{gtk:window}
  @see-function{gtk:window-maximize}
  @see-function{gtk:window-unmaximize}"
  (window (g:object window)))

(export 'window-is-maximized)

;;; ----------------------------------------------------------------------------
;;; gtk_window_is_fullscreen
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_is_fullscreen" window-is-fullscreen) :boolean
 #+liber-documentation
 "@version{#2023-8-20}
  @argument[window]{a @class{gtk:window} widget}
  @return{A boolean whether the window has a fullscreen state.}
  @begin{short}
    Retrieves the current fullscreen state of window.
  @end{short}

  Note that since fullscreening is ultimately handled by the window manager and
  happens asynchronously to an application request, you should not assume the
  return value of this function changing immediately, or at all, as an effect of
  calling the @fun{gtk:window-fullscreen} or @fun{gtk:window-unfullscreen}
  functions.

  If the window is not yet mapped, the value returned will whether the initial
  requested state is fullscreen.
  @see-class{gtk:window}
  @see-function{gtk:window-fullscreen}
  @see-function{gtk:window-unfullscreen}"
  (window (g:object window)))

(export 'window-is-fullscreen)

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_toplevels -> window-toplevels
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_get_toplevels" window-toplevels)
    (g:object g:list-model)
 #+liber-documentation
 "@version{#2023-8-20}
  @return{A @class{g:list-model} object with the list of toplevel widgets.}
  @begin{short}
    Returns a list of all existing toplevel windows.
  @end{short}
  If you want to iterate through the list and perform actions involving
  callbacks that might destroy the widgets or add new ones, be aware that the
  list of toplevels will change and emit the @code{\"items-changed\"} signal.
  @see-class{gtk:window}
  @see-class{g:list-model}
  @see-function{gtk:window-list-toplevels}")

(export 'window-toplevels)

;;; ----------------------------------------------------------------------------
;;; gtk_window_list_toplevels
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_list_toplevels" window-list-toplevels)
    (g:list-t (g:object window))
 #+liber-documentation
 "@version{2023-8-20}
  @return{A list of toplevel @class{gtk:widget} widgets.}
  @begin{short}
    Returns a list of all existing toplevel windows.
  @end{short}
  @see-class{gtk:window}
  @see-class{gtk:widget}
  @see-function{gtk:window-toplevels}")

(export 'window-list-toplevels)

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_focus
;;; gtk_window_set_focus -> window-focus
;;; ----------------------------------------------------------------------------

(defun (setf window-focus) (focus window)
  (cffi:foreign-funcall "gtk_window_set_focus"
                        (g:object window) window
                        (g:object widget) focus
                        :void)
  focus)

(cffi:defcfun ("gtk_window_get_focus" window-focus) (g:object widget)
 #+liber-documentation
 "@version{#2023-8-20}
  @syntax[]{(gtk:window-focus window) => focus}
  @syntax[]{(setf (gtk:window-focus window) focus)}
  @argument[window]{a @class{gtk:window} widget}
  @argument[focus]{the @class{gtk:widget} object to be the focus widget, or
    @code{nil} to unset any focus widget for the toplevel window}
  @begin{short}
    Accessor of the focus widget of the window.
  @end{short}
  The @fun{gtk:window-focus} function retrieves the current focused widget
  within the window. If the @arg{focus} argument is not the current focus
  widget, and is focusable, the @setf{gtk:window-focus} function sets the focus
  widget.

  If the @arg{focus} argument is @code{nil}, unsets the focus widget for the
  window. To set the focus to a particular widget in the toplevel, it is usually
  more convenient to use the @fun{gtk:widget-grab-focus} function instead of
  this function.

  Note that this is the widget that would have the focus if the toplevel window
  focused. If the toplevel window is not focused then the
  @fun{gtk:widget-has-focus} function will not return @em{true} for the widget.
  @see-class{gtk:window}
  @see-class{gtk:widget}
  @see-function{gtk:widget-grab-focus}
  @see-function{gtk:widget-has-focus}"
  (window (g:object window)))

(export 'window-focus)

;;; ----------------------------------------------------------------------------
;;; gtk_window_present
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_present" window-present) :void
 #+liber-documentation
 "@version{2024-4-1}
  @argument[window]{a @class{gtk:window} widget}
  @begin{short}
    Presents a window to the user.
  @end{short}
  This may mean raising the window in the stacking order, unminimizing it,
  moving it to the current desktop and/or giving it the keyboard focus, possibly
  dependent on the platform of the user, window manager and preferences. If the
  window is hidden, this function also makes it visible.
  @see-class{gtk:window}
  @see-function{gtk:window-present-with-time}"
  (window (g:object window)))

(export 'window-present)

;;; ----------------------------------------------------------------------------
;;; gtk_window_present_with_time
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_present_with_time" window-present-with-time) :void
 #+liber-documentation
 "@version{#2023-8-20}
  @argument[window]{a @class{gtk:window} widget}
  @argument[timestamp]{an unsigned integer with the timestamp of the user
    interaction, typically a button or key press event, which triggered this
    call}
  @begin{short}
    Presents a window to the user in response to an user interaction.
  @end{short}
  This may mean raising the window in the stacking order, unminimizing it,
  moving it to the current desktop, and/or giving it the keyboard focus,
  possibly dependent on the platform of the user, window manager, and
  preferences.

  If the window is hidden, this function also makes it visible.

  The timestamp should be gathered when the window was requested to be shown,
  when clicking a link for example, rather than once the window is ready to be
  shown.
  @see-class{gtk:window}
  @see-function{gtk:window-present}"
  (window (g:object window))
  (timestamp :uint32))

(export 'window-present-with-time)

;;; ----------------------------------------------------------------------------
;;; gtk_window_close
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_close" window-close) :void
 #+liber-documentation
 "@version{2023-8-20}
  @argument[window]{a @class{gtk:window} widget}
  @begin{short}
    Requests that the window is closed.
  @end{short}
  This is similar to what happens when a window manager Close button is clicked.
  This function can be used with Close buttons in custom titlebars.
  @see-class{gtk:window}"
  (window (g:object window)))

(export 'window-close)

;;; ----------------------------------------------------------------------------
;;; gtk_window_minimize
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_minimize" window-minimize) :void
 #+liber-documentation
 "@version{#2023-8-20}
  @argument[window]{a @class{gtk:window} widget}
  @begin{short}
    Asks to minimize the window.
  @end{short}
  Note that you should not assume the window is definitely minimized afterward,
  because other entities, e.g. the user or window manager, could unminimize it
  again, or there may not be a window manager in which case minimization is not
  possible.

  It is permitted to call this function before showing a window, in which case
  the window will be minimized before it ever appears onscreen.

  You can track the result of this operation via the @slot[gdk:toplevel]{state}
  property.
  @see-class{gtk:window}
  @see-function{gdk:toplevel-state}
  @see-function{gtk:window-maximize}"
  (window (g:object window)))

(export 'window-minimize)

;;; ----------------------------------------------------------------------------
;;; gtk_window_unminimize
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_unminimize" window-unminimize) :void
 #+liber-documentation
 "@version{#2023-8-20}
  @argument[window]{a @class{gtk:window} widget}
  @begin{short}
    Asks to unminimize the window.
  @end{short}
  Note that you should not assume the window is definitely unminimized
  afterward, because the windowing system might not support this functionality.
  Other entities, e.g. the user or the window manager could minimize it again,
  or there may not be a window manager in which case minimization is not
  possible.

  You can track the result of this operation via the @slot[gdk:toplevel]{state}
  property.
  @see-class{gtk:window}
  @see-class{gdk:toplevel}
  @see-function{gtk:window-maximized}"
  (window (g:object window)))

(export 'window-unminimize)

;;; ----------------------------------------------------------------------------
;;; gtk_window_maximize
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_maximize" window-maximize) :void
 #+liber-documentation
 "@version{#2023-8-20}
  @argument[window]{a @class{gtk:window} widget}
  @begin{short}
    Asks to maximize the window, so that it becomes full screen.
  @end{short}
  Note that you should not assume the window is definitely maximized afterward,
  because other entities, e.g. the user or window manager could unmaximize it
  again, and not all window managers support maximization.

  It is permitted to call this function before showing a window, in which case
  the window will be maximized when it appears onscreen initially.

  You can track the result of this operation via the @slot[gdk:toplevel]{state}
  property, or by listening to notifications on the
  @slot[gtk:window]{maximized} property.
  @see-class{gtk:window}
  @see-function{gdk:toplevel-state}
  @see-function{gtk:window-maximized}"
  (window (g:object window)))

(export 'window-maximize)

;;; ----------------------------------------------------------------------------
;;; gtk_window_unmaximize
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_unmaximize" window-unmaximize) :void
 #+liber-documentation
 "@version{#2023-8-20}
  @argument[window]{a @class{gtk:window} widget}
  @begin{short}
    Asks to unmaximize the window.
  @end{short}
  Note that you should not assume the window is definitely unmaximized
  afterward, because other entities, e.g. the user or window manager maximize
  it again, and not all window managers honor requests to unmaximize.

  You can track the result of this operation via the @slot[gdk:toplevel]{state}
  property, or by listening to notifications on the @slot[gtk:window]{maximized}
  property.
  @see-class{gtk:window}
  @see-class{gdk:toplevel}
  @see-function{gtk:window-maximized}"
  (window (g:object window)))

(export 'window-unmaximize)

;;; ----------------------------------------------------------------------------
;;; gtk_window_fullscreen
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_fullscreen" window-fullscreen) :void
 #+liber-documentation
 "@version{#2023-8-20}
  @argument[window]{a @class{gtk:window} widget}
  @begin{short}
    Asks to place the window in the fullscreen state.
  @end{short}
  Note that you should not assume the window is definitely full screen
  afterward, because other entities, e.g. the user or window manager, could
  unfullscreen it again, and not all window managers honor requests to
  fullscreen windows.

  You can track the result of this operation via the @slot[gdk:toplevel]{state}
  property, or by listening to notifications of the
  @slot[gtk:window]{fullscreened} property.
  @see-class{gtk:window}
  @see-function{gdk:toplevel-state}
  @see-function{gtk:window-fullscreened}"
  (window (g:object window)))

(export 'window-fullscreen)

;;; ----------------------------------------------------------------------------
;;; gtk_window_fullscreen_on_monitor
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_fullscreen_on_monitor" window-fullscreen-on-monitor)
    :void
 #+liber-documentation
 "@version{#2023-8-20}
  @argument[window]{a @class{gtk:window} widget}
  @argument[monitor]{a @class{gdk:monitor} object to go fullscreen on}
  @begin{short}
    Asks to place the window in the fullscreen state on the given monitor.
  @end{short}
  Note that you should not assume the window is definitely full screen
  afterward, or that the windowing system allows fullscreen windows on any
  given monitor.

  You can track the result of this operation via the @slot[gdk:toplevel]{state}
  property, or by listening to notifications of the
  @slot[gtk:window]{fullscreened} property.
  @see-class{gtk:window}
  @see-class{gdk:monitor}
  @see-function{gdk:toplevel-state}
  @see-function{gtk:window-fullscreened}"
  (window (g:object window))
  (monitor (g:object gdk:monitor)))

(export 'window-fullscreen-on-monitor)

;;; ----------------------------------------------------------------------------
;;; gtk_window_unfullscreen
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_unfullscreen" window-unfullscreen) :void
 #+liber-documentation
 "@version{#2023-8-20}
  @argument[window]{a @class{gtk:window} widget}
  @begin{short}
    Asks to toggle off the fullscreen state for the window.
  @end{short}
  Note that you should not assume the window is definitely not full screen
  afterward, because other entities, e.g. the user or window manager, could
  fullscreen it again, and not all window managers honor requests to
  unfullscreen windows. But normally the window will end up restored to its
  normal state. Just do not write code that crashes if not.

  You can track the result of this operation via the @slot[gdk:toplevel]{state}
  property, or by listening to notifications of the
  @slot[gtk:window]{fullscreened} property.
  @see-class{gtk:window}
  @see-function{gdk:toplevel-state}
  @see-function{gtk:window-fullscreened}"
  (window (g:object window)))

(export 'window-unfullscreen)

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_default_icon_name
;;; gtk_window_set_default_icon_name -> window-default-icon-name
;;; ----------------------------------------------------------------------------

(defun (setf window-default-icon-name) (name)
  (cffi:foreign-funcall "gtk_window_set_defaul_icon_name"
                        :string name
                        :void)
  name)

(cffi:defcfun ("gtk_window_get_default_icon_name" window-default-icon-name)
    :string
 #+liber-documentation
 "@version{#2023-9-18}
  @syntax[]{(gtk:window-default-icon-name) => name}
  @syntax[]{(setf (gtk:window-default-icon-name) name)}
  @argument[name]{a string with the name of the themed icon}
  @begin{short}
    Accessor of the default icon name of the window.
  @end{short}
  The @fun{gtk:window-default-icon-name} function returns the fallback icon
  name for windows. The @setf{gtk:window-default-icon-name} function sets an
  icon to be used as fallback for windows that have not had the
  @fun{gtk:window-icon-name} function called on them.
  @see-class{gtk:window}
  @see-function{gtk:window-icon-name}")

(export 'window-default-icon-name)

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_group -> window-group
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_get_group" window-group)
    (g:object window-group)
 #+liber-documentation
 "@version{#2023-8-20}
  @argument[window]{a @class{gtk:window} widget, or @code{nil}}
  @return{The @class{gtk:window-group} object for the @arg{window} argument or
    the default window group.}
  @begin{short}
    Returns the window group for the window
  @end{short}
  If the window has no window group, then the default window group is returned.
  @see-class{gtk:window}
  @see-class{gtk:window-group}"
  (window (g:object window)))

(export 'window-group)

;;; ----------------------------------------------------------------------------
;;; gtk_window_has_group
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_has_group" window-has-group) :boolean
 #+liber-documentation
 "@version{#2023-8-20}
  @argument[window]{a @class{gtk:window} widget}
  @return{@em{True} if the @arg{window} argument has an explicit window group.}
  @short{Returns whether the window has an explicit window group.}
  @see-class{gtk:window}
  @see-class{gtk:window-group}"
  (window (g:object window)))

(export 'window-has-group)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_auto_startup_notification
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_set_auto_startup_notification"
               window-set-auto-startup-notification) :void
 #+liber-documentation
 "@version{#2023-8-20}
  @argument[setting]{@em{true} to automatically do startup notification}
  @begin{short}
    Call this function to disable the automatic startup notification.
  @end{short}
  By default, after showing the first window, GTK calls the
  @fun{gdk:display-notify-startup-complete} function. Call this function to
  disable the automatic startup notification. You might do this if your first
  window is a splash screen, and you want to delay notification until after your
  real main window has been shown, for example.

  In that example, you would disable startup notification temporarily, show your
  splash screen, then re-enable it so that showing the main window would
  automatically result in notification.
  @see-class{gtk:window}
  @see-function{gdk:display-notify-startup-complete}"
  (setting :boolean))

(export 'window-set-auto-startup-notification)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_titlebar                                Until 4.4
;;; gtk_window_get_titlebar -> window-titlebar
;;; ----------------------------------------------------------------------------

#-gtk-4-6
(defun (setf window-titlebar) (widget window)
  (cffi:foreign-funcall "gtk_window_set_titlebar"
                        (g:object window) window
                        (g:object widget) widget
                        :void)
  widget)

#-gtk-4-6
(cffi:defcfun ("gtk_window_get_titlebar" window-titlebar) (g:object widget)
 #+liber-documentation
 "@version{#2023-9-18}
  @syntax[]{(gtk:window-titlebar window) => widget}
  @syntax[]{(setf (gtk:window-titlebar window) widget)}
  @argument[window]{a @class{gtk:window} widget}
  @argument[widget]{a @class{gtk:widget} object to use as titlebar}
  @begin{short}
    Accessor of the custom titlebar widget of the window.
  @end{short}
  The @fun{gtk:window-titlebar} function returns the custom titlebar for the
  window. The @setf{gtk:window-titlebar} function sets a custom titlebar.

  A typical widget used here is the @class{gtk:header-bar} widget, as it
  provides various features expected of a titlebar while allowing the addition
  of child widgets to it.

  If you set a custom titlebar, GTK will do its best to convince the window
  manager not to put its own titlebar on the window. Depending on the system,
  this function may not work for a window that is already visible, so you set
  the titlebar before calling the @fun{gtk:widget-show} function.
  @see-class{gtk:window}
  @see-class{gtk:widget}
  @see-class{gtk:header-bar}
  @see-function{gtk:widget-show}"
  (window (g:object window)))

#-gtk-4-6
(export 'window-titlebar)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_interactive_debugging -> window-set-interactice-debugging
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_set_interactive_debugging"
               window-set-interactive-debugging) :void
 #+liber-documentation
 "@version{#2023-8-20}
  @argument[enable]{@em{true} to enable interactice debugging}
  @begin{short}
    Opens or closes the interactive debugger.
  @end{short}
  The debugger offers access to the widget hierarchy of the application and to
  useful debugging tools.
  @see-class{gtk:window}"
  (enable :boolean))

(export 'window-set-interactive-debugging)

;;; ----------------------------------------------------------------------------
;;; gtk_window_is_suspended
;;; ----------------------------------------------------------------------------

#+gtk-4-12
(cffi:defcfun ("gtk_window_is_suspended" window-is-suspended) :boolean
 #+liber-documentation
 "@version{#2023-8-20}
  @argument[window]{a @class{gtk:window} widget}
  @return{A boolean whether the window is suspended.}
  @begin{short}
    Retrieves the current suspended state of the window.
  @end{short}
  A window being suspended means it is currently not visible to the user, for
  example by being on a inactive workspace, minimized, obstructed.

  Since 4.12
  @see-class{gtk:window}"
  (window (g:object window)))

#+gtk-4-12
(export 'window-is-suspended)

;;; --- End of file gtk4.window.lisp -------------------------------------------

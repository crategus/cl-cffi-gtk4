;;; ----------------------------------------------------------------------------
;;; gtk4.application-window.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.6 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2013 - 2023 Dieter Kaiser
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
;;; GtkApplicationWindow
;;;
;;;     GtkWindow subclass with GtkApplication support
;;;
;;; Types and Values
;;;
;;;     GtkApplicationWindow
;;;
;;; Accessors
;;;
;;;     gtk_application_window_get_show_menubar
;;;     gtk_application_window_set_show_menubar
;;;
;;; Functions
;;;
;;;     gtk_application_window_new
;;;     gtk_application_window_get_id
;;;     gtk_application_window_get_help_overlay
;;;     gtk_application_window_set_help_overlay
;;;
;;; Properties
;;;
;;;     show-menubar
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkWindow
;;;                 ╰── GtkApplicationWindow
;;;
;;; Implemented Interfaces
;;;
;;;     GActionGroup
;;;     GActionMap
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;;     GtkNative
;;;     GtkRoot
;;;     GtkShortcutManager
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkApplicationWindow
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkApplicationWindow" application-window
  (:superclass window
   :export t
   :interfaces ("GActionGroup"
                "GActionMap"
                "GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget"
                "GtkNative"
                "GtkRoot"
                "GtkShortcutManager")
   :type-initializer "gtk_application_window_get_type")
   ((show-menubar
     application-window-show-menubar
     "show-menubar" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'application-window 'type)
 "@version{2023-3-11}
  @begin{short}
    The @sym{gtk:application-window} class is a @class{gtk:window} subclass
    that offers some extra functionality for better integration with
    @class{gtk:application} features.
  @end{short}
  Notably, it can handle an application menubar.

  This class implements the @class{g:action-group} and @class{g:action-map}
  interfaces, to let you add window specific actions that will be exported by
  the associated @class{gtk:application} instance, together with its
  application-wide actions. Window specific actions are prefixed with the
  \"win.\" prefix and application-wide actions are prefixed with the \"app.\"
  prefix. Actions must be addressed with the prefixed name when referring to
  them from a @class{g:menu-model} object.

  Note that widgets that are placed inside an application window can also
  activate these actions, if they implement the @class{gtk:actionable}
  interface.

  The @slot[gtk:settings]{gtk-shell-shows-app-menu} and
  @slot[gtk:settings]{gtk-shell-shows-menubar} settings tell GTK whether the
  desktop environment is showing the application menu and menubar models outside
  the application as part of the desktop shell. For instance, on OS X, both
  menus will be displayed remotely. On Windows neither will be.

  If the desktop environment does not display the menubar, then the application
  window will automatically show a menubar for it. This behaviour can be
  overridden with the @code{show-menubar} property. If the desktop environment
  does not display the application menu, then it will automatically be included
  in the menubar or in the windows client-side decorations.

  See the @class{gtk:popover-menu} documentation for information about the XML
  language used by the @class{gtk:builder} object for menu models.
  @begin[Example]{dictionary}
    An application window with a menubar.
    @begin{pre}
(let ((builder (make-instance 'gtk:builder))
      (window (make-instance 'gtk:application-window
                             :application application
                             :title \"Application Window Menubar\"
                             :show-menubar t)))
  ;; Read the menus from a string
  (gtk:builder-add-from-string
      builder
      (format nil
  \"<interface> ~
     <menu id='menubar'> ~
       <submenu> ~
         <attribute name='label' translatable='yes'>_Edit</attribute> ~
         <item> ~
           <attribute name='label' translatable='yes'>_Copy</attribute> ~
           <attribute name='action'>win.copy</attribute> ~
         </item> ~
         <item> ~
           <attribute name='label' translatable='yes'>_Paste</attribute> ~
           <attribute name='action'>win.paste</attribute> ~
         </item> ~
       </submenu> ~
     </menu> ~
   </interface>\"))
  ;; Set the menubar
  (setf (gtk:application-menubar application)
        (gtk:builder-object builder \"menubar\"))
    @end{pre}
  @end{dictionary}
  @see-slot{gtk:application-window-show-menubar}
  @see-constructor{gtk:application-window-new}
  @see-class{gtk:window}
  @see-class{gtk:application}
  @see-class{g:action-group}
  @see-class{g:action-map}
  @see-class{g:menu-model}
  @see-class{gtk:actionable}
  @see-class{gtk:builder}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- application-window-show-menubar ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "show-menubar"
                                               'application-window) t)
 "The @code{show-menubar} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  If this property is @em{true}, the application window will display a menubar
  unless it is shown by the desktop shell. If @em{false}, the applicaton window
  will not display a menubar, regardless of whether the desktop shell is showing
  it or not. @br{}
  Default value: @code{true}")

#+liber-documentation
(setf (liber:alias-for-function 'application-window-show-menubar)
      "Accessor"
      (documentation 'application-window-show-menubar 'function)
 "@version{2023-3-11}
  @syntax[]{(gtk:application-window-show-menubar object) => show}
  @syntax[]{(setf (gtk:application-window-show-menubar object) show)}
  @argument[window]{a @class{gtk:application-window} widget}
  @argument[show]{a boolean whether to show a menubar when needed}
  @begin{short}
    Accessor of the @slot[gtk:application-window]{show-menubar} slot of the
    @class{gtk:application-window} class.
  @end{short}
  The @sym{gtk:application-window-show-menubar} function returns whether the
  window will display a menubar for the application menu and menubar as needed.
  The @sym{(setf gtk:application-window-show-menubar)} function sets whether
  the window will display a menubar.
  @see-class{gtk:application-window}")

;;; ----------------------------------------------------------------------------
;;; gtk_application_window_new
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_application_window_new" application-window-new) (g:object widget)
 #+liber-documentation
 "@version{2023-3-11}
  @argument[application]{a @class{gtk:application} instance}
  @return{A newly created @class{gtk:application-window} widget.}
  @short{Creates a new application window.}
  @see-class{gtk:application}
  @see-class{gtk:application-window}"
  (application (g:object gtk:application)))

(export 'application-window-new)

;;; ----------------------------------------------------------------------------
;;; gtk_application_window_get_id -> application-window-id
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_application_window_get_id" application-window-id) :uint
 #+liber-documentation
 "@version{2023-3-11}
  @argument[window]{a @class{gtk:application-window} widget}
  @begin{return}
    The unique ID for @arg{window}, or 0 if @arg{window} has not yet been
    added to a @class{gtk:application} instance.
  @end{return}
  @begin{short}
    Returns the unique ID of the application window.
  @end{short}
  If the application window has not yet been added to a @class{gtk:application}
  instance, returns 0.
  @see-class{gtk:application}
  @see-class{gtk:application-window}"
  (window (g:object application-window)))

(export 'application-window-id)

;;; ----------------------------------------------------------------------------
;;; gtk_application_window_get_help_overlay
;;; gtk_application_window_set_help_overlay -> application-window-help-overlay
;;; ----------------------------------------------------------------------------

(defun (setf application-window-help-overlay) (overlay window)
  (cffi:foreign-funcall "gtk_application_window_set_help_overlay"
                        (g:object application-window) window
                        (g:object shortcuts-window) overlay
                        :void)
  overlay)

(defcfun ("gtk_application_window_get_help_overlay"
           application-window-help-overlay) (g:object shortcuts-window)
 #+liber-documentation
 "@version{2023-3-11}
  @syntax[]{(gtk:application-window-help-overlay window) => overlay}
  @syntax[]{(setf (gtk:application-window-help-overlay window) overlay)}
  @argument[window]{a @class{gtk:application-window} widget}
  @argument[overlay]{a @class{gtk:shortcuts-window} widget}
  @begin{short}
    Accessor of the shortcuts window associated with the application window.
  @end{short}
  The @sym{gtk:application-window-help-overlay} function gets the shortcuts
  window. The @sym{(setf gtk:applicaton-window-help-overlay)} function
  associates a shortcuts window with the application window, and sets up an
  action with the name \"win.show-help-overlay\" to present it.
  @see-class{gtk:application-window}
  @see-class{gtk:shortcuts-window}"
  (window (g:object application-window)))

(export 'application-window-help-overlay)

;;; --- End of file gtk4.application-window.lisp -------------------------------

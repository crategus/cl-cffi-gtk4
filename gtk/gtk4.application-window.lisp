;;; ----------------------------------------------------------------------------
;;; gtk4.application-window.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.20 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2013 - 2026 Dieter Kaiser
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
;;;     gtk_application_window_get_help_overlay             Deprecated 4.18
;;;     gtk_application_window_set_help_overlay             Deprecated 4.18
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

(gobject:define-gobject "GtkApplicationWindow" application-window
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
 "@version{2025-07-11}
  @begin{short}
    The @class{gtk:application-window} class is a @class{gtk:window} subclass
    that offers some extra functionality for better integration with
    @class{gtk:application} features.
  @end{short}
  Notably, it can handle an application menubar.

  This class implements the @class{g:action-group} and @class{g:action-map}
  interfaces, to let you add window specific actions that will be exported by
  the associated @class{gtk:application} instance, together with its
  application-wide actions. Window specific actions are prefixed with the
  @code{\"win.\"} prefix and application-wide actions are prefixed with the
  @code{\"app.\"} prefix. Actions must be addressed with the prefixed name when
  referring to them from a @class{g:menu-model} object.

  Note that widgets that are placed inside an application window can also
  activate these actions, if they implement the @class{gtk:actionable}
  interface.

  The @slot[gtk:settings]{gtk-shell-shows-app-menu} and
  @slot[gtk:settings]{gtk-shell-shows-menubar} settings tell GTK whether the
  desktop environment is showing the application menu and menubar models
  outside the application as part of the desktop shell. For instance, on OS X,
  both menus will be displayed remotely. On Windows neither will be.

  If the desktop environment does not display the menubar, then the application
  window will automatically show a menubar for it. This behaviour can be
  overridden with the @slot[gtk:application-window]{show-menubar} property. If
  the desktop environment does not display the application menu, then it will
  automatically be included in the menubar or in the windows client-side
  decorations.

  See the @class{gtk:popover-menu} documentation for information about the XML
  language used by the @class{gtk:builder} object for menu models.
  @begin[Examples]{dictionary}
    Create an application window with a menubar.
    @begin{pre}
(defun do-application-window (&optional application)
  (let ((menu
         \"<interface>
           <menu id='menubar'>
             <submenu>
               <attribute name='label'>_Edit</attribute>
               <item>
                 <attribute name='label'>_Copy</attribute>
                 <attribute name='action'>win.copy</attribute>
               </item>
               <item>
                 <attribute name='label'>_Paste</attribute>
                 <attribute name='action'>win.paste</attribute>
               </item>
             </submenu>
           </menu>
         </interface>\")
        (builder (make-instance 'gtk:builder))
        (window (make-instance 'gtk:application-window
                               :application application
                               :title \"Application Window\"
                               :show-menubar t)))
    ;; Read the menu from a string
    (gtk:builder-add-from-string builder menu)
    ;; Set the menubar
    (setf (gtk:application-menubar application)
          (gtk:builder-object builder \"menubar\"))
    ;; Present the application window
    (gtk:window-present window)))
    @end{pre}
  @end{dictionary}
  @see-slot{gtk:application-window-show-menubar}
  @see-constructor{gtk:application-window-new}
  @see-class{gtk:window}
  @see-class{gtk:application}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:application-window-show-menubar ------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "show-menubar"
                                               'application-window) t)
 "The @code{show-menubar} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  If this property is @em{true}, the application window will display a menubar
  unless it is shown by the desktop shell. If @em{false}, the applicaton window
  will not display a menubar, regardless of whether the desktop shell is
  showing it or not. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'application-window-show-menubar)
      "Accessor"
      (documentation 'application-window-show-menubar 'function)
 "@version{2025-07-28}
  @syntax{(gtk:application-window-show-menubar object) => show}
  @syntax{(setf (gtk:application-window-show-menubar object) show)}
  @argument[window]{a @class{gtk:application-window} widget}
  @argument[show]{a boolean whether to show a menubar when needed}
  @begin{short}
    The accessor for the @slot[gtk:application-window]{show-menubar} slot of the
    @class{gtk:application-window} class gets or sets whether the window will
    display a menubar for the application menu and menubar as needed.
  @end{short}
  @see-class{gtk:application-window}")

;;; ----------------------------------------------------------------------------
;;; gtk_application_window_new
;;; ----------------------------------------------------------------------------

(defun application-window-new (&optional application)
 #+liber-documentation
 "@version{2025-07-11}
  @argument[application]{an optional @class{gtk:application} instance}
  @return{The newly created @class{gtk:application-window} widget.}
  @short{Creates a new application window.}
  New application windows must be added to an application after the
  @sig[g:application]{startup} signal has been emitted. See also the
  @fun{gtk:application-add-window} documentation.
  @see-class{gtk:application}
  @see-class{gtk:application-window}
  @see-function{gtk:application-add-window}"
  (make-instance 'application-window
                 :application application))

(export 'application-window-new)

;;; ----------------------------------------------------------------------------
;;; gtk_application_window_get_id
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_application_window_get_id" application-window-id) :uint
 #+liber-documentation
 "@version{2025-07-11}
  @argument[window]{a @class{gtk:application-window} widget}
  @begin{return}
    The unsigned integer for the unique ID of @arg{window}, or 0 if @arg{window}
    has not yet been added to a @class{gtk:application} instance.
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
;;; gtk_application_window_get_help_overlay                 Deprecated 4.18
;;; gtk_application_window_set_help_overlay
;;; ----------------------------------------------------------------------------

;; FIXME: We have a problem with the memory management. When destroying the
;; objects related to the GtkShortcutsWindow an error occured which crashes
;; the testsuite.

(defun (setf application-window-help-overlay) (overlay window)
  #+(and gtk-4-18 gtk-warn-deprecated)
  (when gtk-init:*warn-deprecated*
    (warn "GTK:APPLICATION-WINDOW-HELP-OVERLAY is deprecated since 4.18."))
  (cffi:foreign-funcall "gtk_application_window_set_help_overlay"
                        (g:object application-window) window
                        (g:object shortcuts-window) overlay
                        :void)
  overlay)

(cffi:defcfun ("gtk_application_window_get_help_overlay"
               %application-window-help-overlay) (g:object shortcuts-window)
  (window (g:object application-window)))

(defun application-window-help-overlay (window)
 #+liber-documentation
 "@version{2025-08-03}
  @syntax{(gtk:application-window-help-overlay window) => overlay}
  @syntax{(setf (gtk:application-window-help-overlay window) overlay)}
  @argument[window]{a @class{gtk:application-window} widget}
  @argument[overlay]{a @class{gtk:shortcuts-window} widget}
  @begin{short}
    Gets the shortcuts window, or associates a shortcuts window with the
    application window, and sets up an action with the name
    @code{\"win.show-help-overlay\"} to present it.
  @end{short}
  The window takes responsibility for destroying the help overlay.
  @begin[Warning]{dictionary}
    This function is deprecated since 4.18. The @class{gtk:shortcuts-window}
    widget will be removed in GTK 5.
  @end{dictionary}
  @see-class{gtk:application-window}
  @see-class{gtk:shortcuts-window}"
  #+(and gtk-4-18 gtk-warn-deprecated)
  (when gtk-init:*warn-deprecated*
    (warn "GTK:APPLICATION-WINDOW-HELP-OVERLAY is deprecated since 4.18."))
  (%application-window-help-overlay window))

(export 'application-window-help-overlay)

;;; --- End of file gtk4.application-window.lisp -------------------------------

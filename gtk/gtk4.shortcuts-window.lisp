;;; ----------------------------------------------------------------------------
;;; gtk4.shortcuts-window.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.18 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2019 - 2025 Dieter Kaiser
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
;;; GtkShortcutsWindow
;;;
;;;     Toplevel which shows help for shortcuts
;;;
;;; Types and Values
;;;
;;;     GtkShortcutsWindow
;;;
;;; Functions
;;;
;;;     gtk_shortcuts_window_add_section                    Since 4.14
;;;
;;; Properties
;;;
;;;     section-name
;;;     view-name
;;;
;;; Signals
;;;
;;;     close
;;;     search
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkWindow
;;;                 ╰── GtkShortcutsWindow
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;;     GtkNative
;;;     GtkShortcutManager
;;;     GtkRoot
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkShortcutsWindow
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkShortcutsWindow" shortcuts-window
  (:superclass window
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget"
                "GtkNative"
                "GtkRoot"
                "GtkShortcutManager")
   :type-initializer "gtk_shortcuts_window_get_type")
   ((section-name
    shortcuts-window-section-name
    "section-name" "gchararray" t t)
   (view-name
    shortcuts-window-view-name
    "view-name" "gchararray" t t)))

#+(and gtk-4-18 gtk-warn-deprecated)
(defmethod initialize-instance :after ((obj shortcuts-window) &key)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:SHORTCUTS-WINDOW is deprecated since 4.18")))

#+liber-documentation
(setf (documentation 'shortcuts-window 'type)
 "@version{2025-07-01}
  @begin{short}
    The @class{gtk:shortcuts-window} widget shows brief information about the
    keyboard shortcuts and gestures of an application.
  @end{short}
  The shortcuts can be grouped, and you can have multiple sections in this
  window, corresponding to the major modes of your application.

  Additionally, the shortcuts can be filtered by the current view, to avoid
  showing information that is not relevant in the current application context.

  The recommended way to construct a shortcuts window is with a
  @class{gtk:builder} UI definition, by populating a shortcuts window with one
  or more @class{gtk:shortcuts-section} objects, which contain
  @class{gtk:shortcuts-group} objects that in turn contain objects of the
  @class{gtk:shortcuts-shortcut} class.
  @begin[Examples]{dictionary}
    A simple example:
    This example has as single section. As you can see, the shortcut groups are
    arranged in columns, and spread across several pages if there are too many
    to find on a single page. The @code{.ui} file for this example can be found
    @url[https://gitlab.gnome.org/GNOME/gtk/-/blob/master/demos/gtk-demo/shortcuts-gedit.ui]{here}.

    @image[gedit-shortcuts]{Figure: GEdit shortcuts}

    An example with multiple views:
    This example shows a shortcuts window that has been configured to show only
    the shortcuts relevant to the \"stopwatch\" view. The @code{.ui} file for
    this example can be found
    @url[https://gitlab.gnome.org/GNOME/gtk/-/blob/master/demos/gtk-demo/shortcuts-clocks.ui]{here}.

    @image[clocks-shortcuts]{Figure: Clock shortcuts}

    An example with multiple sections:
    This example shows a shortcuts window with two sections,
    \"Editor Shortcuts\" and \"Terminal Shortcuts\". The @code{.ui} file for
    this example can be found
    @url[https://gitlab.gnome.org/GNOME/gtk/-/blob/master/demos/gtk-demo/shortcuts-builder.ui]{here}.

    @image[builder-shortcuts]{Figure: Builder shortcuts}
  @end{dictionary}
  @begin[Warning]{dictionary}
    The @class{gtk:shortcuts-window} implementation is deprecated since 4.18.
    This widget will be removed in GTK 5.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @begin[shortcuts-window::close]{signal}
      @begin{pre}
lambda (shortcutswindow)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[shortcutswindow]{The @class{gtk:shortcuts-window} object.}
      @end{simple-table}
      The signal is a keybinding signal which gets emitted when the user uses a
      keybinding to close the window. The default binding for this signal is the
      @kbd{Escape} key.
    @end{signal}
    @begin[shortcuts-window::search]{signal}
      @begin{pre}
lambda (shortcutswindow)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[shortcutswindow]{The @class{gtk:shortcuts-window} object.}
      @end{simple-table}
      The signal is a keybinding signal which gets emitted when the user uses a
      keybinding to start a search. The default binding for this signal is the
      @kbd{Control-F} key.
    @end{signal}
  @end{dictionary}
  @see-slot{gtk:shortcuts-window-section-name}
  @see-slot{gtk:shortcuts-window-view-name}
  @see-class{gtk:shortcuts-section}
  @see-class{gtk:shortcuts-group}
  @see-class{gtk:shortcuts-shortcut}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:shortcuts-window-section-name --------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "section-name"
                      'shortcuts-window) t)
 "The @code{section-name} property of type @code{:string} (Read / Write) @br{}
  The name of the section to show. This should be the section name of one of the
  @class{gtk:shortcuts-section} objects that are in this shortcuts window. @br{}
  Default value: @code{\"internal-search\"}")

#+liber-documentation
(setf (liber:alias-for-function 'shortcuts-window-section-name)
      "Accessor"
      (documentation 'shortcuts-window-section-name 'function)
 "@version{2025-05-14}
  @syntax{(gtk:shortcuts-window-section-name object) => name}
  @syntax{(setf (gtk:shortcuts-window-section-name object) name)}
  @argument[object]{a @class{gtk:shortcuts-window} widget}
  @argument[name]{a string for a name of the section to show}
  @begin{short}
    Accessor of the @slot[gtk:shortcuts-window]{section-name} slot of the
    @class{gtk:shortcuts-window} class.
  @end{short}
  The name of the section to show. This should be the section name of one of the
  @class{gtk:shortcuts-section} objects that are in this shortcuts window.
  @begin[Warning]{dictionary}
    The @fun{gtk:shortcuts-window-section-name} function is deprecated since
    4.18. This widget will be removed in GTK 5.
  @end{dictionary}
  @see-class{gtk:shortcuts-window}
  @see-class{gtk:shortcuts-section}")

;;; --- gtk:shortcuts-window-view-name -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "view-name" 'shortcuts-window) t)
 "The @code{view-name} property of type @code{:string} (Read / Write) @br{}
  The view name by which to filter the contents. This should correspond to the
  @slot[gtk:shortcuts-group]{view} property of some of the
  @class{gtk:shortcuts-group} objects that are inside this shortcuts window.
  Set this to @code{nil} to show all groups. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'shortcuts-window-view-name)
      "Accessor"
      (documentation 'shortcuts-window-view-name 'function)
 "@version{2025-05-14}
  @syntax{(gtk:shortcuts-window-view-name object) => name}
  @syntax{(setf (gtk:shortcuts-window-view-name object) name)}
  @argument[object]{a @class{gtk:shortcuts-window} widget}
  @argument[name]{a string for the view name by which to filter the contents}
  @begin{short}
    Accessor of the @slot[gtk:shortcuts-window]{view-name} slot of the
    @class{gtk:shortcuts-window} class.
  @end{short}
  The view name by which to filter the contents. This should correspond to the
  @slot[gtk:shortcuts-group]{view} property of some of the
  @class{gtk:shortcuts-group} objects that are inside this shortcuts window.
  Set this to @code{nil} to show all groups.
  @begin[Warning]{dictionary}
    The @fun{gtk:shortcuts-window-view-name} function is deprecated since
    4.18. This widget will be removed in GTK 5.
  @end{dictionary}
  @see-class{gtk:shortcuts-window}
  @see-class{gtk:shortcuts-group}")

;;; ----------------------------------------------------------------------------
;;; gtk_shortcuts_window_add_section
;;; ----------------------------------------------------------------------------

#+gtk-4-14
(cffi:defcfun ("gtk_shortcuts_window_add_section" shortcuts-window-add-section)
    :void
 #+liber-documentation
 "@version{#2025-05-14}
  @argument[window]{a @class{gtk:shortcuts-window} widget}
  @argument[section]{a @class{gtk:shortcuts-section} widget to add}
  @begin{short}
    Adds a section to the shortcuts window.
  @end{short}
  This is the programmatic equivalent to using a @class{gtk:builder} UI
  definition and a @code{<child>} tag to add the child. Using the
  @fun{gtk:window-child} function is not appropriate as the shortcuts window
  manages its children internally.

  Since 4.14
  @begin[Warning]{dictionary}
    The @fun{gtk:shortcuts-window-add-section} function is deprecated since
    4.18. This widget will be removed in GTK 5.
  @end{dictionary}
  @see-class{gtk:shortcuts-window}
  @see-class{gtk:shortcuts-section}"
  (window (g:object shortcuts-window))
  (section (g:object shortcuts-section)))

#+gtk-4-14
(export 'shortcuts-window-add-section)

;;; --- End of file gtk4.shortcuts-window.lisp ---------------------------------

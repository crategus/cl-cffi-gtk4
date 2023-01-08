;;; ----------------------------------------------------------------------------
;;; gtk.search-bar.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2019 - 2022 Dieter Kaiser
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
;;; GtkSearchBar
;;;
;;;     A toolbar to integrate a search entry with
;;;
;;; Types and Values
;;;
;;;     GtkSearchBar
;;;
;;; Accessors
;;;
;;;     gtk_search_bar_set_child
;;;     gtk_search_bar_get_child
;;;     gtk_search_bar_set_key_capture_widget
;;;     gtk_search_bar_get_key_capture_widget
;;;     gtk_search_bar_get_show_close_button
;;;     gtk_search_bar_set_show_close_button
;;;
;;; Functions
;;;
;;;     gtk_search_bar_new
;;;     gtk_search_bar_connect_entry
;;;     gtk_search_bar_get_search_mode
;;;     gtk_search_bar_set_search_mode
;;;
;;; Properties
;;;
;;;     child
;;;     key-capture-widget
;;;     search-mode-enabled
;;;     show-close-button
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkSearchBar
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkSearchBar
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkSearchBar" search-bar
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget")
   :type-initializer "gtk_search_bar_get_type")
  ((child
    search-bar-child
    "child" "GtkWidget" t t)
   (key-capture-widget
    search-bar-key-capture-widget
    "key-capture-widget" "GtkWidget" t t)
   (search-mode-enabled
    search-bar-search-mode-enabled
    "search-mode-enabled" "gboolean" t t)
   (show-close-button
    search-bar-show-close-button
    "show-close-button" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'search-bar 'type)
 "@version{#2020-6-1}
  @begin{short}
    @sym{gtk:search-bar} is a container made to have a search entry, possibly
    with additional connex widgets, such as drop-down menus, or buttons
    built-in.
  @end{short}
  The search bar would appear when a search is started through typing on the
  keyboard, or the application’s search mode is toggled on.

  @image[search-bar]{}

  For keyboard presses to start a search, events will need to be forwarded from
  the toplevel window that contains the search bar. See the
  @func{gtk:search-bar-handle-event} function for example code. Common shortcuts
  such as the @kbd{Ctrl+F} should be handled as an application action, or
  through the menu items.

  For keyboard presses to start a search, the search bar must be told of a
  widget to capture key events from through the
  @fun{gtk:search-bar-key-capture-widget} function. This widget will typically
  be the top-level window, or a parent container of the search bar. Common
  shortcuts such as the @kbd{Ctrl+F} key should be handled as an application
  action, or through the menu items.

  You will also need to tell the search bar about which entry you are using as
  your search entry using the @fun{gtk:search-bar-connect-entry} function. The
  following example shows you how to create a more complex search entry.

  @begin[CSS nodes]{dictionary}
    @begin{pre}
searchbar
╰── revealer
    ╰── box
         ├── [child]
         ╰── [button.close]
    @end{pre}
    The @sym{gtk:search-bar} implementation has a single CSS node with name
    @code{searchbar}. It has a child node with name @code{revealer} that
    contains a node with name @code{box}. The box node contains both the CSS
    node of the child widget as well as an optional button node which gets
    the @code{.close} style class applied.
  @end{dictionary}
  @begin[Accessibility]{dictionary}
    The @sym{gtk:search-bar} implementation uses the @code{:search} role of the
    @symbol{gtk:accessible-role} enumeration.
  @end{dictionary}
  @begin[Example]{dictionary}
    Creating a search bar.
    @begin{pre}
#include <gtk/gtk.h>

static void
activate_cb (GtkApplication *app, gpointer user_data)
{
  GtkWidget *window;
  GtkWidget *search_bar;
  GtkWidget *box;
  GtkWidget *entry;
  GtkWidget *menu_button;

  window = gtk_application_window_new (app);
  gtk_widget_show (window);

  search_bar = gtk_search_bar_new ();
  gtk_widget_set_valign (search_bar, GTK_ALIGN_START);
  gtk_window_set_child (GTK_WINDOW (window), search_bar);
  gtk_widget_show (search_bar);

  box = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 6);
  gtk_search_bar_set_child (GTK_SEARCH_BAR (search_bar), box);

  entry = gtk_search_entry_new ();
  gtk_widget_set_hexpand (entry, TRUE);
  gtk_box_append (GTK_BOX (box), entry);

  menu_button = gtk_menu_button_new ();
  gtk_box_append (GTK_BOX (box), menu_button);

  gtk_search_bar_connect_entry (GTK_SEARCH_BAR (search_bar), GTK_EDITABLE (entry));
  gtk_search_bar_set_key_capture_widget (GTK_SEARCH_BAR (search_bar), window);
@}

int
main (int argc, char *argv[])
{
  GtkApplication *app;

  app = gtk_application_new (\"org.gtk.Example.GtkSearchBar\",
      G_APPLICATION_FLAGS_NONE);
  g_signal_connect (app, \"activate\",
      G_CALLBACK (activate_cb), NULL);

  return g_application_run (G_APPLICATION (app), argc, argv);
@}
    @end{pre}
  @end{dictionary}
  @see-slot{gtk:search-bar-child}
  @see-slot{gtk:search-bar-key-capture-widget}
  @see-slot{gtk:search-bar-search-mode-enabled}
  @see-slot{gtk:search-bar-show-close-button}
  @see-constructor{gtk:search-bar-new}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- search-bar-child ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "child" 'search-bar) t)
 "The @code{child} property of type @class{gtk:widget}
  (Read / Write / Construct) @br{}
  The child widget of the search bar.")

#+liber-documentation
(setf (liber:alias-for-function 'search-bar-child)
      "Accessor"
      (documentation 'search-bar-child 'function)
 "@version{#2011-6-12}
  @syntax[]{(gtk:search-bar-child object) => child}
  @syntax[]{(setf (gtk:search-bar-child object) child)}
  @argument[object]{a @class{gtk:search-bar} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @begin{short}
    Accessor of the @slot[gtk:search-bar]{child} slot of the
    @class{gtk:search-bar} class.
  @end{short}

  The @sym{gtk:search-bar-child} function gets the child widget of the search
  bar. The @sym{(setf gtk:search-bar-child)} sets the child widget.
  @see-class{gtk:search-bar}")

;;; --- search-bar-key-capture-widget --------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "key-capture-widget"
                                               'search-bar) t)
 "The @code{key-capture-widget} property of type @class{gtk:widget}
  (Read / Write / Construct) @br{}
  The widget that the search bar will capture key events from.")

#+liber-documentation
(setf (liber:alias-for-function 'search-bar-key-capture-widget)
      "Accessor"
      (documentation 'search-bar-key-capture-widget 'function)
 "@version{#2011-6-12}
  @syntax[]{(gtk:search-bar-key-capture-widget object) => widget}
  @syntax[]{(setf (gtk:search-bar-key-capture-widget object) widget)}
  @argument[object]{a @class{gtk:search-bar} widget}
  @argument[widget]{a @class{gtk:widget} key capture widget}
  @begin{short}
    Accessor of the @slot[gtk:search-bar]{key-capture-widget} slot of the
    @class{gtk:search-bar} class.
  @end{short}

  The @sym{gtk:search-bar-key-capture-widget} function gets the widget that the
  search bar is capturing key events from. The
  @sym{(setf gtk:search-bar-key-capture-widget)} function sets @arg{widget} as
  the widget that the search bar will capture key events from.

  If key events are handled by the search bar, the search bar will be shown,
  and the search entry populated with the entered text.
  @see-class{gtk:search-bar}")

;;; --- search-bar-search-mode-enabled -------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "search-mode-enabled"
                                               'search-bar) t)
 "The @code{search-mode-enabled} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether the search mode is on and the search bar shown. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'search-bar-search-mode-enabled)
      "Accessor"
      (documentation 'search-bar-search-mode-enabled 'function)
 "@version{#2020-6-1}
  @syntax[]{(gtk:search-bar-search-mode-enabled object) => search-mode}
  @syntax[]{(setf (gtk:search-bar-search-mode-enabled object) search-mode)}
  @argument[object]{a @class{gtk:search-bar} widget}
  @argument[search-mode]{a boolean with the state of the search mode}
  @begin{short}
    Accessor of the @slot[gtk:search-bar]{search-mode-enabled} slot of the
    @class{gtk:search-bar} class.
  @end{short}

  Switches the search mode on or off.
  @see-class{gtk:search-bar}")

;;; --- search-bar-show-close-button ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "show-close-button"
                                               'search-bar) t)
 "The @code{show-close-button} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  Whether to show the Close button in the toolbar. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'search-bar-show-close-button)
      "Accessor"
      (documentation 'search-bar-show-close-button 'function)
 "@version{#2020-6-1}
  @syntax[]{(gtk:search-bar-show-close-button object) => visible}
  @syntax[]{(setf (gtk:search-bar-show-close-button object) visible)}
  @argument[object]{a @class{gtk:search-bar} widget}
  @argument[visible]{a boolean whether the Close button will be shown or not}
  @begin{short}
    Accessor of the @slot[gtk:search-bar]{show-close-button} slot of the
    @class{gtk:search-bar} class.
  @end{short}

  The@sym{gtk:search-bar-show-close-button} function returns whether the Close
  button is shown. The @sym{(setf gtk:search-bar-show-close-button} function
  shows or hides the Close button.

  Applications that already have a \"search\" toggle button should not show a
  Close button in their search bar, as it duplicates the role of the toggle
  button.
  @see-class{gtk:search-bar}")

;;; ----------------------------------------------------------------------------
;;; gtk_search_bar_new ()
;;; ----------------------------------------------------------------------------

(defun search-bar-new ()
 #+liber-documentation
 "@version{#2020-6-1}
  @return{A new @class{gtk:search-bar} widget.}
  @begin{short}
    Creates a search bar.
  @end{short}
  You will need to tell it about which widget is going to be your text entry
  using the @fun{gtk:search-bar-connect-entry} function.
  @see-class{gtk:search-bar}
  @see-function{gtk:search-bar-connect-entry}"
  (make-instance 'search-bar))

(export 'search-bar-new)

;;; ----------------------------------------------------------------------------
;;; gtk_search_bar_connect_entry ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_search_bar_connect_entry" search-bar-connect-entry) :void
 #+liber-documentation
 "@version{#2020-6-1}
  @argument[search-bar]{a @class{gtk:search-bar} widget}
  @argument[entry]{a @class{gtk:entry} widget}
  @begin{short}
    Connects the entry widget passed as the one to be used in this search bar.
  @end{short}
  The entry should be a descendant of the search bar. This is only required if
  the entry is not the direct child of the search bar, as in our main example.
  @see-class{gtk:search-bar}"
  (search-bar (g:object search-bar))
  (entry (g:object entry)))

(export 'search-bar-connect-entry)

;;; ----------------------------------------------------------------------------
;;; gtk_search_bar_get_search_mode ()
;;; ----------------------------------------------------------------------------

;; Implemented as the accessor search-bar-search-mode-enabled

;;; ----------------------------------------------------------------------------
;;; gtk_search_bar_set_search_mode ()
;;; ----------------------------------------------------------------------------

;; Implemented as the accessor (setf search-bar-search-mode-enabled)

;;; --- End of file gtk.search-bar.lisp ----------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk4.search-bar.lisp
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
;;; GtkSearchBar
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkSearchBar" search-bar
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
 "@version{2024-04-20}
  @begin{short}
    The @class{gtk:search-bar} widget is a container made to have a search
    entry.
  @end{short}
  It can also contain additional widgets, such as drop-down menus, or buttons.
  The search bar would appear when a search is started through typing on the
  keyboard, or the search mode of the application is toggled on.

  @image[search-bar]{Figure: GtkSearchBar}

  For keyboard presses to start a search, the search bar must be told of a
  widget to capture key events from through the
  @fun{gtk:search-bar-key-capture-widget} function. This widget will typically
  be the toplevel window, or a parent container of the search bar. Common
  shortcuts such as @kbd{Ctrl+F} should be handled as an application action, or
  through the menu items.

  You will also need to tell the search bar about which entry you are using as
  your search entry using the @fun{gtk:search-bar-connect-entry} function.
  @begin[Examples]{dictionary}
    The following example shows you how to create a more complex search entry.
    @begin{pre}
(defun do-search-bar (&optional application)
  (let* ((box (make-instance 'gtk:box
                             :orientation :horizontal
                             :spacing 6))
         (searchbar (make-instance 'gtk:search-bar
                                   :child box
                                   :valign :start))
         (window (make-instance 'gtk:window
                                :title \"Search Bar\"
                                :application application
                                :child searchbar))
         (entry (make-instance 'gtk:search-entry
                               :hexpand t))
         (button (make-instance 'gtk:menu-button)))
    (gtk:search-bar-connect-entry searchbar entry)
    (setf (gtk:search-bar-key-capture-widget searchbar) window)
    (gtk:box-append box entry)
    (gtk:box-append box button)
    (gtk:window-present window)))
    @end{pre}
  @end{dictionary}
  @begin[CSS nodes]{dictionary}
    @begin{pre}
searchbar
╰── revealer
    ╰── box
         ├── [child]
         ╰── [button.close]
    @end{pre}
    The @class{gtk:search-bar} implementation has a single CSS node with name
    @code{searchbar}. It has a child node with name @code{revealer} that
    contains a node with name @code{box}. The box node contains both the CSS
    node of the child widget as well as an optional button node which gets
    the @code{.close} style class applied.
  @end{dictionary}
  @begin[Accessibility]{dictionary}
    The @class{gtk:search-bar} implementation uses the
    @val[gtk:accessible-role]{:search} role of the @sym{gtk:accessible-role}
    enumeration.
  @end{dictionary}
  @see-constructor{gtk:search-bar-new}
  @see-slot{gtk:search-bar-child}
  @see-slot{gtk:search-bar-key-capture-widget}
  @see-slot{gtk:search-bar-search-mode-enabled}
  @see-slot{gtk:search-bar-show-close-button}
  @see-class{gtk:search-entry}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:search-bar-child ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "child" 'search-bar) t)
 "The @code{child} property of type @class{gtk:widget}
  (Read / Write / Construct) @br{}
  The child widget of the search bar.")

#+liber-documentation
(setf (liber:alias-for-function 'search-bar-child)
      "Accessor"
      (documentation 'search-bar-child 'function)
 "@version{2024-04-20}
  @syntax{(gtk:search-bar-child object) => child}
  @syntax{(setf (gtk:search-bar-child object) child)}
  @argument[object]{a @class{gtk:search-bar} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @begin{short}
    Accessor of the @slot[gtk:search-bar]{child} slot of the
    @class{gtk:search-bar} class.
  @end{short}
  The @fun{gtk:search-bar-child} function gets the child widget of the search
  bar. The @setf{gtk:search-bar-child} function sets the child widget.
  @see-class{gtk:search-bar}
  @see-class{gtk:widget}")

;;; --- gtk:search-bar-key-capture-widget --------------------------------------

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
 "@version{2024-04-20}
  @syntax{(gtk:search-bar-key-capture-widget object) => widget}
  @syntax{(setf (gtk:search-bar-key-capture-widget object) widget)}
  @argument[object]{a @class{gtk:search-bar} widget}
  @argument[widget]{a @class{gtk:widget} key capture widget}
  @begin{short}
    Accessor of the @slot[gtk:search-bar]{key-capture-widget} slot of the
    @class{gtk:search-bar} class.
  @end{short}
  The @fun{gtk:search-bar-key-capture-widget} function gets the widget that the
  search bar is capturing key events from. The
  @setf{gtk:search-bar-key-capture-widget} function sets @arg{widget} as
  the widget that the search bar will capture key events from.

  If key events are handled by the search bar, the search bar will be shown,
  and the search entry populated with the entered text.
  @see-class{gtk:search-bar}
  @see-class{gtk:widget}")

;;; --- gtk:search-bar-search-mode-enabled -------------------------------------

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
 "@version{2025-07-17}
  @syntax{(gtk:search-bar-search-mode-enabled object) => mode}
  @syntax{(setf (gtk:search-bar-search-mode-enabled object) mode)}
  @argument[object]{a @class{gtk:search-bar} widget}
  @argument[mode]{a boolean for the state of the search mode}
  @begin{short}
    Accessor of the @slot[gtk:search-bar]{search-mode-enabled} slot of the
    @class{gtk:search-bar} class.
  @end{short}
  Switches the search mode on or off.
  @see-class{gtk:search-bar}")

;;; --- gtk:search-bar-show-close-button ---------------------------------------

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
 "@version{2024-04-20}
  @syntax{(gtk:search-bar-show-close-button object) => visible}
  @syntax{(setf (gtk:search-bar-show-close-button object) visible)}
  @argument[object]{a @class{gtk:search-bar} widget}
  @argument[visible]{a boolean whether the Close button will be shown or not}
  @begin{short}
    Accessor of the @slot[gtk:search-bar]{show-close-button} slot of the
    @class{gtk:search-bar} class.
  @end{short}
  The @fun{gtk:search-bar-show-close-button} function returns whether the Close
  button is shown. The @setf{gtk:search-bar-show-close-button} function shows
  or hides the Close button.

  Applications that already have a search toggle button should not show a Close
  button in their search bar, as it duplicates the role of the toggle button.
  @see-class{gtk:search-bar}")

;;; ----------------------------------------------------------------------------
;;; gtk_search_bar_new
;;; ----------------------------------------------------------------------------

(defun search-bar-new ()
 #+liber-documentation
 "@version{2024-04-20}
  @return{The new @class{gtk:search-bar} widget.}
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
;;; gtk_search_bar_connect_entry
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_search_bar_connect_entry" search-bar-connect-entry) :void
 #+liber-documentation
 "@version{2024-04-20}
  @argument[searchbar]{a @class{gtk:search-bar} widget}
  @argument[entry]{a @class{gtk:entry} widget}
  @begin{short}
    Connects the text entry passed as the one to be used in the search bar.
  @end{short}
  The text entry should be a descendant of the search bar. This is only required
  if the text entry is not the direct child of the search bar, as in our main
  example.
  @see-class{gtk:search-bar}
  @see-class{gtk:entry}"
  (searchbar (g:object search-bar))
  (entry (g:object editable)))

(export 'search-bar-connect-entry)

;;; ----------------------------------------------------------------------------
;;; gtk_search_bar_get_search_mode
;;; ----------------------------------------------------------------------------

;; Implemented as the accessor search-bar-search-mode-enabled

;;; ----------------------------------------------------------------------------
;;; gtk_search_bar_set_search_mode
;;; ----------------------------------------------------------------------------

;; Implemented as the accessor (setf search-bar-search-mode-enabled)

;;; --- End of file gtk4.search-bar.lisp ---------------------------------------

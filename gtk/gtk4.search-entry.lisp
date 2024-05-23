;;; ----------------------------------------------------------------------------
;;; gtk4.search-entry.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.12 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2013 - 2024 Dieter Kaiser
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
;;; GtkSearchEntry
;;;
;;;     An entry which shows a search icon
;;;
;;; Types and Values
;;;
;;;     GtkSearchEntry
;;;
;;; Accessors
;;;
;;;     gtk_search_entry_get_search_delay                  Since 4.8
;;;     gtk_search_entry_set_search_delay                  Since 4.8
;;;     gtk_search_entry_get_placeholder_text              Since 4.10
;;;     gtk_search_entry_set_placeholder_text              Since 4.10
;;;
;;; Functions
;;;
;;;     gtk_search_entry_new
;;;     gtk_search_entry_set_key_capture_widget
;;;     gtk_search_entry_get_key_capture_widget
;;;
;;; Properties
;;;
;;;     activates-default
;;;     placeholder-text
;;;     search-delay                                       Since 4.8
;;;
;;; Signals
;;;
;;;     activate
;;;     next-match
;;;     previous-match
;;;     search-changed
;;;     search-started
;;;     stop-search
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkSearchEntry
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;;     GtkEditable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkSearchEntry
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkSearchEntry" search-entry
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget"
                "GtkEditable")
   :type-initializer "gtk_search_entry_get_type")
  ((activates-default
    search-entry-activates-default
    "activates-default" "gboolean" t t)
   (placeholder-text
    search-entry-placeholder-text
    "placeholder-text" "gchararray" t t)
   #+gtk-4-8
   (search-delay
    search-entry-search-delay
    "search-delay" "guint" t t)))

#+liber-documentation
(setf (documentation 'search-entry 'type)
 "@version{2024-4-20}
  @begin{short}
    The @class{gtk:search-entry} widget is a text entry that has been tailored
    for use as a search entry.
  @end{short}
  The main API for interacting with a @class{gtk:search-entry} widget as entry
  is the @class{gtk:editable} interface.

  @image[search-entry]{Figure: GtkSearchEntry}

  It will show an inactive symbolic find icon when the search entry is empty,
  and a symbolic clear icon when there is text. Clicking on the clear icon will
  empty the search entry.

  To make filtering appear more reactive, it is a good idea to not react to
  every change in the search entry text immediately, but only after a short
  delay. To support this, the @class{gtk:search-entry} widget emits the
  @code{\"search-changed\"} signal which can be used instead of the
  @code{\"changed\"} signal.

  The @code{\"previous-match\"}, @code{\"next-match\"} and
  @code{\"stop-search\"} signals can be used to implement moving between search
  results and ending the search.

  Often, the @class{gtk:search-entry} widget will be fed events by means of
  being placed inside a @class{gtk:search-bar} widget. If that is not the case,
  you can use the @fun{gtk:search-entry-key-capture-widget} function to let it
  capture key input from another widget.
  @begin[CSS Nodes]{dictionary}
    @begin{pre}
entry.search
╰── text
    @end{pre}
    The @class{gtk:search-entry} implementation has a single CSS node with name
    @code{entry} that carries a @code{.search} style class, and the @code{text}
    node is a child of that.
  @end{dictionary}
  @begin[Accessibility]{dictionary}
    The @class{gtk:search-entry} implementation uses the @code{:search-box} role
    of the @symbol{gtk:accessible-role} enumeration.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"activate\" signal}
      @begin{pre}
lambda (entry)    :action
      @end{pre}
      The signal is forwarded from the @code{\"activated\"} signal, which is a
      keybinding signal for all forms of the @kbd{Enter} key.
      @begin[code]{table}
        @entry[entry]{The @class{gtk:search-entry} widget on which the signal is
          emitted.}
      @end{table}
    @subheading{The \"next-match\" signal}
      @begin{pre}
lambda (entry)    :action
      @end{pre}
      The signal is a keybinding signal which gets emitted when the user
      initiates a move to the next match for the current search string.
      Applications should connect to it, to implement moving between matches.
      The default bindings for this signal is the @kbd{Ctrl-g} key.
      @begin[code]{table}
        @entry[entry]{The @class{gtk:search-entry} widget on which the signal is
          emitted.}
      @end{table}
    @subheading{The \"previous-match\" signal}
      @begin{pre}
lambda (entry)    :action
      @end{pre}
      The signal is a keybinding signal which gets emitted when the user
      initiates a move to the previous match for the current search string.
      Applications should connect to it, to implement moving between matches.
      The default bindings for this signal is the @kbd{Ctrl-Shift-g} key.
      @begin[code]{table}
        @entry[entry]{The @class{gtk:search-entry} widget on which the signal is
          emitted.}
        @end{table}
    @subheading{The \"search-changed\" signal}
      @begin{pre}
lambda (entry)    :run-last
      @end{pre}
      The signal is emitted with a short delay of 150 milliseconds after the
      last change to the text entry.
      @begin[code]{table}
        @entry[entry]{The @class{gtk:search-entry} widget on which the signal is
          emitted.}
      @end{table}
    @subheading{The \"stop-search\" signal}
      @begin{pre}
lambda (entry)    :action
      @end{pre}
      The signal is a keybinding signal which gets emitted when the user stops
      a search via keyboard input. Applications should connect to it, to
      implement hiding the search entry in this case. The default bindings for
      this signal is the @kbd{Escape} key.
      @begin[code]{table}
        @entry[entry]{The @class{gtk:search-entry} widget on which the signal is
          emitted.}
      @end{table}
  @end{dictionary}
  @see-constructor{gtk:search-entry-new}
  @see-slot{gtk:search-entry-activates-default}
  @see-slot{gtk:search-entry-placeholder-text}
  @see-slot{gtk:search-entry-search-delay}
  @see-class{gtk:entry}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:search-entry-activates-default -------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "activates-default"
                                               'search-entry) t)
 "The @code{activates-default} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether to activate the default widget, such as the default button in a
  dialog, when the @kbd{Enter} key is pressed. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'search-entry-activates-default)
      "Accessor"
      (documentation 'search-entry-activates-default 'function)
 "@version{2024-5-22}
  @syntax{(gtk:search-entry-activates-default object) => setting}
  @syntax{(setf (gtk:search-entry-activates-default object) setting)}
  @argument[object]{a @class{gtk:search-entry} widget}
  @argument[setting]{@em{true} to activate the default widget of the window on
    @kbd{Enter} keypress}
  @begin{short}
    Accessor of the @slot[gtk:search-entry]{activates-default} slot of the
    @class{gtk:search-entry} class.
  @end{short}
  The @fun{gtk:search-entry-activates-default} function retrieves whether to
  activate the default widget, when the @kbd{Enter} key is pressed.

  If the @arg{setting} argument is @em{true}, pressing the @kbd{Enter} key in
  the search entry will activate the default widget for the window containing
  the search entry. This usually means that the dialog containing the search
  entry will be closed, since the default widget is usually one of the dialog
  buttons.
  @see-class{gtk:search-entry}")

;;; --- gtk:search-entry-placeholder-text --------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "placeholder-text"
                                               'search-entry) t)
 "The @code{placeholder-text} property of type @code{:string} (Read / Write)
  @br{}
  The text that will be displayed in the text entry when it is empty and
  unfocused. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'search-entry-placeholder-text)
      "Accessor"
      (documentation 'search-entry-placeholder-text 'function)
 "@version{2024-4-20}
  @syntax{(gtk:search-entry-placeholder-text object) => text}
  @syntax{(setf (gtk:search-entry-placeholder-text object) text)}
  @argument[object]{a @class{gtk:search-entry} widget}
  @argument[text]{a string to be displayed when @arg{entry} is empty and
    unfocused, or @code{nil}}
  @begin{short}
    Accessor of the @slot[gtk:search-entry]{placeholder-text} slot of the
    @class{gtk:search-entry} class.
  @end{short}
  The @fun{gtk:search-entry-placeholder-text} function retrieves the text that
  will be displayed when the search entry is empty and unfocused. The
  @setf{gtk:search-entry-placeholder-text} function sets the text. This can be
  used to give a visual hint of the expected contents of the search entry.

  Note that since the placeholder text gets removed when the search entry
  received focus, using this feature is a bit problematic if the search entry
  is given the initial focus in a window. Sometimes this can be worked around
  by delaying the initial focus setting until the first key event arrives.
  @see-class{gtk:search-entry}")

;;; --- gtk:search-entry-search-delay ------------------------------------------

#+(and gtk-4-8 liber-documentation)
(setf (documentation (liber:slot-documentation "search-delay" 'search-entry) t)
 "The @code{search-delay} property of type @code{:uint} (Read / Write) @br{}
  The delay in milliseconds from last keypress to the search changed signal.
  @br{}
  Default value: 150")

#+(and gtk-4-8 liber-documentation)
(setf (liber:alias-for-function 'search-entry-search-delay)
      "Accessor"
      (documentation 'search-entry-search-delay 'function)
 "@version{2024-4-20}
  @syntax{(gtk:search-entry-search-delay object) => delay}
  @syntax{(setf (gtk:search-entry-search-delay object) delay)}
  @argument[object]{a @class{gtk:search-entry} widget}
  @argument[delay]{an unsigned integer with the delay in milliseconds}
  @begin{short}
    Accessor of the @slot[gtk:search-entry]{search-delay} slot of the
    @class{gtk:search-entry} class.
  @end{short}
  The @fun{gtk:search-entry-search-delay} function gest the delay to be used
  between the last keypress and the @code{\"search-changed\"} signal being
  emitted. The @setf{gtk:search-entry-search-delay} function sets the search
  delay.

  Since 4.8
  @see-class{gtk:search-entry}")

;;; ----------------------------------------------------------------------------
;;; gtk_search_entry_new
;;; ----------------------------------------------------------------------------

(declaim (inline search-entry-new))

(defun search-entry-new ()
 #+liber-documentation
 "@version{2024-4-20}
  @return{The new @class{gtk:search-entry} widget.}
  @begin{short}
    Creates a search entry, with a find icon when the search field is empty,
    and a clear icon when it is not.
  @end{short}
  @see-class{gtk:search-entry}"
  (make-instance 'search-entry))

(export 'search-entry-new)

;;; ----------------------------------------------------------------------------
;;; gtk_search_entry_get_key_capture_widget
;;; gtk_search_entry_set_key_capture_widget
;;; ----------------------------------------------------------------------------

(defun (setf search-entry-key-capture-widget) (widget entry)
  (cffi:foreign-funcall "gtk_search_entry_set_key_capture_widget"
                        (g:object search-entry) entry
                        (g:object widget) widget
                        :void)
  widget)

(cffi:defcfun ("gtk_search_entry_get_key_capture_widget"
               search-entry-key-capture-widget) (g:object widget)
 #+liber-documentation
 "@version{#2024-4-20}
  @syntax{(gtk:search-entry-key-capture-widget entry) => widget}
  @syntax{(setf (gtk:search-entry-key-capture-widget entry) widget)}
  @argument[entry]{a @class{gtk:search-entry} widget}
  @argument[widget]{a @class{gtk:widget} key capture widget}
  @begin{short}
    Accessor of the key capture widget of the search entry.
  @end{short}
  The @fun{gtk:search-entry-key-capture-widget} function gets the widget that
  the search entry is capturing key events from. The
  @setf{gtk:search-entry-key-capture-widget} function sets @arg{widget} as the
  widget that the search entry will capture key events from.

  Key events are consumed by the search entry to start or continue a search.

  If the search entry is part of a @class{gtk:search-bar} widget, it is
  preferable to call the @fun{gtk:search-bar-key-capture-widget} function
  instead, which will reveal the text entry in addition to triggering the
  search entry.
  @see-class{gtk:search-entry}
  @see-class{gtk:search-bar}
  @see-class{gtk:widget}
  @see-function{gtk:search-bar-key-capture-widget}"
  (entry (g:object search-entry)))

(export 'search-entry-key-capture-widget)

;;; --- End of file gtk4.search-entry.lisp -------------------------------------

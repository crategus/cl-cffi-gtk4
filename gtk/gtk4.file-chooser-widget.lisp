;;; ----------------------------------------------------------------------------
;;; gtk4.file-chooser-widget.lisp
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
;;; GtkFileChooserWidget                                   Deprecated 4.10
;;;
;;;     A file chooser widget
;;;
;;; Types and Values
;;;
;;;     GtkFileChooserWidget
;;;
;;; Functions
;;;
;;;     gtk_file_chooser_widget_new
;;;
;;; Properties
;;;
;;;     search-mode
;;;     show-time                                          Since 4.10
;;;     subtitle
;;;
;;; Signals
;;;
;;;     desktop-folder
;;;     down-folder
;;;     home-folder
;;;     location-popup
;;;     location-popup-on-paste
;;;     location-toggle-popup
;;;     places-shortcut
;;;     quick-bookmark
;;;     recent-shortcut
;;;     search-shortcut
;;;     show-hidden
;;;     up-folder
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkFileChooserWidget
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;;     GtkFileChooser
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkFileChooserWidget
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkFileChooserWidget" file-chooser-widget
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget"
                "GtkFileChooser")
   :type-initializer "gtk_file_chooser_widget_get_type")
  ((search-mode
    file-chooser-widget-search-mode
    "search-mode" "gboolean" t t)
   #+gtk-4-10
   (show-time
    file-chooser-widget-show-time
    "show-time" "gboolean" t nil)
   (subtitle
    file-chooser-widget-subtitle
    "subtitle" "gchararray" t nil)))

#+(and gtk-4-10 gtk-warn-deprecated)
(defmethod initialize-instance :after ((obj file-chooser-widget) &key)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:FILE-CHOOSER-WIDGET is deprecated since 4.10")))

#+liber-documentation
(setf (documentation 'file-chooser-widget 'type)
 "@version{2023-8-30}
  @begin{short}
    The @class{gtk:file-chooser-widget} widget is a widget for choosing files.
  @end{short}
  It exposes the @class{gtk:file-chooser} interface, and you should use the
  methods of this interface to interact with the widget.
  @begin[CSS nodes]{dictionary}
    The @class{gtk:file-chooser-widget} implementation has a single CSS node
    with name @code{filechooser}.
  @end{dictionary}
  @begin[Warning]{dictionary}
    The @class{gtk:file-chooser-widget} class is deprecated since 4.10. Direct
    use of the widget is deprecated.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"desktop-folder\" signal}
      @begin{pre}
lambda (widget)    :action
      @end{pre}
      A keybinding signal which gets emitted when the user asks for it. This is
      used to make the file chooser show the user's Desktop folder in the file
      list. The default binding for this signal is the @kbd{Alt+D} key.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:file-chooser-widget} object which
          received the signal.}
      @end{table}
    @subheading{The \"down-folder\" signal}
      @begin{pre}
lambda (widget)    :action
      @end{pre}
      A keybinding signal which gets emitted when the user asks for it. This is
      used to make the file chooser go to a child of the current folder in the
      file hierarchy. The subfolder that will be used is displayed in the path
      bar widget of the file chooser. For example, if the path bar is showing
      @file{\"/foo/bar/baz\"}, with @file{bar} currently displayed, then this
      will cause the file chooser to switch to the @file{\"baz\"} subfolder.
      The default binding for this signal is the @kbd{Alt+Down} key.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:file-chooser-widget} object which
          received the signal.}
      @end{table}
    @subheading{The \"home-folder\" signal}
      @begin{pre}
lambda (widget)    :action
      @end{pre}
      A keybinding signal which gets emitted when the user asks for it. This is
      used to make the file chooser show the user's home folder in the file
      list. The default binding for this signal is the @kbd{Alt+Home} key.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:file-chooser-widget} object which
          received the signal.}
      @end{table}
    @subheading{The \"location-popup\" signal}
      @begin{pre}
lambda (widget path)    :action
      @end{pre}
      A keybinding signal which gets emitted when the user asks for it. This is
      used to make the file chooser show a \"Location\" prompt which the user
      can use to manually type the name of the file he wishes to select. The
      default bindings for this signal are the @kbd{Control+L} key with a path
      string of \"\" (the empty string). It is also bound to / with a path
      string of \"/\" (a slash): this lets you type / and immediately type a
      path name. On Unix systems, this is bound to ~ (tilde) with a path string
      of \"~\" itself for access to home directories.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:file-chooser-widget} object which
          received the signal.}
        @entry[path]{The string that gets put in the text entry for the file
          name.}
      @end{table}
    @subheading{The \"location-popup-on-paste\" signal}
      @begin{pre}
lambda (widget)    :action
      @end{pre}
      A keybinding signal which gets emitted when the user asks for it. This is
      used to make the file chooser show a \"Location\" prompt when the user
      pastes into a @class{gtk:file-chooser-widget} widget. The default binding
      for this signal is the @kbd{Control+V} key.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:file-chooser-widget} object which
          received the signal.}
      @end{table}
    @subheading{The \"location-toggle-popup\" signal}
      @begin{pre}
lambda (widget)    :action
      @end{pre}
      A keybinding signal which gets emitted when the user asks for it. This is
      used to toggle the visibility of a \"Location\" prompt which the user can
      use to manually type the name of the file he wishes to select. The default
      binding for this signal is the @kbd{Control+L} key.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:file-chooser-widget} object which
          received the signal.}
      @end{table}
    @subheading{The \"places-shortcut\" signal}
      @begin{pre}
lambda (widget)    :action
      @end{pre}
      A keybinding signal which gets emitted when the user asks for it. This
      is used to move the focus to the places sidebar. The default binding for
      this signal is the @kbd{Alt+P} key.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:file-chooser-widget} object which
          received the signal.}
      @end{table}
    @subheading{The \"quick-bookmark\" signal}
      @begin{pre}
lambda (widget index)    :action
      @end{pre}
      A keybinding signal which gets emitted when the user asks for it. This is
      used to make the file chooser switch to the bookmark specified in the
      @arg{index} parameter. For example, if you have three bookmarks, you
      can pass 0, 1, 2 to this signal to switch to each of them, respectively.
      The default binding for this signal is the @kbd{Alt+1} key, the
      @kbd{Alt+2} key, etc. until the @kbd{Alt+0} key. Note that in the default
      binding, that the @kbd{Alt+1} key is actually defined to switch to the
      bookmark at index 0, and so on successively. The @kbd{Alt+0} key is
      defined to switch to the bookmark at index 10.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:file-chooser-widget} object which
          received the signal.}
        @entry[index]{The integer with the number of the bookmark to switch to.}
      @end{table}
    @subheading{The \"recent-shortcut\" signal}
      @begin{pre}
lambda (widget)    :action
      @end{pre}
      A keybinding signal which gets emitted when the user asks for it. This is
      used to make the file chooser show the Recent location. The default
      binding for this signal is the @kbd{Alt+R}.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:file-chooser-widget} object which
          received the signal.}
      @end{table}
    @subheading{The \"search-shortcut\" signal}
      @begin{pre}
lambda (widget)    :action
      @end{pre}
      A keybinding signal which gets emitted when the user asks for it. This is
      used to make the file chooser show the search entry. The default binding
      for this signal is the @kbd{Alt+S} key.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:file-chooser-widget} object which
          received the signal.}
      @end{table}
    @subheading{The \"show-hidden\" signal}
      @begin{pre}
lambda (widget)    :action
      @end{pre}
      A keybinding signal which gets emitted when the user asks for it. This is
      used to make the file chooser display hidden files. The default binding
      for this signal is the @kbd{Control+H} key.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:file-chooser-widget} object which
          received the signal.}
      @end{table}
    @subheading{The \"up-folder\" signal}
      @begin{pre}
lambda (widget)    :action
      @end{pre}
      A keybinding signal which gets emitted when the user asks for it. This is
      used to make the file chooser go to the parent of the current folder in
      the file hierarchy. The default binding for this signal is the
      @kbd{Alt+Up} key.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:file-chooser-widget} object which
          received the signal.}
      @end{table}
  @end{dictionary}
  @see-constructor{gtk:file-chooser-widget-new}
  @see-slot{gtk:file-chooser-widget-search-mode}
  @see-slot{gtk:file-chooser-widget-show-time}
  @see-slot{gtk:file-chooser-widget-subtitle}
  @see-class{gtk:file-chooser}
  @see-class{gtk:file-chooser-dialog}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:file-chooser-widget-search-mode ------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "search-mode"
                                               'file-chooser-widget) t)
 "The @code{search-mode} property of type @code{:boolean} (Read / Write) @br{}
  The search mode. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'file-chooser-widget-search-mode)
      "Accessor"
      (documentation 'file-chooser-widget-search-mode 'function)
 "@version{2024-3-8}
  @syntax{(gtk:file-chooser-widget-search-mode object) => mode}
  @syntax{(setf (gtk:file-chooser-widget-search-mode object) mode)}
  @argument[object]{a @class{gtk:file-chooser-widget} widget}
  @argument[mode]{a boolean whether in search mode}
  @begin{short}
    Accessor of the @slot[gtk:file-chooser-widget]{search-mode} slot of the
    @class{gtk:file-chooser-widget} class.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:file-chooser-widget} implementation is deprecated since 4.10.
  @end{dictionary}
  @see-class{gtk:file-chooser-widget}")

;;; --- gtk:file-chooser-widget-show-time --------------------------------------

#+(and gtk-4-10 liber-documentation)
(setf (documentation (liber:slot-documentation "show-time"
                                               'file-chooser-widget) t)
 "The @code{show-time} property of type @code{:boolean} (Read) @br{}
  Whether to show the time. Since 4.10 @br{}
  Default value: @em{false}")

#+(and gtk-4-10 liber-documentation)
(setf (liber:alias-for-function 'file-chooser-widget-show-time)
      "Accessor"
      (documentation 'file-chooser-widget-show-time 'function)
 "@version{2024-3-8}
  @syntax{(gtk:file-chooser-widget-show-time object) => setting}
  @argument[object]{a @class{gtk:file-chooser-widget} widget}
  @argument[setting]{a boolean whether in search mode}
  @begin{short}
    Accessor of the @slot[gtk:file-chooser-widget]{show-time} slot of the
    @class{gtk:file-chooser-widget} class.
  @end{short}

  Since 4.10
  @begin[Warning]{dictionary}
    The @class{gtk:file-chooser-widget} implementation is deprecated since 4.10.
  @end{dictionary}
  @see-class{gtk:file-chooser-widget}")

;;; --- gtk:file-chooser-widget-subtitle ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "subtitle"
                                               'file-chooser-widget) t)
 "The @code{subtitle} property of type @code{:string} (Read) @br{}
  The subtitle. @br{}
  Default value: \"\"")

#+liber-documentation
(setf (liber:alias-for-function 'file-chooser-widget-subtitle)
      "Accessor"
      (documentation 'file-chooser-widget-subtitle 'function)
 "@version{2024-3-8}
  @syntax{(gtk:file-chooser-widget-subtitle object) => subtitle}
  @argument[object]{a @class{gtk:file-chooser-widget} widget}
  @argument[subtitle]{a string with the subtitle}
  @begin{short}
    Accessor of the @slot[gtk:file-chooser-widget]{subtitle} slot of the
    @class{gtk:file-chooser-widget} class.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:file-chooser-widget} implementation is deprecated since 4.10.
  @end{dictionary}
  @see-class{gtk:file-chooser-widget}")

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_widget_new
;;; ----------------------------------------------------------------------------

(declaim (inline file-chooser-widget-new))

(defun file-chooser-widget-new (action)
 #+liber-documentation
 "@version{2024-3-8}
  @argument[action]{a @symbol{gtk:file-chooser-action} value for open or save
    mode}
  @return{The new @class{gtk:file-chooser-widget} widget.}
  @begin{short}
    Creates a new file chooser widget.
  @end{short}
  This is a file chooser widget that can be embedded in custom windows, and it
  is the same widget that is used by the @class{gtk:file-chooser-dialog} widget.
  @begin[Warning]{dictionary}
    The @class{gtk:file-chooser-widget} implementation is deprecated since 4.10.
  @end{dictionary}
  @see-class{gtk:file-chooser-widget}
  @see-class{gtk:file-chooser-dialog}
  @see-symbol{gtk:file-chooser-action}"
  (make-instance 'file-chooser-widget
                 :action action))

(export 'file-chooser-widget-new)

;;; --- End of file gtk4.file-chooser-widget.lisp ------------------------------

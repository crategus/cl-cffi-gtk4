;;; ----------------------------------------------------------------------------
;;; gtk4.shortcuts-group.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.12 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2019 - 2023 Dieter Kaiser
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
;;; GtkShortcutsGroup
;;;
;;;     Represents a group of shortcuts in a GtkShortcutsWindow
;;;
;;; Types and Values
;;;
;;;     GtkShortcutsGroup
;;;
;;; Properties
;;;
;;;     accel-size-group
;;;     height
;;;     title
;;;     title-size-group
;;;     view
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkBox
;;;                 ╰── GtkShortcutsGroup
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;;     GtkOrientable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkShortcutsGroup
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkShortcutsGroup" shortcuts-group
  (:superclass box
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget"
                "GtkOrientable")
   :type-initializer "gtk_shortcuts_group_get_type")
   ((accel-size-group
    shortcuts-group-accel-size-group
    "accel-size-group" "GtkSizeGroup" nil t)
   (height
    shortcuts-group-height
    "height" "guint" t nil)
   (title
    shortcuts-group-title
    "title" "gchararray" t t)
   (title-size-group
    shortcuts-group-title-size-group
    "title-size-group" "GtkSizeGroup" nil t)
   (view
    shortcuts-group-view
    "view" "gchararray" t t)))

#+liber-documentation
(setf (documentation 'shortcuts-group 'type)
 "@version{2023-8-28}
  @begin{short}
    The @class{gtk:shortcuts-group} widget represents a group of related 
    keyboard shortcuts or gestures.
  @end{short}
  The group has a title. It may optionally be associated with a view of the
  application, which can be used to show only relevant shortcuts depending on
  the application context.

  This widget is only meant to be used with the @class{gtk:shortcuts-window}
  widget.
  @see-slot{gtk:shortcuts-group-accel-size-group}
  @see-slot{gtk:shortcuts-group-height}
  @see-slot{gtk:shortcuts-group-title}
  @see-slot{gtk:shortcuts-group-title-size-group}
  @see-slot{gtk:shortcuts-group-view}
  @see-class{gtk:shortcuts-window}")

;;; --- shortcuts-group-accel-size-group ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "accel-size-group"
                                               'shortcuts-group) t)
 "The @code{accel-size-group} property of type @class{gtk:size-group} (Write)
  @br{}
  The size group for the accelerator portion of shortcuts in this group.
  This is used internally by GTK, and must not be modified by applications.")

#+liber-documentation
(setf (liber:alias-for-function 'shortcuts-group-accel-size-group)
      "Accessor"
      (documentation 'shortcuts-group-accel-size-group 'function)
 "@version{#2021-5-4}
  @syntax[]{(gtk:shortcuts-group-accel-size-group object) => size-group}
  @syntax[]{(setf (gtk:shortcuts-group-accel-size-group object) size-group)}
  @argument[object]{a @class{gtk:shortcuts-group} widget}
  @argument[size-group]{a @class{gtk:size-group} object}
  @begin{short}
    Accessor of the @slot[gtk:shortcuts-group]{accel-size-group} slot of the
    @class{gtk:shortcuts-group} class.
  @end{short}
  The size group for the accelerator portion of shortcuts in this group.
  This is used internally by GTK, and must not be modified by applications.
  @see-class{gtk:shortcuts-group}
  @see-class{gtk:size-group}")

;;; --- shortcuts-group-height -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "height" 'shortcuts-group) t)
 "The @code{height} property of type @code{:uint} (Read) @br{}
  A rough measure for the number of lines in this group. This is used internally
  by GTK, and is not useful for applications. @br{}
  Default value: 1")

#+liber-documentation
(setf (liber:alias-for-function 'shortcuts-group-height)
      "Accessor"
      (documentation 'shortcuts-group-height 'function)
 "@version{#2021-5-4}
  @syntax[]{(gtk:shortcuts-group-height object) => height}
  @syntax[]{(setf (gtk:shortcuts-group-height object) height)}
  @argument[object]{a @class{gtk:shortcuts-group} widget}
  @argument[height]{an unsigned integer with the measure for the number of
    lines}
  @begin{short}
    Accessor of the @slot[gtk:shortcuts-group]{height} slot of the
    @class{gtk:shortcuts-group} class.
  @end{short}
  A rough measure for the number of lines in this group. This is used
  internally by GTK, and is not useful for applications.
  @see-class{gtk:shortcuts-group}")

;;; --- shortcuts-group-title --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "title"
                      'shortcuts-group) t)
 "The @code{title} property of type @code{:string} (Read / Write) @br{}
  The title for this group of shortcuts. @br{}
  Default value: \"\"")

#+liber-documentation
(setf (liber:alias-for-function 'shortcuts-group-title)
      "Accessor"
      (documentation 'shortcuts-group-title 'function)
 "@version{#2021-5-4}
  @syntax[]{(gtk:shortcuts-group-title object) => title}
  @syntax[]{(setf (gtk:shortcuts-group-title object) title)}
  @argument[object]{a @class{gtk:shortcuts-group} widget}
  @argument[title]{a string with the title for this group of shortcuts}
  @begin{short}
    Accessor of the @slot[gtk:shortcuts-group]{title} slot of the
    @class{gtk:shortcuts-group} class.
  @end{short}
  The title for this group of shortcuts.
  @see-class{gtk:shortcuts-group}")

;;; --- shortcuts-group-title-size-group ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "title-size-group"
                      'shortcuts-group) t)
 "The @code{title-size-group} property of type @class{gtk:size-group} (Write)
  @br{}
  The size group for the textual portion of shortcuts in this group. This is
  used internally by GTK, and must not be modified by applications.")

#+liber-documentation
(setf (liber:alias-for-function 'shortcuts-group-title-size-group)
      "Accessor"
      (documentation 'shortcuts-group-title-size-group 'function)
 "@version{#2021-5-4}
  @syntax[]{(gtk:shortcuts-group-title-size-group object) => size-group}
  @syntax[]{(setf (gtk:shortcuts-group-title-size-group object) size-group)}
  @argument[object]{a @class{gtk:shortcuts-group} widget}
  @argument[title]{a @class{gtk:size-group} object}
  @begin{short}
    Accessor of the @slot[gtk:shortcuts-group]{title-size-group} slot of the
    @class{gtk:shortcuts-group} class.
  @end{short}
  The size group for the textual portion of shortcuts in this group. This is
  used internally by GTK, and must not be modified by applications.
  @see-class{gtk:shortcuts-group}
  @see-class{gtk:size-group}")

;;; --- shortcuts-group-view ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "view"
                      'shortcuts-group) t)
 "The @code{view} property of type @code{:string} (Read / Write) @br{}
  An optional view that the shortcuts in this group are relevant for. The group
  will be hidden if the @slot[gtk:shortcuts-window]{view-name} property does not
  match the view of this group. Set this to @code{nil} to make the group always
  visible. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'shortcuts-group-view)
      "Accessor"
      (documentation 'shortcuts-group-view 'function)
 "@version{#2021-5-4}
  @syntax[]{(gtk:shortcuts-group-view object) => view}
  @syntax[]{(setf (gtk:shortcuts-group-view object) view)}
  @argument[object]{a @class{gtk:shortcuts-group} widget}
  @argument[view]{a string with an optional view}
  @begin{short}
    Accessor of the @slot[gtk:shortcuts-group]{view} slot of the
    @class{gtk:shortcuts-group} class.
  @end{short}
  An optional view that the shortcuts in this group are relevant for. The group
  will be hidden if the @slot[gtk:shortcuts-window]{view-name} property does not
  match the view of this group. Set this to @code{nil} to make the group always
  visible.
  @see-class{gtk:shortcuts-group}
  @see-function{gtk:shortcuts-window-view-name}")

;;; --- End of file gtk4.shortcuts-group.lisp ----------------------------------

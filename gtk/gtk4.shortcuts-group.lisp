;;; ----------------------------------------------------------------------------
;;; gtk4.shortcuts-group.lisp
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
;;; GtkShortcutsGroup
;;;
;;;     Represents a group of shortcuts in a GtkShortcutsWindow
;;;
;;; Types and Values
;;;
;;;     GtkShortcutsGroup
;;;
;;; Functions
;;;
;;;     gtk_shortcuts_group_add_shortcut                    Since 4.14
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
;;; GtkShortcutsGroup
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkShortcutsGroup" shortcuts-group
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

#+(and gtk-4-18 gtk-warn-deprecated)
(defmethod initialize-instance :after ((obj shortcuts-group) &key)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:SHORTCUTS-GROUP is deprecated since 4.18")))

#+liber-documentation
(setf (documentation 'shortcuts-group 'type)
 "@version{2025-05-14}
  @begin{short}
    The @class{gtk:shortcuts-group} widget represents a group of related
    keyboard shortcuts or gestures.
  @end{short}
  The group has a title. It may optionally be associated with a view of the
  application, which can be used to show only relevant shortcuts depending on
  the application context.

  This widget is only meant to be used with the @class{gtk:shortcuts-window}
  widget.
  @begin[Warning]{dictionary}
    The @class{gtk:shortcuts-group} implementation is deprecated since 4.18.
    This widget will be removed in GTK 5.
  @end{dictionary}
  @see-slot{gtk:shortcuts-group-accel-size-group}
  @see-slot{gtk:shortcuts-group-height}
  @see-slot{gtk:shortcuts-group-title}
  @see-slot{gtk:shortcuts-group-title-size-group}
  @see-slot{gtk:shortcuts-group-view}
  @see-class{gtk:shortcuts-window}")

;;; --- gtk:shortcuts-group-accel-size-group -----------------------------------

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
 "@version{2025-08-11}
  @syntax{(setf (gtk:shortcuts-group-accel-size-group object) group)}
  @argument[object]{a @class{gtk:shortcuts-group} widget}
  @argument[group]{a @class{gtk:size-group} object}
  @begin{short}
    The accessor for the @slot[gtk:shortcuts-group]{accel-size-group} slot of
    the @class{gtk:shortcuts-group} class gets the size group for the
    accelerator portion of shortcuts in this group.
  @end{short}
  This is used internally by GTK, and must not be modified by applications.
  @begin[Warning]{dictionary}
    The @fun{gtk:shortcuts-group-accel-size-group} function is deprecated since
    4.18. This widget will be removed in GTK 5.
  @end{dictionary}
  @see-class{gtk:shortcuts-group}
  @see-class{gtk:size-group}")

;;; --- gtk:shortcuts-group-height ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "height" 'shortcuts-group) t)
 "The @code{height} property of type @code{:uint} (Read) @br{}
  The rough measure for the number of lines in this group. This is used
  internally by GTK, and is not useful for applications. @br{}
  Default value: 1")

#+liber-documentation
(setf (liber:alias-for-function 'shortcuts-group-height)
      "Accessor"
      (documentation 'shortcuts-group-height 'function)
 "@version{2025-08-11}
  @syntax{(gtk:shortcuts-group-height object) => height}
  @argument[object]{a @class{gtk:shortcuts-group} widget}
  @argument[height]{an unsigned integer for the measure for the number of
    lines}
  @begin{short}
    The accessor for the @slot[gtk:shortcuts-group]{height} slot of the
    @class{gtk:shortcuts-group} class gets the rough measure for the number of
    lines in this group.
  @end{short}
  This is used internally by GTK, and is not useful for applications.
  @begin[Warning]{dictionary}
    The @fun{gtk:shortcuts-group-height} function is deprecated since 4.18.
    This widget will be removed in GTK 5.
  @end{dictionary}
  @see-class{gtk:shortcuts-group}")

;;; --- gtk:shortcuts-group-title ----------------------------------------------

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
 "@version{2025-08-11}
  @syntax{(gtk:shortcuts-group-title object) => title}
  @syntax{(setf (gtk:shortcuts-group-title object) title)}
  @argument[object]{a @class{gtk:shortcuts-group} widget}
  @argument[title]{a string for the title for this group of shortcuts}
  @begin{short}
    The accessor for the @slot[gtk:shortcuts-group]{title} slot of the
    @class{gtk:shortcuts-group} class gets or sets the title for this group of
    shortcuts.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:shortcuts-group-title} function is deprecated since 4.18.
    This widget will be removed in GTK 5.
  @end{dictionary}
  @see-class{gtk:shortcuts-group}")

;;; --- gtk:shortcuts-group-title-size-group -----------------------------------

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
 "@version{2025-08-11}
  @syntax{(setf (gtk:shortcuts-group-title-size-group object) group)}
  @argument[object]{a @class{gtk:shortcuts-group} widget}
  @argument[group]{a @class{gtk:size-group} object}
  @begin{short}
    The accessor for the @slot[gtk:shortcuts-group]{title-size-group} slot of
    the @class{gtk:shortcuts-group} class sets the size group for the textual
    portion of shortcuts in this group.
  @end{short}
  This is used internally by GTK, and must not be modified by applications.
  @begin[Warning]{dictionary}
    The @fun{gtk:shortcuts-group-title-size-group} function is deprecated since
    4.18. This widget will be removed in GTK 5.
  @end{dictionary}
  @see-class{gtk:shortcuts-group}
  @see-class{gtk:size-group}")

;;; --- gtk:shortcuts-group-view -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "view"
                      'shortcuts-group) t)
 "The @code{view} property of type @code{:string} (Read / Write) @br{}
  The optional view that the shortcuts in this group are relevant for. The group
  will be hidden if the @slot[gtk:shortcuts-window]{view-name} property does not
  match the view of this group. Set this to @code{nil} to make the group always
  visible. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'shortcuts-group-view)
      "Accessor"
      (documentation 'shortcuts-group-view 'function)
 "@version{2025-08-11}
  @syntax{(gtk:shortcuts-group-view object) => view}
  @syntax{(setf (gtk:shortcuts-group-view object) view)}
  @argument[object]{a @class{gtk:shortcuts-group} widget}
  @argument[view]{a string for an optional view}
  @begin{short}
    The accessor for the @slot[gtk:shortcuts-group]{view} slot of the
    @class{gtk:shortcuts-group} class gets or sets the optional view that the
    shortcuts in this group are relevant for.
  @end{short}
  The group will be hidden if the @slot[gtk:shortcuts-window]{view-name}
  property does not match the view of this group. Set this to @code{nil} to make
  the group always visible.
  @begin[Warning]{dictionary}
    The @fun{gtk:shortcuts-group-view} function is deprecated since 4.18.
    This widget will be removed in GTK 5.
  @end{dictionary}
  @see-class{gtk:shortcuts-group}
  @see-function{gtk:shortcuts-window-view-name}")

;;; ----------------------------------------------------------------------------
;;; gtk_shortcuts_group_add_shortcut                        Since 4.14
;;; ----------------------------------------------------------------------------

#+gtk-4-14
(cffi:defcfun ("gtk_shortcuts_group_add_shortcut" shortcuts-group-add-shortcut)
    :void
 #+liber-documentation
 "@version{#2025-05-14}
  @argument[group]{a @class{gtk:shortcuts-group} widget}
  @argument[shortcut]{a @class{gtk:shortcuts-shortcut} widget}
  @begin{short}
    Adds a shortcut to the shortcuts group.
  @end{short}
  This is the programmatic equivalent to using a @class{gtk:builder} UI
  definition and a @code{<child>} tag to add the child. Adding children with
  other API is not appropriate as the @class{gtk:shortcuts-group} widget manages
  its children internally.

  Since 4.14
  @begin[Warning]{dictionary}
    The @fun{gtk:shortcuts-group-add-shortcut} function is deprecated since
    4.18. This widget will be removed in GTK 5.
  @end{dictionary}
  @see-class{gtk:shortcuts-group}
  @see-class{gtk:shortcuts-shortcut}"
  (group (g:object shortcuts-group))
  (shortcut (g:object shortcuts-shortcut)))

#+gtk-4-14
(export 'shortcuts-group-add-shortcut)

;;; --- End of file gtk4.shortcuts-group.lisp ----------------------------------

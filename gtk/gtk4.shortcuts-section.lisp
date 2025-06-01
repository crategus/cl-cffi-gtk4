;;; ----------------------------------------------------------------------------
;;; gtk4.shortcuts-section.lisp
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
;;; GtkShortcutsSection
;;;
;;;     Represents an application mode in a GtkShortcutsWindow
;;;
;;; Types and Values
;;;
;;;     GtkShortcutsSection
;;;
;;;     gtk_shortcuts_section_add_group                     Since 4.14
;;;
;;; Properties
;;;
;;;     max-height
;;;     section-name
;;;     title
;;;     view-name
;;;
;;; Signals
;;;
;;;     change-current-page
;;;
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkBox
;;;                 ╰── GtkShortcutsSection
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
;;; GtkShortcutsSection
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkShortcutsSection" shortcuts-section
  (:superclass box
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget"
                "GtkOrientable")
   :type-initializer "gtk_shortcuts_section_get_type")
   ((max-height
    shortcuts-section-max-height
    "max-height" "guint" t t)
   (section-name
    shortcuts-section-section-name
    "section-name" "gchararray" t t)
   (title
    shortcuts-section-title
    "title" "gchararray" t t)
   (view-name
    shortcuts-section-view-name
    "view-name" "gchararray" t t)))

#+(and gtk-4-18 gtk-warn-deprecated)
(defmethod initialize-instance :after ((obj shortcuts-section) &key)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:SHORTCUTS-SECTION is deprecated since 4.18")))

#+liber-documentation
(setf (documentation 'shortcuts-section 'type)
 "@version{2025-05-14}
  @begin{short}
    The @class{gtk:shortcuts-section} widget collects all the keyboard shortcuts
    and gestures for a major application mode.
  @end{short}
  If your application needs multiple sections, you should give each section a
  unique section name and a title that can be shown in the section selector of
  the @class{gtk:shortcuts-window} widget.

  The @slot[gtk:shortcuts-section]{max-height} property can be used to influence
  how the groups in the section are distributed over pages and columns.

  This widget is only meant to be used with @class{gtk:shortcuts-window}
  widgets.
  @begin[Warning]{dictionary}
    The @class{gtk:shortcuts-section} implementation is deprecated since 4.18.
    This widget will be removed in GTK 5.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"change-current-page\" signal}
      @begin{pre}
lambda (section offset data)    :action
      @end{pre}
      @begin[code]{table}
        @entry[section]{The @class{gtk:shortcuts-section} widget.}
        @entry[offset]{The integer for the offset.}
        @entry[data]{The foreign pointer to user data.}
        @entry[Return]{The boolean whether the page was changed.}
      @end{table}
      Emitted when the current page changes. The default bindings for this
      signal are @kbd{Ctrl+PgUp}, @kbd{PgUp}, @kbd{Ctrl+PgDn}, @kbd{PgDn}.
  @end{dictionary}
  @see-slot{gtk:shortcuts-section-max-height}
  @see-slot{gtk:shortcuts-section-section-name}
  @see-slot{gtk:shortcuts-section-title}
  @see-slot{gtk:shortcuts-section-view-name}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:shortcuts-section-max-height ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "max-height"
                      'shortcuts-section) t)
 "The @code{max-heigth} property of type @code{:uint} (Read / Write) @br{}
  The maximum number of lines to allow per column. This property can be used to
  influence how the groups in this section are distributed across pages and
  columns. The default value of 15 should work in for most cases. @br{}
  Default value: 15")

#+liber-documentation
(setf (liber:alias-for-function 'shortcuts-section-max-height)
      "Accessor"
      (documentation 'shortcuts-section-max-height 'function)
 "@version{2025-05-14}
  @syntax{(gtk:shortcuts-section-max-height object) => height}
  @syntax{(setf (gtk:shortcuts-section-max-height object) height)}
  @argument[object]{a @class{gtk:shortcuts-section} widget}
  @argument[height]{an unsigned integer for the maximum number of lines to
    allow per column}
  @begin{short}
    Accessor of the @slot[gtk:shortcuts-section]{max-height} slot of the
    @class{gtk:shortcuts-section} class.
  @end{short}
  The maximum number of lines to allow per column. This property can be used to
  influence how the groups in this section are distributed across pages and
  columns. The default value of 15 should work in for most cases.
  @begin[Warning]{dictionary}
    The @fun{gtk:shortcuts-section-max-height} function is deprecated since
    4.18. This widget will be removed in GTK 5.
  @end{dictionary}
  @see-class{gtk:shortcuts-section}")

;;; --- gtk:shortcuts-section-section-name -------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "section-name"
                      'shortcuts-section) t)
 "The @code{section-name} property of type @code{:string} (Read / Write) @br{}
  The unique name to identify this section among the sections added to the
  @class{gtk:shortcuts-window} widget. Setting the
  @slot[gtk:shortcuts-section]{section-name} property to this string will make
  this section shown in the @class{gtk:shortcuts-window} widget. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'shortcuts-section-section-name)
      "Accessor"
      (documentation 'shortcuts-section-section-name 'function)
 "@version{2025-05-14}
  @syntax{(gtk:shortcuts-section-section-name object) => name}
  @syntax{(setf (gtk:shortcuts-section-section-name object) name)}
  @argument[object]{a @class{gtk:shortcuts-section} widget}
  @argument[name]{a string for a unique name to identify this section}
  @begin{short}
    Accessor of the @slot[gtk:shortcuts-section]{section-name} slot of the
    @class{gtk:shortcuts-section} class.
  @end{short}
  A unique name to identify this section among the sections added to the
  @class{gtk:shortcuts-window} widget. Setting the
  @slot[gtk:shortcuts-section]{section-name} property to this string will make
  this section shown in the @class{gtk:shortcuts-window} widget.
  @begin[Warning]{dictionary}
    The @fun{gtk:shortcuts-section-section-name} function is deprecated since
    4.18. This widget will be removed in GTK 5.
  @end{dictionary}
  @see-class{gtk:shortcuts-section}
  @see-class{gtk:shortcuts-window}")

;;; --- gtk:shortcuts-section-title --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "title" 'shortcuts-section) t)
 "The @code{title} property of type @code{:string} (Read / Write) @br{}
  The string to show in the section selector of the @class{gtk:shortcuts-window}
  widget for this section. If there is only one section, you do not need to set
  a title, since the section selector will not be shown in this case. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'shortcuts-section-title)
      "Accessor"
      (documentation 'shortcuts-section-title 'function)
 "@version{2025-05-14}
  @syntax{(gtk:shortcuts-section-title object) => title}
  @syntax{(setf (gtk:shortcuts-section-title object) title)}
  @argument[object]{a @class{gtk:shortcuts-section} widget}
  @argument[title]{a string to show in the section selector}
  @begin{short}
    Accessor of the @slot[gtk:shortcuts-section]{title} slot of the
    @class{gtk:shortcuts-section} class.
  @end{short}
  The string to show in the section selector of the @class{gtk:shortcuts-window}
  widget for this section. If there is only one section, you do not need to set
  a title, since the section selector will not be shown in this case.
  @begin[Warning]{dictionary}
    The @fun{gtk:shortcuts-section-title} function is deprecated since 4.18.
    This widget will be removed in GTK 5.
  @end{dictionary}
  @see-class{gtk:shortcuts-section}
  @see-class{gtk:shortcuts-window}")

;;; --- gtk:shortcuts-section-view-name ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "view-name"
                                               'shortcuts-section) t)
 "The @code{view-name} property of type @code{:string} (Read / Write) @br{}
  The view name to filter the groups in this section by. See the
  @slot[gtk:shortcuts-group]{view} property. Applications are expected to use
  the @slot[gtk:shortcuts-section]{view-name} property for this purpose. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'shortcuts-section-view-name)
      "Accessor"
      (documentation 'shortcuts-section-view-name 'function)
 "@version{2025-05-14}
  @syntax{(gtk:shortcuts-section-view-name object) => name}
  @syntax{(setf (gtk:shortcuts-section-view-name object) name)}
  @argument[object]{a @class{gtk:shortcuts-section} widget}
  @argument[name]{a string for a view name to filter the groups in this
    section by}
  @begin{short}
    Accessor of the @slot[gtk:shortcuts-section]{view-name} slot of the
    @class{gtk:shortcuts-section} class.
  @end{short}
  A view name to filter the groups in this section by. See the
  @slot[gtk:shortcuts-group]{view} property. Applications are expected to use
  the @slot[gtk:shortcuts-section]{view-name} property for this purpose.
  @begin[Warning]{dictionary}
    The @fun{gtk:shortcuts-section-view-name} function is deprecated since
    4.18. This widget will be removed in GTK 5.
  @end{dictionary}
  @see-class{gtk:shortcuts-section}
  @see-function{gtk:shortcuts-group-view}")

;;; ----------------------------------------------------------------------------
;;; gtk_shortcuts_section_add_group                         Since 4.14
;;; ----------------------------------------------------------------------------

#+gtk-4-14
(cffi:defcfun ("gtk_shortcuts_section_add_group" shortcuts-section-add-group)
    :void
 #+liber-documentation
 "@version{#2025-05-14}
  @argument[section]{a @class{gtk:shortcuts-section} widget}
  @argument[group]{a @class{gtk:shortcuts-group} widget to add}
  @begin{short}
    Adds a group to the shortcuts section.
  @end{short}
  This is the programmatic equivalent to using a @class{gtk:builder} UI
  definition and a @code{<child>} tag to add the child. Adding children with the
  @class{gtk:box} API is not appropriate, as the @class{gtk:shortcuts-section}
  widget manages its children internally.

  Since 4.14
  @begin[Warning]{dictionary}
    The @fun{gtk:shortcuts-section-add-group} function is deprecated since
    4.18. This widget will be removed in GTK 5.
  @end{dictionary}
  @see-class{gtk:shortcuts-section}
  @see-class{gtk:shortcuts-group}"
  (section (g:object shortcuts-section))
  (group (g:object shortcuts-group)))

#+gtk-4-14
(export 'shortcuts-section-add-group)

;;; --- End of file gtk4.shortcuts-section.lisp --------------------------------

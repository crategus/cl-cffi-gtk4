;;; ----------------------------------------------------------------------------
;;; gtk4.shortcuts-section.lisp
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
;;; GtkShortcutsSection
;;;
;;;     Represents an application mode in a GtkShortcutsWindow
;;;
;;; Types and Values
;;;
;;;     GtkShortcutsSection
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
;;; struct GtkShortcutsSection
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkShortcutsSection" shortcuts-section
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

#+liber-documentation
(setf (documentation 'shortcuts-section 'type)
 "@version{2023-8-28}
  @begin{short}
    A @class{gtk:shortcuts-section} widget collects all the keyboard shortcuts
    and gestures for a major application mode.
  @end{short}
  If your application needs multiple sections, you should give each section a
  unique @code{section-name} and a @code{title} that can be shown in the
  section selector of the @class{gtk:shortcuts-window} widget.

  The @code{max-height} property can be used to influence how the groups in the
  section are distributed over pages and columns.

  This widget is only meant to be used with @class{gtk:shortcuts-window}
  widgets.
  @begin[Signal Details]{dictionary}
    @subheading{The \"change-current-page\" signal}
      @begin{pre}
lambda (shortcutsection arg)    :action
      @end{pre}
      @begin[code]{table}
        @entry[shortcutswindow]{The @class{gtk:shortcuts-window} object.}
        @entry[arg]{An integer, no description available.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk:shortcuts-section-max-height}
  @see-slot{gtk:shortcuts-section-section-name}
  @see-slot{gtk:shortcuts-section-title}
  @see-slot{gtk:shortcuts-section-view-name}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- shortcuts-section-max-height -------------------------------------------

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
 "@version{#2020-9-8}
  @syntax[]{(gtk:shortcuts-section-max-height object) => max-height}
  @syntax[]{(setf (gtk:shortcuts-section-max-height object) max-height)}
  @argument[object]{a @class{gtk:shortcuts-section} widget}
  @argument[max-height]{a @code{:uint} with the maximum number of lines to
    allow per column}
  @begin{short}
    Accessor of the @slot[gtk:shortcuts-section]{max-height} slot of the
    @class{gtk:shortcuts-section} class.
  @end{short}
  The maximum number of lines to allow per column. This property can be used to
  influence how the groups in this section are distributed across pages and
  columns. The default value of 15 should work in for most cases.
  @see-class{gtk:shortcuts-section}")

;;; --- shortcuts-section-section-name -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "section-name"
                      'shortcuts-section) t)
 "The @code{section-name} property of type @code{:string} (Read / Write) @br{}
  A unique name to identify this section among the sections added to the
  @class{gtk:shortcuts-window}. Setting the @code{section-name} property to this
  string will make this section shown in the @class{gtk:shortcuts-window}. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'shortcuts-section-section-name)
      "Accessor"
      (documentation 'shortcuts-section-section-name 'function)
 "@version{#2020-9-8}
  @syntax[]{(gtk:shortcuts-section-section-name object) => section-name}
  @syntax[]{(setf (gtk:shortcuts-section-section-name object) section-name)}
  @argument[object]{a @class{gtk:shortcuts-section} widget}
  @argument[section-name]{a @code{:string} with a unique name to identify this
    section}
  @begin{short}
    Accessor of the @slot[gtk:shortcuts-section]{section-name} slot of the
    @class{gtk:shortcuts-section} class.
  @end{short}
  A unique name to identify this section among the sections added to the
  @class{gtk:shortcuts-window} widget. Setting the @code{section-name} property
  to this string will make this section shown in the
  @class{gtk:shortcuts-window} widget.
  @see-class{gtk:shortcuts-section}")

;;; --- shortcuts-section-title ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "title" 'shortcuts-section) t)
 "The @code{title} property of type @code{:string} (Read / Write) @br{}
  The string to show in the section selector of the @class{gtk:shortcuts-window}
  for this section. If there is only one section, you do not need to set a
  title, since the section selector will not be shown in this case. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'shortcuts-section-title)
      "Accessor"
      (documentation 'shortcuts-section-title 'function)
 "@version{#2020-9-8}
  @syntax[]{(gtk:shortcuts-section-title object) => title}
  @syntax[]{(setf (gtk:shortcuts-section-title object) title)}
  @argument[object]{a @class{gtk:shortcuts-section} widget}
  @argument[title]{a @code{:string} to show in the section selector}
  @begin{short}
    Accessor of the @slot[gtk:shortcuts-section]{title} slot of the
    @class{gtk:shortcuts-section} class.
  @end{short}
  The string to show in the section selector of the @class{gtk:shortcuts-window}
  for this section. If there is only one section, you do not need to set a
  title, since the section selector will not be shown in this case.
  @see-class{gtk:shortcuts-section}")

;;; --- shortcuts-section-view-name --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "view-name"
                      'shortcuts-section) t)
 "The @code{view-name} property of type @code{:string} (Read / Write) @br{}
  A view name to filter the groups in this section by. See \"view\".
  Applications are expected to use the @code{view-name} property for this
  purpose. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'shortcuts-section-view-name)
      "Accessor"
      (documentation 'shortcuts-section-view-name 'function)
 "@version{#2020-9-8}
  @syntax[]{(gtk:shortcuts-section-view-name object) => view-name}
  @syntax[]{(setf (gtk:shortcuts-section-view-name object) view-name)}
  @argument[object]{a @class{gtk:shortcuts-section} widget}
  @argument[view-name]{a @code{:string} with a view name to filter the groups
    in this section by}
  @begin{short}
    Accessor of the slot @slot[gtk:shortcuts-section]{view-name} of the
    @class{gtk:shortcuts-section} class.
  @end{short}
  A view name to filter the groups in this section by. See \"view\".
  Applications are expected to use the @code{view-name} property for this
  purpose.
  @see-class{gtk:shortcuts-section}")

;;; --- End of file gtk4.shortcuts-section.lisp --------------------------------

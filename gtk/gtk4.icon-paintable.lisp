;;; ----------------------------------------------------------------------------
;;; gtk4.icon-paintable.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.16 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 - 2024 Dieter Kaiser
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
;;; GtkIconPaintable
;;;
;;;     Contains information found when looking up an icon in an icon theme.
;;;
;;; Types and Values
;;;
;;;     GtkSymbolicPaintable                               Since 4.6
;;;     GtkIconPaintable
;;;
;;; Accessors
;;;
;;;     gtk_icon_paintable_get_file
;;;     gtk_icon_paintable_get_icon_name
;;;     gtk_icon_paintable_is_symbolic
;;;
;;; Functions
;;;
;;;     gtk_icon_paintable_new_for_file
;;;
;;;     gtk_symbolic_paintable_snapshot_symbolic           Since 4.6
;;;
;;; Properties
;;;
;;;     file
;;;     icon-name
;;;     is-symbolic
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkIconPaintable
;;;
;;; Implementes
;;;
;;;     GdkPaintable
;;;     GtkSymbolicPaintable (since 4.6)
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkSymbolicPaintable
;;; ----------------------------------------------------------------------------

#+gtk-4-6
(gobject:define-ginterface "GtkSymbolicPaintable" symbolic-paintable
  (:export t
   :type-initializer "gtk_symbolic_paintable_get_type")
  nil)

#+(and gtk-4-6 liber-documentation)
(setf (liber:alias-for-class 'symbolic-paintable)
      "Interface"
      (documentation 'symbolic-paintable 'type)
 "@version{2023-8-30}
  @begin{short}
    The @class{gtk:symbolic-paintable} interface is an interface that support
    symbolic colors in paintables.
  @end{short}
  The @class{gdk:paintable} classes implementing the interface will have the
  @code{Gtk.SymbolicPaintableInterface.snapshot_symbolic} virtual function
  called and have the colors for drawing symbolic icons passed. At least 4
  colors are guaranteed to be passed every time.

  These 4 colors are the foreground color, and the colors to use for errors,
  warnings and success information in that order. More colors may be added in
  the future.

  Since 4.6
  @see-class{gdk:paintable}
  @see-class{gtk:icon-paintable}")

;;; ----------------------------------------------------------------------------
;;; gtk_symbolic_paintable_snapshot_symbolic
;;;
;;; void
;;; gtk_symbolic_paintable_snapshot_symbolic (GtkSymbolicPaintable* paintable,
;;;                                           GdkSnapshot* snapshot,
;;;                                           double width,
;;;                                           double height,
;;;                                           const GdkRGBA* colors,
;;;                                           gsize n_colors)
;;;
;;; Snapshots the paintable with the given colors.
;;;
;;; If less than 4 colors are provided, GTK will pad the array with default
;;; colors.
;;;
;;; Available since: 4.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkIconPaintable
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkIconPaintable" icon-paintable
  (:superclass g:object
   :export t
   :interfaces #-gtk-4-6
               ("GdkPaintable")
               #+gtk-4-6
               ("GdkPaintable" "GtkSymbolicPaintable")
   :type-initializer "gtk_icon_paintable_get_type")
  ((file
    icon-paintable-file
    "file" "GFile" t t)
   (icon-name
    icon-paintable-icon-name
    "icon-name" "gchararray" t t)
   (is-symbolic
    icon-paintable-is-symbolic
    "is-symbolic" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'icon-paintable 'type)
 "@version{2023-8-30}
  @begin{short}
    Contains information found when looking up an icon in a
    @class{gtk:icon-theme} object and supports painting it as a
    @class{gdk:paintable} object.
  @end{short}
  The @class{gtk:icon-paintable} class implements the @class{gdk:paintable} and
  @class{gtk:symbolic-paintable} interfaces.
  @see-slot{gtk:icon-paintable-file}
  @see-slot{gtk:icon-paintable-icon-name}
  @see-slot{gtk:icon-paintable-is-symbolic}
  @see-constructor{gtk:icon-paintable-new-for-file}
  @see-class{gtk:icon-theme}
  @see-class{gdk:paintable}
  @see-class{gtk:symbol-paintable}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:icon-paintable-file ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "file" 'icon-paintable) t)
 "The @code{file} property of type @class{g-file} (Read / Write / Construct
  only) @br{}
  The file representing the icon, if any.")

#+liber-documentation
(setf (liber:alias-for-function 'icon-paintable-file)
      "Accessor"
      (documentation 'icon-paintable-file 'function)
 "@version{2023-8-30}
  @syntax{(gtk:icon-paintable-file object) => file}
  @argument[object]{a @class{gtk:icon-paintable} object}
  @argument[file]{a @class{g-file} object for the icon, or @code{nil}}
  @begin{short}
    Accessor of the @slot[gtk:icon-paintable]{file} slot of the
    @class{gtk:icon-paintable} class.
  @end{short}
  The @fun{gtk:icon-paintable-file} function gets the @class{g-file} object
  that was used to load the icon, or @code{nil} if the icon was not loaded from
  a file.
  @see-class{gtk:icon-paintable}
  @see-class{g-file}")

;;; --- gtk:icon-paintable-icon-name -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "icon-name" 'icon-paintable) t)
 "The @code{icon-name} property of type @code{:string}
  (Read / Write / Construct only) @br{}
  The icon name that was chosen during lookup.")

#+liber-documentation
(setf (liber:alias-for-function 'icon-paintable-icon-name)
      "Accessor"
      (documentation 'icon-paintable-icon-name 'function)
 "@version{2023-8-30}
  @syntax{(gtk:icon-paintable-icon-name object) => name}
  @argument[object]{a @class{gtk:icon-paintable} object}
  @argument[name]{a string with the themed icon name for the icon, or
    @code{nil} if its not a themed icon}
  @begin{short}
    Accessor of the @slot[gtk:icon-paintable]{icon-name} slot of the
    @class{gtk:icon-paintable} class.
  @end{short}
  The @fun{gtk:icon-paintable-icon-name} function gets the icon name being used
  for this icon.

  When an icon looked up in the icon theme was not available, the icon theme
  may use fallback icons - either those specified to the
  @fun{gtk:icon-theme-lookup-icon} function or the always-available
  \"image-missing\". The icon chosen is returned by this function.

  If the icon was created without an icon theme, this function returns
  @code{nil}.
  @see-class{gtk:icon-paintable}
  @see-class{gtk:icon-theme}
  @see-function{gtk:icon-theme-lookup-icon}")

;;; --- gtk:icon-paintable-is-symbolic -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "is-symbolic" 'icon-paintable) t)
 "The @code{is-symbolic} property of type @code{:boolean} (Read / Write /
  Construct only) @br{}
  Whether the icon is symbolic or not.")

#+liber-documentation
(setf (liber:alias-for-function 'icon-paintable-is-symbolic)
      "Accessor"
      (documentation 'icon-paintable-is-symbolic 'function)
 "@version{2023-8-30}
  @syntax{(gtk:icon-paintable-is-symbolic object) => symbolic}
  @argument[object]{a @class{gtk:icon-paintable} object}
  @argument[symbolic]{@em{true} if the icon is symbolic, @em{false} otherwise}
  @begin{short}
    Accessor of the @slot[gtk:icon-paintable]{is-symbolic} slot of the
    @class{gtk:icon-paintable} class.
  @end{short}
  The @fun{gtk:icon-paintable-is-symbolic} function checks if the icon is
  symbolic or not. This currently uses only the file name and not the file
  contents for determining this. This behaviour may change in the future.

  Note that to render a symbolic @class{gtk:icon-paintable} object properly,
  with recoloring, you have to set its icon name on a @class{gtk:image} widget.
  @see-class{gtk:icon-paintable}
  @see-class{gtk:image}")

;;; ----------------------------------------------------------------------------
;;; gtk_icon_paintable_new_for_file
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_paintable_new_for_file" icon-paintable-new-for-file)
    (g:object icon-paintable :already-referenced)
 #+liber-documentation
 "@version{2024-10-9}
  @argument[file]{a @class{g-file} object}
  @argument[size]{an integer with the desired icon size}
  @argument[scale]{an integer with he desired scale}
  @return{The @class{gtk:icon-paintable} object containing the icon.}
  @begin{short}
    Creates a @class{gtk:icon-paintable} object for a file with a given size
    and scale.
  @end{short}
  The icon can then be rendered by using it as a @class{gdk:paintable} object.
  @see-class{gtk:icon-paintable}
  @see-class{gdk:paintable}
  @see-class{g-file}"
  (file g:object)
  (size :int)
  (scale :int))

(export 'icon-paintable-new-for-file)

;;; --- Enf of file gtk4.icon-paintable.lisp -----------------------------------

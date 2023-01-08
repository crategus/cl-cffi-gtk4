;;; ----------------------------------------------------------------------------
;;; gtk.fixed-layout.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 Dieter Kaiser
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
;;; GtkFixedLayout
;;;
;;;     A layout manager that allows positioning at fixed coordinates
;;;
;;; Types and Values
;;;
;;;     GtkFixedLayout
;;;     GtkFixedLayoutChild
;;;
;;; Functions
;;;
;;;     gtk_fixed_layout_new
;;;     gtk_fixed_layout_child_set_transform
;;;     gtk_fixed_layout_child_get_transform
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ├── GtkLayoutChild
;;;     │   ╰── GtkFixedLayoutChild
;;;     ╰── GtkLayoutManager
;;;         ╰── GtkFixedLayout
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkFixedLayoutChild
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkFixedLayoutChild" fixed-layout-child
  (:superclass layout-child
   :export t
   :interfaces ()
   :type-initializer "gtk_fixed_layout_child_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; GtkFixedLayout
;;;
;;; GtkFixedLayout is a layout manager which can place child widgets at fixed
;;; positions, and with fixed sizes.
;;;
;;; Most applications should never use this layout manager; fixed positioning
;;; and sizing requires constant recalculations on where children need to be
;;; positioned and sized. Other layout managers perform this kind of work
;;; internally so that application developers don't need to do it. Specifically,
;;; widgets positioned in a fixed layout manager will need to take into account:
;;;
;;; Themes, which may change widget sizes.
;;;
;;; Fonts other than the one you used to write the app will of course change the
;;; size of widgets containing text; keep in mind that users may use a larger
;;; font because of difficulty reading the default, or they may be using a
;;; different OS that provides different fonts.
;;;
;;; Translation of text into other languages changes its size. Also, display of
;;; non-English text will use a different font in many cases.
;;;
;;; In addition, GtkFixedLayout does not pay attention to text direction and
;;; thus may produce unwanted results if your app is run under right-to-left
;;; languages such as Hebrew or Arabic. That is: normally GTK will order
;;; containers appropriately depending on the text direction, e.g. to put labels
;;; to the right of the thing they label when using an RTL language;
;;; GtkFixedLayout won't be able to do that for you.
;;;
;;; Finally, fixed positioning makes it kind of annoying to add/remove GUI
;;; elements, since you have to reposition all the other elements. This is a
;;; long-term maintenance problem for your application.
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkFixedLayout" fixed-layout
  (:superclass layout-manager
   :export t
   :interfaces ()
   :type-initializer "gtk_fixed_layout_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; gtk_fixed_layout_new ()
;;;
;;; GtkLayoutManager *
;;; gtk_fixed_layout_new (void);
;;;
;;; Creates a new GtkFixedLayout.
;;;
;;; Returns :
;;;     the newly created GtkFixedLayout
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_fixed_layout_child_set_transform ()
;;;
;;; void
;;; gtk_fixed_layout_child_set_transform (GtkFixedLayoutChild *child,
;;;                                       GskTransform *transform);
;;;
;;; Sets the transformation of the child of a GtkFixedLayout.
;;;
;;; child :
;;;     a GtkFixedLayoutChild
;;;
;;; transform :
;;;     a GskTransform
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_fixed_layout_child_get_transform ()
;;;
;;; GskTransform *
;;; gtk_fixed_layout_child_get_transform (GtkFixedLayoutChild *child);
;;;
;;; Retrieves the transformation of the child of a GtkFixedLayout.
;;;
;;; child :
;;;     a GtkFixedLayoutChild
;;;
;;; Returns :
;;;     a GskTransform.
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.fixed-layout.lisp --------------------------------------

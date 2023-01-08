;;; ----------------------------------------------------------------------------
;;; gtk.overlay-layout.lisp
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
;;; GtkOverlayLayout
;;;
;;;     Layout manager that places widgets as overlays
;;;
;;; Types and Values
;;;
;;;     GtkOverlayLayout
;;;     GtkOverlayLayoutChild
;;;
;;; Accessors
;;;
;;;     gtk_overlay_layout_child_set_measure
;;;     gtk_overlay_layout_child_get_measure
;;;     gtk_overlay_layout_child_set_clip_overlay
;;;     gtk_overlay_layout_child_get_clip_overlay
;;;
;;; Functions
;;;
;;;     gtk_overlay_layout_new
;;;
;;; Properties
;;;
;;;     clip-overlay
;;;     measure
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ├── GtkLayoutChild
;;;     │   ╰── GtkOverlayLayoutChild
;;;     ╰── GtkLayoutManager
;;;         ╰── GtkOverlayLayout
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkOverlayLayoutChild
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkOverlayLayoutChild" overlay-layout-child
  (:superclass layout-child
   :export t
   :interfaces ()
   :type-initializer "gtk_overlay_layout_child_get_type")
  ((clip-overlay
    overlay-layout-child-clip-overlay
    "clip-overlay" "gboolean" t t)
   (measure
    overlay-layout-child-measure
    "measure" "gboolean" t t)))

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “clip-overlay” property
;;;
;;;  “clip-overlay”             gboolean
;;;
;;; Whether the child should be clipped to fit the parent's size.
;;;
;;; Owner: GtkOverlayLayoutChild
;;;
;;; Flags: Read / Write
;;;
;;; Default value: FALSE
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “measure” property
;;;
;;;  “measure”                  gboolean
;;;
;;; Whether the child size should contribute to the GtkOverlayLayout's
;;; measurement.
;;;
;;; Owner: GtkOverlayLayoutChild
;;;
;;; Flags: Read / Write
;;;
;;; Default value: FALSE
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkOverlayLayout
;;;
;;; GtkOverlayLayout is the layout manager used by GtkOverlay. It places widgets
;;; as overlays on top of the main child.
;;;
;;; This is not a reusable layout manager, since it expects its widget to be a
;;; GtkOverlay. It only listed here so that its layout properties get
;;; documented.
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkOverlayLayout" overlay-layout
  (:superclass layout-manager
   :export t
   :interfaces ()
   :type-initializer "gtk_overlay_layout_get_type")
  nil)



;;;Functions
;;;gtk_overlay_layout_new ()
;;;GtkLayoutManager *
;;;gtk_overlay_layout_new (void);
;;;Creates a new GtkOverlayLayout instance.

;;;Returns
;;;the newly created instance

;;;gtk_overlay_layout_child_set_measure ()
;;;void
;;;gtk_overlay_layout_child_set_measure (GtkOverlayLayoutChild *child,
;;;                                      gboolean measure);
;;;Sets whether to measure this child.

;;;Parameters
;;;child

;;;a GtkOverlayLayoutChild

;;;measure

;;;whether to measure this child

;;;gtk_overlay_layout_child_get_measure ()
;;;gboolean
;;;gtk_overlay_layout_child_get_measure (GtkOverlayLayoutChild *child);
;;;Retrieves whether the child is measured.

;;;Parameters
;;;child

;;;a GtkOverlayLayoutChild

;;;Returns
;;;whether the child is measured

;;;gtk_overlay_layout_child_set_clip_overlay ()
;;;void
;;;gtk_overlay_layout_child_set_clip_overlay
;;;                               (GtkOverlayLayoutChild *child,
;;;                                gboolean clip_overlay);
;;;Sets whether to clip this child.

;;;Parameters
;;;child

;;;a GtkOverlayLayoutChild

;;;clip_overlay

;;;whether to clip this child

;;;gtk_overlay_layout_child_get_clip_overlay ()
;;;gboolean
;;;gtk_overlay_layout_child_get_clip_overlay
;;;                               (GtkOverlayLayoutChild *child);
;;;Retrieves whether the child is clipped.

;;;Parameters
;;;child

;;;a GtkOverlayLayoutChild

;;;Returns
;;;whether the child is clipped


;;; --- End of file gtk.overlay-layout.lisp ------------------------------------

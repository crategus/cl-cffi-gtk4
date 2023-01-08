;;; ----------------------------------------------------------------------------
;;; gtk.constraint-guide.lisp
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
;;; GtkConstraintGuide
;;;
;;;     An invisible constraint target
;;;
;;; Types and Values
;;;
;;;     GtkConstraintGuide
;;;
;;; Accessor
;;;
;;;     gtk_constraint_guide_set_name
;;;     gtk_constraint_guide_get_name
;;;     gtk_constraint_guide_set_strength
;;;     gtk_constraint_guide_get_strength
;;;
;;; Functions
;;;
;;;     gtk_constraint_guide_new
;;;     gtk_constraint_guide_set_min_size
;;;     gtk_constraint_guide_get_min_size
;;;     gtk_constraint_guide_set_nat_size
;;;     gtk_constraint_guide_get_nat_size
;;;     gtk_constraint_guide_set_max_size
;;;     gtk_constraint_guide_get_max_size
;;;
;;; Properties
;;;
;;;     max-height
;;;     max-width
;;;     min-height
;;;     min-width
;;;     name
;;;     nat-height
;;;     nat-width
;;;     strength
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkConstraintGuide
;;;
;;; Implemented Interfaces
;;;
;;;     GtkConstraintTarget
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkConstraintGuide
;;;
;;; An object that can be added to a GtkConstraintLayout and be used in
;;; constraints like a widget, without being drawn.
;;;
;;; Guides have a minimum, maximum and natural size. Depending on the
;;; constraints that are applied, they can act like a guideline that widgets
;;; can be aligned to, or like 'flexible space'.
;;;
;;; Description
;;;
;;; A GtkConstraintGuide is an invisible layout element that can be used by
;;; widgets inside a GtkConstraintLayout as a source or a target of a
;;; GtkConstraint. Guides can be used like guidelines or as flexible space.
;;;
;;; Unlike a GtkWidget, a GtkConstraintGuide will not be drawn.
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkConstraintGuide" constraint-guide
  (:superclass g:object
   :export t
   :interfaces ()
   :type-initializer "gtk_constraint_guide_get_type")
  ((max-height
    constraint-guide-max-height
    "max-height" "gint" t t)
   (max-width
    constraint-guide-max-width
    "max-width" "gint" t t)
   (min-height
    constraint-guide-min-height
    "min-height" "gint" t t)
   (min-width
    constraint-guide-min-width
    "min-width" "gint" t t)
   (name
    constraint-guide-name
    "name" "gchararray" t t)
   (nat-height
    constraint-guide-nat-height
    "nat-height" "gint" t t)
   (nat-width
    constraint-guide-nat-width
    "nat-width" "gint" t t)
   (strength
    constraint-guide-strength
    "strength" "GtkConstraintStrength" t t)))

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “max-height” property
;;;
;;;  “max-height”               int
;;;
;;; The maximum height of the guide.
;;;
;;; Flags: Read / Write
;;; Allowed values: >= 0
;;; Default value: 2147483647
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “max-width” property
;;;
;;;  “max-width”                int
;;;
;;; The maximum width of the guide.
;;;
;;; Flags: Read / Write
;;; Allowed values: >= 0
;;; Default value: 2147483647
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “min-height” property
;;;
;;;  “min-height”               int
;;;
;;; The minimum height of the guide.
;;;
;;; Flags: Read / Write
;;; Allowed values: >= 0
;;; Default value: 0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “min-width” property
;;;
;;;  “min-width”                int
;;;
;;; The minimum width of the guide.
;;;
;;; Flags: Read / Write
;;; Allowed values: >= 0
;;; Default value: 0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “name” property
;;;
;;;  “name”                     char *
;;;
;;; A name that identifies the GtkConstraintGuide, for debugging.
;;;
;;; Flags: Read / Write
;;; Default value: NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “nat-height” property
;;;
;;;  “nat-height”               int
;;;
;;; The preferred, or natural, height of the guide.
;;;
;;; Flags: Read / Write
;;; Allowed values: >= 0
;;; Default value: 0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “nat-width” property
;;;
;;;  “nat-width”                int
;;;
;;; The preferred, or natural, width of the guide.
;;;
;;; Flags: Read / Write
;;; Allowed values: >= 0
;;; Default value: 0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “strength” property
;;;
;;;  “strength”                 GtkConstraintStrength
;;;
;;; The GtkConstraintStrength to be used for the constraint on the natural size 
;;; of the guide.
;;;
;;; Flags: Read / Write
;;; Default value: GTK_CONSTRAINT_STRENGTH_MEDIUM
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_constraint_guide_new ()
;;;
;;; GtkConstraintGuide *
;;; gtk_constraint_guide_new (void);
;;;
;;; Creates a new GtkConstraintGuide object.
;;;
;;; Return : 
;;;     a new GtkConstraintGuide object.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_constraint_guide_set_name ()
;;;
;;; void
;;; gtk_constraint_guide_set_name (GtkConstraintGuide *guide,
;;;                                const char *name);
;;;
;;; Sets a name for the given GtkConstraintGuide.
;;;
;;; The name is useful for debugging purposes.
;;;
;;; guide :
;;;     a GtkConstraintGuide
;;;
;;; name :
;;;     a name for the guide .
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_constraint_guide_get_name ()
;;;
;;; const char *
;;; gtk_constraint_guide_get_name (GtkConstraintGuide *guide);
;;;
;;; Retrieves the name set using gtk_constraint_guide_set_name().
;;;
;;; guide :
;;;     a GtkConstraintGuide
;;;
;;; Returns :
;;;     the name of the guide.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_constraint_guide_set_strength ()
;;;
;;; void
;;; gtk_constraint_guide_set_strength (GtkConstraintGuide *guide,
;;;                                    GtkConstraintStrength strength);
;;;
;;; Sets the strength of the constraint on the natural size of the given 
;;; GtkConstraintGuide.
;;;
;;; guide :
;;;     a GtkConstraintGuide
;;;
;;; strength :
;;;     the strength of the constraint
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_constraint_guide_get_strength ()
;;;
;;; GtkConstraintStrength
;;; gtk_constraint_guide_get_strength (GtkConstraintGuide *guide);
;;;
;;; Retrieves the strength set using gtk_constraint_guide_set_strength().
;;;
;;; guide :
;;;     a GtkConstraintGuide
;;;
;;; Returns :
;;;     the strength of the constraint on the natural size
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_constraint_guide_set_min_size ()
;;;
;;; void
;;; gtk_constraint_guide_set_min_size (GtkConstraintGuide *guide,
;;;                                    int width,
;;;                                    int height);
;;;
;;; Sets the minimum size of guide .
;;;
;;; If guide is attached to a GtkConstraintLayout, the constraints will be 
;;; updated to reflect the new size.
;;;
;;; guide :
;;;     a GtkConstraintGuide object
;;;
;;; width :
;;;     the new minimum width, or -1 to not change it
;;;
;;; height :
;;;     the new minimum height, or -1 to not change it
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_constraint_guide_get_min_size ()
;;;
;;; void
;;; gtk_constraint_guide_get_min_size (GtkConstraintGuide *guide,
;;;                                    int *width,
;;;                                    int *height);
;;;
;;; Gets the minimum size of guide .
;;;
;;; guide :
;;;     a GtkConstraintGuide object
;;;
;;; width :
;;;     return location for the minimum width, or NULL.
;;;
;;; height :
;;;     return location for the minimum height, or NULL.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_constraint_guide_set_nat_size ()
;;;
;;; void
;;; gtk_constraint_guide_set_nat_size (GtkConstraintGuide *guide,
;;;                                    int width,
;;;                                    int height);
;;;
;;; Sets the natural size of guide .
;;;
;;; If guide is attached to a GtkConstraintLayout, the constraints will be 
;;; updated to reflect the new size.
;;;
;;; guide :
;;;     a GtkConstraintGuide object
;;;
;;; width :
;;;     the new natural width, or -1 to not change it
;;;
;;; height :
;;;     the new natural height, or -1 to not change it
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_constraint_guide_get_nat_size ()
;;;
;;; void
;;; gtk_constraint_guide_get_nat_size (GtkConstraintGuide *guide,
;;;                                    int *width,
;;;                                    int *height);
;;;
;;; Gets the natural size of guide .
;;;
;;; guide :
;;;     a GtkConstraintGuide object
;;;
;;; width :
;;;     return location for the natural width, or NULL.
;;;
;;; height :
;;;     return location for the natural height, or NULL.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_constraint_guide_set_max_size ()
;;;
;;; void
;;; gtk_constraint_guide_set_max_size (GtkConstraintGuide *guide,
;;;                                    int width,
;;;                                    int height);
;;;
;;; Sets the maximum size of guide .
;;;
;;; If guide is attached to a GtkConstraintLayout, the constraints will be 
;;; updated to reflect the new size.
;;;
;;; guide :
;;;     a GtkConstraintGuide object
;;;
;;; width :
;;;     the new maximum width, or -1 to not change it
;;;
;;; height :
;;;     the new maximum height, or -1 to not change it
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_constraint_guide_get_max_size ()
;;;
;;; void
;;; gtk_constraint_guide_get_max_size (GtkConstraintGuide *guide,
;;;                                    int *width,
;;;                                    int *height);
;;;
;;; Gets the maximum size of guide . 
;;;
;;; guide :
;;;     a GtkConstraintGuide object
;;;
;;; width :
;;;     return location for the maximum width, or NULL.
;;;
;;; height :
;;;     return location for the maximum height, or NULL.
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.constraint-guide.lisp ----------------------------------

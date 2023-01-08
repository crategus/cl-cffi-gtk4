;;; ----------------------------------------------------------------------------
;;; gtk.constraint-layout.lisp
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
;;; GtkConstraintLayout
;;;
;;;     A layout manager using constraints
;;;
;;; Types and Values
;;;
;;;     GtkConstraintLayout
;;;     GtkConstraintLayoutChild
;;;     GtkConstraintVflParserError
;;;
;;; Functions
;;;
;;;     gtk_constraint_layout_new
;;;     gtk_constraint_layout_add_constraint
;;;     gtk_constraint_layout_remove_constraint
;;;     gtk_constraint_layout_remove_all_constraints
;;;     gtk_constraint_layout_add_guide
;;;     gtk_constraint_layout_remove_guide
;;;     gtk_constraint_layout_add_constraints_from_description
;;;     gtk_constraint_layout_add_constraints_from_descriptionv
;;;     gtk_constraint_layout_observe_constraints
;;;     gtk_constraint_layout_observe_guides
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ├── GtkLayoutChild
;;;     │   ╰── GtkConstraintLayoutChild
;;;     ╰── GtkLayoutManager
;;;         ╰── GtkConstraintLayout
;;;
;;; Implemented Interfaces
;;;
;;;     GtkBuildable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; enum GtkConstraintVflParserError
;;;
;;; Domain for VFL parsing errors.
;;;
;;; GTK_CONSTRAINT_VFL_PARSER_ERROR_INVALID_SYMBOL :
;;;     Invalid or unknown symbol
;;;
;;; GTK_CONSTRAINT_VFL_PARSER_ERROR_INVALID_ATTRIBUTE :
;;;     Invalid or unknown attribute
;;;
;;; GTK_CONSTRAINT_VFL_PARSER_ERROR_INVALID_VIEW :
;;;     Invalid or unknown view
;;;
;;; GTK_CONSTRAINT_VFL_PARSER_ERROR_INVALID_METRIC :
;;;     Invalid or unknown metric
;;;
;;; GTK_CONSTRAINT_VFL_PARSER_ERROR_INVALID_PRIORITY :
;;;     Invalid or unknown priority
;;;
;;; GTK_CONSTRAINT_VFL_PARSER_ERROR_INVALID_RELATION :
;;;     Invalid or unknown relation
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkConstraintLayoutChild
;;;
;;; A GtkLayoutChild in a GtkConstraintLayout.
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkConstraintLayoutChild" constraint-layout-child
  (:superclass layout-child
   :export t
   :interfaces ()
   :type-initializer "gtk_constraint_layout_child_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; GtkConstraintLayout
;;;
;;; A layout manager using GtkConstraint to describe relations between widgets.
;;;
;;; Description
;;;
;;; GtkConstraintLayout is a layout manager that uses relations between widget 
;;; attributes, expressed via GtkConstraint instances, to measure and allocate 
;;; widgets.
;;;
;;; How do constraints work
;;;
;;; Constraints are objects defining the relationship between attributes of a 
;;; widget; you can read the description of the GtkConstraint class to have a 
;;; more in depth definition.
;;;
;;; By taking multiple constraints and applying them to the children of a widget 
;;; using GtkConstraintLayout, it's possible to describe complex layout 
;;; policies; each constraint applied to a child or to the parent widgets 
;;; contributes to the full description of the layout, in terms of parameters 
;;; for resolving the value of each attribute.
;;;
;;; It is important to note that a layout is defined by the totality of 
;;; constraints; removing a child, or a constraint, from an existing layout 
;;; without changing the remaining constraints may result in an unstable or 
;;; unsolvable layout.
;;;
;;; Constraints have an implicit "reading order"; you should start describing 
;;; each edge of each child, as well as their relationship with the parent 
;;; container, from the top left (or top right, in RTL languages), horizontally 
;;; first, and then vertically.
;;;
;;; A constraint-based layout with too few constraints can become "unstable", 
;;; that is: have more than one solution. The behavior of an unstable layout is 
;;; undefined.
;;;
;;; A constraint-based layout with conflicting constraints may be unsolvable, 
;;; and lead to an unstable layout. You can use the “strength” property of 
;;; GtkConstraint to "nudge" the layout towards a solution.
;;;
;;; GtkConstraintLayout as GtkBuildable
;;;
;;; GtkConstraintLayout implements the GtkBuildable interface and has a custom 
;;; "constraints" element which allows describing constraints in a GtkBuilder 
;;; UI file.
;;;
;;; An example of a UI definition fragment specifying a constraint:
;;;
;;; The definition above will add two constraints to the GtkConstraintLayout:
;;;
;;; a required constraint between the leading edge of "button" and the leading 
;;; edge of the widget using the constraint layout, plus 12 pixels
;;;
;;; a strong, constant constraint making the width of "button" greater than, or 
;;; equal to 250 pixels
;;;
;;; The "target" and "target-attribute" attributes are required.
;;;
;;; The "source" and "source-attribute" attributes of the "constraint" element 
;;; are optional; if they are not specified, the constraint is assumed to be a 
;;; constant.
;;;
;;; The "relation" attribute is optional; if not specified, the constraint is 
;;; assumed to be an equality.
;;;
;;; The "strength" attribute is optional; if not specified, the constraint is 
;;; assumed to be required.
;;;
;;; The "source" and "target" attributes can be set to "super" to indicate that
;;; the constraint target is the widget using the GtkConstraintLayout.
;;;
;;; There can be "constant" and "multiplier" attributes.
;;;
;;; Additionally, the "constraints" element can also contain a description of 
;;; the GtkConstraintGuides used by the layout:
;;;
;;; The "guide" element has the following optional attributes:
;;;
;;; "min-width", "nat-width", and "max-width", describe the minimum, natural, 
;;; and maximum width of the guide, respectively
;;;
;;; "min-height", "nat-height", and "max-height", describe the minimum, natural, 
;;; and maximum height of the guide, respectively
;;;
;;; "strength" describes the strength of the constraint on the natural size of 
;;; the guide; if not specified, the constraint is assumed to have a medium 
;;; strength
;;;
;;; "name" describes a name for the guide, useful when debugging
;;;
;;; Using the Visual Format Language
;;;
;;; Complex constraints can be described using a compact syntax called VFL, or 
;;; *Visual Format Language*.
;;;
;;; The Visual Format Language describes all the constraints on a row or column, 
;;; typically starting from the leading edge towards the trailing one. Each 
;;; element of the layout is composed by "views", which identify a 
;;; GtkConstraintTarget.
;;;
;;; For instance:
;;;
;;; Describes a constraint that binds the trailing edge of "button" to the 
;;; leading edge of "textField", leaving a default space between the two.
;;;
;;; Using VFL is also possible to specify predicates that describe constraints 
;;; on attributes like width and height:
;;;
;;; The default orientation for a VFL description is horizontal, unless 
;;; otherwise specified:
;;;
;;; It's also possible to specify multiple predicates, as well as their 
;;; strength:
;;;
;;; Finally, it's also possible to use simple arithmetic operators:
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkConstraintLayout" constraint-layout
  (:superclass layout-manager
   :export t
   :interfaces ()
   :type-initializer "gtk_constraint_layout_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; gtk_constraint_layout_new ()
;;;
;;; GtkLayoutManager *
;;; gtk_constraint_layout_new (void);
;;;
;;; Creates a new GtkConstraintLayout layout manager.
;;;
;;; Returns :
;;;     the newly created GtkConstraintLayout
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_constraint_layout_add_constraint ()
;;;
;;; void
;;; gtk_constraint_layout_add_constraint (GtkConstraintLayout *layout,
;;;                                       GtkConstraint *constraint);
;;;
;;; Adds a GtkConstraint to the layout manager.
;;;
;;; The “source” and “target” properties of constraint can be:
;;;
;;; set to NULL to indicate that the constraint refers to the widget using 
;;; layout
;;;
;;; set to the GtkWidget using layout
;;;
;;; set to a child of the GtkWidget using layout
;;;
;;; set to a guide that is part of layout
;;;
;;; The layout acquires the ownership of constraint after calling this function.
;;;
;;; layout :
;;;     a GtkConstraintLayout
;;;
;;; constraint :
;;;     a GtkConstraint.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_constraint_layout_remove_constraint ()
;;;
;;; void
;;; gtk_constraint_layout_remove_constraint
;;;                                (GtkConstraintLayout *layout,
;;;                                 GtkConstraint *constraint);
;;;
;;; Removes constraint from the layout manager, so that it no longer influences 
;;; the layout.
;;;
;;; layout :
;;;     a GtkConstraintLayout
;;;
;;; constraint :
;;;     a GtkConstraint
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_constraint_layout_remove_all_constraints ()
;;;
;;; void
;;; gtk_constraint_layout_remove_all_constraints
;;;                                (GtkConstraintLayout *layout);
;;;
;;; Removes all constraints from the layout manager.
;;;
;;; layout :
;;;     a GtkConstraintLayout
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_constraint_layout_add_guide ()
;;;
;;; void
;;; gtk_constraint_layout_add_guide (GtkConstraintLayout *layout,
;;;                                  GtkConstraintGuide *guide);
;;;
;;; Adds a guide to layout . A guide can be used as the source or target of 
;;; constraints, like a widget, but it is not visible.
;;;
;;; The layout acquires the ownership of guide after calling this function.
;;;
;;; layout :
;;;     a GtkConstraintLayout
;;;
;;; guide :
;;;     a GtkConstraintGuide object.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_constraint_layout_remove_guide ()
;;;
;;; void
;;; gtk_constraint_layout_remove_guide (GtkConstraintLayout *layout,
;;;                                     GtkConstraintGuide *guide);
;;;
;;; Removes guide from the layout manager, so that it no longer influences the 
;;; layout.
;;;
;;; layout :
;;;     a GtkConstraintLayout
;;;
;;; guide :
;;;     a GtkConstraintGuide object
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_constraint_layout_add_constraints_from_description ()
;;;
;;; GList *
;;; gtk_constraint_layout_add_constraints_from_description
;;;                                (GtkConstraintLayout *layout,
;;;                                 const char * const lines[],
;;;                                 gsize n_lines,
;;;                                 int hspacing,
;;;                                 int vspacing,
;;;                                 GError **error,
;;;                                 const char *first_view,
;;;                                 ...);
;;;
;;; Creates a list of constraints they formal description using a compact 
;;; description syntax called VFL, or "Visual Format Language".
;;;
;;; This function is a convenience wrapper around 
;;; gtk_constraint_layout_add_constraints_from_descriptionv(), using variadic 
;;; arguments to populate the view/target map.
;;;
;;; layout :
;;;     a GtkConstraintLayout
;;;
;;; lines :
;;;     an array of Visual Format Language lines defining a set of constraints.
;;;
;;; n_lines :
;;;     the number of lines
;;;
;;; hspacing :
;;;     default horizontal spacing value, or -1 for the fallback value
;;;
;;; vspacing :
;;;     default vertical spacing value, or -1 for the fallback value
;;;
;;; error :
;;;     return location for a GError
;;;
;;; first_view :
;;;     the name of a view in the VFL description, followed by the 
;;;     GtkConstraintTarget to which it maps
;;;
;;; ... :
;;;     a NULL-terminated list of view names and GtkConstraintTargets
;;;
;;; Returns :
;;;     the list of GtkConstraints that were added to the layout.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_constraint_layout_add_constraints_from_descriptionv ()
;;;
;;; GList *
;;; gtk_constraint_layout_add_constraints_from_descriptionv
;;;                                (GtkConstraintLayout *layout,
;;;                                 const char * const lines[],
;;;                                 gsize n_lines,
;;;                                 int hspacing,
;;;                                 int vspacing,
;;;                                 GHashTable *views,
;;;                                 GError **error);
;;;
;;; Creates a list of constraints from a formal description using a compact 
;;; description syntax called VFL, or "Visual Format Language".
;;;
;;; The Visual Format Language is based on Apple's AutoLayout VFL.
;;;
;;; The views dictionary is used to match GtkConstraintTargets to the symbolic 
;;; view name inside the VFL.
;;;
;;; The VFL grammar is:
;;;
;;; **Note**: The VFL grammar used by GTK is slightly different than the one 
;;; defined by Apple, as it can use symbolic values for the constraint's 
;;; strength instead of numeric values; additionally, GTK allows adding simple 
;;; arithmetic operations inside predicates.
;;;
;;; Examples of VFL descriptions are:
;;;
;;; <object class="GtkConstraintLayout">
;;;   <constraints>
;;;     <constraint target="button" target-attribute="start"
;;;                 relation="eq"
;;;                 source="super" source-attribute="start"
;;;                 constant="12"
;;;                 strength="required" />
;;;     <constraint target="button" target-attribute="width"
;;;                 relation="ge"
;;;                 constant="250"
;;;                 strength="strong" />
;;;   </constraints>
;;; </object>
;;; [rename-to gtk_constraint_layout_add_constraints_from_description]
;;;
;;; layout :
;;;     a GtkConstraintLayout
;;;
;;; lines :
;;;     an array of Visual Format Language lines defining a set of constraints.
;;;
;;; n_lines :
;;;     the number of lines
;;;
;;; hspacing :
;;;     default horizontal spacing value, or -1 for the fallback value
;;;
;;; vspacing :
;;;     default vertical spacing value, or -1 for the fallback value
;;;
;;; views :
;;;     a dictionary of [ name, target ] pairs; the name keys map to the view 
;;;     names in the VFL lines, while the target values map to children of the 
;;;     widget using a GtkConstraintLayout, or guides.
;;;
;;; error :
;;;     return location for a GError
;;;
;;; Returns :
;;;     the list of GtkConstraints that were added to the layout.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_constraint_layout_observe_constraints ()
;;;
;;; GListModel *
;;; gtk_constraint_layout_observe_constraints
;;;                                (GtkConstraintLayout *layout);
;;;
;;; Returns a GListModel to track the constraints that are part of layout .
;;;
;;; Calling this function will enable extra internal bookkeeping to track 
;;; constraints and emit signals on the returned listmodel. It may slow down 
;;; operations a lot.
;;;
;;; Applications should try hard to avoid calling this function because of the 
;;; slowdowns.
;;;
;;; layout :
;;;     a GtkConstraintLayout
;;;
;;; Returns :
;;;     a GListModel tracking layout 's constraints.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_constraint_layout_observe_guides ()
;;;
;;; GListModel *
;;; gtk_constraint_layout_observe_guides (GtkConstraintLayout *layout);
;;;
;;; Returns a GListModel to track the guides that are part of layout .
;;;
;;; Calling this function will enable extra internal bookkeeping to track guides 
;;; and emit signals on the returned listmodel. It may slow down operations a 
;;; lot.
;;;
;;; Applications should try hard to avoid calling this function because of the 
;;; slowdowns.
;;;
;;; layout :
;;;     a GtkConstraintLayout
;;;
;;; Returns :
;;;     a GListModel tracking layout 's guides.
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.constraint-layout.lisp ---------------------------------

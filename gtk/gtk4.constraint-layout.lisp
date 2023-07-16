;;; ----------------------------------------------------------------------------
;;; gtk4.constraint-layout.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.10 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 - 2023 Dieter Kaiser
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
;;; GtkConstraintLayout
;;;
;;;     A layout manager using constraints
;;;
;;; Types and Values
;;;
;;;     GtkConstraintLayout
;;;     GtkConstraintLayoutChild
;;;
;;;     GtkConstraintVflParserError              -> gtk4.enumerations.lisp
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
;;; GtkConstraintLayoutChild
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkConstraintLayoutChild" constraint-layout-child
  (:superclass layout-child
   :export t
   :interfaces ()
   :type-initializer "gtk_constraint_layout_child_get_type")
  nil)

#+liber-documentation
(setf (documentation 'constraint-layout-child 'type)
 "@version{#2023-4-20}
  @begin{short}
    The @sym{gtk:constraint-layout-child} subclass for children in a
    @class{gtk:constraint-layout} object.
  @end{short}
  @see-class{gtk:constraint-layout}")

;;; ----------------------------------------------------------------------------
;;; GtkConstraintLayout
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkConstraintLayout" constraint-layout
  (:superclass layout-manager
   :export t
   :interfaces ()
   :type-initializer "gtk_constraint_layout_get_type")
  nil)

#+liber-documentation
(setf (documentation 'constraint-layout 'type)
 "@version{#2023-4-20}
  @begin{short}
    The @sym{gtk:constraint-layout} object is a layout manager that uses
    relations between widget attributes, expressed via @class{gtk:constraint}
    instances, to measure and allocate widgets.
  @end{short}
  @begin[How do constraints work]{dictionary}
    Constraints are objects defining the relationship between attributes of a
    widget. You can read the description of the @class{gtk:constraint} class to
    have a more in depth definition.

    By taking multiple constraints and applying them to the children of a
    widget using the @sym{gtk:constraint-layout} object, it is possible to
    describe complex layout policies. Each constraint applied to a child or to
    the parent widgets contributes to the full description of the layout, in
    terms of parameters for resolving the value of each attribute.

    It is important to note that a layout is defined by the totality of
    constraints. Removing a child, or a constraint, from an existing layout
    without changing the remaining constraints may result in an unstable or
    unsolvable layout.

    Constraints have an implicit \"reading order\". You should start describing
    each edge of each child, as well as their relationship with the parent
    container, from the top left (or top right, in RTL languages), horizontally
    first, and then vertically.

    A constraint-based layout with too few constraints can become \"unstable\",
    that is: have more than one solution. The behavior of an unstable layout is
    undefined.

    A constraint-based layout with conflicting constraints may be unsolvable,
    and lead to an unstable layout. You can use the
    @slot[gtk:constraint]{strength} property to \"nudge\" the layout towards
    a solution.
  @end{dictionary}
  @begin[GtkConstraintLayout as GtkBuildable]{dictionary}
    The @sym{gtk:constraint-layout} class implements the @class{gtk:buildable}
    interface and has a custom @code{constraints} element which allows
    describing constraints in a @class{gtk:builder} object.

    An example of a UI definition fragment specifying a constraint:
    @begin{pre}
<object class=\"GtkConstraintLayout\">
  <constraints>
    <constraint target=\"button\" target-attribute=\"start\"
                relation=\"eq\"
                source=\"super\" source-attribute=\"start\"
                constant=\"12\"
                strength=\"required\" />
    <constraint target=\"button\" target-attribute=\"width\"
                relation=\"ge\"
                constant=\"250\"
                strength=\"strong\" />
  </constraints>
</object>
    @end{pre}
    The definition above will add two constraints to the
    @sym{gtk:constraint-layout} object:
    @begin{itemize}
      @item{a required constraint between the leading edge of \"button\" and
        the leading edge of the widget using the constraint layout, plus 12
        pixels,}
      @item{a strong, constant constraint making the width of \"button\"
        greater than, or equal to 250 pixels.}
    @end{itemize}
    The \"target\" and \"target-attribute\" attributes are required.

    The \"source\" and \"source-attribute\" attributes of the \"constraint\"
    element are optional. If they are not specified, the constraint is assumed
    to be a constant.

    The \"relation\" attribute is optional. If not specified, the constraint is
    assumed to be an equality.

    The \"strength\" attribute is optional. If not specified, the constraint is
    assumed to be required.

    The \"source\" and \"target\" attributes can be set to \"super\" to
    indicate that the constraint target is the widget using the
    @sym{gtk:constraint-layout} object.

    There can be \"constant\" and \"multiplier\" attributes.

    Additionally, the \"constraints\" element can also contain a description of
    the @class{gtk:constraint-guides} object used by the layout:
    @begin{pre}
<constraints>
  <guide min-width=\"100\" max-width=\"500\" name=\"hspace\"/>
  <guide min-height=\"64\" nat-height=\"128\" name=\"vspace\" strength=\"strong\"/>
</constraints>
    @end{pre}
    The \"guide\" element has the following optional attributes:
    @begin{itemize}
      @item{\"min-width\", \"nat-width\", and \"max-width\", describe the
        minimum, natural, and maximum width of the guide, respectively}
      @item{\"min-height\", \"nat-height\", and \"max-height\", describe the
        minimum, natural, and maximum height of the guide, respectively}
      @item{\"strength\" describes the strength of the constraint on the
        natural size of the guide, if not specified, the constraint is assumed
        to have a medium strength}
      @item{\"name\" describes a name for the guide, useful when debugging}
    @end{itemize}
  @end{dictionary}
  @begin[Using the Visual Format Language]{dictionary}
    Complex constraints can be described using a compact syntax called VFL, or
    Visual Format Language.

    The Visual Format Language describes all the constraints on a row or column,
    typically starting from the leading edge towards the trailing one. Each
    element of the layout is composed by \"views\", which identify a
    @class{gtk:constraint-target} object.

    For instance:
    @begin{pre}
[button]-[textField]
    @end{pre}
    Describes a constraint that binds the trailing edge of \"button\" to the
    leading edge of \"textField\", leaving a default space between the two.

    Using VFL is also possible to specify predicates that describe constraints
    on attributes like width and height:
    @begin{pre}
// Width must be greater than, or equal to 50
[button(>=50)]

// Width of button1 must be equal to width of button2
[button1(==button2)]
    @end{pre}
    The default orientation for a VFL description is horizontal, unless
    otherwise specified:
    @begin{pre}
// horizontal orientation, default attribute: width
H:[button(>=150)]

// vertical orientation, default attribute: height
V:[button1(==button2)]
    @end{pre}
    It is also possible to specify multiple predicates, as well as their
    strength:
    @begin{pre}
// minimum width of button must be 150
// natural width of button can be 250
[button(>=150@@required, ==250@@medium)]
    @end{pre}
    Finally, it is also possible to use simple arithmetic operators:
    @begin{pre}
// width of button1 must be equal to width of button2
// divided by 2 plus 12
[button1(button2 / 2 + 12)]
    @end{pre}
  @end{dictionary}
  @see-constructor{gtk:constraint-layout-new}
  @see-class{gtk:constraint-layout-child}")

;;; ----------------------------------------------------------------------------
;;; gtk_constraint_layout_new ()
;;; ----------------------------------------------------------------------------

(defun constraint-layout-new ()
 #+liber-documentation
 "@version{#2023-4-20}
  @return{The newly created @class{gtk:constraint-layout} object.}
  @short{Creates a new @class{gtk:constraint-layout} layout manager.}
  @see-class{gtk:constraint-layout}"
  (make-instance 'constraint-layout))

(export 'constraint-layout-new)

;;; ----------------------------------------------------------------------------
;;; gtk_constraint_layout_add_constraint ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_constraint_layout_add_constraint"
               constraint-layout-add-constraint) :void
 #+liber-documentation
 "@version{#2023-4-20}
  @argument[layout]{a @class{gtk:constraint-layout} object}
  @argument[constraint]{a @class{gtk:constraint} object}
  @begin{short}
    Adds a @class{gtk:constraint} object to the layout manager.
  @end{short}
  The @slot[gtk:constraint]{source} and @slot[gtk:constraint]{target} properties
  of @arg{constraint} can be:
  @begin{itemize}
    @item{set to @code{nil} to indicate that the constraint refers to the
      widget using layout}
    @item{set to the @class{gtk:widget} widget using @arg{layout}}
    @item{set to a child of the @class{gtk:widget} widget using @arg{layout}}
    @item{set to a @class{gtk:constraint-guide} object that is part of
      @arg{layout}}
  @end{itemize}
  The layout acquires the ownership of constraint after calling this function.
  @see-class{gtk:constraint-layout}
  @see-class{gtk:constraint}
  @see-class{gtk:constraint-guide}"
  (layout (g:object constraint-layout))
  (constraint (g:object constraint)))

(export 'constraint-layout-add-constraint)

;;; ----------------------------------------------------------------------------
;;; gtk_constraint_layout_remove_constraint ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_constraint_layout_remove_constraint"
               constraint-layout-remove-constraint) :void
 #+liber-documentation
 "@version{#2023-4-21}
  @argument[layout]{a @class{gtk:constraint-layout} object}
  @argument[constraint]{a @class{gtk:constraint} object}
  @begin{short}
    Removes constraint from the layout manager, so that it no longer influences
    the layout.
  @end{short}
  @see-class{gtk:constraint-layout}"
  (layout (g:object constraint-layout))
  (constraint (g:object constraint)))

(export 'constraint-layout-remove-constraint)

;;; ----------------------------------------------------------------------------
;;; gtk_constraint_layout_remove_all_constraints ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_constraint_layout_remove_all_constraints"
               constraint-layout-remove-all-constraints) :void
 #+liber-documentation
 "@version{#2023-4-21}
  @argument[layout]{a @class{gtk:constraint-layout} object}
  @begin{short}
    Removes all constraints from the layout manager.
  @end{short}
  @see-class{gtk:constraint-layout}"
  (layout (g:object constraint-layout)))

(export 'constraint-layout-remove-all-constraints)

;;; ----------------------------------------------------------------------------
;;; gtk_constraint_layout_add_guide ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_constraint_layout_add_guide" constraint-layout-add-guide)
    :void
 #+liber-documentation
 "@version{#2023-4-21}
  @argument[layout]{a @class{gtk:constraint-layout} object}
  @argument[guide]{a @class{gtk:constraint-guide} object}
  @begin{short}
    Adds a guide to @arg{layout}.
  @end{short}
  A guide can be used as the source or target of constraints, like a widget,
  but it is not visible.

  The layout acquires the ownership of guide after calling this function.
  @see-class{gtk:constraint-layout}"
  (layout (g:object constraint-layout))
  (guide (g:object constraint-guide)))

(export 'constraint-layout-add-guide)

;;; ----------------------------------------------------------------------------
;;; gtk_constraint_layout_remove_guide ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_constraint_layout_remove_guide"
               constraint-layout-remove-guide) :void
 #+liber-documentation
 "@version{#2023-4-21}
  @argument[layout]{a @class{gtk:constraint-layout} object}
  @argument[guide]{a @class{gtk:constraint-guide} object}
  @begin{short}
    Removes @arg{guide} from the layout manager, so that it no longer
    influences the layout.
  @end{short}
  @see-class{gtk:constraint-layout}"
  (layout (g:object constraint-layout))
  (guide (g:object constraint-guide)))

(export 'constraint-layout-remove-guide)

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

;;; --- End of file gtk4.constraint-layout.lisp --------------------------------

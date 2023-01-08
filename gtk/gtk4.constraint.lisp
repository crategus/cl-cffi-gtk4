;;; ----------------------------------------------------------------------------
;;; gtk.constraint.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.6 and modified to document the Lisp binding to the GTK library.
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
;;; GtkConstraint
;;;
;;;     The description of a constraint
;;;
;;; Types and Values
;;;
;;;     GtkConstraint
;;;     GtkConstraintTarget
;;;     GtkConstraintAttribute                             gtk.enumerations
;;;     GtkConstraintRelation                              gtk.enumerations
;;;     GtkConstraintStrength                              gtk.enumerations
;;;
;;; Accessors
;;;
;;;     gtk_constraint_get_target
;;;     gtk_constraint_get_target_attribute
;;;     gtk_constraint_get_relation
;;;     gtk_constraint_get_source
;;;     gtk_constraint_get_source_attribute
;;;     gtk_constraint_get_multiplier
;;;     gtk_constraint_get_constant
;;;     gtk_constraint_get_strength
;;;
;;; Functions
;;;
;;;     gtk_constraint_new
;;;     gtk_constraint_new_constant
;;;     gtk_constraint_is_required
;;;     gtk_constraint_is_attached
;;;     gtk_constraint_is_constant
;;;
;;; Properties
;;;
;;;     constant
;;;     multiplier
;;;     relation
;;;     source
;;;     source-attribute
;;;     strength
;;;     target
;;;     target-attribute
;;;
;;; Hierarchy
;;;
;;;    GInterface
;;;    ╰── GtkConstraintTarget
;;;
;;;    GObject
;;;    ╰── GtkConstraint
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkConstraintTarget
;;; ----------------------------------------------------------------------------

(define-g-interface "GtkConstraintTarget" constraint-target
  (:export t
   :type-initializer "gtk_constraint_target_get_type")
  nil)

#+liber-documentation
(setf (liber:alias-for-class 'constraint-target)
      "Interface"
      (documentation 'constraint-target 'type)
 "@version{#2022-1-28}
  @begin{short}
    The @class{gtk:constraint-target} interface is implemented by objects that
    can be used as source or target in @class{gtk:constraint} objects.
  @end{short}
  Besides @class{gtk:widget} objects, it is also implemented by
  the @class{gtk:constraint-guide} object.
  @see-class{gtk:constraint}
  @see-class{gtk:constraint-guide}
  @see-class{gtk:widget}")

;;; ----------------------------------------------------------------------------
;;; GtkConstraint
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkConstraint" constraint
  (:superclass g:object
   :export t
   :interfaces ()
   :type-initializer "gtk_constraint_get_type")
  ((constant
    constraint-constant
    "constant" "gdouble" t t)
   (multiplier
    constraint-multiplier
    "multiplier" "gdouble" t t)
   (relation
    constraint-relation
    "relation" "GtkConstraintRelation" t t)
   (source
    constraint-source
    "source" "GtkConstraintTarget" t t)
   (source-attribute
    constraint-source-attribute
    "source-attribute" "GtkConstraintAttribute" t t)
   (strength
    constraint-strength
    "strength" "gint" t t)
   (target
    constraint-target
    "target" "GtkConstraintTarget" t t)
   (target-attribute
    constraint-target-attribute
    "target-attribute" "GtkConstraintAttribute" t t)))

#+liber-documentation
(setf (documentation 'constraint 'type)
 "@version{#2022-1-28}
  @begin{short}
    The @sym{gtk:constraint} class describes a constraint between an attribute
    on a widget and another attribute on another widget, expressed as a linear
    equation.
  @end{short}

  The typical equation for a constraint is:
  @begin{pre}
target.target_attr = source.source_attr × multiplier + constant
  @end{pre}
  Each @sym{gtk:constraint} object is part of a system that will be solved by
  a @class{gtk:constraint-layout} object in order to allocate and position each
  child widget.

  The source and target widgets, as well as their attributes, of a
  @sym{gtk:constraint} object are immutable after creation.
  @see-slot{gtk:constraint-constant}
  @see-slot{gtk:constraint-multiplier}
  @see-slot{gtk:constraint-relation}
  @see-slot{gtk:constraint-source}
  @see-slot{gtk:constraint-source-attribute}
  @see-slot{gtk:constraint-strength}
  @see-slot{gtk:constraint-target}
  @see-slot{gtk:constraint-target-attribute}
  @see-class{gtk:constraint-target}
  @see-class{gtk:constraint-layout}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- constraint-constant ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "constant" 'constraint) t)
 "The @code{constant} property of type @code{:double}
  (Read / Write / Construct only) @br{}
  The constant value to be added to the @code{source-attribute} property. @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'constraint-constant)
      "Accessor"
      (documentation 'constraint-constant 'function)
 "@version{#2022-1-28}
  @syntax[]{(gtk:constraint-constant object) => constant}
  @syntax[]{(setf (gtk:constraint-constant object) constant)}
  @argument[object]{a @class{gtk:constraint} object}
  @argument[constant]{a double float with the constant value to be added to the
    @slot[gtk:constraint]{source-attribute} property}
  @begin{short}
    Accessor of the @slot[gtk:constraint]{constant} slot of the
    @class{gtk:constraint} class.
  @end{short}
  @see-class{gtk:constraint}
  @see-function{gtk:constraint-source-attribute}")

;;; --- constraint-multiplier ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "multiplier" 'constraint) t)
 "The @code{multiplier} property of type @code{:double}
  (Read / Write / Construct only) @br{}
  The multiplication factor to be applied to the @code{source-attribute}
  property. @br{}
  Default value: 1")

#+liber-documentation
(setf (liber:alias-for-function 'constraint-multiplier)
      "Accessor"
      (documentation 'constraint-multiplier 'function)
 "@version{#2022-1-28}
  @syntax[]{(gtk:constraint-multiplier object) => multiplier}
  @syntax[]{(setf (gtk:constraint-multiplier object) multiplier)}
  @argument[object]{a @class{gtk:constraint} object}
  @argument[multiplier]{a double float with the multiplication factor to be
    applied to the @slot[gtk:constraint]{source-attribute} property}
  @begin{short}
    Accessor of the @slot[gtk:constraint]{multiplier} slot of the
    @class{gtk:constraint} class.
  @end{short}
  @see-class{gtk:constraint}
  @see-function{gtk:constraint-source-attribute}")

;;; --- constraint-relation ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "relation" 'constraint) t)
 "The @code{relation} property of type @symbol{gtk:constraint-relation}
  (Read / Write / Construct only) @br{}
  The order relation between the terms of the constraint. @br{}
  Default value: @code{:eq}")

#+liber-documentation
(setf (liber:alias-for-function 'constraint-relation)
      "Accessor"
      (documentation 'constraint-relation 'function)
 "@version{#2022-1-28}
  @syntax[]{(gtk:constraint-relation object) => relation}
  @syntax[]{(setf (gtk:constraint-relation object) relation)}
  @argument[object]{a @class{gtk:constraint} object}
  @argument[relation]{a @symbol{gtk:constraint-relation} value with the order
    relation between the terms of the constraint}
  @begin{short}
    Accessor of the @slot[gtk:constraint]{relation} slot of the
    @class{gtk:constraint} class.
  @end{short}
  @see-class{gtk:constraint}
  @see-symbol{gtk:constraint-relation}")

;;; --- constraint-source --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "source" 'constraint) t)
 "The @code{source} property of type @class{gtk:constraint-target}
  (Read / Write / Construct only) @br{}
  The source of the constraint. The constraint will set the
  @code{target-attribute} property of the target using the
  @code{source-attribute} property of the source widget.")

#+liber-documentation
(setf (liber:alias-for-function 'constraint-source)
      "Accessor"
      (documentation 'constraint-source 'function)
 "@version{#2022-1-28}
  @syntax[]{(gtk:constraint-source object) => source}
  @syntax[]{(setf (gtk:constraint-source object) source)}
  @argument[object]{a @class{gtk:constraint} object}
  @argument[source]{a @class{gtk:constraint-target} object}
  @begin{short}
    Accessor of the @slot[gtk:constraint]{source} slot of the
    @class{gtk:constraint} class.
  @end{short}

  The source of the constraint. The constraint will set the
  @slot[gtk:constraint]{target-attribute} property of the target using the
  @slot[gtk:constraint]{source-attribute} property of the source.
  @see-class{gtk:constraint}
  @see-class{gtk:constraint-target}
  @see-function{gtk:constraint-target-attribute}
  @see-function{gtk:constraint-source-attribute}")

;;; --- constraint-source-attribute ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "source-attribute"
                                               'constraint) t)
 "The @code{source-attribute} property of type @symbol{gtk:constraint-attribute}
  (Read / Write / Construct only) @br{}
  The attribute of the @code{source} property read by the constraint. @br{}
  Default value: @code{:none}")

#+liber-documentation
(setf (liber:alias-for-function 'constraint-source-attribute)
      "Accessor"
      (documentation 'constraint-source-attribute 'function)
 "@version{#2022-1-28}
  @syntax[]{(gtk:constraint-source-attribute object) => attribute}
  @syntax[]{(setf (gtk:constraint-source-attribute object) attribute)}
  @argument[object]{a @class{gtk:constraint} object}
  @argument[attribute]{a @symbol{gtk:constraint-attribute} value with the
    attribute of the @slot[gtk:constraint]{source} property read by the
    constraint}
  @begin{short}
    Accessor of the @slot[gtk:constraint]{source-attribute} slot of the
    @class{gtk:constraint} class.
  @end{short}
  @see-class{gtk:constraint}
  @see-symbol{gtk:constraint-attribute}
  @see-function{gtk:constraint-source}")

;;; --- constraint-strength ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "strength" 'constraint) t)
 "The @code{strength} property of type @code{:int}
  (Read / Write / Construct only) @br{}
  The strength of the constraint. The strength can be expressed either using one
  of the symbolic values of the @symbol{gtk:constraint-strength} enumeration, or
  any positive integer value. @br{}
  Allowed values: [0, 1001001000] @br{}
  Default value: 1001001000")

#+liber-documentation
(setf (liber:alias-for-function 'constraint-strength)
      "Accessor"
      (documentation 'constraint-strength 'function)
 "@version{#2022-1-28}
  @syntax[]{(gtk:constraint-strength object) => strength}
  @syntax[]{(setf (gtk:constraint-strength object) strength)}
  @argument[object]{a @class{gtk:constraint} object}
  @argument[strength]{an integer with the strength of the constraint}
  @begin{short}
    Accessor of the @slot[gtk:constraint]{source-attribute} slot of the
    @class{gtk:constraint} class.
  @end{short}

  The strength of the constraint. The strength can be expressed either using one
  of the symbolic values of the @symbol{gtk:constraint-strength} enumeration, or
  any positive integer value.
  @see-class{gtk:constraint}
  @see-symbol{gtk:constraint-strength}")

;;; --- constraint-target --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "target" 'constraint) t)
 "The @code{target} property of type @class{gtk:constraint-target}
  (Read / Write / Construct only) @br{}
  The target of the constraint. The constraint will set the
  @code{target-attribute} property of the target using the
  @code{source-attribute} property of the source widget.")

#+liber-documentation
(setf (liber:alias-for-function 'constraint-target)
      "Accessor"
      (documentation 'constraint-target 'function)
 "@version{#2022-1-28}
  @syntax[]{(gtk:constraint-target object) => target}
  @syntax[]{(setf (gtk:constraint-target object) target)}
  @argument[object]{a @class{gtk:constraint} object}
  @argument[source]{a @class{gtk:constraint-target} object}
  @begin{short}
    Accessor of the @slot[gtk:constraint]{target} slot of the
    @class{gtk:constraint} class.
  @end{short}

  The target of the constraint. The constraint will set the
  @slot[gtk:constraint]{target-attribute} property of the target using the
  @slot[gtk:constraint]{source-attribute} property of the source widget.
  @see-class{gtk:constraint}
  @see-class{gtk:constraint-target}
  @see-function{gtk:constraint-target-attribute}
  @see-function{gtk:constraint-source-attribute}")

;;; --- constraint-target-attribute ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "target-attribute"
                                               'constraint) t)
 "The @code{target-attribute} property of type @symbol{gtk:constraint-attribute}
  (Read / Write / Construct only) @br{}
  The attribute of the @code{target} property set by the constraint. @br{}
  Default value: @code{:none}")

#+liber-documentation
(setf (liber:alias-for-function 'constraint-target-attribute)
      "Accessor"
      (documentation 'constraint-target-attribute 'function)
 "@version{#2022-1-28}
  @syntax[]{(gtk:constraint-target-attribute object) => attribute}
  @syntax[]{(setf (gtk:constraint-target-attribute object) attribute)}
  @argument[object]{a @class{gtk:constraint} object}
  @argument[attribute]{a @symbol{gtk:constraint-attribute} value with the
    attribute of the @slot[gtk:constraint]{target} property set by the
    constraint}
  @begin{short}
    Accessor of the @slot[gtk:constraint]{target-attribute} slot of the
    @class{gtk:constraint} class.
  @end{short}
  @see-class{gtk:constraint}
  @see-symbol{gtk:constraint-attribute}
  @see-function{gtk:constraint-target}")

;;; ----------------------------------------------------------------------------
;;; gtk_constraint_new ()
;;;
;;; GtkConstraint *
;;; gtk_constraint_new (gpointer target,
;;;                     GtkConstraintAttribute target_attribute,
;;;                     GtkConstraintRelation relation,
;;;                     gpointer source,
;;;                     GtkConstraintAttribute source_attribute,
;;;                     double multiplier,
;;;                     double constant,
;;;                     int strength);
;;;
;;; Creates a new GtkConstraint representing a relation between a layout
;;; attribute on a source and a layout attribute on a target.
;;;
;;; target :
;;;     a GtkConstraintTarget.
;;;
;;; target_attribute :
;;;     the attribute of target to be set
;;;
;;; relation :
;;;     the relation equivalence between target_attribute and source_attribute
;;;
;;; source :
;;;     a GtkConstraintTarget.
;;;
;;; source_attribute :
;;;     the attribute of source to be read
;;;
;;; multiplier :
;;;     a multiplication factor to be applied to source_attribute
;;;
;;; constant :
;;;     a constant factor to be added to source_attribute
;;;
;;; strength :
;;;     the strength of the constraint
;;;
;;; Returns :
;;;     the newly created GtkConstraint
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_constraint_new_constant ()
;;;
;;; GtkConstraint *
;;; gtk_constraint_new_constant (gpointer target,
;;;                              GtkConstraintAttribute target_attribute,
;;;                              GtkConstraintRelation relation,
;;;                              double constant,
;;;                              int strength);
;;;
;;; Creates a new GtkConstraint representing a relation between a layout
;;; attribute on a target and a constant value.
;;
;;; target :
;;;     a GtkConstraintTarget.
;;;
;;; target_attribute :
;;;     the attribute of target to be set
;;;
;;; relation :
;;;     the relation equivalence between target_attribute and constant
;;;
;;; constant :
;;;     a constant factor to be set on target_attribute
;;;
;;; strength :
;;;     the strength of the constraint
;;;
;;; Returns :
;;;     the newly created GtkConstraint
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_constraint_get_target ()
;;;
;;; GtkConstraintTarget *
;;; gtk_constraint_get_target (GtkConstraint *constraint);
;;;
;;; Retrieves the GtkConstraintTarget used as the target for constraint .
;;;
;;; If the “target” property is set to NULL, the constraint will use the
;;; GtkConstraintLayout's widget.
;;;
;;; constraint :
;;;     a GtkConstraint
;;;
;;; Returns :
;;;     a GtkConstraintTarget
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_constraint_get_target_attribute ()
;;;
;;; GtkConstraintAttribute
;;; gtk_constraint_get_target_attribute (GtkConstraint *constraint);
;;;
;;; Retrieves the attribute of the target to be set by the constraint .
;;;
;;; constraint :
;;;     a GtkConstraint
;;;
;;; Returns :
;;;     the target's attribute
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_constraint_get_relation ()
;;;
;;; GtkConstraintRelation
;;; gtk_constraint_get_relation (GtkConstraint *constraint);
;;;
;;; The order relation between the terms of the constraint .
;;;
;;; constraint :
;;;     a GtkConstraint
;;;
;;; Returns :
;;;     a GtkConstraintRelation value
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_constraint_get_source ()
;;;
;;; GtkConstraintTarget *
;;; gtk_constraint_get_source (GtkConstraint *constraint);
;;;
;;; Retrieves the GtkConstraintTarget used as the source for constraint .
;;;
;;; If the “source” property is set to NULL, the constraint will use the
;;; GtkConstraintLayout's widget.
;;;
;;; constraint :
;;;     a GtkConstraint
;;;
;;; Returns :
;;;     a GtkConstraintTarget.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_constraint_get_source_attribute ()
;;;
;;; GtkConstraintAttribute
;;; gtk_constraint_get_source_attribute (GtkConstraint *constraint);
;;;
;;; Retrieves the attribute of the source to be read by the constraint .
;;;
;;; constraint :
;;;     a GtkConstraint
;;;
;;; Returns :
;;;     the target's attribute
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_constraint_get_multiplier ()
;;;
;;; double
;;; gtk_constraint_get_multiplier (GtkConstraint *constraint);
;;;
;;; Retrieves the multiplication factor applied to the source attribute's value.
;;;
;;; constraint :
;;;     a GtkConstraint
;;;
;;; Returns :
;;;     a multiplication factor
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_constraint_get_constant ()
;;;
;;; double
;;; gtk_constraint_get_constant (GtkConstraint *constraint);
;;;
;;; Retrieves the constant factor added to the source attributes' value.
;;;
;;; constraint :
;;;     a GtkConstraint
;;;
;;; Returns :
;;;     a constant factor
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_constraint_get_strength ()
;;;
;;; int
;;; gtk_constraint_get_strength (GtkConstraint *constraint);
;;;
;;; Retrieves the strength of the constraint.
;;;
;;; constraint :
;;;     a GtkConstraint
;;;
;;; Returns :
;;;     the strength of the constraint
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_constraint_is_required ()
;;;
;;; gboolean
;;; gtk_constraint_is_required (GtkConstraint *constraint);
;;;
;;; Checks whether the constraint is a required relation for solving the
;;; constraint layout.
;;;
;;; constraint :
;;;     a GtkConstraint
;;;
;;; Returns :
;;;     TRUE if the constraint is required
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_constraint_is_attached ()
;;;
;;; gboolean
;;; gtk_constraint_is_attached (GtkConstraint *constraint);
;;;
;;; Checks whether the constraint is attached to a GtkConstraintLayout, and it
;;; is contributing to the layout.
;;;
;;; constraint :
;;;     a GtkConstraint
;;;
;;; Returns :
;;;     TRUE if the constraint is attached
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_constraint_is_constant ()
;;;
;;; gboolean
;;; gtk_constraint_is_constant (GtkConstraint *constraint);
;;;
;;; Checks whether the constraint describes a relation between an attribute on
;;; the “target” and a constant value.
;;;
;;; constraint :
;;;     a GtkConstraint
;;;
;;; Returns :
;;;     TRUE if the constraint is a constant relation
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.constraint.lisp ----------------------------------------

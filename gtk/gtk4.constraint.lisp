;;; ----------------------------------------------------------------------------
;;; gtk4.constraint.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.12 and modified to document the Lisp binding to the GTK library.
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

(gobject:define-g-interface "GtkConstraintTarget" constraint-target
  (:export t
   :type-initializer "gtk_constraint_target_get_type")
  nil)

#+liber-documentation
(setf (liber:alias-for-class 'constraint-target)
      "Interface"
      (documentation 'constraint-target 'type)
 "@version{2023-9-2}
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

(gobject:define-g-object-class "GtkConstraint" constraint
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
 "@version{2023-9-2}
  @begin{short}
    The @class{gtk:constraint} class describes a constraint between an attribute
    on a widget and another attribute on another widget, expressed as a linear
    equation.
  @end{short}

  The typical equation for a constraint is:
  @begin{pre}
target.target_attr = source.source_attr × multiplier + constant
  @end{pre}
  Each @class{gtk:constraint} object is part of a system that will be solved by
  a @class{gtk:constraint-layout} object in order to allocate and position each
  child widget.

  The source and target widgets, as well as their attributes, of a
  @class{gtk:constraint} object are immutable after creation.
  @see-constructor{gtk:constraint-new}
  @see-constructor{gtk:constraint-new-constant}
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

;;; --- gtk:constraint-constant ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "constant" 'constraint) t)
 "The @code{constant} property of type @code{:double}
  (Read / Write / Construct only) @br{}
  The constant value to be added to the @slot[gtk:constraint]{source-attribute}
  property. @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'constraint-constant)
      "Accessor"
      (documentation 'constraint-constant 'function)
 "@version{2024-4-23}
  @syntax{(gtk:constraint-constant object) => constant}
  @syntax{(setf (gtk:constraint-constant object) constant)}
  @argument[object]{a @class{gtk:constraint} object}
  @argument[constant]{a double float with the constant value to be added to the
    @slot[gtk:constraint]{source-attribute} property}
  @begin{short}
    Accessor of the @slot[gtk:constraint]{constant} slot of the
    @class{gtk:constraint} class.
  @end{short}
  The @fun{gtk:constraint-constant} function retrieves the constant factor
  added to the source attributes' value.
  @see-class{gtk:constraint}
  @see-function{gtk:constraint-source-attribute}")

;;; --- gtk:constraint-multiplier ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "multiplier" 'constraint) t)
 "The @code{multiplier} property of type @code{:double}
  (Read / Write / Construct only) @br{}
  The multiplication factor to be applied to the
  @slot[gtk:constraint]{source-attribute} property. @br{}
  Default value: 1")

#+liber-documentation
(setf (liber:alias-for-function 'constraint-multiplier)
      "Accessor"
      (documentation 'constraint-multiplier 'function)
 "@version{2024-4-23}
  @syntax{(gtk:constraint-multiplier object) => multiplier}
  @syntax{(setf (gtk:constraint-multiplier object) multiplier)}
  @argument[object]{a @class{gtk:constraint} object}
  @argument[multiplier]{a double float with the multiplication factor to be
    applied to the @slot[gtk:constraint]{source-attribute} property}
  @begin{short}
    Accessor of the @slot[gtk:constraint]{multiplier} slot of the
    @class{gtk:constraint} class.
  @end{short}
  The @fun{gtk:constraint-multiplier} function retrieves the multiplication
  factor applied to the source attribute's value.
  @see-class{gtk:constraint}
  @see-function{gtk:constraint-source-attribute}")

;;; --- gtk:constraint-relation ------------------------------------------------

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
 "@version{2024-4-23}
  @syntax{(gtk:constraint-relation object) => relation}
  @syntax{(setf (gtk:constraint-relation object) relation)}
  @argument[object]{a @class{gtk:constraint} object}
  @argument[relation]{a @symbol{gtk:constraint-relation} value with the order
    relation between the terms of the constraint}
  @begin{short}
    Accessor of the @slot[gtk:constraint]{relation} slot of the
    @class{gtk:constraint} class.
  @end{short}
  The @fun{gtk:constraint-relation} function the order relation between the
  terms of the constraint.
  @see-class{gtk:constraint}
  @see-symbol{gtk:constraint-relation}")

;;; --- gtk:constraint-source --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "source" 'constraint) t)
 "The @code{source} property of type @class{gtk:constraint-target}
  (Read / Write / Construct only) @br{}
  The source of the constraint. The constraint will set the
  @slot[gtk:constraint]{target-attribute} property of the target using the
  @slot[gtk:constraint]{source-attribute} property of the source widget.")

#+liber-documentation
(setf (liber:alias-for-function 'constraint-source)
      "Accessor"
      (documentation 'constraint-source 'function)
 "@version{2024-4-23}
  @syntax{(gtk:constraint-source object) => source}
  @syntax{(setf (gtk:constraint-source object) source)}
  @argument[object]{a @class{gtk:constraint} object}
  @argument[source]{a @class{gtk:constraint-target} object}
  @begin{short}
    Accessor of the @slot[gtk:constraint]{source} slot of the
    @class{gtk:constraint} class.
  @end{short}
  The @fun{gtk:constraint-source} function retrieves the
  @symbol{gtk:constraint-target} value used as the source for the constraint.

  If the @slot[gtk:constraint]{source} property is set to @code{nil}, the
  constraint will use the @class{gtk:constraint-layout} object of the widget.
  @see-class{gtk:constraint}
  @see-class{gtk:constraint-target}
  @see-function{gtk:constraint-layout}")

;;; --- gtk:constraint-source-attribute ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "source-attribute"
                                               'constraint) t)
 "The @code{source-attribute} property of type @symbol{gtk:constraint-attribute}
  (Read / Write / Construct only) @br{}
  The attribute of the @slot[gtk:constraint]{source} property read by the
  constraint. @br{}
  Default value: @code{:none}")

#+liber-documentation
(setf (liber:alias-for-function 'constraint-source-attribute)
      "Accessor"
      (documentation 'constraint-source-attribute 'function)
 "@version{2024-4-23}
  @syntax{(gtk:constraint-source-attribute object) => attribute}
  @syntax{(setf (gtk:constraint-source-attribute object) attribute)}
  @argument[object]{a @class{gtk:constraint} object}
  @argument[attribute]{a @symbol{gtk:constraint-attribute} value with the
    attribute of the @slot[gtk:constraint]{source} property read by the
    constraint}
  @begin{short}
    Accessor of the @slot[gtk:constraint]{source-attribute} slot of the
    @class{gtk:constraint} class.
  @end{short}
  The @fun{gtk:constraint-source-attribute} function retrieves the attribute of
  the source to be read by the constraint.
  @see-class{gtk:constraint}
  @see-symbol{gtk:constraint-attribute}
  @see-function{gtk:constraint-source}")

;;; --- gtk:constraint-strength ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "strength" 'constraint) t)
 "The @code{strength} property of type @code{:int}
  (Read / Write / Construct only) @br{}
  The strength of the constraint. The strength can be expressed either using
  one of the symbolic values of the @symbol{gtk:constraint-strength}
  enumeration, or any positive integer value. @br{}
  Allowed values: [0, 1001001000] @br{}
  Default value: 1001001000")

#+liber-documentation
(setf (liber:alias-for-function 'constraint-strength)
      "Accessor"
      (documentation 'constraint-strength 'function)
 "@version{2024-4-23}
  @syntax{(gtk:constraint-strength object) => strength}
  @syntax{(setf (gtk:constraint-strength object) strength)}
  @argument[object]{a @class{gtk:constraint} object}
  @argument[strength]{an integer with the strength of the constraint}
  @begin{short}
    Accessor of the @slot[gtk:constraint]{source-attribute} slot of the
    @class{gtk:constraint} class.
  @end{short}
  The @fun{gtk:constraint-strength} function returns the strength of the
  constraint. The strength can be expressed either using one of the values of
  the @symbol{gtk:constraint-strength} enumeration, or any positive integer
  value.
  @see-class{gtk:constraint}
  @see-symbol{gtk:constraint-strength}")

;;; --- gtk:constraint-target --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "target" 'constraint) t)
 "The @code{target} property of type @class{gtk:constraint-target}
  (Read / Write / Construct only) @br{}
  The target of the constraint. The constraint will set the
  @slot[gtk:constraint]{target-attribute} property of the target using the
  @slot[gtk:constraint]{source-attribute} property of the source widget.")

#+liber-documentation
(setf (liber:alias-for-function 'constraint-target)
      "Accessor"
      (documentation 'constraint-target 'function)
 "@version{2024-4-23}
  @syntax{(gtk:constraint-target object) => target}
  @syntax{(setf (gtk:constraint-target object) target)}
  @argument[object]{a @class{gtk:constraint} object}
  @argument[source]{a @class{gtk:constraint-target} object}
  @begin{short}
    Accessor of the @slot[gtk:constraint]{target} slot of the
    @class{gtk:constraint} class.
  @end{short}
  The @fun{gtk:constraint-target} function retrieves the
  @symbol{gtk:constraint-target} value used as the target for the constraint.
  If the @slot[gtk:constraint]{target} property is set to @code{nil}, the
  constraint will use the @class{gtk:constraint-layout} object of the widget.
  @see-class{gtk:constraint}
  @see-class{gtk:constraint-target}
  @see-function{gtk:constraint-layout}")

;;; --- gtk:constraint-target-attribute ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "target-attribute"
                                               'constraint) t)
 "The @code{target-attribute} property of type @symbol{gtk:constraint-attribute}
  (Read / Write / Construct only) @br{}
  The attribute of the @slot[gtk:constraint]{target} property set by the
  constraint. @br{}
  Default value: @code{:none}")

#+liber-documentation
(setf (liber:alias-for-function 'constraint-target-attribute)
      "Accessor"
      (documentation 'constraint-target-attribute 'function)
 "@version{2024-4-23}
  @syntax{(gtk:constraint-target-attribute object) => attribute}
  @syntax{(setf (gtk:constraint-target-attribute object) attribute)}
  @argument[object]{a @class{gtk:constraint} object}
  @argument[attribute]{a @symbol{gtk:constraint-attribute} value with the
    attribute of the @slot[gtk:constraint]{target} property set by the
    constraint}
  @begin{short}
    Accessor of the @slot[gtk:constraint]{target-attribute} slot of the
    @class{gtk:constraint} class.
  @end{short}
  The @fun{gtk:constraint-target} function retrieves the attribute of the
  target to be set by the constraint.
  @see-class{gtk:constraint}
  @see-symbol{gtk:constraint-attribute}
  @see-function{gtk:constraint-target}")

;;; ----------------------------------------------------------------------------
;;; gtk_constraint_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_constraint_new" constraint-new) (g:object constraint)
 #+liber-documentation
 "@version{#2023-4-21}
  @argument[target]{a @class{gtk:constraint-target} object}
  @argument[target-attribute]{a @symbol{gtk:constraint-attribute} value with
    the attribute of @arg{target} to be set}
  @argument[relation]{a @symbol{gtk:constraint-relation} value the relation
    equivalence between @arg{target-attribute} and @arg{source-attribute}}
  @argument[source]{a @class{gtk:constraint-target} object}
  @argument[source-attribute]{a @symbol{gtk:constraint-attribute} value with
    the attribute of @arg{source} to be set}
  @argument[multiplier]{a double float with a multiplication factor to be
    applied to @arg{source-attribute}}
  @argument[constant]{a double float with a constant factor to be added to
    @arg{source-attribute}}
  @argument[strength]{a @symbol{gtk:constraint-strength} value with the
    strength of the constraint}
  @return{The newly created @class{gtk:constraint} object.}
  @begin{short}
    Creates a new @class{gtk:constraint} object representing a relation between
    a layout attribute on a source and a layout attribute on a target.
  @end{short}
  @see-class{gtk:constraint}
  @see-class{gtk:constraint-target}
  @see-symbol{gtk:constraint-attribute}
  @see-symbol{gtk:constraint-relation}
  @see-symbol{gtk:constraint-strength}"
  (target :pointer)
  (target-attribute constraint-attribute)
  (relation constraint-relation)
  (source :pointer)
  (source-attribute constraint-attribute)
  (multiplier :double)
  (constant :double)
  (strength constraint-strength))

(export 'constraint-new)

;;; ----------------------------------------------------------------------------
;;; gtk_constraint_new_constant
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_constraint_new_constant" constraint-new-constant)
    (g:object constraint)
 #+liber-documentation
 "@version{#2023-4-21}
  @argument[target]{a @class{gtk:constraint-target} object}
  @argument[target-attribute]{a @symbol{gtk:constraint-attribute} value with
    the attribute of @arg{target} to be set}
  @argument[relation]{a @symbol{gtk:constraint-relation} value the relation
    equivalence between @arg{target-attribute} and @arg{source-attribute}}
  @argument[constant]{a double float with a constant factor to be added to
    @arg{source-attribute}}
  @argument[strength]{a @symbol{gtk:constraint-strength} value with the
    strength of the constraint}
  @return{The newly created @class{gtk:constraint} object.}
  @begin{short}
    Creates a new @class{gtk:constraint} object representing a relation between
    a layout attribute on a target and a constant value.
  @end{short}
  @see-class{gtk:constraint}
  @see-class{gtk:constraint-target}
  @see-symbol{gtk:constraint-attribute}
  @see-symbol{gtk:constraint-relation}
  @see-symbol{gtk:constraint-strength}"
  (target :pointer)
  (target-attribute constraint-attribute)
  (relation constraint-relation)
  (constant :double)
  (strength constraint-strength))

(export 'constraint-new-constant)

;;; ----------------------------------------------------------------------------
;;; gtk_constraint_is_required
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_constraint_is_required" constraint-is-required) :boolean
 #+liber-documentation
 "@version{#2023-4-21}
  @argument[constraint]{a @class{gtk:constraint} object}
  @return{@em{True} if the constraint is required.}
  @begin{short}
    Checks whether the constraint is a required relation for solving the
    constraint layout.
  @end{short}
  @see-class{gtk:constraint}"
  (constraint (g:object constraint)))

(export 'constraint-is-required)

;;; ----------------------------------------------------------------------------
;;; gtk_constraint_is_attached
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_constraint_is_attached" constraint-is-attached) :boolean
 #+liber-documentation
 "@version{#2023-4-21}
  @argument[constraint]{a @class{gtk:constraint} object}
  @return{@em{True} if the constraint is attached.}
  @begin{short}
    Checks whether the constraint is attached to a @class{gtk:constraint-layout}
    object, and it is contributing to the layout.
  @end{short}
  @see-class{gtk:constraint}"
  (constraint (g:object constraint)))

(export 'constraint-is-attached)

;;; ----------------------------------------------------------------------------
;;; gtk_constraint_is_constant
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_constraint_is_constant" constraint-is-constant) :boolean
 #+liber-documentation
 "@version{#2023-4-21}
  @argument[constraint]{a @class{gtk:constraint} object}
  @return{@em{True} if the constraint is as constant relation.}
  @begin{short}
    Checks whether the constraint describes a relation between an attribute on
    the \"target\" and a constant value.
  @end{short}
  @see-class{gtk:constraint}"
  (constraint (g:object constraint)))

(export 'constraint-is-constant)

;;; --- End of file gtk4.constraint.lisp ---------------------------------------

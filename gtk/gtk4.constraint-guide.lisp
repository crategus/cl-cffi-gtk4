;;; ----------------------------------------------------------------------------
;;; gtk4.constraint-guide.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.12 and modified to document the Lisp binding to the GTK library.
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
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkConstraintGuide" constraint-guide
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

#+liber-documentation
(setf (documentation 'constraint-guide 'type)
 "@version{#2023-4-21}
  @begin{short}
    An object that can be added to a @class{gtk:constraint-layout} object and
    be used in constraints like a widget, without being drawn.
  @end{short}

  Guides have a minimum, maximum and natural size. Depending on the constraints
  that are applied, they can act like a guideline that widgets can be aligned
  to, or like 'flexible space'.
  @see-constructor{gtk:constraint-guide-new}
  @see-slot{gtk:constraint-guide-max-height}
  @see-slot{gtk:constraint-guide-max-width}
  @see-slot{gtk:constraint-guide-min-height}
  @see-slot{gtk:constraint-guide-min-width}
  @see-slot{gtk:constraint-guide-name}
  @see-slot{gtk:constraint-guide-nat-height}
  @see-slot{gtk:constraint-guide-nat-width}
  @see-slot{gtk:constraint-guide-strength}
  @see-class{gtk:constraint-layout}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- constraint-guide-max-height --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "max-height"
                                               'constraint-guide) t)
 "The @code{max-height} property of type @code{:int} (Read / Write) @br{}
  The maximum height of the guide. @br{}
  Allowed values: >= 0 @br{}
  Default value: 2147483647")

#+liber-documentation
(setf (liber:alias-for-function 'constraint-guide-max-height)
      "Accessor"
      (documentation 'constraint-guide-max-height 'function)
 "@version{#2023-4-21}
  @syntax[]{(gtk:constraint-guide-max-height object) => height}
  @syntax[]{(setf (gtk:constraint-guide-max-height object) height)}
  @argument[object]{a @class{gtk:constraint-guide} object}
  @argument[height]{an integer with the maximum height of the guide}
  @begin{short}
    Accessor of the @slot[gtk:constraint-guide]{max-height} slot of the
    @class{gtk:constraint-guide} class.
  @end{short}
  @see-class{gtk:constraint-guide}")

;;; --- constraint-guide-max-width ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "max-width"
                                               'constraint-guide) t)
 "The @code{max-width} property of type @code{:int} (Read / Write) @br{}
  The maximum width of the guide. @br{}
  Allowed values: >= 0 @br{}
  Default value: 2147483647")

#+liber-documentation
(setf (liber:alias-for-function 'constraint-guide-max-width)
      "Accessor"
      (documentation 'constraint-guide-max-width 'function)
 "@version{#2023-4-21}
  @syntax[]{(gtk:constraint-guide-max-width object) => width}
  @syntax[]{(setf (gtk:constraint-guide-max-width object) width)}
  @argument[object]{a @class{gtk:constraint-guide} object}
  @argument[height]{an integer with the maximum width of the guide}
  @begin{short}
    Accessor of the @slot[gtk:constraint-guide]{max-width} slot of the
    @class{gtk:constraint-guide} class.
  @end{short}
  @see-class{gtk:constraint-guide}")

;;; --- constraint-guide-min-height --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "min-height"
                                               'constraint-guide) t)
 "The @code{min-height} property of type @code{:int} (Read / Write) @br{}
  The minimum height of the guide. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'constraint-guide-min-height)
      "Accessor"
      (documentation 'constraint-guide-min-height 'function)
 "@version{#2023-4-21}
  @syntax[]{(gtk:constraint-guide-min-height object) => height}
  @syntax[]{(setf (gtk:constraint-guide-min-height object) height)}
  @argument[object]{a @class{gtk:constraint-guide} object}
  @argument[height]{an integer with the minimum height of the guide}
  @begin{short}
    Accessor of the @slot[gtk:constraint-guide]{min-height} slot of the
    @class{gtk:constraint-guide} class.
  @end{short}
  @see-class{gtk:constraint-guide}")

;;; --- constraint-guide-min-width ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "min-width"
                                               'constraint-guide) t)
 "The @code{min-width} property of type @code{:int} (Read / Write) @br{}
  The minimum width of the guide. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'constraint-guide-min-width)
      "Accessor"
      (documentation 'constraint-guide-min-width 'function)
 "@version{#2023-4-21}
  @syntax[]{(gtk:constraint-guide-min-width object) => width}
  @syntax[]{(setf (gtk:constraint-guide-min-width object) width)}
  @argument[object]{a @class{gtk:constraint-guide} object}
  @argument[height]{an integer with the minimum width of the guide}
  @begin{short}
    Accessor of the @slot[gtk:constraint-guide]{min-width} slot of the
    @class{gtk:constraint-guide} class.
  @end{short}
  @see-class{gtk:constraint-guide}")

;;; --- constraint-guide-name --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "name" 'constraint-guide) t)
 "The @code{name} property of type @code{:string} (Read / Write) @br{}
  A name that identifies the @class{gtk:constraint-guide} object, for debugging.
  @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'constraint-guide-name)
      "Accessor"
      (documentation 'constraint-guide-name 'function)
 "@version{#2023-4-21}
  @syntax[]{(gtk:constraint-guide-name object) => name}
  @syntax[]{(setf (gtk:constraint-guide-name object) name)}
  @argument[object]{a @class{gtk:constraint-guide} object}
  @argument[name]{a string with the name for the guide}
  @begin{short}
    Accessor of the @slot[gtk:constraint-guide]{name} slot of the
    @class{gtk:constraint-guide} class.
  @end{short}
  The @fun{gtk:constraint-guide-name} function retrieves the name for the
  constraint guide. The @setf{gtk:constraint-guide-name} function sets a name.
  The name is useful for debugging purposes.
  @see-class{gtk:constraint-guide}")

;;; --- constraint-guide-nat-height --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "nat-height"
                                               'constraint-guide) t)
 "The @code{nat-height} property of type @code{:int} (Read / Write) @br{}
  The preferred, or natural, height of the guide. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'constraint-guide-nat-height)
      "Accessor"
      (documentation 'constraint-guide-nat-height 'function)
 "@version{#2023-4-21}
  @syntax[]{(gtk:constraint-guide-nat-height object) => height}
  @syntax[]{(setf (gtk:constraint-guide-nat-height object) height)}
  @argument[object]{a @class{gtk:constraint-guide} object}
  @argument[height]{an integer with the preferred, or natural, height of the
    guide}
  @begin{short}
    Accessor of the @slot[gtk:constraint-guide]{nat-height} slot of the
    @class{gtk:constraint-guide} class.
  @end{short}
  @see-class{gtk:constraint-guide}")

;;; --- constraint-guide-nat-width ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "nat-width"
                                               'constraint-guide) t)
 "The @code{nat-width} property of type @code{:int} (Read / Write) @br{}
  The preferred, or natural, width of the guide. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'constraint-guide-nat-width)
      "Accessor"
      (documentation 'constraint-guide-nat-width 'function)
 "@version{#2023-4-21}
  @syntax[]{(gtk:constraint-guide-nat-width object) => height}
  @syntax[]{(setf (gtk:constraint-guide-nat-width object) height)}
  @argument[object]{a @class{gtk:constraint-guide} object}
  @argument[height]{an integer with the preferred, or natural, width of the
    guide}
  @begin{short}
    Accessor of the @slot[gtk:constraint-guide]{nat-width} slot of the
    @class{gtk:constraint-guide} class.
  @end{short}
  @see-class{gtk:constraint-guide}")

;;; --- constraint-guide-strength ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "strength" 'constraint-guide) t)
 "The @code{name} property of type @code{:string} (Read / Write) @br{}
  The @symbol{gtk:constraint-strength} value to be used for the constraint on
  the natural size of the guide. @br{}
  Default value: @code{:medium}")

#+liber-documentation
(setf (liber:alias-for-function 'constraint-guide-strength)
      "Accessor"
      (documentation 'constraint-guide-strength 'function)
 "@version{#2023-4-21}
  @syntax[]{(gtk:constraint-guide-strength object) => strength}
  @syntax[]{(setf (gtk:constraint-guide-strength object) strength)}
  @argument[object]{a @class{gtk:constraint-guide} object}
  @argument[strength]{a @symbol{gtk:constraint-strength} value with the
    strength of the constraint on the natural size}
  @begin{short}
    Accessor of the @slot[gtk:constraint-guide]{strength} slot of the
    @class{gtk:constraint-guide} class.
  @end{short}
  The @fun{gtk:constraint-guide-strength} function retrieves the strength of
  the constraint on the natural size of the given @arg{guide}. The
  @setf{gtk:constraint-guide-strength} function sets the strength of the
  constraint.
  @see-class{gtk:constraint-guide}
  @see-symbol{gtk:constraint-strength}")

;;; ----------------------------------------------------------------------------
;;; gtk_constraint_guide_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline constraint-guide-new))

(defun constraint-guide-new ()
 #+liber-documentation
 "@version{#2023-4-21}
  @return{The newly created @class{gtk:constraint-guide} object.}
  @short{Creates a new constraint guide.}
  @see-class{gtk:constraint-guide}"
  (make-instance 'conatraint-guide))

(export 'constraint-guide-new)

;;; --- End of file gtk4.constraint-guide.lisp ---------------------------------

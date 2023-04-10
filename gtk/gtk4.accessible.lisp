;;; ----------------------------------------------------------------------------
;;; gtk4.accessible.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.6 and modified to document the Lisp binding to the GTK library.
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
;;; GtkAccessible
;;;
;;;     Accessible interface
;;;
;;; Types and Values
;;;
;;;     GtkAccessible
;;;
;;;     GtkAccessibleRole                                  gtk.enumerations
;;;     GtkAccessibleState                                 gtk.enumerations
;;;     GtkAccessibleProperty                              gtk.enumerations
;;;     GtkAccessibleRelation                              gtk.enumerations
;;;     GtkAccessibleTristate                              gtk.enumerations
;;;     GtkAccessibleInvalidState                          gtk.enumerations
;;;     GtkAccessibleAutocomplete                          gtk.enumerations
;;;     GtkAccessibleSort                                  gtk.enumerations
;;;
;;; Accessors
;;;
;;;     gtk_accessible_get_accessible_role
;;;
;;; Functions
;;;
;;;     gtk_accessible_update_state
;;;     gtk_accessible_update_state_value
;;;     gtk_accessible_reset_state
;;;     gtk_accessible_update_property
;;;     gtk_accessible_update_property_value
;;;     gtk_accessible_reset_property
;;;     gtk_accessible_update_relation
;;;     gtk_accessible_update_relation_value
;;;     gtk_accessible_reset_relation
;;;
;;;     gtk_accessible_property_init_value
;;;     gtk_accessible_relation_init_value
;;;     gtk_accessible_state_init_value
;;;
;;; Properties
;;;
;;;     accessible-role
;;;
;;; Hierarchy
;;;
;;;     GInterface
;;;     ╰── GtkAccessible
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkAccessible
;;; ----------------------------------------------------------------------------

(define-g-interface "GtkAccessible" accessible
  (:export t
   :type-initializer "gtk_accessible_get_type")
  ((accessible-role
    accessible-accessible-role
    "accessible-role" "GtkAccessibleRole" t t)))

#+liber-documentation
(setf (liber:alias-for-class 'accessible)
      "Interface"
      (documentation 'accessible 'type)
 "@version{#2022-1-30}
  @begin{short}
    The @sym{gtk:accessible} interface is an interface for describing UI
    elements for Assistive Technologies.
  @end{short}
  Every accessible implementation has:
  @begin{itemize}
    @begin{item}
      a \"role\", represented by a value of the @symbol{gtk:accessible-role}
      enumeration
    @end{item}
    @begin{item}
      an \"attribute\", represented by a set of @symbol{gtk:accessible-state},
      @symbol{gtk:accessible-property} and @symbol{gtk:accessible-relation}
      values
    @end{item}
  @end{itemize}
  The role cannot be changed after instantiating a @sym{gtk:accessible}
  implementation.

  The attributes are updated every time a state of a UI element changes in a
  way that should be reflected by assistive technologies. For instance, if a
  @class{gtk:widget} visibility changes, the @code{:hidden} state will also
  change to reflect the @slot[gtk:widget]{visible} property.
  @see-slot{gtk:accessible-accessible-role}
  @see-class{gtk:widget}
  @see-symbol{gtk:accessible-role}
  @see-symbol{gtk:accessible-state}
  @see-symbol{gtk:accessible-property}
  @see-symbol{gtk:accessible-relation}
  @see-function{gtk:widget-visible}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- accessible-accessible-role ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "accessible-role"
                                               'accessible) t)
 "The @code{accessible-role} property of type @symbol{gtk:accessible-role}
  (Read / Write) @br{}
  The accessible role of the given @sym{gtk:accessible} implementation. The
  accessible role cannot be changed once set.")

#+liber-documentation
(setf (liber:alias-for-function 'accessible-accessible-role)
      "Accessor"
      (documentation 'accessible-accessible-role 'function)
 "@version{#2022-1-2}
  @syntax[]{(gtk:accessible-accessible-role object) => role}
  @syntax[]{(setf (gtk:accessible-accessible-role object) role)}
  @argument[object]{a @class{gtk:accessible} widget}
  @argument[role]{a value of the @symbol{gtk:accessible-role} enumeration}
  @begin{short}
    Accessor of the @slot[gtk:accessible]{accessible-role} slot of the
    @class{gtk:accessible} class.
  @end{short}

  Retrieves the @symbol{gtk:accessible-role} value for the given
  @class{gtk:accessible} widget.
  @see-class{gtk:accessible}
  @see-symbol{gtk:accessible-role}")

;;; ----------------------------------------------------------------------------
;;; gtk_accessible_update_state ()
;;;
;;; void
;;; gtk_accessible_update_state (GtkAccessible *self,
;;;                              GtkAccessibleState first_state,
;;;                              ...);
;;;
;;; Updates a list of accessible states. See the GtkAccessibleState
;;; documentation for the value types of accessible states.
;;;
;;; This function should be called by GtkWidget types whenever an accessible
;;; state change must be communicated to assistive technologies.
;;;
;;; Example:
;;;
;;; value = GTK_ACCESSIBLE_TRISTATE_MIXED;
;;; gtk_accessible_update_state (GTK_ACCESSIBLE (check_button),
;;;                              GTK_ACCESSIBLE_STATE_CHECKED, value,
;;;                              -1);
;;;
;;; self :
;;;     a GtkAccessible
;;;
;;; first_state :
;;;     the first GtkAccessibleState
;;;
;;; ... :
;;;     a list of state and value pairs, terminated by -1
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accessible_update_state_value ()
;;;
;;; void
;;; gtk_accessible_update_state_value (GtkAccessible *self,
;;;                                    int n_states,
;;;                                    GtkAccessibleState states[],
;;;                                    const GValue values[]);
;;;
;;; Updates an array of accessible states.
;;;
;;; This function should be called by GtkWidget types whenever an accessible
;;; state change must be communicated to assistive technologies.
;;;
;;; This function is meant to be used by language bindings.
;;;
;;; self :
;;;     a GtkAccessible
;;;
;;; n_states :
;;;     the number of accessible states to set
;;;
;;; states :
;;;     an array of GtkAccessibleState.
;;;
;;; values :
;;;    an array of GValues, one for each state.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accessible_reset_state
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_accessible_reset_state" accessible-reset-state) :void
 "@version{#2022-1-4}
  @argument[accessible]{a @class{gtk:accessible} widget}
  @argument[state]{a @symbol{gtk:accessible-state} value}
  @begin{short}
    Resets the accessible @arg{state} to its default value.
  @end{short}
  @see-class{gtk:accessible}
  @see-symbol{gtk:accessible-state}"
  (accessible (g:object accessible))
  (state accessible-state))

(export 'accessible-reset-state)

;;; ----------------------------------------------------------------------------
;;; gtk_accessible_update_property ()
;;;
;;; void
;;; gtk_accessible_update_property (GtkAccessible *self,
;;;                                 GtkAccessibleProperty first_property,
;;;                                 ...);
;;;
;;; Updates a list of accessible properties. See the GtkAccessibleProperty
;;; documentation for the value types of accessible properties.
;;;
;;; This function should be called by GtkWidget types whenever an accessible
;;; property change must be communicated to assistive technologies.
;;;
;;; Example :
;;;
;;; value = gtk_adjustment_get_value (adjustment);
;;; gtk_accessible_update_property (GTK_ACCESSIBLE (spin_button),
;;;                                    GTK_ACCESSIBLE_PROPERTY_VALUE_NOW, value,
;;;                                    -1);
;;;
;;; self :
;;;     a GtkAccessible
;;;
;;; first_property :
;;;     the first GtkAccessibleProperty
;;;
;;; ... :
;;;     a list of property and value pairs, terminated by -1
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accessible_update_property_value
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_accessible_update_property_value"
          %accessible-update-property-value) :void
  (accessible (g:object accessible))
  (n-properties :int)
  (properties (:pointer accessible-property))
  (values (:pointer (:struct g:value))))

(defun accessible-update-property-value (accessible properties values)
 #+liber-documentation
 "@version{#2022-7-24}
  @argument[accessible]{a @class{gtk:accessible} object}
  @argument[properties]{a list of @class{gtk:accessible-property} values}
  @argument[values]{a list of @symbol{g:value} instance, one for each property}
  @begin{short}
    Updates an array of accessible properties.
  @end{short}
  This function should be called by @class{gtk:widget} types whenever an
  accessible property change must be communicated to assistive technologies.
  This function is meant to be used by language bindings.
  @see-class{gtk:accessible}
  @see-class{gtk:widget}
  @see-symbol{gtk:accessible-property}
  @see-symbol{g:value}"
  (let ((n (length values)))
    (with-foreign-objects ((value-ar '(:struct g:value) n)
                           (prop-ar 'accessible-property n))
      (loop for i from 0 below n
            for value in values
            for property in properties
         do (with-foreign-object (gvalue '(:struct g:value))
              (setf (cffi:mem-aref prop-ar 'accessible-property i) property)
              (g:value-init gvalue)
              (accessible-property-init-value property gvalue)
              (set-g-value (cffi:mem-aptr value-ar '(:struct g:value) i)
                           value
                           (gobject:value-type gvalue)
                           :zero-g-value t)))
      (%accessible-update-property-value accessible n prop-ar value-ar)
      (loop for i from 0 below n
         do (gobject:value-unset (cffi:mem-aptr value-ar
                                                '(:struct g:value) i))))))

(export 'accessible-update-property-value)

;;; ----------------------------------------------------------------------------
;;; gtk_accessible_reset_property
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_accessible_reset_property" accessible-reset-property) :void
 #+liber-documentation
 "@version{#2022-1-4}
  @argument[accessible]{a @class{gtk:accessible} widget}
  @argument[property]{a @symbol{gtk:accessible-property} value}
  @begin{short}
    Resets the accessible @arg{property} to its default value.
  @end{short}
  @see-class{gtk:accessible}
  @see-symbol{gtk:accessible-property}"
  (accessible (g:object accessible))
  (property accessible-property))

(export 'accessible-reset-property)

;;; ----------------------------------------------------------------------------
;;; gtk_accessible_update_relation ()
;;;
;;; void
;;; gtk_accessible_update_relation (GtkAccessible *self,
;;;                                 GtkAccessibleRelation first_relation,
;;;                                 ...);
;;;
;;; Updates a list of accessible relations.
;;;
;;; This function should be called by GtkWidget types whenever an accessible
;;; relation change must be communicated to assistive technologies.
;;;
;;; If the GtkAccessibleRelation requires a list of references, you should pass
;;; each reference individually, followed by NULL, e.g.
;;;
;;; value = GTK_ACCESSIBLE_TRISTATE_MIXED;
;;; gtk_accessible_update_state (GTK_ACCESSIBLE (check_button),
;;;                              GTK_ACCESSIBLE_STATE_CHECKED, value,
;;;                              -1);
;;;
;;; self :
;;;     a GtkAccessible
;;;
;;; first_relation :
;;;     the first GtkAccessibleRelation
;;;
;;; ... :
;;;     a list of relation and value pairs, terminated by -1
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accessible_update_relation_value ()
;;;
;;; void
;;; gtk_accessible_update_relation_value (GtkAccessible *self,
;;;                                       int n_relations,
;;;                                       GtkAccessibleRelation relations[],
;;;                                       const GValue values[]);
;;;
;;; Updates an array of accessible relations.
;;;
;;; This function should be called by GtkWidget types whenever an accessible
;;; relation change must be communicated to assistive technologies.
;;;
;;; This function is meant to be used by language bindings.
;;;
;;; self :
;;;     a GtkAccessible
;;;
;;; n_relations :
;;;     the number of accessible relations to set
;;;
;;; relations :
;;;     an array of GtkAccessibleRelation.
;;;
;;; values :
;;;     an array of GValues, one for each relation.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accessible_reset_relation
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_accessible_reset_relation" accessible-reset-relation) :void
 #+liber-documentation
 "@version{#2022-1-4}
  @argument[accessible]{a @class{gtk:accessible} widget}
  @argument[relation]{a @symbol{gtk:accessible-relation} value}
  @begin{short}
    Resets the accessible @arg{relation} to its default value.
  @end{short}
  @see-class{gtk:accessible}
  @see-symbol{gtk:accessible-relation}"
  (accessible (g:object accessible))
  (relation accessible-relation))

(export 'accessible-reset-relation)

;;; ----------------------------------------------------------------------------
;;; gtk_accessible_property_init_value
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_accessible_property_init_value" accessible-property-init-value)
    :void
 #+liber-documentation
 "@version{#2022-1-4}
  @argument[property]{a @symbol{gtk:accessible-property} value}
  @argument[gvalue]{an uninitialized @symbol{g:value} instance}
  @begin{short}
    Initializes @arg{gvalue} with the appropriate type for @arg{property}.
  @end{short}
  This function is mostly meant for language bindings, in conjunction with
  the @fun{gtk:accessible-update-property-value} function.
  @see-symbol{gtk:accessible-property}
  @see-function{gtk:accessible-update-property-value}"
  (property accessible-property)
  (value (:pointer (:struct g:value))))

(export 'accessible-property-init-value)

;;; ----------------------------------------------------------------------------
;;; gtk_accessible_relation_init_value
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_accessible_relation_init_value" accessible-relation-init-value)
    :void
 #+liber-documentation
 "@version{#2022-1-4}
  @argument[relation]{a @symbol{gtk:accessible-relation} value}
  @argument[gvalue]{an uninitialized @symbol{g:value} value}
  @begin{short}
    Initializes @arg{value} with the appropriate type for @arg{relation}.
  @end{short}
  This function is mostly meant for language bindings, in conjunction with the
  @fun{gtk:accessible-update-relation-value} function.
  @see-symbol{gtk:accessible-relation}
  @see-function{gtk:accessible-update-relation-value}"
  (relation accessible-relation)
  (value (:pointer (:struct g:value))))

(export 'accessible-relation-init-value)

;;; ----------------------------------------------------------------------------
;;; gtk_accessible_state_init_value
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_accessible_state_init_value" accessible-state-init-value) :void
 #+liber-documentation
 "@version{#2022-1-4}
  @argument[state]{a @symbol{gtk:accessible-state} value}
  @argument[gvalue]{an uninitialized @symbol{g:value} value}
  @begin{short}
    Initializes @arg{gvalue} with the appropriate type for @arg{state}.
  @end{short}
  This function is mostly meant for language bindings, in conjunction with the
  @fun{gtk:accessible-update-state-value} function.
  @see-symbol{gtk:accessible-state}
  @see-function{gtk:accessible-update-state-value}"
  (state accessible-state)
  (value (:pointer (:struct g:value))))

(export 'accessible-state-init-value)

;;; --- End of file gtk4.accessible.lisp ---------------------------------------

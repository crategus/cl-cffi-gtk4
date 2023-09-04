(in-package :gtk-test)

(def-suite gtk-constraint :in gtk-suite)
(in-suite gtk-constraint)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkConstraintAttribute                             gtk.enumerations
;;;     GtkConstraintRelation                              gtk.enumerations
;;;     GtkConstraintStrength                              gtk.enumerations

;;;     GtkConstraintTarget

(test gtk-constraint-target-interface
  ;; Type check
  (is (g:type-is-interface "GtkConstraintTarget"))
  ;; Check the registered name
  (is (eq 'gtk:constraint-target
          (glib:symbol-for-gtype "GtkConstraintTarget")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkConstraintTarget")
          (g:gtype (cffi:foreign-funcall "gtk_constraint_target_get_type" 
                                         :size))))
  ;; Check the interface prerequisites
  (is (equal '("GObject")
             (list-interface-prerequisites "GtkConstraintTarget")))
  ;; Check the interface properties
  (is (equal '()
             (list-interface-properties "GtkConstraintTarget")))
  ;; Check the interface signals
  (is (equal '()
             (list-signals "GtkConstraintTarget")))
  ;; Get the interface definition
  (is (equal '(GOBJECT:DEFINE-G-INTERFACE "GtkConstraintTarget" 
                                          GTK-CONSTRAINT-TARGET
                            (:EXPORT T :TYPE-INITIALIZER
                             "gtk_constraint_target_get_type"))
             (gobject:get-g-type-definition "GtkConstraintTarget"))))

;;;     GtkConstraint

(test gtk-constraint-class
  ;; Type check
  (is (g:type-is-object "GtkConstraint"))
  ;; Check the registered name
  (is (eq 'gtk:constraint
          (glib:symbol-for-gtype "GtkConstraint")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkConstraint")
          (g:gtype (cffi:foreign-funcall "gtk_constraint_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkConstraint")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkConstraint")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkConstraint")))
  ;; Check the properties
  (is (equal '("constant" "multiplier" "relation" "source" "source-attribute" 
               "strength" "target" "target-attribute")
             (list-properties "GtkConstraint")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkConstraint")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkConstraint" GTK-CONSTRAINT
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                                :TYPE-INITIALIZER "gtk_constraint_get_type")
                               ((CONSTANT GTK-CONSTRAINT-CONSTANT "constant"
                                 "gdouble" T NIL)
                                (MULTIPLIER GTK-CONSTRAINT-MULTIPLIER
                                 "multiplier" "gdouble" T NIL)
                                (RELATION GTK-CONSTRAINT-RELATION "relation"
                                 "GtkConstraintRelation" T NIL)
                                (SOURCE GTK-CONSTRAINT-SOURCE "source"
                                 "GtkConstraintTarget" T NIL)
                                (SOURCE-ATTRIBUTE
                                 GTK-CONSTRAINT-SOURCE-ATTRIBUTE
                                 "source-attribute" "GtkConstraintAttribute" T
                                 NIL)
                                (STRENGTH GTK-CONSTRAINT-STRENGTH "strength"
                                 "gint" T NIL)
                                (TARGET GTK-CONSTRAINT-TARGET "target"
                                 "GtkConstraintTarget" T NIL)
                                (TARGET-ATTRIBUTE
                                 GTK-CONSTRAINT-TARGET-ATTRIBUTE
                                 "target-attribute" "GtkConstraintAttribute" T
                                 NIL)))
             (gobject:get-g-type-definition "GtkConstraint"))))

;;; --- Properties -------------------------------------------------------------

;;;     constant
;;;     multiplier
;;;     relation
;;;     source
;;;     source-attribute
;;;     strength
;;;     target
;;;     target-attribute

;;; --- Functions --------------------------------------------------------------

;;;     gtk_constraint_new
;;;     gtk_constraint_new_constant
;;;     gtk_constraint_is_required
;;;     gtk_constraint_is_attached
;;;     gtk_constraint_is_constant

;;; --- 2023-9-2 ---------------------------------------------------------------

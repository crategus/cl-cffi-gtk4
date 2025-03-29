(in-package :gtk-test)

(def-suite gtk-constraint :in gtk-layout-managers)
(in-suite gtk-constraint)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkConstraintAttribute                             gtk.enumerations
;;;     GtkConstraintRelation                              gtk.enumerations
;;;     GtkConstraintStrength                              gtk.enumerations

;;;     GtkConstraintTarget

(test gtk-constraint-target-interface
  ;; Check type
  (is (g:type-is-interface "GtkConstraintTarget"))
  ;; Check registered name
  (is (eq 'gtk:constraint-target
          (glib:symbol-for-gtype "GtkConstraintTarget")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkConstraintTarget")
          (g:gtype (cffi:foreign-funcall "gtk_constraint_target_get_type"
                                         :size))))
  ;; Check interface prerequisites
  (is (equal '("GObject")
             (glib-test:list-interface-prerequisites "GtkConstraintTarget")))
  ;; Check interface properties
  (is (equal '()
             (glib-test:list-interface-properties "GtkConstraintTarget")))
  ;; Check interface signals
  (is (equal '()
             (glib-test:list-signals "GtkConstraintTarget")))
  ;; Get interface definition
  (is (equal '(GOBJECT:DEFINE-GINTERFACE "GtkConstraintTarget"
                                         GTK:CONSTRAINT-TARGET
                      (:EXPORT T
                       :TYPE-INITIALIZER "gtk_constraint_target_get_type"))
             (gobject:get-gtype-definition "GtkConstraintTarget"))))

;;;     GtkConstraint

(test gtk-constraint-class
  ;; Check type
  (is (g:type-is-object "GtkConstraint"))
  ;; Check registered name
  (is (eq 'gtk:constraint
          (glib:symbol-for-gtype "GtkConstraint")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkConstraint")
          (g:gtype (cffi:foreign-funcall "gtk_constraint_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkConstraint")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkConstraint")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkConstraint")))
  ;; Check properties
  (is (equal '("constant" "multiplier" "relation" "source" "source-attribute"
               "strength" "target" "target-attribute")
             (glib-test:list-properties "GtkConstraint")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkConstraint")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkConstraint" GTK:CONSTRAINT
                      (:SUPERCLASS GOBJECT:OBJECT
                       :EXPORT T
                       :INTERFACES NIL
                       :TYPE-INITIALIZER "gtk_constraint_get_type")
                      ((CONSTANT CONSTRAINT-CONSTANT "constant" "gdouble" T NIL)
                       (MULTIPLIER CONSTRAINT-MULTIPLIER
                        "multiplier" "gdouble" T NIL)
                       (RELATION CONSTRAINT-RELATION
                        "relation" "GtkConstraintRelation" T NIL)
                       (SOURCE CONSTRAINT-SOURCE
                        "source" "GtkConstraintTarget" T NIL)
                       (SOURCE-ATTRIBUTE CONSTRAINT-SOURCE-ATTRIBUTE
                        "source-attribute" "GtkConstraintAttribute" T NIL)
                       (STRENGTH CONSTRAINT-STRENGTH
                        "strength" "gint" T NIL)
                       (TARGET CONSTRAINT-TARGET
                        "target" "GtkConstraintTarget" T NIL)
                       (TARGET-ATTRIBUTE CONSTRAINT-TARGET-ATTRIBUTE
                        "target-attribute" "GtkConstraintAttribute" T NIL)))
             (gobject:get-gtype-definition "GtkConstraint"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-constraint-properties
  (glib-test:with-check-memory (constraint)
    (setf constraint (make-instance 'gtk:constraint))
    (is (= 0.0d0 (gtk:constraint-constant constraint)))
    (is (= 1.0d0 (gtk:constraint-multiplier constraint)))
    (is (eq :eq (gtk:constraint-relation constraint)))
    (is-false (gtk:constraint-source constraint))
    (is (eq :none (gtk:constraint-source-attribute constraint)))
    (is (= 1001001000 (gtk:constraint-strength constraint)))
    (is-false (gtk:constraint-target constraint))
    (is (eq :none (gtk:constraint-target-attribute constraint)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_constraint_new

(test gtk-constraint-new
  (glib-test:with-check-memory (constraint button1 button2)
    (setf button1 (make-instance 'gtk:button))
    (setf button2 (make-instance 'gtk:button))
    (is (typep (setf constraint
                     (gtk:constraint-new button1
                                         :width
                                         :eq
                                         button2
                                         :width
                                         1.0
                                         0.0
                                         :required)) 'gtk:constraint))
    (is (= 0.0d0 (gtk:constraint-constant constraint)))
    (is (= 1.0d0 (gtk:constraint-multiplier constraint)))
    (is (eq :eq (gtk:constraint-relation constraint)))
    (is (eq button2 (gtk:constraint-source constraint)))
    (is (eq :width (gtk:constraint-source-attribute constraint)))
    (is (= 1001001000 (gtk:constraint-strength constraint)))
    (is (eq button1 (gtk:constraint-target constraint)))
    (is (eq :width (gtk:constraint-target-attribute constraint)))))

;;;     gtk_constraint_new_constant

(test gtk-constraint-new-constant
  (glib-test:with-check-memory (constraint button)
    (setf button (make-instance 'gtk:button))
    (is (typep (setf constraint
                     (gtk:constraint-new-constant button
                                                  :width
                                                  :le
                                                  200
                                                  :required)) 'gtk:constraint))
    (is (= 200.0d0 (gtk:constraint-constant constraint)))
    (is (= 1.0d0 (gtk:constraint-multiplier constraint)))
    (is (eq :le (gtk:constraint-relation constraint)))
    (is-false (gtk:constraint-source constraint))
    (is (eq :none (gtk:constraint-source-attribute constraint)))
    (is (= 1001001000 (gtk:constraint-strength constraint)))
    (is (eq button (gtk:constraint-target constraint)))
    (is (eq :width (gtk:constraint-target-attribute constraint)))))

;;;     gtk_constraint_is_required
;;;     gtk_constraint_is_attached
;;;     gtk_constraint_is_constant

(test gtk-constraint-is
  (glib-test:with-check-memory (constraint button)
    (setf button (make-instance 'gtk:button))
    (is (typep (setf constraint
                     (gtk:constraint-new-constant button
                                                  :width
                                                  :le
                                                  200
                                                  :required)) 'gtk:constraint))
    (is-true (gtk:constraint-is-required constraint))
    (is-false (gtk:constraint-is-attached constraint))
    (is-true (gtk:constraint-is-constant constraint))))

;;; 2025-3-25

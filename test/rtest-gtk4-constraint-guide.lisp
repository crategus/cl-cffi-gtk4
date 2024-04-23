(in-package :gtk-test)

(def-suite gtk-constraint-guide :in gtk-suite)
(in-suite gtk-constraint-guide)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkConstraintGuide

(test gtk-constraint-guide-class
  ;; Check type
  (is (g:type-is-object "GtkConstraintGuide"))
  ;; Check registered name
  (is (eq 'gtk:constraint-guide
          (glib:symbol-for-gtype "GtkConstraintGuide")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkConstraintGuide")
          (g:gtype (cffi:foreign-funcall "gtk_constraint_guide_get_type"
                                         :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkConstraintGuide")))
  ;; Check children
  (is (equal '()
             (list-children "GtkConstraintGuide")))
  ;; Check interfaces
  (is (equal '("GtkConstraintTarget")
             (list-interfaces "GtkConstraintGuide")))
  ;; Check properties
  (is (equal '("max-height" "max-width" "min-height" "min-width" "name"
               "nat-height" "nat-width" "strength")
             (list-properties "GtkConstraintGuide")))
  ;; Check signals
  (is (equal '()
             (list-signals "GtkConstraintGuide")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkConstraintGuide" GTK-CONSTRAINT-GUIDE
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES
                                ("GtkConstraintTarget") :TYPE-INITIALIZER
                                "gtk_constraint_guide_get_type")
                               ((MAX-HEIGHT GTK-CONSTRAINT-GUIDE-MAX-HEIGHT
                                 "max-height" "gint" T T)
                                (MAX-WIDTH GTK-CONSTRAINT-GUIDE-MAX-WIDTH
                                 "max-width" "gint" T T)
                                (MIN-HEIGHT GTK-CONSTRAINT-GUIDE-MIN-HEIGHT
                                 "min-height" "gint" T T)
                                (MIN-WIDTH GTK-CONSTRAINT-GUIDE-MIN-WIDTH
                                 "min-width" "gint" T T)
                                (NAME GTK-CONSTRAINT-GUIDE-NAME "name"
                                 "gchararray" T T)
                                (NAT-HEIGHT GTK-CONSTRAINT-GUIDE-NAT-HEIGHT
                                 "nat-height" "gint" T T)
                                (NAT-WIDTH GTK-CONSTRAINT-GUIDE-NAT-WIDTH
                                 "nat-width" "gint" T T)
                                (STRENGTH GTK-CONSTRAINT-GUIDE-STRENGTH
                                 "strength" "GtkConstraintStrength" T T)))
             (gobject:get-g-type-definition "GtkConstraintGuide"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-constraint-guide-properties
  (let ((guide (make-instance 'gtk:constraint-guide)))
    (is (= 2147483647 (gtk:constraint-guide-max-height guide)))
    (is (= 2147483647 (gtk:constraint-guide-max-width guide)))
    (is (= 0 (gtk:constraint-guide-min-height guide)))
    (is (= 0 (gtk:constraint-guide-min-width guide)))
    (is-false (gtk:constraint-guide-name guide))
    (is (= 0 (gtk:constraint-guide-nat-height guide)))
    (is (= 0 (gtk:constraint-guide-nat-width guide)))
    (is (eq :medium (gtk:constraint-guide-strength guide)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_constraint_guide_new

(test gtk-constraint-guide-new
  (is (typep (gtk:constraint-guide-new) 'gtk:constraint-guide)))

;;;     gtk_constraint_guide_set_min_size
;;;     gtk_constraint_guide_get_min_size
;;;     gtk_constraint_guide_set_nat_size
;;;     gtk_constraint_guide_get_nat_size
;;;     gtk_constraint_guide_set_max_size
;;;     gtk_constraint_guide_get_max_size

;;; 2024-4-23

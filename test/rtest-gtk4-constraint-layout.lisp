(in-package :gtk-test)

(def-suite gtk-constraint-layout :in gtk-suite)
(in-suite gtk-constraint-layout)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkConstraintVflParserError              -> gtk4.enumerations.lisp

;;;     GtkConstraintLayoutChild

(test gtk-constraint-layout-child-class
  ;; Check type
  (is (g:type-is-object "GtkConstraintLayoutChild"))
  ;; Check registered name
  (is (eq 'gtk:constraint-layout-child
          (glib:symbol-for-gtype "GtkConstraintLayoutChild")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkConstraintLayoutChild")
          (g:gtype (cffi:foreign-funcall "gtk_constraint_layout_child_get_type"
                                         :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkLayoutChild")
          (g:type-parent "GtkConstraintLayoutChild")))
  ;; Check children
  (is (equal '()
             (list-children "GtkConstraintLayoutChild")))
  ;; Check interfaces
  (is (equal '()
             (list-interfaces "GtkConstraintLayoutChild")))
  ;; Check properties
  (is (equal '()
             (list-properties "GtkConstraintLayoutChild")))
  ;; Check signals
  (is (equal '()
             (list-signals "GtkConstraintLayoutChild")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkConstraintLayoutChild"
                               GTK-CONSTRAINT-LAYOUT-CHILD
                               (:SUPERCLASS GTK-LAYOUT-CHILD :EXPORT T
                                :INTERFACES NIL :TYPE-INITIALIZER
                                "gtk_constraint_layout_child_get_type")
                               NIL)
             (gobject:get-g-type-definition "GtkConstraintLayoutChild"))))

;;; --- Types and Values -------------------------------------------------------

;;;     GtkConstraintLayout

(test gtk-constraint-layout-class
  ;; Check type
  (is (g:type-is-object "GtkConstraintLayout"))
  ;; Check registered name
  (is (eq 'gtk:constraint-layout
          (glib:symbol-for-gtype "GtkConstraintLayout")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkConstraintLayout")
          (g:gtype (cffi:foreign-funcall "gtk_constraint_layout_get_type"
                                         :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkLayoutManager")
          (g:type-parent "GtkConstraintLayout")))
  ;; Check children
  (is (equal '()
             (list-children "GtkConstraintLayout")))
  ;; Check interfaces
  (is (equal '()
             (list-interfaces "GObject")))
  ;; Check properties
  (is (equal '()
             (list-properties "GtkConstraintLayout")))
  ;; Check signals
  (is (equal '()
             (list-signals "GtkConstraintLayout")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkConstraintLayout" GTK-CONSTRAINT-LAYOUT
                               (:SUPERCLASS GTK-LAYOUT-MANAGER :EXPORT T
                                :INTERFACES ("GtkBuildable") :TYPE-INITIALIZER
                                "gtk_constraint_layout_get_type")
                               NIL)
             (gobject:get-g-type-definition "GtkConstraintLayout"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_constraint_layout_new

(test gtk-constraint-layout-new
  (is (typep (gtk:constraint-layout-new) 'gtk:constraint-layout)))

;;;     gtk_constraint_layout_add_constraint
;;;     gtk_constraint_layout_remove_constraint
;;;     gtk_constraint_layout_remove_all_constraints
;;;     gtk_constraint_layout_add_guide
;;;     gtk_constraint_layout_remove_guide
;;;     gtk_constraint_layout_add_constraints_from_description
;;;     gtk_constraint_layout_add_constraints_from_descriptionv
;;;     gtk_constraint_layout_observe_constraints
;;;     gtk_constraint_layout_observe_guides

;;; 2024-4-23

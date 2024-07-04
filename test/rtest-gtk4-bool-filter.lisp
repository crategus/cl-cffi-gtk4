(in-package :gtk-test)

(def-suite gtk-bool-filter :in gtk-suite)
(in-suite gtk-bool-filter)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkBoolFilter

(test gtk-bool-filter-class
  ;; Check type
  (is (g:type-is-object "GtkBoolFilter"))
  ;; Check registered name
  (is (eq 'gtk:bool-filter
          (glib:symbol-for-gtype "GtkBoolFilter")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkBoolFilter")
          (g:gtype (cffi:foreign-funcall "gtk_bool_filter_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkFilter")
          (g:type-parent "GtkBoolFilter")))
  ;; Check children
  (is (equal '()
             (gtk-test:list-children "GtkBoolFilter")))
  ;; Check interfaces
  (is (equal '()
             (gtk-test:list-interfaces "GtkBoolFilter")))
  ;; Check properties
  (is (equal '("expression" "invert")
             (gtk-test:list-properties "GtkBoolFilter")))
  ;; Check signals
  (is (equal '()
             (gtk-test:list-signals "GtkBoolFilter")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkBoolFilter" GTK-BOOL-FILTER
                               (:SUPERCLASS GTK-FILTER :EXPORT T :INTERFACES
                                NIL :TYPE-INITIALIZER
                                "gtk_bool_filter_get_type")
                               ((EXPRESSION GTK-BOOL-FILTER-EXPRESSION
                                 "expression" "GtkExpression" T T)
                                (INVERT GTK-BOOL-FILTER-INVERT "invert"
                                 "gboolean" T T)))
             (gobject:get-g-type-definition "GtkBoolFilter"))))

;;; --- Properties -------------------------------------------------------------

;;;     expression
;;;     invert

;;; --- Functions --------------------------------------------------------------

;;;     gtk_bool_filter_new

(test gtk-bool-filter-new
  (let ((filter nil)
        (expression nil))
    (cffi:with-foreign-object (gvalue '(:struct g:value))
      (is (eq (g:gtype "gboolean")
              (g:value-type (g:value-init gvalue "gboolean"))))
      (is-true (setf (g:value-boolean gvalue) t))
      (is-true (setf expression (gtk:constant-expression-new-for-value gvalue)))
      (is (typep (setf filter
                       (gtk:bool-filter-new expression)) 'gtk:bool-filter))
      (is (cffi:pointer-eq expression (gtk:bool-filter-expression filter)))
      (is-false (gtk:bool-filter-invert filter)))))

;;; 2024-7-4

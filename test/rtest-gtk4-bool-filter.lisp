(in-package :gtk-test)

(def-suite gtk-bool-filter :in gtk-list-model-support)
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
             (glib-test:list-children "GtkBoolFilter")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkBoolFilter")))
  ;; Check properties
  (is (equal '("expression" "invert")
             (glib-test:list-properties "GtkBoolFilter")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkBoolFilter")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkBoolFilter" GTK:BOOL-FILTER
                       (:SUPERCLASS GTK:FILTER
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_bool_filter_get_type")
                       ((EXPRESSION BOOL-FILTER-EXPRESSION
                         "expression" "GtkExpression" T T)
                        (INVERT BOOL-FILTER-INVERT "invert" "gboolean" T T)))
             (gobject:get-gtype-definition "GtkBoolFilter"))))

;;; --- Properties -------------------------------------------------------------

;;;     gtk:bool-filter-expression

(test gtk-bool-filter-expression
  (let ((filter (make-instance 'gtk:bool-filter))
        (expr (gtk:constant-expression-new "gboolean" t)))
    (is (cffi:null-pointer-p (gtk:bool-filter-expression filter)))
    (is (cffi:pointer-eq expr
                         (setf (gtk:bool-filter-expression filter) expr)))
    (is (cffi:pointer-eq expr (gtk:bool-filter-expression filter)))))

;;;     gtk:bool-filter-invert

(test gtk-bool-filter-invert
  (let ((filter (make-instance 'gtk:bool-filter)))
    (is-false (gtk:bool-filter-invert filter))
    (is-true (setf (gtk:bool-filter-invert filter) t))
    (is-true (gtk:bool-filter-invert filter))))

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
      ;; Create the bool filter
      (is (typep (setf filter
                       (gtk:bool-filter-new expression)) 'gtk:bool-filter))
      (is (cffi:pointer-eq expression (gtk:bool-filter-expression filter)))
      (is-false (gtk:bool-filter-invert filter)))))

;;; 2024-9-19

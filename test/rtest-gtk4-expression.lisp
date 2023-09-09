(in-package :gtk-test)

(def-suite gtk-expression :in gtk-suite)
(in-suite gtk-expression)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkExpression

;;;     GtkExpressionWatch
;;;     GtkParamSpecExpression

;;; --- Functions --------------------------------------------------------------

;;;     GtkExpressionNotify

;;;     gtk_expression_ref
;;;     gtk_expression_unref
;;;     gtk_expression_get_value_type
;;;     gtk_expression_is_static
;;;     gtk_expression_evaluate

;;;     gtk_expression_watch
;;;     gtk_expression_bind
;;;     gtk_expression_watch_ref
;;;     gtk_expression_watch_unref
;;;     gtk_expression_watch_evaluate
;;;     gtk_expression_watch_unwatch

;;;     gtk_property_expression_new
;;;     gtk_property_expression_new_for_pspec
;;;     gtk_property_expression_get_expression
;;;     gtk_property_expression_get_pspec

;;;     gtk_constant_expression_new

;;;     gtk_constant_expression_new_for_value
;;;     gtk_constant_expression_get_value

(test gtk-constant-expression-new-for-value
  (cffi:with-foreign-object (gvalue '(:struct g:value))

    (g:value-init gvalue "gchararray")
    (setf (g:value-string gvalue) "string")
    (let ((expression (gtk:constant-expression-new-for-value gvalue)))
      (is (eq (g:gtype "gchararray") (gtk:expression-value-type expression)))
      (is (string= "string"
                   (g:value-string (gtk:constant-expression-value expression))))
)))

;;;     gtk_object_expression_new
;;;     gtk_object_expression_get_object
;;;     gtk_closure_expression_new
;;;     gtk_cclosure_expression_new

;;;     GTK_VALUE_HOLDS_EXPRESSION()

;;;     gtk_value_get_expression
;;;     gtk_value_set_expression

(test gtk-value-expression
  (cffi:with-foreign-objects ((gvalue1 '(:struct g:value))
                              (gvalue2 '(:struct g:value)))
    (g:value-init gvalue1 "gchararray")
    (g:value-init gvalue2 "GtkExpression")
    (setf (g:value-string gvalue1) "string")
    (let ((expression (gtk:constant-expression-new-for-value gvalue1)))
      (setf (gtk:value-expression gvalue2) expression)
      (is (eq (g:gtype "gchararray")
              (gtk:expression-value-type (gtk:value-expression gvalue2))))
)))

;;;     gtk_value_take_expression

;;;     gtk_value_dup_expression

;;;     gtk_param_spec_expression

;;; --- 2023-8-11 --------------------------------------------------------------

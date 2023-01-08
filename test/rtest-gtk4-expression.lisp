(def-suite gtk-expression :in gtk-suite)
(in-suite gtk-expression)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkExpression

#+nil
(test gtk-expression-class
  ;; Type check
  (is (g:type-is-object "GtkExpression"))
  ;; Check the registered name
  (is (eq 'g-object
          (gobject:symbol-for-gtype "GtkExpression")))
  ;; Check the type initializer
  (is (eq (gtype "GtkExpression")
          (gtype (foreign-funcall "gtk_expression_get_type" :size))))
  ;; Check the parent
  (is (eq (gtype "GObject")
          (g-type-parent "GtkExpression")))
  ;; Check the children
  (is (equal '()
             (mapcar #'g-type-name (g-type-children "GtkExpression"))))
  ;; Check the interfaces
  (is (equal '()
             (mapcar #'g-type-name (g-type-interfaces "GtkExpression"))))
  ;; Check the class properties
  (is (equal '()
             (list-class-property-names "GtkExpression")))
  ;; Check the list of signals
  (is (equal '()
             (sort (mapcar #'g-signal-name
                           (g-signal-list-ids "GtkExpression"))
                   #'string<)))
  ;; Check the class definition
  (is (equal '()
             (get-g-type-definition "GtkExpression"))))

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

;;;     gtk_object_expression_new
;;;     gtk_object_expression_get_object
;;;     gtk_closure_expression_new
;;;     gtk_cclosure_expression_new
;;;
;;;     GTK_VALUE_HOLDS_EXPRESSION()
;;;
;;;     gtk_value_set_expression
;;;     gtk_value_take_expression
;;;     gtk_value_get_expression
;;;     gtk_value_dup_expression

;;;     gtk_param_spec_expression

;;; 2022-9-6

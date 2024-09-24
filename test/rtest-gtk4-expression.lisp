(in-package :gtk-test)

(def-suite gtk-expression :in gtk-suite)
(in-suite gtk-expression)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkExpression

;;;     GtkExpressionWatch

(test gtk-expression-watch-boxed
  ;; Check type
  (is (g:type-is-boxed "GtkExpressionWatch"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkExpressionWatch")
          (g:gtype (cffi:foreign-funcall "gtk_expression_watch_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:expression-watch
          (glib:symbol-for-gtype "GtkExpressionWatch"))))

;;;     GtkParamSpecExpression

;;; --- Functions --------------------------------------------------------------

;;;     GtkExpressionNotify

;;;     gtk_expression_ref
;;;     gtk_expression_unref

(test gtk-expression-ref/unref
  (let ((expr (gtk:property-expression-new "GtkLabel" nil "label")))
    (is (cffi:pointerp (gtk:expression-ref expr)))
    (is-false (gtk:expression-unref expr))
    (is-false (gtk:expression-unref expr))))

;;;     gtk_expression_get_value_type
;;;     gtk_expression_is_static

(test gtk-expression-value-type/is-static
  (gobject:with-g-value (gvalue "gchararray")
    (let ((expression (gtk:constant-expression-new-for-value gvalue)))
      (is (eq (g:gtype "gchararray")
              (gtk:expression-value-type expression)))
      (is-true (gtk:expression-is-static expression))
      (is-false (gtk:expression-unref expression)))))

;;;     gtk_expression_evaluate

(test gtk-expression-evaluate/evaluate-value
  (gobject:with-g-values ((gvalue "gchararray" "string") gvalue1)
    ;; Check value of gvalue
    (is (string= "string" (g:value-get gvalue)))
    ;; Initialize a constant expression
    (let ((expression (gtk:constant-expression-new-for-value gvalue)))
      ;; Check gtype and the value of the expression
      (is (eq (g:gtype "gchararray") (gtk:expression-value-type expression)))
      (is (string= "string"
                   (g:value-string (gtk:constant-expression-value expression))))
      ;; Evaluate the expression
      (is-true (gtk:expression-evaluate expression nil gvalue1))
      (is (string= "string" (g:value-get gvalue1)))
      (is (string= "string" (gtk:expression-evaluate-value expression nil)))
      (is-false (gtk:expression-unref expression)))))

;;;     gtk_expression_watch

;;;     gtk_expression_bind
;;;     gtk_expression_watch_ref
;;;     gtk_expression_watch_unref
;;;     gtk_expression_watch_evaluate
;;;     gtk_expression_watch_unwatch

;;;     gtk_property_expression_new
;;;     gtk_property_expression_get_expression
;;;     gtk_property_expression_get_pspec

(test gtk-property-expression-new.1
  (gobject:with-g-value (gvalue)
    (let ((label (gtk:label-new "text"))
          (expr (gtk:property-expression-new "GtkLabel" nil "label")))
      ;; Check gtype
      (is (eq (g:gtype "gchararray")
              (gtk:expression-value-type expr)))
      (is-false (gtk:expression-is-static expr))

      (is-false (gtk:property-expression-expression expr))

      (is (g:is-param-spec (gtk:property-expression-pspec expr)))
      (is (eq (g:gtype "GParamString")
              (g:param-spec-type (gtk:property-expression-pspec expr))))

      (is-false (gtk:expression-evaluate expr nil gvalue))
      (is-true (gtk:expression-evaluate expr label gvalue))
      (is (string= "text" (g:value-get gvalue)))
      (is (string= "text"
                   (gtk:expression-evaluate-value expr label)))

      (is-false (gtk:expression-unref expr)))))

(test gtk-property-expression-new.2
  (gobject:with-g-value (gvalue)
    (let ((label (gtk:label-new "label"))
          (expr (gtk:property-expression-new "GtkLabel" nil "justify")))

      (is (eq (g:gtype "GtkJustification")
              (gtk:expression-value-type expr)))
      (is-false (gtk:expression-is-static expr))

      (is-false (gtk:property-expression-expression expr))
      (is (g:is-param-spec (gtk:property-expression-pspec expr)))
      (is (eq (g:gtype "GParamEnum")
              (g:param-spec-type (gtk:property-expression-pspec expr))))

      (is-false (gtk:expression-evaluate expr nil gvalue))
      (is-true (gtk:expression-evaluate expr label gvalue))
      (is (eq :left (g:value-get gvalue)))
      (is (eq :left (gtk:expression-evaluate-value expr label)))

      (is-false (gtk:expression-unref expr)))))

(test gtk-property-expression-new.3
  (let* ((expr1 (gtk:property-expression-new "GtkButton" nil "child"))
         (expr2 (gtk:property-expression-new "GtkLabel" expr1 "label"))
         (button (gtk:button-new-with-label "button")))

    (is (eq (g:gtype "GtkWidget") (gtk:expression-value-type expr1)))
    (is-false (gtk:property-expression-expression expr1))
    (is (eq (g:gtype "GParamObject")
            (g:param-spec-type (gtk:property-expression-pspec expr1))))

    (is (eq (g:gtype "gchararray") (gtk:expression-value-type expr2)))
    (is (cffi:pointer-eq expr1
                         (gtk:property-expression-expression expr2)))
    (is (eq (g:gtype "GParamString")
            (g:param-spec-type (gtk:property-expression-pspec expr2))))

    (is-false (gtk:expression-evaluate-value expr1 nil))
    (is-false (gtk:expression-evaluate-value expr2 nil))

    (is (eq (g:gtype "GtkLabel")
            (g:type-from-instance
              (gtk:expression-evaluate-value expr1 button))))
    (is (string= "button" (gtk:expression-evaluate-value expr2 button)))

    (is-false (gtk:expression-unref expr2))))

;;;     gtk_property_expression_new_for_pspec

(test gtk-property-expression-new-for-pspec
  (let* ((pspec (g:param-spec-boolean "name" "nick" "blurb" t '(:readable)))
         (expr (gtk:property-expression-new-for-pspec nil pspec)))
    (is-false (gtk:property-expression-expression expr))
    (is (eq (g:gtype "GParamBoolean")
            (g:param-spec-type (gtk:property-expression-pspec expr))))))

;;;     gtk_constant_expression_new

(test gtk-constant-expression-new
  (let ((expr (gtk:constant-expression-new "gint" 100)))
    (is (eq (g:gtype "gint") (gtk:expression-value-type expr)))
    (is (= 100 (g:value-int (gtk:constant-expression-value expr))))
    (gtk:expression-unref expr)))

;;;     gtk_constant_expression_new_for_value
;;;     gtk_constant_expression_get_value

(test gtk-constant-expression-new-for-value
  (gobject:with-g-value (gvalue "gchararray" "string")
    (let ((expression (gtk:constant-expression-new-for-value gvalue)))
      (is (eq (g:gtype "gchararray") (gtk:expression-value-type expression)))
      (is (string= "string"
                   (g:value-string
                     (gtk:constant-expression-value expression)))))))

;;;     gtk_object_expression_new
;;;     gtk_object_expression_get_object

(test gtk-object-expression-new/object
  (let* ((label (gtk:label-new "text"))
         (expr (gtk:object-expression-new label)))

    (is (eq (g:gtype "GtkLabel") (gtk:expression-value-type expr)))
    (is-false (gtk:expression-is-static expr))

    (is (eq (g:gtype "GtkLabel")
            (g:type-from-instance (gtk:expression-evaluate-value expr nil))))))

;;;     gtk_closure_expression_new
;;;     gtk_cclosure_expression_new

;;;     GTK_VALUE_HOLDS_EXPRESSION()

;;;     gtk_value_get_expression
;;;     gtk_value_set_expression

(test gtk-value-expression
  (gobject:with-g-values ((gvalue1 "gchararray" "string")
                          (gvalue2 "GtkExpression"))
    (let ((expression (gtk:constant-expression-new-for-value gvalue1)))
      (setf (gtk:value-expression gvalue2) expression)
      (is (eq (g:gtype "gchararray")
              (gtk:expression-value-type (gtk:value-expression gvalue2)))))))

;;;     gtk_value_take_expression
;;;     gtk_value_dup_expression
;;;     gtk_param_spec_expression

;;; 2024-7-4

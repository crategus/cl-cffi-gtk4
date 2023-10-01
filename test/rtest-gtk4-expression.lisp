(in-package :gtk-test)

(def-suite gtk-expression :in gtk-suite)
(in-suite gtk-expression)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkExpression

;;;     GtkExpressionWatch

(test gtk-expression-watch-boxed
  ;; Type check
  (is (g:type-is-a (g:gtype "GtkExpressionWatch") g:+g-type-boxed+))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkExpressionWatch")
          (g:gtype (cffi:foreign-funcall "gtk_expression_watch_get_type"
                                         :size))))
  ;; Check the registered name
  (is (eq 'gtk:expression-watch
          (glib:symbol-for-gtype "GtkExpressionWatch"))))

;;;     GtkParamSpecExpression

;;; --- Functions --------------------------------------------------------------

;;;     GtkExpressionNotify

;;;     gtk_expression_ref
;;;     gtk_expression_unref

;;;     gtk_expression_get_value_type
;;;     gtk_expression_is_static

(test gtk-expression-value-type/is-static
  (cffi:with-foreign-object (gvalue '(:struct g:value))
    (g:value-init gvalue "gchararray")
    (let ((expression (gtk:constant-expression-new-for-value gvalue)))
      (is (eq (g:gtype "gchararray")
              (gtk:expression-value-type expression)))
      (is-true (gtk:expression-is-static expression))
      (is-false (gtk:expression-unref expression)))))

;;;     gtk_expression_evaluate

(test gtk-expression-evaluate
  (cffi:with-foreign-object (gvalue '(:struct g:value))
    (g:value-init gvalue "gchararray")
    (setf (g:value-string gvalue) "string")
    (let ((expression (gtk:constant-expression-new-for-value gvalue)))
      (is (eq (g:gtype "gchararray") (gtk:expression-value-type expression)))
      (is (string= "string"
                   (g:value-string (gtk:constant-expression-value expression))))

      (is (string= "string" (gtk:expression-evaluate expression nil)))

)))


;;;     gtk_expression_watch
;;;     gtk_expression_bind
;;;     gtk_expression_watch_ref
;;;     gtk_expression_watch_unref
;;;     gtk_expression_watch_evaluate
;;;     gtk_expression_watch_unwatch

;;;     gtk_property_expression_new

;; TODO: What is the correct usage of EXPRESSION?

(test gtk-property-expression-new.1
  (cffi:with-foreign-object (expression 'gtk:expression)
    (let ((label (gtk:label-new "label")))
      (setf expression
            (gtk:property-expression-new "GtkLabel" nil "label"))
      (is (eq (g:gtype "gchararray")
              (gtk:expression-value-type expression)))
      (is (string= "label"
                   (gtk:expression-evaluate expression label)))
      (is-false (gtk:expression-unref expression)))))

;; TODO: What is the correct usage of EXPRESSION?

(test gtk-property-expression-new.2
  (cffi:with-foreign-object (expression 'gtk:expression)
    (let ((label (gtk:label-new "label")))
      (setf expression
            (gtk:property-expression-new "GtkLabel" nil "justify"))
      (is (eq (g:gtype "GtkJustification")
              (gtk:expression-value-type expression)))
      (is (eq :left
              (gtk:expression-evaluate expression label)))
      (is-false (gtk:expression-unref expression)))))

;; TODO: What is wrong with this example and the implementation!?

#+nil
(test gtk-property-expression-new.3
  (cffi:with-foreign-objects ((expression1 'gtk:expression)
                              (expression2 'gtk:expression))

    (let ((button (gtk:button-new-with-label "button")))

      (setf expression1
            (gtk:property-expression-new "GtkButton" nil "child"))
      (setf expression2
            (gtk:property-expression-new "GtkLabel" expression1 "label"))

      (is (eq (g:gtype "GtkWidget") (gtk:expression-value-type expression1)))
      (is (eq (g:gtype "gchararray") (gtk:expression-value-type expression2)))

;      (is-false (gtk:expression-evaluate expression1 nil))
;      (is-false (gtk:expression-evaluate expression2 nil))

      ;; FIXME: We get the :LEFT value. Why?!
;      (is-false (gtk:expression-evaluate expression1 button))
      (is (eq :left (gtk:expression-evaluate expression2 button)))
;
;      (is-false (gtk:expression-unref expression2))
    )))

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
              (gtk:expression-value-type (gtk:value-expression gvalue2)))))))

;;;     gtk_value_take_expression

;;;     gtk_value_dup_expression

;;;     gtk_param_spec_expression

;;; --- 2023-9-27 --------------------------------------------------------------

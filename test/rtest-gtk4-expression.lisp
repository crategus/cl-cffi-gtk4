(in-package :gtk-test)

(def-suite gtk-expression :in gtk-list-model-support)
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

;;;     GtkParamSpecExpression                              not implemented

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
  (gobject:with-value (gvalue "gchararray")
    (let ((expression (gtk:constant-expression-new-for-value gvalue)))
      (is (eq (g:gtype "gchararray")
              (gtk:expression-value-type expression)))
      (is-true (gtk:expression-is-static expression))
      (is-false (gtk:expression-unref expression)))))

;;;     gtk_expression_evaluate

(test gtk-expression-evaluate/evaluate-value
  (gobject:with-values ((gvalue "gchararray" "string") gvalue1)
    ;; Check value of gvalue
    (is (string= "string" (g:value-get gvalue)))
    ;; Initialize a constant expression
    (let ((expression (gtk:constant-expression-new-for-value gvalue)))
      ;; Check gtype and the value of the expression
      (is (eq (g:gtype "gchararray") (gtk:expression-value-type expression)))
      (is (string= "string"
                   (g:value-get (gtk:constant-expression-value expression))))
      ;; Evaluate the expression
      (is-true (gtk:expression-evaluate expression nil gvalue1))
      (is (string= "string" (g:value-get gvalue1)))
      (is (string= "string" (gtk:expression-evaluate-value expression nil)))
      (is-false (gtk:expression-unref expression)))))

;;;     gtk_expression_bind

(test gtk-expression-bind
  (let ((target (gtk:label-new "target"))
        (source (gtK:label-new "source"))
        (expression (gtk:property-expression-new "GtkLabel" nil "label"))
        (watch nil))
    ;; Check TARGET and SOURCE values
    (is (string= "target" (gtk:expression-evaluate-value expression target)))
    (is (string= "source" (gtk:expression-evaluate-value expression source)))
    ;; Bind TARGET to SOURCE
    (is (typep (setf watch
                     (gtk:expression-bind expression target "label" source))
               'gtk:expression-watch))
    ;; TARGET value has changed
    (is (string= "source" (gtk:expression-evaluate-value expression target)))
    (is (string= "source" (gtk:expression-evaluate-value expression source)))
    ;; Set new SOURCE value
    (is (string= "value" (setf (gtk:label-label source) "value")))
    (is (string= "value" (gtk:expression-evaluate-value expression target)))
    (is (string= "value" (gtk:expression-evaluate-value expression source)))
    ;; Check memory management
    ;; TODO: It is important for a correct memory management to call
    ;; gtk:expression-watch-unwatch. Document this.
    (is-false (gtk:expression-watch-unwatch watch))
    (is (= 1 (g:object-ref-count target)))
    (is (= 1 (g:object-ref-count source)))
    (is-false (gtk:expression-unref expression))))

;;;     gtk_expression_watch

(test gtk-expression-watch
  (let ((object (gtk:label-new "object"))
        (expression (gtk:property-expression-new "GtkLabel" nil "label"))
        (watch nil)
        (msg nil))
    (is (typep (setf watch
                     (gtk:expression-watch expression object
                             (lambda ()
                               (push "EXPRESSION-NOTIFY" msg))))
               'gtk:expression-watch))
    ;; First change of label
    (is (string= "first"
                 (setf (gtk:label-label object) "first")))
    (is (string= "first" (gtk:expression-evaluate-value expression object)))
    (is (equal '("EXPRESSION-NOTIFY") msg))
    ;; Second change of label
    (is (string= "second"
                 (setf (gtk:label-label object) "second")))
    (is (string= "second" (gtk:expression-evaluate-value expression object)))
    (is (equal '("EXPRESSION-NOTIFY" "EXPRESSION-NOTIFY") msg))
    ;; Check memory management
    (is-false (gtk:expression-watch-unwatch watch))
    (is (= 1 (g:object-ref-count object)))
    (is-false (gtk:expression-unref expression))))

;;;     gtk_expression_watch_ref
;;;     gtk_expression_watch_unref

;;;     gtk_expression_watch_evaluate
;;;     gtk_expression_watch_unwatch

(test gtk-expression-watch-evaluate
  (let ((object (gtk:label-new "object"))
        (expression (gtk:property-expression-new "GtkLabel" nil "label"))
        (watch nil)
        (msg nil))
    (is (typep (setf watch
                     (gtk:expression-watch expression object
                             (lambda ()
                               (push "EXPRESSION-NOTIFY" msg))))
               'gtk:expression-watch))
    ;; Change label
    (is (string= "first"
                 (setf (gtk:label-label object) "first")))
    (is (string= "first" (gtk:expression-evaluate-value expression object)))
    (is (string= "first" (gtk:expression-watch-evaluate-value watch)))
    (is (equal '("EXPRESSION-NOTIFY") msg))

    (is-false (gtk:expression-watch-unwatch watch))

    ;; Change label
    (is (string= "second"
                 (setf (gtk:label-label object) "second")))
    (is (string= "second" (gtk:expression-evaluate-value expression object)))
;; FIXME: Can crash after serveral runs of the testsuite, but never on the first
;; runs of the testsuite. Seems to be a problem with the memory management of
;; the boxed type for WATCH. Why?
;    (is-false (gtk:expression-watch-evaluate-value watch))
    (is (equal '("EXPRESSION-NOTIFY") msg))
    ;; Check memory management
    (is (= 1 (g:object-ref-count object)))
    (is-false (gtk:expression-unref expression))))

;;;     gtk_property_expression_new
;;;     gtk_property_expression_get_expression
;;;     gtk_property_expression_get_pspec

(test gtk-property-expression-new.1
  (gobject:with-value (gvalue)
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
  (gobject:with-value (gvalue)
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

;; TODO: Check again the memory management. We finish with a strong reference
;; to a GtkLabel widget. Can we remove the strong reference?

(test gtk-property-expression-new.3
  (let* ((expr1 (gtk:property-expression-new "GtkButton" nil "child"))
         (expr2 (gtk:property-expression-new "GtkLabel" expr1 "label"))
         (button (gtk:button-new-with-label "button"))
         (label nil) (child nil))
    ;; Get the child label from BUTTON for further checks
    (is (typep (setf child (gtk:button-child button)) 'gtk:label))
    (is (= 2 (g:object-ref-count child)))
    ;; Check EXPR1
    (is (eq (g:gtype "GtkWidget") (gtk:expression-value-type expr1)))
    (is-false (gtk:property-expression-expression expr1))
    (is (eq (g:gtype "GParamObject")
            (g:param-spec-type (gtk:property-expression-pspec expr1))))
    ;; Check EXPR2
    (is (eq (g:gtype "gchararray") (gtk:expression-value-type expr2)))
    (is (cffi:pointer-eq expr1
                         (gtk:property-expression-expression expr2)))
    (is (eq (g:gtype "GParamString")
            (g:param-spec-type (gtk:property-expression-pspec expr2))))
    ;; Evaluate EXPR1 and EXPR2 for NIL: Result is NIL
    (is-false (gtk:expression-evaluate-value expr1 nil))
    (is-false (gtk:expression-evaluate-value expr2 nil))
    ;; Evaluate EXPR1 for BUTTON: Result is the child label
    (is (eq (g:gtype "GtkLabel")
            (g:type-from-instance
              (setf label (gtk:expression-evaluate-value expr1 button)))))
    ;; Evaluate EXPR2 for BUTTON: Result is the text of the child label
    (is (string= "button" (gtk:expression-evaluate-value expr2 button)))
    ;; Check memory management
    ;; EXPR1 is a resource associated to EXPR2 and destroyed with EXPR2
    (is-false (gtk:expression-unref expr2))
    (is-false (setf (gtk:button-child button) nil))
    (is (= 1 (g:object-ref-count child)))
    (is (= 1 (g:object-ref-count label)))
    (is (= 1 (g:object-ref-count button)))))

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
    (is (= 100 (g:value-get (gtk:constant-expression-value expr))))
    (is-false (gtk:expression-unref expr))))

;;;     gtk_constant_expression_new_for_value
;;;     gtk_constant_expression_get_value

(test gtk-constant-expression-new-for-value
  (gobject:with-value (gvalue "gchararray" "string")
    (let ((expression (gtk:constant-expression-new-for-value gvalue)))
      (is (eq (g:gtype "gchararray") (gtk:expression-value-type expression)))
      (is (string= "string"
                   (g:value-get (gtk:constant-expression-value expression))))
      (is-false (gtk:expression-unref expression)))))

;;;     gtk_object_expression_new
;;;     gtk_object_expression_get_object

(test gtk-object-expression-new/object
  (let* ((label (gtk:label-new "text"))
         (expr (gtk:object-expression-new label))
         (widget nil))
    ;; Check expression
    (is (eq (g:gtype "GtkLabel")
            (gtk:expression-value-type expr)))
    (is-false (gtk:expression-is-static expr))
    ;; Evaluate expression
    (is (eq (g:gtype "GtkLabel")
            (g:type-from-instance
                (setf widget
                      (gtk:expression-evaluate-value expr nil)))))
    (is (eq widget label))
    ;; Check memory management
    (is-false (gtk:expression-unref expr))
    (is (= 1 (g:object-ref-count label)))
    (is (= 1 (g:object-ref-count widget)))))

;;;     gtk_closure_expression_new
;;;     gtk_cclosure_expression_new

;;;     gtk_value_get_expression
;;;     gtk_value_set_expression

(test gtk-value-expression
  (gobject:with-values ((gvalue1 "gchararray" "string")
                        (gvalue2 "GtkExpression"))
    (let ((expression (gtk:constant-expression-new-for-value gvalue1)))
      (setf (gtk:value-expression gvalue2) expression)
      (is (eq (g:gtype "gchararray")
              (gtk:expression-value-type (gtk:value-expression gvalue2))))
      (is-false (gtk:expression-unref expression)))))

;;;     gtk_value_take_expression                           not implemented
;;;     gtk_value_dup_expression                            not implemented

;;;     gtk_param_spec_expression                           not implemented

;;; 2024-11-29

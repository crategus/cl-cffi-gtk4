(in-package :gtk-test)

(def-suite gtk-check-button :in gtk-suite)
(in-suite gtk-check-button)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkCheckButton

(test gtk-check-button-class
  ;; Type check
  (is (g:type-is-object "GtkCheckButton"))
  ;; Check the registered name
  (is (eq 'gtk:check-button
          (gobject:symbol-for-gtype "GtkCheckButton")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkCheckButton")
          (g:gtype (cffi:foreign-funcall "gtk_check_button_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkCheckButton")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkCheckButton")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkActionable")
             (list-interfaces "GtkCheckButton")))
  ;; Check the properties
  (is (equal '("action-name" "action-target" "active" "group" "inconsistent"
               "label" "use-underline")
             (list-properties "GtkCheckButton")))
  ;; Check the signals
  (is (equal '("activate" "toggled")
             (list-signals "GtkCheckButton")))
  ;; CSS information
  (is (string= "checkbutton"
               (gtk:widget-class-css-name "GtkCheckButton")))
  (is (string=
"checkbutton:dir(ltr)
"
               (gtk:style-context-to-string
                   (gtk:widget-style-context (make-instance 'gtk:check-button))
                   :none)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkCheckButton" GTK-CHECK-BUTTON
                       (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                        ("GtkAccessible" "GtkActionable" "GtkBuildable"
                         "GtkConstraintTarget")
                        :TYPE-INITIALIZER "gtk_check_button_get_type")
                       ((ACTIVE GTK-CHECK-BUTTON-ACTIVE "active" "gboolean" T
                         T)
                        (GROUP GTK-CHECK-BUTTON-GROUP "group" "GtkCheckButton"
                         NIL T)
                        (INCONSISTENT GTK-CHECK-BUTTON-INCONSISTENT
                         "inconsistent" "gboolean" T T)
                        (LABEL GTK-CHECK-BUTTON-LABEL "label" "gchararray" T T)
                        (USE-UNDERLINE GTK-CHECK-BUTTON-USE-UNDERLINE
                         "use-underline" "gboolean" T T)))
             (gobject:get-g-type-definition "GtkCheckButton"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-check-button-properties
  (let ((group (make-instance 'gtk:check-button))
        (button (make-instance 'gtk:check-button)))
    ;; active
    (is-true (setf (gtk:check-button-active button) t))
    (is-true (gtk:check-button-active button))
    ;; child availabe since 4.8
    ;; group
    (is (typep (setf (gtk:check-button-group button) group) 'gtk:check-button))
    ;; group is not readable
    (signals (error) (gtk:check-button-group button))
    ;; inconsistent
    (is-true (setf (gtk:check-button-inconsistent button) t))
    (is-true (gtk:check-button-inconsistent button))
    ;; label
    (is (string= "label" (setf (gtk:check-button-label button) "label")))
    (is (string= "label" (gtk:check-button-label button)))
    ;; use-underline
    (is-true (setf (gtk:check-button-use-underline button) t))
    (is-true (gtk:check-button-use-underline button))))

;;; --- Signals ----------------------------------------------------------------

;;;     activate                                           Since 4.2
;;;     toggled

;;; --- Functions --------------------------------------------------------------

;;;     gtk_check_button_new
;;;     gtk_check_button_new_with_label
;;;     gtk_check_button_new_with_mnemonic

(test gtk-check-button-new
  (let (button)
    (is (typep (setf button (gtk:check-button-new)) 'gtk:check-button))
    (is-false (gtk:check-button-label button))
    (is (typep (setf button (gtk:check-button-new-with-label "label"))
               'gtk:check-button))
    (is (string= "label" (gtk:check-button-label button)))
    (is (typep (setf button (gtk:check-button-new-with-mnemonic "_label"))
               'gtk:check-button))
    (is (string= "_label" (gtk:check-button-label button)))
    (is-true (gtk:check-button-use-underline button))))

;;; --- 2023-3-20 --------------------------------------------------------------

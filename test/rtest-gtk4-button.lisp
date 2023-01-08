(in-package :gtk-test)

(def-suite gtk-button :in gtk-suite)
(in-suite gtk-button)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkButton

(test button-class
  ;; Type check
  (is (g:type-is-object "GtkButton"))
  ;; Check the registered name
  (is (eq 'gtk:button
          (gobject:symbol-for-gtype "GtkButton")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkButton")
          (g:gtype (foreign-funcall "gtk_button_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkButton")))
  ;; Check the children
  (is (equal '("GtkLinkButton" "GtkLockButton" "GtkToggleButton")
             (list-children "GtkButton")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkActionable")
             (list-interfaces "GtkButton")))
  ;; Check the class properties
  (is (equal '("action-name" "action-target" "child" "has-frame" "icon-name"
               "label" "use-underline")
             (list-properties "GtkButton")))
  ;; Check the signals
  (is (equal '("activate" "clicked")
             (list-signals "GtkButton")))
  ;; CSS information
  (is (string="button"
               (gtk:widget-class-css-name "GtkButton")))
  (is (string=
"button:dir(ltr)
"
               (gtk:style-context-to-string
                   (gtk:widget-style-context (make-instance 'gtk:button))
                   :none)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkButton" GTK-BUTTON
                       (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                        ("GtkAccessible" "GtkActionable" "GtkBuildable"
                         "GtkConstraintTarget")
                        :TYPE-INITIALIZER "gtk_button_get_type")
                       ((CHILD GTK-BUTTON-CHILD "child" "GtkWidget" T T)
                        (HAS-FRAME GTK-BUTTON-HAS-FRAME "has-frame" "gboolean"
                         T T)
                        (ICON-NAME GTK-BUTTON-ICON-NAME "icon-name"
                         "gchararray" T T)
                        (LABEL GTK-BUTTON-LABEL "label" "gchararray" T T)
                        (USE-UNDERLINE GTK-BUTTON-USE-UNDERLINE "use-underline"
                         "gboolean" T T)))
             (gobject:get-g-type-definition "GtkButton"))))

;;; --- Properties -------------------------------------------------------------

(test button-properties
  (let ((button (make-instance 'gtk:button)))
    (is-false (gtk:button-child button))
    (is-true  (gtk:button-has-frame button))
    (is-false (gtk:button-icon-name button))
    (is-false (gtk:button-label button))
    (is-false (gtk:button-use-underline button))))

;;; --- Signals ----------------------------------------------------------------

;;;     activate
;;;     clicked

;;; --- Functions --------------------------------------------------------------

;;;     gtk_button_new

(test button-new
  (let ((button (gtk:button-new)))
    (is-false (gtk:button-child button))
    (is-true  (gtk:button-has-frame button))
    (is-false (gtk:button-icon-name button))
    (is-false (gtk:button-label button))
    (is-false (gtk:button-use-underline button))))

;;;     gtk_button_new_from_icon_name

(test button-new-from-icon-name
  (let ((button (gtk:button-new-from-icon-name "battery")))
    (is (eq (g:gtype "GtkImage") (g:object-type (gtk:button-child button))))
    (is-true  (gtk:button-has-frame button))
    (is (string= "battery" (gtk:button-icon-name button)))
    (is-false (gtk:button-label button))
    (is-false (gtk:button-use-underline button))))

;;;     gtk_button_new_with_label

(test button-new-with-label
  (let ((button (gtk:button-new-with-label "battery")))
    (is (eq (g:gtype "GtkLabel") (g:object-type (gtk:button-child button))))
    (is-true  (gtk:button-has-frame button))
    (is-false (gtk:button-icon-name button))
    (is (string= "battery" (gtk:button-label button)))
    (is-false (gtk:button-use-underline button))))

;;;     gtk_button_new_with_mnemonic

(test button-new-with-mnemonic
  (let ((button (gtk:button-new-with-mnemonic "_battery")))
    (is (eq (g:gtype "GtkLabel") (g:object-type (gtk:button-child button))))
    (is-true  (gtk:button-has-frame button))
    (is-false (gtk:button-icon-name button))
    (is (string= "_battery" (gtk:button-label button)))
    (is-true (gtk:button-use-underline button))))

;;; 2022-11-11

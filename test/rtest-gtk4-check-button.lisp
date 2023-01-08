(in-package :gtk-test)

(def-suite gtk-check-button :in gtk-suite)
(in-suite gtk-check-button)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkCheckButton

(test check-button-class
  ;; Type check
  (is (g:type-is-object "GtkCheckButton"))
  ;; Check the registered name
  (is (eq 'gtk:check-button
          (gobject:symbol-for-gtype "GtkCheckButton")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkCheckButton")
          (g:gtype (foreign-funcall "gtk_check_button_get_type" :size))))
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
  check:dir(ltr)
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

;;;     active
;;;     child                                              Since 4.8
;;;     group
;;;     inconsistent
;;;     label
;;;     use-underline

;;; --- Signals ----------------------------------------------------------------

;;;     activate                                           Since 4.2
;;;     toggled

;;; --- Functions --------------------------------------------------------------

;;;     gtk_check_button_new
;;;     gtk_check_button_new_with_label
;;;     gtk_check_button_new_with_mnemonic
;;;     gtk_check_button_set_group

;;; 2022-11-11

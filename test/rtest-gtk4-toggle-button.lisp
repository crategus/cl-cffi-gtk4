(in-package :gtk-test)

(def-suite gtk-toggle-button :in gtk-suite)
(in-suite gtk-toggle-button)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkToggleButton

(test toggle-button-class
  ;; Type check
  (is (g:type-is-object "GtkToggleButton"))
  ;; Check the registered name
  (is (eq 'gtk:toggle-button
          (glib:symbol-for-gtype "GtkToggleButton")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkToggleButton")
          (g:gtype (cffi:foreign-funcall "gtk_toggle_button_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkButton")
          (g:type-parent "GtkToggleButton")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkToggleButton")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkActionable")
             (list-interfaces "GtkToggleButton")))
  ;; Check the properties
  (is (equal '("active" "group")
             (list-properties "GtkToggleButton")))
  ;; Check the signals
  (is (equal '("toggled")
             (list-signals "GtkToggleButton")))
  ;; CSS name
  (is (string= "button"
               (gtk:widget-class-css-name "GtkToggleButton")))
  ;; CSS classes
  (is (equal '("toggle")
             (gtk:widget-css-classes (make-instance 'gtk:toggle-button))))
  ;; Accessible role
  (is (eq :toggle-button (gtk:widget-class-accessible-role "GtkToggleButton")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkToggleButton" GTK-TOGGLE-BUTTON
                       (:SUPERCLASS GTK-BUTTON :EXPORT T :INTERFACES
                        ("GtkAccessible" "GtkActionable" "GtkBuildable"
                         "GtkConstraintTarget")
                        :TYPE-INITIALIZER "gtk_toggle_button_get_type")
                       ((ACTIVE GTK-TOGGLE-BUTTON-ACTIVE "active" "gboolean" T
                         T)
                        (GROUP GTK-TOGGLE-BUTTON-GROUP "group"
                         "GtkToggleButton" NIL T)))
             (gobject:get-g-type-definition "GtkToggleButton"))))

;;; --- Properties -------------------------------------------------------------

;;;     active
;;;     group

;;; --- Signals ----------------------------------------------------------------

;;;     toggled

;;; --- Functions --------------------------------------------------------------

;;;     gtk_toggle_button_new
;;;     gtk_toggle_button_new_with_label
;;;     gtk_toggle_button_new_with_mnemonic

(test gtk-toggle-button-new
  (let (button)
    (is (typep (setf button
                     (gtk:toggle-button-new)) 'gtk:toggle-button))
    (is-false (gtk:button-label button))
    (is-false (gtk:toggle-button-active button))

    (is (typep (setf button
                     (gtk:toggle-button-new-with-label "label"))
               'gtk:toggle-button))
    (is (string= "label"(gtk:button-label button)))
    (is-false (gtk:toggle-button-active button))

    (is (typep (setf button
                     (gtk:toggle-button-new-with-mnemonic "_label"))
               'gtk:toggle-button))
    (is (string= "_label" (gtk:button-label button)))
    (is-false (gtk:toggle-button-active button))))

;;;     gtk_toggle_button_toggled

;;; --- 2023-5-29 --------------------------------------------------------------

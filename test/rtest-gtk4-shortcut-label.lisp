(in-package :gtk-test)

(def-suite gtk-shortcut-label :in gtk-suite)
(in-suite gtk-shortcut-label)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkShortcutLabel

(test gtk-shortcut-label-class
  ;; Type check
  (is (g:type-is-object "GtkShortcutLabel"))
  ;; Check the registered name
  (is (eq 'gtk:shortcut-label
          (glib:symbol-for-gtype "GtkShortcutLabel")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkShortcutLabel")
          (g:gtype (cffi:foreign-funcall "gtk_shortcut_label_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkShortcutLabel")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkShortcutLabel")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (list-interfaces "GtkShortcutLabel")))
  ;; Check the properties
  (is (equal '("accelerator" "disabled-text")
             (list-properties "GtkShortcutLabel")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkShortcutLabel")))
  ;; CSS name
  (is (string= "shortcut"
               (gtk:widget-class-css-name "GtkShortcutLabel")))
  ;; CSS classes
  (is (equal '()
             (gtk:widget-css-classes (make-instance 'gtk:shortcut-label))))
  ;; Accessible role
  (is (eq :group (gtk:widget-class-accessible-role "GtkShortcutLabel")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkShortcutLabel"
                                             GTK-SHORTCUT-LABEL
                               (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                                ("GtkAccessible" "GtkBuildable"
                                 "GtkConstraintTarget")
                                :TYPE-INITIALIZER
                                "gtk_shortcut_label_get_type")
                               ((ACCELERATOR GTK-SHORTCUT-LABEL-ACCELERATOR
                                 "accelerator" "gchararray" T T)
                                (DISABLED-TEXT GTK-SHORTCUT-LABEL-DISABLED-TEXT
                                 "disabled-text" "gchararray" T T)))
             (gobject:get-g-type-definition "GtkShortcutLabel"))))

;;; --- Properties -------------------------------------------------------------

;;;     accelerator
;;;     disabled-text

;;; --- Functions --------------------------------------------------------------

;;;     gtk_shortcut_label_new

;;; --- 2023-11-4 --------------------------------------------------------------

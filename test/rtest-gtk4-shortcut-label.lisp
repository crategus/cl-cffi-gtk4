(in-package :gtk-test)

(def-suite gtk-shortcut-label :in gtk-suite)
(in-suite gtk-shortcut-label)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkShortcutLabel

(test gtk-shortcut-label-class
  ;; Check type
  (is (g:type-is-object "GtkShortcutLabel"))
  ;; Check registered name
  (is (eq 'gtk:shortcut-label
          (glib:symbol-for-gtype "GtkShortcutLabel")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkShortcutLabel")
          (g:gtype (cffi:foreign-funcall "gtk_shortcut_label_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkShortcutLabel")))
  ;; Check children
  (is (equal '()
             (gtk-test:list-children "GtkShortcutLabel")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (gtk-test:list-interfaces "GtkShortcutLabel")))
  ;; Check properties
  (is (equal '("accelerator" "disabled-text")
             (gtk-test:list-properties "GtkShortcutLabel")))
  ;; Check signals
  (is (equal '()
             (gtk-test:list-signals "GtkShortcutLabel")))
  ;; Check CSS name
  (is (string= "shortcut"
               (gtk:widget-class-css-name "GtkShortcutLabel")))
  ;; Check accessible role
  (is (eq :group (gtk:widget-class-accessible-role "GtkShortcutLabel")))
  ;; Check class definition
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

;;; 2024-7-4

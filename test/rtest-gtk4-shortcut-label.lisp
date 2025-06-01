(in-package :gtk-test)

(def-suite gtk-shortcut-label :in gtk-shortcuts-widgets)
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
             (glib-test:list-children "GtkShortcutLabel")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (glib-test:list-interfaces "GtkShortcutLabel")))
  ;; Check properties
  (is (equal '("accelerator" "disabled-text")
             (glib-test:list-properties "GtkShortcutLabel")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkShortcutLabel")))
  ;; Check CSS name
  (is (string= "shortcut"
               (gtk:widget-class-css-name "GtkShortcutLabel")))
  ;; Check accessible role
  (is (eq :group (gtk:widget-class-accessible-role "GtkShortcutLabel")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkShortcutLabel" GTK:SHORTCUT-LABEL
                      (:SUPERCLASS GTK:WIDGET
                       :EXPORT T
                       :INTERFACES
                       ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
                       :TYPE-INITIALIZER "gtk_shortcut_label_get_type")
                      ((ACCELERATOR SHORTCUT-LABEL-ACCELERATOR
                        "accelerator" "gchararray" T T)
                       (DISABLED-TEXT SHORTCUT-LABEL-DISABLED-TEXT
                        "disabled-text" "gchararray" T T)))
             (gobject:get-gtype-definition "GtkShortcutLabel"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-shortcut-label-properties
  (let ((*gtk-warn-deprecated* nil))
    (glib-test:with-check-memory (label)
      (is (typep (setf label (make-instance 'gtk:shortcut-label))
                 'gtk:shortcut-label))
      (is-false (gtk:shortcut-label-accelerator label))
      (is-false (gtk:shortcut-label-disabled-text label)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_shortcut_label_new

(test gtk-shortcut-label-new
  (let ((*gtk-warn-deprecated* nil))
    (glib-test:with-check-memory (label)
      (is (typep (setf label (gtk:shortcut-label-new "<Control>a"))
                 'gtk:shortcut-label)))))

;;; 2025-05-15

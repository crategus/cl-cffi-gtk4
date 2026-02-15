(in-package :gtk-test)

(def-suite gtk-combo-box-text :in gtk-deprecated)
(in-suite gtk-combo-box-text)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkComboBoxText

(test gtk-combo-box-text-class
  ;; Check type
  (is (g:type-is-object "GtkComboBoxText"))
  ;; Check registered name
  (is (eq 'gtk:combo-box-text
          (glib:symbol-for-gtype "GtkComboBoxText")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkComboBoxText")
          (g:gtype (cffi:foreign-funcall "gtk_combo_box_text_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkComboBox")
          (g:type-parent "GtkComboBoxText")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkComboBoxText")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkCellLayout" "GtkCellEditable")
             (glib-test:list-interfaces "GtkComboBoxText")))
  ;; Check properties
  (is (equal '()
             (glib-test:list-properties "GtkComboBoxText")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkComboBoxText")))
  ;; Check CSS name
  (is (string= "combobox"
               (gtk:widget-class-css-name "GtkComboBoxText")))
  ;; Check accessible role
  (is (eq :COMBO-BOX (gtk:widget-class-accessible-role "GtkComboBoxText")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkComboBoxText" GTK:COMBO-BOX-TEXT
                      (:SUPERCLASS GTK:COMBO-BOX
                       :EXPORT T
                       :INTERFACES
                       ("GtkAccessible" "GtkBuildable" "GtkCellEditable"
                        "GtkCellLayout" "GtkConstraintTarget")
                       :TYPE-INITIALIZER "gtk_combo_box_text_get_type")
                      NIL)
             (gobject:get-gtype-definition "GtkComboBoxText"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_combo_box_text_new

(test gtk-combo-box-text-new
  (let ((gtk-init:*warn-deprecated* nil))
    (glib-test:with-check-memory ()
      (is (typep (gtk:combo-box-text-new) 'gtk:combo-box-text)))))

;;;     gtk_combo_box_text_new_with_entry

(test gtk-combo-box-text-new-with-entry.1
  (let ((gtk-init:*warn-deprecated* nil))
    (glib-test:with-check-memory ()
      (is (typep (gtk:combo-box-text-new-with-entry) 'gtk:combo-box-text)))))

(test gtk-combo-box-text-new-with-entry.2
  (when *first-run-testsuite*
    (let ((gtk-init:*warn-deprecated* nil))
      (glib-test:with-check-memory (combo :strong 1)
        (is (typep (setf combo
                         (gtk:combo-box-text-new-with-entry))
                   'gtk:combo-box-text))
        (is (typep (gtk:combo-box-child combo) 'gtk:entry))))))

;;;     gtk_combo_box_text_append
;;;     gtk_combo_box_text_prepend
;;;     gtk_combo_box_text_insert

;;;     gtk_combo_box_text_append_text
;;;     gtk_combo_box_text_prepend_text
;;;     gtk_combo_box_text_insert_text

;;;     gtk_combo_box_text_remove
;;;     gtk_combo_box_text_remove_all

;;;     gtk_combo_box_text_get_active_text

(test gtk-combo-box-text-active-text
  (when *first-run-testsuite*
    (let ((gtk-init:*warn-deprecated* nil))
      (glib-test:with-check-memory (combo (entry 2) :strong 1)
        (setf combo (gtk:combo-box-text-new-with-entry))
        (setf entry (gtk:combo-box-child combo))
        (is (string= "text" (setf (gtk:editable-text entry) "text")))
        (is (string= "text" (gtk:combo-box-text-active-text combo)))))))

;;; 2024-12-24

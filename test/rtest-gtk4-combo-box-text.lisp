(in-package :gtk-test)

(def-suite gtk-combo-box-text :in gtk-suite)
(in-suite gtk-combo-box-text)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkComboBoxText

(test gtk-combo-box-text-class
  ;; Type check
  (is (g:type-is-object "GtkComboBoxText"))
  ;; Check the registered name
  (is (eq 'gtk:combo-box-text
          (glib:symbol-for-gtype "GtkComboBoxText")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkComboBoxText")
          (g:gtype (cffi:foreign-funcall "gtk_combo_box_text_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkComboBox")
          (g:type-parent "GtkComboBoxText")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkComboBoxText")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget" 
               "GtkCellLayout" "GtkCellEditable")
             (list-interfaces "GtkComboBoxText")))
  ;; Check the properties
  (is (equal '()
             (list-properties "GtkComboBoxText")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkComboBoxText")))
  ;; CSS name
  (is (string= "combobox"
               (gtk:widget-class-css-name "GtkComboBoxText")))
  ;; Accessible role
  (is (eq :COMBO-BOX (gtk:widget-class-accessible-role "GtkComboBoxText")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkComboBoxText" 
                                             GTK-COMBO-BOX-TEXT
                               (:SUPERCLASS GTK-COMBO-BOX :EXPORT T :INTERFACES
                                ("GtkAccessible" "GtkBuildable"
                                 "GtkCellEditable" "GtkCellLayout"
                                 "GtkConstraintTarget")
                                :TYPE-INITIALIZER
                                "gtk_combo_box_text_get_type")
                               NIL)
             (gobject:get-g-type-definition "GtkComboBoxText"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_combo_box_text_new
;;;     gtk_combo_box_text_new_with_entry
;;;     gtk_combo_box_text_append
;;;     gtk_combo_box_text_prepend
;;;     gtk_combo_box_text_insert
;;;     gtk_combo_box_text_append_text
;;;     gtk_combo_box_text_prepend_text
;;;     gtk_combo_box_text_insert_text
;;;     gtk_combo_box_text_remove
;;;     gtk_combo_box_text_remove_all
;;;     gtk_combo_box_text_get_active_text

;;; --- 2023-9-1 ---------------------------------------------------------------

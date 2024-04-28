(in-package :gtk-test)

(def-suite gtk-combo-box :in gtk-suite)
(in-suite gtk-combo-box)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkSensitivityType

(test gtk-sensitivity-type
  ;; Check type
  (is (g:type-is-enum "GtkSensitivityType"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkSensitivityType")
          (g:gtype (cffi:foreign-funcall "gtk_sensitivity_type_get_type"
                                         :size))))
  ;; Check registered name
  (is (eq 'gtk:sensitivity-type
          (glib:symbol-for-gtype "GtkSensitivityType")))
  ;; Check names
  (is (equal '("GTK_SENSITIVITY_AUTO" "GTK_SENSITIVITY_ON"
               "GTK_SENSITIVITY_OFF")
             (list-enum-item-name "GtkSensitivityType")))
  ;; Check values
  (is (equal '(0 1 2)
             (list-enum-item-value "GtkSensitivityType")))
  ;; Check nick names
  (is (equal '("auto" "on" "off")
             (list-enum-item-nick "GtkSensitivityType")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkSensitivityType"
                                     GTK-SENSITIVITY-TYPE
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "gtk_sensitivity_type_get_type")
                                     (:AUTO 0)
                                     (:ON 1)
                                     (:OFF 2))
             (gobject:get-g-type-definition "GtkSensitivityType"))))

;;;     GtkComboBox

(test gtk-combo-box-class
  ;; Check type
  (is (g:type-is-object "GtkComboBox"))
  ;; Check registered name
  (is (eq 'gtk:combo-box
          (glib:symbol-for-gtype "GtkComboBox")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkComboBox")
          (g:gtype (cffi:foreign-funcall "gtk_combo_box_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget") (g:type-parent "GtkComboBox")))
  ;; Check children
  (is (equal '("GtkComboBoxText")
             (list-children "GtkComboBox")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkCellLayout" "GtkCellEditable")
             (list-interfaces "GtkComboBox")))
  ;; Check class properties
  (is (equal '("active" "active-id" "button-sensitivity" "child"
               "editing-canceled" "entry-text-column" "has-entry" "has-frame"
               "id-column" "model" "popup-fixed-width" "popup-shown")
             (list-properties "GtkComboBox")))
  ;; Check list of signals
  (is (equal '("activate" "changed" "format-entry-text" "move-active" "popdown"
               "popup")
             (list-signals "GtkComboBox")))
  ;; Check CSS information
  (is (string= "combobox"
               (gtk:widget-class-css-name "GtkComboBox")))
  ;; Check accessible role
  (is (eq :COMBO-BOX (gtk:widget-class-accessible-role "GtkComboBox")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkComboBox" GTK-COMBO-BOX
                       (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                        ("GtkAccessible" "GtkBuildable" "GtkCellEditable"
                         "GtkCellLayout" "GtkConstraintTarget")
                        :TYPE-INITIALIZER "gtk_combo_box_get_type")
                       ((ACTIVE GTK-COMBO-BOX-ACTIVE "active" "gint" T T)
                        (ACTIVE-ID GTK-COMBO-BOX-ACTIVE-ID "active-id"
                         "gchararray" T T)
                        (BUTTON-SENSITIVITY GTK-COMBO-BOX-BUTTON-SENSITIVITY
                         "button-sensitivity" "GtkSensitivityType" T T)
                        (CHILD GTK-COMBO-BOX-CHILD "child" "GtkWidget" T T)
                        (ENTRY-TEXT-COLUMN GTK-COMBO-BOX-ENTRY-TEXT-COLUMN
                         "entry-text-column" "gint" T T)
                        (HAS-ENTRY GTK-COMBO-BOX-HAS-ENTRY "has-entry"
                         "gboolean" T NIL)
                        (HAS-FRAME GTK-COMBO-BOX-HAS-FRAME "has-frame"
                         "gboolean" T T)
                        (ID-COLUMN GTK-COMBO-BOX-ID-COLUMN "id-column" "gint" T
                         T)
                        (MODEL GTK-COMBO-BOX-MODEL "model" "GtkTreeModel" T T)
                        (POPUP-FIXED-WIDTH GTK-COMBO-BOX-POPUP-FIXED-WIDTH
                         "popup-fixed-width" "gboolean" T T)
                        (POPUP-SHOWN GTK-COMBO-BOX-POPUP-SHOWN "popup-shown"
                         "gboolean" T NIL)))
             (gobject:get-g-type-definition "GtkComboBox"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-combo-box-properties
  (let ((gtk-init:*gtk-warn-deprecated* nil))
    (let ((combo (gtk:combo-box-new)))
      (is (= -1 (gtk:combo-box-active combo)))
      (is-false (gtk:combo-box-active-id combo))
      (is (eq :auto (gtk:combo-box-button-sensitivity combo)))
      (is (typep (gtk:combo-box-child combo) 'gtk:cell-view))
      (is (= -1 (gtk:combo-box-entry-text-column combo)))
      (is-false (gtk:combo-box-has-entry combo))
      (is-true (gtk:combo-box-has-frame combo))
      (is (= -1 (gtk:combo-box-id-column combo)))
      (is-false (gtk:combo-box-model combo))
      (is-true (gtk:combo-box-popup-fixed-width combo))
      (is-false (gtk:combo-box-popup-shown combo)))))

;;; --- Signals ----------------------------------------------------------------

;;;     activate                                           Since 4.6
;;;     changed
;;;     format-entry-text
;;;     move-active
;;;     popdown
;;;     popup

;;; --- Functions --------------------------------------------------------------

;;;     gtk_combo_box_new

(test gtk-combo-box-new
  (let ((gtk-init:*gtk-warn-deprecated* nil))
    (is (typep (gtk:combo-box-new) 'gtk:combo-box))))

;;;     gtk_combo_box_new_with_entry

(test gtk-combo-box-new-with-entry
  (let ((gtk-init:*gtk-warn-deprecated* nil))
    (is (typep (gtk:combo-box-new-with-entry) 'gtk:combo-box))))

;;;     gtk_combo_box_new_with_model

(test gtk-combo-box-new-with-model
  (let ((gtk-init:*gtk-warn-deprecated* nil))
    (let ((model (gtk:list-store-new "gchararray")))
      (is (typep (gtk:combo-box-new-with-model model) 'gtk:combo-box)))))

;;;     gtk_combo_box_new_with_model_and_entry

(test gtk-combo-box-new-with-model-and-entry
  (let ((gtk-init:*gtk-warn-deprecated* nil))
    (let ((model (gtk:list-store-new "gchararray")))
      (is (typep (gtk:combo-box-new-with-model-and-entry model)
                 'gtk:combo-box)))))

;;;     gtk_combo_box_get_active_iter
;;;     gtk_combo_box_set_active_iter
;;;     gtk_combo_box_popup
;;;     gtk_combo_box_popup_for_device
;;;     gtk_combo_box_popdown
;;;     gtk_combo_box_get_row_separator_func
;;;     gtk_combo_box_set_row_separator_func

;;; 2024-4-26

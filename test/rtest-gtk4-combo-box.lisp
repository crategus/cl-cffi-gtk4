(in-package :gtk-test)

(def-suite gtk-combo-box :in gtk-suite)
(in-suite gtk-combo-box)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkSensitivityType

(test gtk-sensitivity-type
  ;; Check the type
  (is (g:type-is-enum "GtkSensitivityType"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkSensitivityType")
          (g:gtype (cffi:foreign-funcall "gtk_sensitivity_type_get_type"
                                         :size))))
  ;; Check the registered name
  (is (eq 'gtk:sensitivity-type
          (glib:symbol-for-gtype "GtkSensitivityType")))
  ;; Check the names
  (is (equal '("GTK_SENSITIVITY_AUTO" "GTK_SENSITIVITY_ON"
               "GTK_SENSITIVITY_OFF")
             (list-enum-item-name "GtkSensitivityType")))
  ;; Check the values
  (is (equal '(0 1 2)
             (list-enum-item-value "GtkSensitivityType")))
  ;; Check the nick names
  (is (equal '("auto" "on" "off")
             (listenum-item-nick "GtkSensitivityType")))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GtkSensitivityType"
                             GTK-SENSITIVITY-TYPE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_sensitivity_type_get_type")
                             (:AUTO 0)
                             (:ON 1)
                             (:OFF 2))
             (gobject:get-g-type-definition "GtkSensitivityType"))))

;;;     GtkComboBox

(test gtk-combo-box-class
  ;; Type check
  (is (g:type-is-object "GtkComboBox"))
  ;; Check the registered name
  (is (eq 'gtk:combo-box
          (glib:symbol-for-gtype "GtkComboBox")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkComboBox")
          (g:gtype (cffi:foreign-funcall "gtk_combo_box_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget") (g:type-parent "GtkComboBox")))
  ;; Check the children
  (is (equal '("GtkComboBoxText")
             (list-children "GtkComboBox")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkCellLayout" "GtkCellEditable")
             (list-interfaces "GtkComboBox")))
  ;; Check the class properties
  (is (equal '("active" "active-id" "button-sensitivity" "child"
               "editing-canceled" "entry-text-column" "has-entry" "has-frame"
               "id-column" "model" "popup-fixed-width" "popup-shown")
             (list-properties "GtkComboBox")))
  ;; Check the list of signals
  (is (equal '("activate" "changed" "format-entry-text" "move-active" "popdown"
               "popup")
             (list-signals "GtkComboBox")))
  ;; CSS information
  (is (string= "combobox"
               (gtk-widget-class-css-name "GtkComboBox")))
  (is (string=
"combobox:dir(ltr)
  box.horizontal.linked:dir(ltr)
    button.combo:dir(ltr)
      box.horizontal:dir(ltr)
        cellview:dir(ltr)
        arrow:dir(ltr)
  [popover.background.menu:dir(ltr)]
    contents:dir(ltr)
      stack:dir(ltr)
    arrow:dir(ltr)
"
               (gtk-style-context-to-string
                   (gtk-widget-style-context (make-instance 'gtk-combo-box))
                   :none)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkComboBox" GTK-COMBO-BOX
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

;;;     active
;;;     active-id
;;;     button-sensitivity
;;;     child
;;;     entry-text-column
;;;     has-entry
;;;     has-frame
;;;     id-column
;;;     model
;;;     popup-fixed-width
;;;     popup-shown

;;; --- Signals ----------------------------------------------------------------

;;;     activate                                           Since 4.6
;;;     changed
;;;     format-entry-text
;;;     move-active
;;;     popdown
;;;     popup

;;; --- Functions --------------------------------------------------------------

;;;     gtk_combo_box_new
;;;     gtk_combo_box_new_with_entry
;;;     gtk_combo_box_new_with_model
;;;     gtk_combo_box_new_with_model_and_entry
;;;     gtk_combo_box_get_active_iter
;;;     gtk_combo_box_set_active_iter
;;;     gtk_combo_box_popup
;;;     gtk_combo_box_popup_for_device
;;;     gtk_combo_box_popdown
;;;     gtk_combo_box_get_row_separator_func
;;;     gtk_combo_box_set_row_separator_func

;;; --- 2023-5-29 --------------------------------------------------------------

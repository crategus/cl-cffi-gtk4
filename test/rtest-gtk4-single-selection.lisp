(in-package :gtk-test)

(def-suite gtk-single-selection :in gtk-suite)
(in-suite gtk-single-selection)

;;; --- Types and Values -------------------------------------------------------

;;;     GTK_INVALID_LIST_POSITION

;;;     GtkSingleSelection

(test gtk-single-selection-class
  ;; Type check
  (is (g:type-is-object "GtkSingleSelection"))
  ;; Check the registered name
  (is (eq 'gtk:single-selection
          (glib:symbol-for-gtype "GtkSingleSelection")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkSingleSelection")
          (g:gtype (cffi:foreign-funcall "gtk_single_selection_get_type"
                                         :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkSingleSelection")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkSingleSelection")))
  ;; Check the interfaces
  (is (equal '("GListModel" "GtkSelectionModel")
             (list-interfaces "GtkSingleSelection")))
  ;; Check the properties
  (is (equal '("autoselect" "can-unselect" "item-type" "model" "n-items"
               "selected" "selected-item")
             (list-properties "GtkSingleSelection")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkSingleSelection")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkSingleSelection"
                                             GTK-SINGLE-SELECTION
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES
                                ("GListModel" "GtkSelectionModel")
                                :TYPE-INITIALIZER
                                "gtk_single_selection_get_type")
                               ((AUTOSELECT GTK-SINGLE-SELECTION-AUTOSELECT
                                 "autoselect" "gboolean" T T)
                                (CAN-UNSELECT GTK-SINGLE-SELECTION-CAN-UNSELECT
                                 "can-unselect" "gboolean" T T)
                                (ITEM-TYPE GTK-SINGLE-SELECTION-ITEM-TYPE
                                 "item-type" "GType" T NIL)
                                (MODEL GTK-SINGLE-SELECTION-MODEL "model"
                                 "GListModel" T T)
                                (N-ITEMS GTK-SINGLE-SELECTION-N-ITEMS "n-items"
                                 "guint" T NIL)
                                (SELECTED GTK-SINGLE-SELECTION-SELECTED
                                 "selected" "guint" T T)
                                (SELECTED-ITEM
                                 GTK-SINGLE-SELECTION-SELECTED-ITEM
                                 "selected-item" "GObject" T NIL)))
             (gobject:get-g-type-definition "GtkSingleSelection"))))

;;; --- Properties -------------------------------------------------------------

;;;     autoselect
;;;     can-unselect
;;;     item-type                                          Since 4.8
;;;     model
;;;     n-items                                            Since 4.8
;;;     selected
;;;     selected-item

;;; --- Functions --------------------------------------------------------------

;;;     gtk_single_selection_new
;;;     gtk_single_selection_get_model
;;;     gtk_single_selection_set_model
;;;     gtk_single_selection_get_selected
;;;     gtk_single_selection_set_selected
;;;     gtk_single_selection_get_selected_item
;;;     gtk_single_selection_get_autoselect
;;;     gtk_single_selection_set_autoselect
;;;     gtk_single_selection_get_can_unselect
;;;     gtk_single_selection_set_can_unselect

;;; --- 2023-8-10 --------------------------------------------------------------

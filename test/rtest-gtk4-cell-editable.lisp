(in-package :gtk-test)

(def-suite gtk-cell-editable :in gtk-suite)
(in-suite gtk-cell-editable)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkCellEditable

(test gtk-cell-editable-interface
  ;; Check type
  (is (g:type-is-interface "GtkCellEditable"))
  ;; Check registered name
  (is (eq 'gtk:cell-editable
          (glib:symbol-for-gtype "GtkCellEditable")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkCellEditable")
          (g:gtype (cffi:foreign-funcall "gtk_cell_editable_get_type" :size))))
  ;; Check interface properties
  (is (equal '("editing-canceled")
             (glib-test:list-interface-properties "GtkCellEditable")))
  ;; Check signals
  (is (equal '("editing-done" "remove-widget")
             (glib-test:list-signals "GtkCellEditable")))
  ;; Check interface definition
  (is (equal '(GOBJECT:DEFINE-GINTERFACE "GtkCellEditable" GTK:CELL-EDITABLE
                      (:EXPORT T
                       :TYPE-INITIALIZER "gtk_cell_editable_get_type")
                      (EDITING-CANCELED CELL-EDITABLE-EDITING-CANCELED
                       "editing-canceled" "gboolean" T T))
             (gobject:get-gtype-definition "GtkCellEditable"))))

;;; --- Properties -------------------------------------------------------------

;;;     editing-canceled

;;; --- Signals ----------------------------------------------------------------

;;;     editing-done
;;;     remove-widget

;;; --- Functions --------------------------------------------------------------

;;;     gtk_cell_editable_start_editing
;;;     gtk_cell_editable_editing_done
;;;     gtk_cell_editable_remove_widget

;;; 2024-9-19

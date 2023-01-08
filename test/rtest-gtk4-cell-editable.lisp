(in-package :gtk-test)

(def-suite gtk-cell-editable :in gtk-suite)
(in-suite gtk-cell-editable)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkCellEditable

(test cell-editable-interface
  ;; Type check
  (is (g:type-is-interface "GtkCellEditable"))
  ;; Check the registered name
  (is (eq 'gtk:cell-editable
          (gobject:symbol-for-gtype "GtkCellEditable")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkCellEditable")
          (g:gtype (foreign-funcall "gtk_cell_editable_get_type" :size))))
  ;; Get the names of the interface properties.
  (is (equal '("editing-canceled")
             (list-interface-properties "GtkCellEditable")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkCellEditable")))
  ;; Get the interface definition
  (is (equal '(DEFINE-G-INTERFACE "GtkCellEditable" GTK-CELL-EDITABLE
                    (:EXPORT T :TYPE-INITIALIZER "gtk_cell_editable_get_type")
                    (EDITING-CANCELED GTK-CELL-EDITABLE-EDITING-CANCELED
                     "editing-canceled" "gboolean" T T))
             (gobject:get-g-type-definition "GtkCellEditable"))))

;;; --- Properties -------------------------------------------------------------

;;;     editing-canceled

;;; --- Signals ----------------------------------------------------------------

;;;     editing-done
;;;     remove-widget

;;; --- Functions --------------------------------------------------------------

;;;     gtk_cell_editable_start_editing ()
;;;     gtk_cell_editable_editing_done ()
;;;     gtk_cell_editable_remove_widget ()

;;; 2022-11-10

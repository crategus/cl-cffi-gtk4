(in-package :gtk-test)

(def-suite gtk-cell-layout :in gtk-suite)
(in-suite gtk-cell-layout)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkCellLayout

(test gtk-cell-layout-interface
  ;; Type check
  (is (g:type-is-interface "GtkCellLayout"))
  ;; Check the registered name
  (is (eq 'gtk:cell-layout
          (glib:symbol-for-gtype "GtkCellLayout")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkCellLayout")
          (g:gtype (cffi:foreign-funcall "gtk_cell_layout_get_type" :size))))
  ;; Check the interface prerequisites
  (is (equal '("GObject")
             (list-interface-prerequisites "GtkCellLayout")))
  ;; Check the interface properties
  (is (equal '()
             (list-interface-properties "GtkCellLayout")))
  ;; Check the interface signals
  (is (equal '()
             (list-signals "GtkCellLayout")))
  ;; Get the interface definition
  (is (equal '(GOBJECT:DEFINE-G-INTERFACE "GtkCellLayout" GTK-CELL-LAYOUT
                            (:EXPORT T :TYPE-INITIALIZER
                             "gtk_cell_layout_get_type"))
             (gobject:get-g-type-definition "GtkCellLayout"))))

;;; --- Functions --------------------------------------------------------------

;;;     GtkCellLayoutDataFunc

;;;     gtk_cell_layout_pack_start
;;;     gtk_cell_layout_pack_end
;;;     gtk_cell_layout_get_area
;;;     gtk_cell_layout_get_cells
;;;     gtk_cell_layout_reorder
;;;     gtk_cell_layout_clear
;;;     gtk_cell_layout_set_attributes
;;;     gtk_cell_layout_add_attribute
;;;     gtk_cell_layout_set_cell_data_func
;;;     gtk_cell_layout_clear_attributes

;;; --- 2023-9-16 --------------------------------------------------------------


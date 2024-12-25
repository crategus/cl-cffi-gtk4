(in-package :gtk-test)

(def-suite gtk-cell-layout :in gtk-deprecated)
(in-suite gtk-cell-layout)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkCellLayout

(test gtk-cell-layout-interface
  ;; Check type
  (is (g:type-is-interface "GtkCellLayout"))
  ;; Check registered name
  (is (eq 'gtk:cell-layout
          (glib:symbol-for-gtype "GtkCellLayout")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkCellLayout")
          (g:gtype (cffi:foreign-funcall "gtk_cell_layout_get_type" :size))))
  ;; Check interface prerequisites
  (is (equal '("GObject")
             (glib-test:list-interface-prerequisites "GtkCellLayout")))
  ;; Check interface properties
  (is (equal '()
             (glib-test:list-interface-properties "GtkCellLayout")))
  ;; Check interface signals
  (is (equal '()
             (glib-test:list-signals "GtkCellLayout")))
  ;; Check interface definition
  (is (equal '(GOBJECT:DEFINE-GINTERFACE "GtkCellLayout" GTK:CELL-LAYOUT
                                         (:EXPORT T
                                          :TYPE-INITIALIZER
                                          "gtk_cell_layout_get_type"))
             (gobject:get-gtype-definition "GtkCellLayout"))))

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

;;; 2024-9-20

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

;;;     gtk_cell_layout_pack_start
;;;     gtk_cell_layout_pack_end
;;;     gtk_cell_layout_get_area
;;;     gtk_cell_layout_get_cells
;;;     gtk_cell_layout_clear

(test gtk-cell-layout-pack-start/end
  (let ((gtk-init:*warn-deprecated* nil))
    (glib-test:with-check-memory (area renderer1 renderer2)
      (is (typep (setf area
                       (gtk:cell-area-box-new)) 'gtk:cell-area-box))
      (is (typep (setf renderer1
                       (gtk:cell-renderer-text-new)) 'gtk:cell-renderer-text))
      (is (typep (setf renderer2
                       (gtk:cell-renderer-text-new)) 'gtk:cell-renderer-text))
      (is (eq area (gtk:cell-layout-area area)))
      (is-false (gtk:cell-layout-pack-start area renderer1))
      (is-false (gtk:cell-layout-pack-end area renderer2))
      (is (= 2 (length (gtk:cell-layout-cells area))))
      (is (every (lambda (x) (typep x 'gtk:cell-renderer-text))
                 (gtk:cell-layout-cells area)))
      ;; Remove references
      (is-false (gtk:cell-layout-clear area)))))

;;;     gtk_cell_layout_reorder

(test gtk-cell-layout-reorder
  (let ((gtk-init:*warn-deprecated* nil))
    (glib-test:with-check-memory (area renderer1 renderer2)
      (is (typep (setf area
                       (gtk:cell-area-box-new)) 'gtk:cell-area-box))
      (is (typep (setf renderer1
                       (gtk:cell-renderer-text-new)) 'gtk:cell-renderer-text))
      (is (typep (setf renderer2
                       (gtk:cell-renderer-pixbuf-new)) 'gtk:cell-renderer-pixbuf))
      (is-false (gtk:cell-layout-pack-end area renderer1))
      (is-false (gtk:cell-layout-pack-end area renderer2))
      (is (equal '(gtk:cell-renderer-text gtk:cell-renderer-pixbuf)
                 (mapcar (lambda (x) (type-of x))
                         (gtk:cell-layout-cells area))))
      (is-false (gtk:cell-layout-reorder area renderer2 0))
      (is (equal '(gtk:cell-renderer-pixbuf gtk:cell-renderer-text)
                 (mapcar (lambda (x) (type-of x))
                         (gtk:cell-layout-cells area))))
      ;; Remove references
      (is-false (gtk:cell-layout-clear area)))))

;;;     gtk_cell_layout_add_attribute

(test gtk-cell-layout-add-attribute
  (let ((gtk-init:*warn-deprecated* nil))
    (glib-test:with-check-memory (area renderer)
      (is (typep (setf area
                       (gtk:cell-area-box-new)) 'gtk:cell-area-box))
      (is (typep (setf renderer
                       (gtk:cell-renderer-text-new)) 'gtk:cell-renderer-text))
      (is-false (gtk:cell-layout-pack-end area renderer))
      (is-false (gtk:cell-layout-add-attribute area renderer "font" 0))
      ;; Remove references
      (is-false (gtk:cell-layout-clear area)))))

;;;     gtk_cell_layout_set_attributes
;;;     gtk_cell_layout_clear_attributes

(test gtk-cell-layout-set-attributes
  (let ((gtk-init:*warn-deprecated* nil))
    (glib-test:with-check-memory (area renderer)
      (is (typep (setf area
                       (gtk:cell-area-box-new)) 'gtk:cell-area-box))
      (is (typep (setf renderer
                       (gtk:cell-renderer-text-new)) 'gtk:cell-renderer-text))
      (is-false (gtk:cell-layout-pack-end area renderer))
      (is-false (gtk:cell-layout-set-attributes area renderer
                                                "font" 0
                                                "text" 1))
      (is-false (gtk:cell-layout-clear-attributes area renderer))
      ;; Remove references
      (is-false (gtk:cell-layout-clear area)))))

;;;     GtkCellLayoutDataFunc
;;;     gtk_cell_layout_set_cell_data_func

;;; 2026-01-17

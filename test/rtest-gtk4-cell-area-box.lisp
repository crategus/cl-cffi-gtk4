(in-package :gtk-test)

(def-suite gtk-cell-area-box :in gtk-deprecated)
(in-suite gtk-cell-area-box)

;;; ---  Types and Values ------------------------------------------------------

;;;     GtkCellAreaBox

(test gtk-cell-area-box-class
  ;; Check type
  (is (g:type-is-object "GtkCellAreaBox"))
  ;; Check registered name
  (is (eq 'gtk:cell-area-box
          (glib:symbol-for-gtype "GtkCellAreaBox")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkCellAreaBox")
          (g:gtype (cffi:foreign-funcall "gtk_cell_area_box_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkCellArea")
          (g:type-parent "GtkCellAreaBox")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkCellAreaBox")))
  ;; Check interfaces
  (is (equal '("GtkCellLayout" "GtkBuildable" "GtkOrientable")
             (glib-test:list-interfaces "GtkCellAreaBox")))
  ;; Check properties
  (is (equal '("orientation" "spacing")
             (glib-test:list-properties "GtkCellAreaBox")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkCellAreaBox")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkCellAreaBox" GTK:CELL-AREA-BOX
                      (:SUPERCLASS GTK:CELL-AREA
                       :EXPORT T
                       :INTERFACES
                       ("GtkBuildable" "GtkCellLayout" "GtkOrientable")
                       :TYPE-INITIALIZER "gtk_cell_area_box_get_type")
                      ((SPACING CELL-AREA-BOX-SPACING "spacing" "gint" T T)))
             (gobject:get-gtype-definition "GtkCellAreaBox"))))

;;; --- Properties -------------------------------------------------------------

;;;     spacing

(test gtk-cell-area-box-properties
  (let* ((gtk-init:*warn-deprecated* nil)
         (box (make-instance 'gtk:cell-area-box)))
    (is (= 0 (gtk:cell-area-box-spacing box)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_cell_area_box_new

(test gtk-cell-area-box-new
  (let ((gtk-init:*warn-deprecated* nil))
    (is (typep (gtk:cell-area-box-new) 'gtk:cell-area-box))))

;;;     gtk_cell_area_box_pack_start
;;;     gtk_cell_area_box_pack_end

;;; 2024-9-20

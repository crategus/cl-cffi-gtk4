(in-package :gtk-test)

(def-suite gtk-cell-area-box :in gtk-suite)
(in-suite gtk-cell-area-box)

;;; ---  Types and Values ------------------------------------------------------

;;;     GtkCellAreaBox

(test gtk-cell-area-box-class
  ;; Type check
  (is (g:type-is-object "GtkCellAreaBox"))
  ;; Check the registered name
  (is (eq 'gtk:cell-area-box
          (glib:symbol-for-gtype "GtkCellAreaBox")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkCellAreaBox")
          (g:gtype (cffi:foreign-funcall "gtk_cell_area_box_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkCellArea")
          (g:type-parent "GtkCellAreaBox")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkCellAreaBox")))
  ;; Check the interfaces
  (is (equal '("GtkCellLayout" "GtkBuildable" "GtkOrientable")
             (list-interfaces "GtkCellAreaBox")))
  ;; Check the properties
  (is (equal '("orientation" "spacing")
             (list-properties "GtkCellAreaBox")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkCellAreaBox")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkCellAreaBox" GTK-CELL-AREA-BOX
                               (:SUPERCLASS GTK-CELL-AREA :EXPORT T :INTERFACES
                                ("GtkBuildable" "GtkCellLayout"
                                 "GtkOrientable")
                                :TYPE-INITIALIZER "gtk_cell_area_box_get_type")
                               ((SPACING GTK-CELL-AREA-BOX-SPACING "spacing"
                                 "gint" T T)))
             (gobject:get-g-type-definition "GtkCellAreaBox"))))

;;; --- Properties -------------------------------------------------------------

;;;     spacing

(test gtk-cell-area-box-properties
  (let ((box (make-instance 'gtk:cell-area-box)))
    (is (= 0 (gtk:cell-area-box-spacing box)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_cell_area_box_new

(test gtk-cell-area-box-new
  (is (typep (gtk:cell-area-box-new) 'gtk:cell-area-box)))

;;;     gtk_cell_area_box_pack_start
;;;     gtk_cell_area_box_pack_end

;;; 2024-2-21

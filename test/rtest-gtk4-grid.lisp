(in-package :gtk-test)

(def-suite gtk-grid :in gtk-suite)
(in-suite gtk-grid)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkGrid

(test gtk-grid-class
  ;; Check type
  (is (g:type-is-object "GtkGrid"))
  ;; Check registered name
  (is (eq 'gtk:grid
          (glib:symbol-for-gtype "GtkGrid")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkGrid")
          (g:gtype (cffi:foreign-funcall "gtk_grid_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkGrid")))
  ;; Check children
  (is (equal '()
             (gtk-test:list-children "GtkGrid")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkOrientable")
             (gtk-test:list-interfaces "GtkGrid")))
  ;; Check properties
  (is (equal '("baseline-row" "column-homogeneous" "column-spacing"
               "orientation" "row-homogeneous" "row-spacing")
             (gtk-test:list-properties "GtkGrid")))
  ;; Check signals
  (is (equal '()
             (gtk-test:list-signals "GtkGrid")))
  ;; Check CSS name
  (is (string= "grid"
               (gtk:widget-class-css-name "GtkGrid")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkGrid" GTK-GRID
                       (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                        ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
                         "GtkOrientable")
                        :TYPE-INITIALIZER "gtk_grid_get_type")
                       ((BASELINE-ROW GTK-GRID-BASELINE-ROW "baseline-row"
                         "gint" T T)
                        (COLUMN-HOMOGENEOUS GTK-GRID-COLUMN-HOMOGENEOUS
                         "column-homogeneous" "gboolean" T T)
                        (COLUMN-SPACING GTK-GRID-COLUMN-SPACING
                         "column-spacing" "gint" T T)
                        (ROW-HOMOGENEOUS GTK-GRID-ROW-HOMOGENEOUS
                         "row-homogeneous" "gboolean" T T)
                        (ROW-SPACING GTK-GRID-ROW-SPACING "row-spacing" "gint"
                         T T)))
             (gobject:get-g-type-definition "GtkGrid"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-grid-properties
  (let ((grid (make-instance 'gtk:grid)))
    (is (= 0 (gtk:grid-baseline-row grid)))
    (is-false (gtk:grid-column-homogeneous grid))
    (is (= 0 (gtk:grid-column-spacing grid)))
    (is-false (gtk:grid-row-homogeneous grid))
    (is (= 0 (gtk:grid-row-spacing grid)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_grid_new

(test gtk-grid-new
  (is (typep (gtk:grid-new) 'gtk:grid)))

;;;     gtk_grid_attach
;;;     gtk_grid_attach_next_to
;;;     gtk_grid_remove
;;;     gtk_grid_get_child_at
;;;     gtk_grid_query_child
;;;     gtk_grid_insert_row
;;;     gtk_grid_insert_column
;;;     gtk_grid_remove_row
;;;     gtk_grid_remove_column
;;;     gtk_grid_insert_next_to
;;;     gtk_grid_get_row_baseline_position
;;;     gtk_grid_set_row_baseline_position

;;; 2024-4-12

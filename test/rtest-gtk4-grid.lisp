(in-package :gtk-test)

(def-suite gtk-grid :in gtk-suite)
(in-suite gtk-grid)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkGrid

(test grid-class
  ;; Type check
  (is (g:type-is-object "GtkGrid"))
  ;; Check the registered name
  (is (eq 'gtk:grid
          (gobject:symbol-for-gtype "GtkGrid")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkGrid")
          (g:gtype (cffi:foreign-funcall "gtk_grid_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkGrid")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkGrid")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkOrientable")
             (list-interfaces "GtkGrid")))
  ;; Check the properties
  (is (equal '("baseline-row" "column-homogeneous" "column-spacing"
               "orientation" "row-homogeneous" "row-spacing")
             (list-properties "GtkGrid")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkGrid")))
  ;; CSS information
  (is (string= "grid"
               (gtk:widget-class-css-name "GtkGrid")))
  (is (string=
"grid.horizontal:dir(ltr)
"
               (gtk:style-context-to-string
                   (gtk:widget-style-context (make-instance 'gtk:grid))
                   :none)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkGrid" GTK-GRID
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

;;;     baseline-row
;;;     column-homogeneous
;;;     column-spacing
;;;     row-homogeneous
;;;     row-spacing

;;; --- Functions --------------------------------------------------------------

;;;     gtk_grid_new
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

;;; 2022-11-11

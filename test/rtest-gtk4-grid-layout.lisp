(in-package :gtk-test)

(def-suite gtk-grid-layout :in gtk-suite)
(in-suite gtk-grid-layout)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkGridLayoutChild

(test gtk-grid-layout-child-class
  ;; Check type
  (is (g:type-is-object "GtkGridLayoutChild"))
  ;; Check registered name
  (is (eq 'gtk:grid-layout-child
          (glib:symbol-for-gtype "GtkGridLayoutChild")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkGridLayoutChild")
          (g:gtype (cffi:foreign-funcall "gtk_grid_layout_child_get_type"
                                         :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkLayoutChild")
          (g:type-parent "GtkGridLayoutChild")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkGridLayoutChild")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkGridLayoutChild")))
  ;; Check properties
  (is (equal '("column" "column-span" "row" "row-span")
             (glib-test:list-properties "GtkGridLayoutChild")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkGridLayoutChild")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkGridLayoutChild" GTK:GRID-LAYOUT-CHILD
                      (:SUPERCLASS GTK:LAYOUT-CHILD
                       :EXPORT T
                       :INTERFACES NIL
                       :TYPE-INITIALIZER "gtk_grid_layout_child_get_type")
                      ((COLUMN GRID-LAYOUT-CHILD-COLUMN "column" "gint" T T)
                       (COLUMN-SPAN GRID-LAYOUT-CHILD-COLUMN-SPAN
                        "column-span" "gint" T T)
                       (ROW GRID-LAYOUT-CHILD-ROW "row" "gint" T T)
                       (ROW-SPAN GRID-LAYOUT-CHILD-ROW-SPAN
                        "row-span" "gint" T T)))
             (gobject:get-gtype-definition "GtkGridLayoutChild"))))

;;; --- Properties -------------------------------------------------------------

;;;     column
;;;     column-span
;;;     row
;;;     row-span

;;; --- Types and Values -------------------------------------------------------

;;;     GtkGridLayout

(test gtk-grid-layout-class
  ;; Check type
  (is (g:type-is-object "GtkGridLayout"))
  ;; Check registered name
  (is (eq 'gtk:grid-layout
          (glib:symbol-for-gtype "GtkGridLayout")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkGridLayout")
          (g:gtype (cffi:foreign-funcall "gtk_grid_layout_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkLayoutManager")
          (g:type-parent "GtkGridLayout")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkGridLayout")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkGridLayout")))
  ;; Check properties
  (is (equal '("baseline-row" "column-homogeneous" "column-spacing"
               "row-homogeneous" "row-spacing")
             (glib-test:list-properties "GtkGridLayout")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkGridLayout")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkGridLayout" GTK:GRID-LAYOUT
                      (:SUPERCLASS GTK:LAYOUT-MANAGER
                       :EXPORT T
                       :INTERFACES NIL
                       :TYPE-INITIALIZER "gtk_grid_layout_get_type")
                      ((BASELINE-ROW GRID-LAYOUT-BASELINE-ROW
                        "baseline-row" "gint" T T)
                       (COLUMN-HOMOGENEOUS GRID-LAYOUT-COLUMN-HOMOGENEOUS
                        "column-homogeneous" "gboolean" T T)
                       (COLUMN-SPACING GRID-LAYOUT-COLUMN-SPACING
                        "column-spacing" "gint" T T)
                       (ROW-HOMOGENEOUS GRID-LAYOUT-ROW-HOMOGENEOUS
                        "row-homogeneous" "gboolean" T T)
                       (ROW-SPACING GRID-LAYOUT-ROW-SPACING
                        "row-spacing" "gint" T T)))
             (gobject:get-gtype-definition "GtkGridLayout"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-grid-layout-properties
  (let ((layout (make-instance 'gtk:grid-layout)))
    (is (= 0 (gtk:grid-layout-baseline-row layout)))
    (is-false (gtk:grid-layout-column-homogeneous layout))
    (is (= 0 (gtk:grid-layout-column-spacing layout)))
    (is-false (gtk:grid-layout-row-homogeneous layout))
    (is (= 0 (gtk:grid-layout-row-spacing layout)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_grid_layout_new

(test gtk-grid-layout-new
  (is (typep (gtk:grid-layout-new) 'gtk:grid-layout)))

;;;     gtk_grid_layout_set_row_baseline_position
;;;     gtk_grid_layout_get_row_baseline_position

(test gtk-grid-layout-row-baseline-position
  (let ((layout (gtk:grid-layout-new)))
    (is (eq :center (gtk:grid-layout-row-baseline-position layout 0)))
    (is (eq :top (setf (gtk:grid-layout-row-baseline-position layout 0) :top)))))

;;; 2024-9-19

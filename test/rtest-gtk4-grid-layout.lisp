(in-package :gtk-test)

(def-suite gtk-grid-layout :in gtk-layout-managers)
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

(test gtk-grid-layout-child-properties
  (glib-test:with-check-memory (layout childlayout grid button)
    (is (typep (setf grid (gtk:grid-new)) 'gtk:grid))
    (is (typep (setf button (gtk:button-new)) 'gtk:button))
    (is-false (gtk:grid-attach grid button 1 2 3 4))
    (is (typep (setf layout
                     (gtk:widget-layout-manager grid)) 'gtk:grid-layout))
    (is (typep (setf childlayout
                     (gtk:layout-manager-layout-child layout button))
               'gtk:grid-layout-child))
    (is (= 1 (gtk:grid-layout-child-column childlayout)))
    (is (= 3 (gtk:grid-layout-child-column-span childlayout)))
    (is (= 2 (gtk:grid-layout-child-row childlayout)))
    (is (= 4 (gtk:grid-layout-child-row-span childlayout)))
    ;; Remove references
    (is-false (gtk:grid-remove grid button))
    (is-false (setf (gtk:widget-layout-manager button) nil))
    (is-false (setf (gtk:widget-layout-manager grid) nil))))


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
  (glib-test:with-check-memory (layout)
    (is (typep (setf layout (make-instance 'gtk:grid-layout)) 'gtk:grid-layout))
    (is (= 0 (gtk:grid-layout-baseline-row layout)))
    (is-false (gtk:grid-layout-column-homogeneous layout))
    (is (= 0 (gtk:grid-layout-column-spacing layout)))
    (is-false (gtk:grid-layout-row-homogeneous layout))
    (is (= 0 (gtk:grid-layout-row-spacing layout)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_grid_layout_new

(test gtk-grid-layout-new
  (glib-test:with-check-memory (layout)
    (is (typep (setf layout (gtk:grid-layout-new)) 'gtk:grid-layout))))

;;;     gtk_grid_layout_set_row_baseline_position
;;;     gtk_grid_layout_get_row_baseline_position

(test gtk-grid-layout-row-baseline-position
  (glib-test:with-check-memory (layout)
    (is (typep (setf layout (gtk:grid-layout-new)) 'gtk:grid-layout))
    (is (eq :center (gtk:grid-layout-row-baseline-position layout 0)))
    (is (eq :top (setf (gtk:grid-layout-row-baseline-position layout 0) :top)))))

;;; 2026-07-20

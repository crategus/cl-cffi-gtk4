(in-package :gtk-test)

(def-suite gtk-column-view-sorter :in gtk-suite)
(in-suite gtk-column-view-sorter)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkColumnViewSorter

(test gtk-column-view-sorter-class
  ;; Check type
  (is (g:type-is-object "GtkColumnViewSorter"))
  ;; Check registered name
  (is (eq 'gtk:column-view-sorter
          (glib:symbol-for-gtype "GtkColumnViewSorter")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkColumnViewSorter")
          (g:gtype (cffi:foreign-funcall "gtk_column_view_sorter_get_type"
                                         :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkSorter")
          (g:type-parent "GtkColumnViewSorter")))
  ;; Check children
  (is (equal '()
             (gtk-test:list-children "GtkColumnViewSorter")))
  ;; Check interfaces
  (is (equal '()
             (gtk-test:list-interfaces "GtkColumnViewSorter")))
  ;; Check properties
  (is (equal '("primary-sort-column" "primary-sort-order")
             (gtk-test:list-properties "GtkColumnViewSorter")))
  ;; Check signals
  (is (equal '()
             (gtk-test:list-signals "GtkColumnViewSorter")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkColumnViewSorter"
                                             GTK-COLUMN-VIEW-SORTER
                               (:SUPERCLASS GTK-SORTER :EXPORT T :INTERFACES
                                NIL :TYPE-INITIALIZER
                                "gtk_column_view_sorter_get_type")
                               ((PRIMARY-SORT-COLUMN
                                 GTK-COLUMN-VIEW-SORTER-PRIMARY-SORT-COLUMN
                                 "primary-sort-column" "GtkColumnViewColumn" T
                                 NIL)
                                (PRIMARY-SORT-ORDER
                                 GTK-COLUMN-VIEW-SORTER-PRIMARY-SORT-ORDER
                                 "primary-sort-order" "GtkSortType" T NIL)))
             (gobject:get-g-type-definition "GtkColumnViewSorter"))))

;;; --- Properties -------------------------------------------------------------

;;;     primary-sort-column
;;;     primary-sort-order

(test gtk-column-view-sorter-properties
  (let ((sorter (make-instance 'gtk:column-view-sorter)))
    (is-false (gtk:column-view-sorter-primary-sort-column sorter))
    (is (eq :ascending (gtk:column-view-sorter-primary-sort-order sorter)))
))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_column_view_sorter_get_n_sort_columns
;;;     gtk_column_view_sorter_get_nth_sort_column

;;; 2024-7-4

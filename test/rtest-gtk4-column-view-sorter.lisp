(in-package :gtk-test)

(def-suite gtk-column-view-sorter :in gtk-suite)
(in-suite gtk-column-view-sorter)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkColumnViewSorter

(test gtk-column-view-sorter-class
  ;; Type check
  (is (g:type-is-object "GtkColumnViewSorter"))
  ;; Check the registered name
  (is (eq 'gtk:column-view-sorter
          (glib:symbol-for-gtype "GtkColumnViewSorter")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkColumnViewSorter")
          (g:gtype (cffi:foreign-funcall "gtk_column_view_sorter_get_type"
                                         :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkSorter")
          (g:type-parent "GtkColumnViewSorter")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkColumnViewSorter")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkColumnViewSorter")))
  ;; Check the properties
  (is (equal '("primary-sort-column" "primary-sort-order")
             (list-properties "GtkColumnViewSorter")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkColumnViewSorter")))
  ;; Check the class definition
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

;;; --- 2023-10-13 -------------------------------------------------------------

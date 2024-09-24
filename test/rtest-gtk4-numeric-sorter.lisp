(in-package :gtk-test)

(def-suite gtk-numeric-sorter :in gtk-suite)
(in-suite gtk-numeric-sorter)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkNumericSorter

(test gtk-numeric-sorter-class
  ;; Check type
  (is (g:type-is-object "GtkNumericSorter"))
  ;; Check registered name
  (is (eq 'gtk:numeric-sorter
          (glib:symbol-for-gtype "GtkNumericSorter")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkNumericSorter")
          (g:gtype (cffi:foreign-funcall "gtk_numeric_sorter_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkSorter")
          (g:type-parent "GtkNumericSorter")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkNumericSorter")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkNumericSorter")))
  ;; Check properties
  (is (equal '("expression" "sort-order")
             (glib-test:list-properties "GtkNumericSorter")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkNumericSorter")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkNumericSorter" GTK:NUMERIC-SORTER
                       (:SUPERCLASS GTK:SORTER
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_numeric_sorter_get_type")
                       ((EXPRESSION NUMERIC-SORTER-EXPRESSION
                         "expression" "GtkExpression" T T)
                        (SORT-ORDER NUMERIC-SORTER-SORT-ORDER
                         "sort-order" "GtkSortType" T T)))
             (gobject:get-gtype-definition "GtkNumericSorter"))))

;;; --- Properties -------------------------------------------------------------

;;;     expression
;;;     sort-order

;;; --- Functions --------------------------------------------------------------

;;;     gtk_numeric_sorter_new

;;; 2024-9-19

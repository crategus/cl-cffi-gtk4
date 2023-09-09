(in-package :gtk-test)

(def-suite gtk-numeric-sorter :in gtk-suite)
(in-suite gtk-numeric-sorter)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkNumericSorter

(test gtk-numeric-sorter-class
  ;; Type check
  (is (g:type-is-object "GtkNumericSorter"))
  ;; Check the registered name
  (is (eq 'gtk:numeric-sorter
          (glib:symbol-for-gtype "GtkNumericSorter")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkNumericSorter")
          (g:gtype (cffi:foreign-funcall "gtk_numeric_sorter_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkSorter")
          (g:type-parent "GtkNumericSorter")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkNumericSorter")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkNumericSorter")))
  ;; Check the properties
  (is (equal '("expression" "sort-order")
             (list-properties "GtkNumericSorter")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkNumericSorter")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkNumericSorter" GTK-NUMERIC-SORTER
                               (:SUPERCLASS GTK-SORTER :EXPORT T :INTERFACES
                                NIL :TYPE-INITIALIZER
                                "gtk_numeric_sorter_get_type")
                               ((EXPRESSION GTK-NUMERIC-SORTER-EXPRESSION
                                 "expression" "GtkExpression" T T)
                                (SORT-ORDER GTK-NUMERIC-SORTER-SORT-ORDER
                                 "sort-order" "GtkSortType" T T)))
             (gobject:get-g-type-definition "GtkNumericSorter"))))

;;; --- Properties -------------------------------------------------------------

;;;     expression
;;;     sort-order

;;; --- Functions --------------------------------------------------------------

;;;     gtk_numeric_sorter_new

;;; --- 2023-9-5 ---------------------------------------------------------------

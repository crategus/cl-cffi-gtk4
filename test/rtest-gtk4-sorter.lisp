(in-package :gtk-test)

(def-suite gtk-sorter :in gtk-suite)
(in-suite gtk-sorter)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkSorterOrder

(test gtk-sorter-order
  ;; Check type
  (is (g:type-is-enum "GtkSorterOrder"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkSorterOrder")
          (g:gtype (cffi:foreign-funcall "gtk_sorter_order_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:sorter-order
          (glib:symbol-for-gtype "GtkSorterOrder")))
  ;; Check names
  (is (equal '("GTK_SORTER_ORDER_PARTIAL" "GTK_SORTER_ORDER_NONE"
               "GTK_SORTER_ORDER_TOTAL")
             (gtk-test:list-enum-item-name "GtkSorterOrder")))
  ;; Check values
  (is (equal '(0 1 2)
             (gtk-test:list-enum-item-value "GtkSorterOrder")))
  ;; Check nick names
  (is (equal '("partial" "none" "total")
             (gtk-test:list-enum-item-nick "GtkSorterOrder")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkSorterOrder" GTK-SORTER-ORDER
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "gtk_sorter_order_get_type")
                                     (:PARTIAL 0)
                                     (:NONE 1)
                                     (:TOTAL 2))
             (gobject:get-g-type-definition "GtkSorterOrder"))))

;;;     GtkSorterChange

(test gtk-sorter-change
  ;; Check type
  (is (g:type-is-enum "GtkSorterChange"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkSorterChange")
          (g:gtype (cffi:foreign-funcall "gtk_sorter_change_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:sorter-change
          (glib:symbol-for-gtype "GtkSorterChange")))
  ;; Check names
  (is (equal '("GTK_SORTER_CHANGE_DIFFERENT" "GTK_SORTER_CHANGE_INVERTED"
               "GTK_SORTER_CHANGE_LESS_STRICT" "GTK_SORTER_CHANGE_MORE_STRICT")
             (gtk-test:list-enum-item-name "GtkSorterChange")))
  ;; Check values
  (is (equal '(0 1 2 3)
             (gtk-test:list-enum-item-value "GtkSorterChange")))
  ;; Check nick names
  (is (equal '("different" "inverted" "less-strict" "more-strict")
             (gtk-test:list-enum-item-nick "GtkSorterChange")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkSorterChange" GTK-SORTER-CHANGE
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "gtk_sorter_change_get_type")
                                     (:DIFFERENT 0)
                                     (:INVERTED 1)
                                     (:LESS-STRICT 2)
                                     (:MORE-STRICT 3))
             (gobject:get-g-type-definition "GtkSorterChange"))))

;;;     GtkSorter

(test gtk-sorter-class
  ;; Check type
  (is (g:type-is-object "GtkSorter"))
  ;; Check registered name
  (is (eq 'gtk:sorter
          (glib:symbol-for-gtype "GtkSorter")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkSorter")
          (g:gtype (cffi:foreign-funcall "gtk_sorter_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkSorter")))
  ;; Check children
  (if *first-run-gtk-test*
      (is (equal '("GtkColumnViewSorter" "GtkCustomSorter" "GtkMultiSorter"
                   "GtkNumericSorter" "GtkStringSorter" "GtkTreeListRowSorter")
                 (gtk-test:list-children "GtkSorter")))
      (is (equal '("GtkColumnViewSorter" "GtkCustomSorter" "GtkMultiSorter"
                   "GtkNumericSorter" "GtkStringSorter" "GtkTreeListRowSorter")
                 (gtk-test:list-children "GtkSorter"))))
  ;; Check interfaces
  (is (equal '()
             (gtk-test:list-interfaces "GtkSorter")))
  ;; Check properties
  (is (equal '()
             (gtk-test:list-properties "GtkSorter")))
  ;; Check signals
  (is (equal '("changed")
             (gtk-test:list-signals "GtkSorter")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkSorter" GTK-SORTER
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                                :TYPE-INITIALIZER "gtk_sorter_get_type")
                               NIL)
             (gobject:get-g-type-definition "GtkSorter"))))

;;; --- Signals ----------------------------------------------------------------

;;;     changed

;;; --- Functions --------------------------------------------------------------

;;;     gtk_sorter_compare
;;;     gtk_sorter_get_order
;;;     gtk_sorter_changed

;;;     gtk_ordering_from_cmpfunc

(test gtk:ordering-from-cmpfunc
  (is (eq :smaller (gtk:ordering-from-cmpfunc -1)))
  (is (eq :equal (gtk:ordering-from-cmpfunc  0)))
  (is (eq :larger (gtk:ordering-from-cmpfunc  1))))

;;; 2024-7-4

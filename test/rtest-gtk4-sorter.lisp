(in-package :gtk-test)

(def-suite gtk-sorter :in gtk-suite)
(in-suite gtk-sorter)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkSorterOrder

(test gtk-sorter-order
  ;; Check the type
  (is (g:type-is-enum "GtkSorterOrder"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkSorterOrder")
          (g:gtype (cffi:foreign-funcall "gtk_sorter_order_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gtk:sorter-order
          (glib:symbol-for-gtype "GtkSorterOrder")))
  ;; Check the names
  (is (equal '("GTK_SORTER_ORDER_PARTIAL" "GTK_SORTER_ORDER_NONE"
               "GTK_SORTER_ORDER_TOTAL")
             (list-enum-item-name "GtkSorterOrder")))
  ;; Check the values
  (is (equal '(0 1 2)
             (list-enum-item-value "GtkSorterOrder")))
  ;; Check the nick names
  (is (equal '("partial" "none" "total")
             (list-enum-item-nick "GtkSorterOrder")))
  ;; Check the enum definition
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
  ;; Check the type
  (is (g:type-is-enum "GtkSorterChange"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkSorterChange")
          (g:gtype (cffi:foreign-funcall "gtk_sorter_change_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gtk:sorter-change
          (glib:symbol-for-gtype "GtkSorterChange")))
  ;; Check the names
  (is (equal '("GTK_SORTER_CHANGE_DIFFERENT" "GTK_SORTER_CHANGE_INVERTED"
               "GTK_SORTER_CHANGE_LESS_STRICT" "GTK_SORTER_CHANGE_MORE_STRICT")
             (list-enum-item-name "GtkSorterChange")))
  ;; Check the values
  (is (equal '(0 1 2 3)
             (list-enum-item-value "GtkSorterChange")))
  ;; Check the nick names
  (is (equal '("different" "inverted" "less-strict" "more-strict")
             (list-enum-item-nick "GtkSorterChange")))
  ;; Check the enum definition
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
  ;; Type check
  (is (g:type-is-object "GtkSorter"))
  ;; Check the registered name
  (is (eq 'gtk:sorter
          (glib:symbol-for-gtype "GtkSorter")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkSorter")
          (g:gtype (cffi:foreign-funcall "gtk_sorter_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkSorter")))
  ;; Check the children
  (if *first-run-gtk-test*
      (is (equal '("GtkColumnViewSorter" "GtkCustomSorter" "GtkMultiSorter"
                   "GtkNumericSorter" "GtkStringSorter" "GtkTreeListRowSorter")
                 (list-children "GtkSorter")))
      (is (equal '("GtkColumnViewSorter" "GtkCustomSorter" "GtkMultiSorter"
                   "GtkNumericSorter" "GtkStringSorter" "GtkTreeListRowSorter")
                 (list-children "GtkSorter"))))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkSorter")))
  ;; Check the properties
  (is (equal '()
             (list-properties "GtkSorter")))
  ;; Check the signals
  (is (equal '("changed")
             (list-signals "GtkSorter")))
  ;; Check the class definition
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

;;; --- 2023-9-13 --------------------------------------------------------------

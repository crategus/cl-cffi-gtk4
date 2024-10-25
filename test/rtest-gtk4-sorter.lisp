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
             (glib-test:list-enum-item-names "GtkSorterOrder")))
  ;; Check values
  (is (equal '(0 1 2)
             (glib-test:list-enum-item-values "GtkSorterOrder")))
  ;; Check nick names
  (is (equal '("partial" "none" "total")
             (glib-test:list-enum-item-nicks "GtkSorterOrder")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkSorterOrder" GTK:SORTER-ORDER
                       (:EXPORT T
                        :TYPE-INITIALIZER "gtk_sorter_order_get_type")
                       (:PARTIAL 0)
                       (:NONE 1)
                       (:TOTAL 2))
             (gobject:get-gtype-definition "GtkSorterOrder"))))

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
             (glib-test:list-enum-item-names "GtkSorterChange")))
  ;; Check values
  (is (equal '(0 1 2 3)
             (glib-test:list-enum-item-values "GtkSorterChange")))
  ;; Check nick names
  (is (equal '("different" "inverted" "less-strict" "more-strict")
             (glib-test:list-enum-item-nicks "GtkSorterChange")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkSorterChange" GTK:SORTER-CHANGE
                       (:EXPORT T
                        :TYPE-INITIALIZER "gtk_sorter_change_get_type")
                       (:DIFFERENT 0)
                       (:INVERTED 1)
                       (:LESS-STRICT 2)
                       (:MORE-STRICT 3))
             (gobject:get-gtype-definition "GtkSorterChange"))))

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
                 (glib-test:list-children "GtkSorter")))
      (is (equal '("GtkColumnViewSorter" "GtkCustomSorter" "GtkMultiSorter"
                   "GtkNumericSorter" "GtkStringSorter" "GtkTreeListRowSorter")
                 (glib-test:list-children "GtkSorter"))))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkSorter")))
  ;; Check properties
  (is (equal '()
             (glib-test:list-properties "GtkSorter")))
  ;; Check signals
  (is (equal '("changed")
             (glib-test:list-signals "GtkSorter")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkSorter" GTK:SORTER
                       (:SUPERCLASS G:OBJECT
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_sorter_get_type")
                       NIL)
             (gobject:get-gtype-definition "GtkSorter"))))

;;; --- Signals ----------------------------------------------------------------

(test gtk-sorter-changed-signal
  (let* ((name "changed")
         (gtype (g:gtype "GtkSorter"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("GtkSorterChange")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_sorter_compare
;;;     gtk_sorter_get_order

(test gtk-sorter-compare
  (let ((sorter (gtk:custom-sorter-new
                    (lambda (obj1 obj2)
                      (let ((len1 (length (gtk:string-object-string obj1)))
                            (len2 (length (gtk:string-object-string obj2))))
                        (cond ((< len1 len2) -1)
                              ((> len1 len2)  1)
                              (t 0))))))
        (str1 (gtk:string-object-new "abc"))
        (str2 (gtk:string-object-new "abcd")))
    (is (eq :partial (gtk:sorter-order sorter)))
    (is (eq :smaller (gtk:sorter-compare sorter str1 str2)))
    (is (eq :larger (gtk:sorter-compare sorter str2 str1)))
    (is (eq :equal (gtk:sorter-compare sorter str1 str1)))
    (is (eq :equal (gtk:sorter-compare sorter str2 str2)))))

;;;     gtk_sorter_changed

(test gtk-sorter-changed
  (let ((sorter (gtk:custom-sorter-new))
        (msg nil))
    (g:signal-connect sorter "changed"
                      (lambda (sorter changed)
                        (declare (ignore sorter))
                        (push changed msg)))
    (is-false (gtk:sorter-changed sorter :different))
    (is (equal '(:different) msg))))

;;;     gtk_ordering_from_cmpfunc

(test gtk:ordering-from-cmpfunc
  (is (eq :smaller (gtk:ordering-from-cmpfunc -1)))
  (is (eq :equal (gtk:ordering-from-cmpfunc  0)))
  (is (eq :larger (gtk:ordering-from-cmpfunc  1))))

;;; 2024-10-18

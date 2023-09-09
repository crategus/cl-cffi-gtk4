(in-package :gtk-test)

(def-suite gtk-multi-sorter :in gtk-suite)
(in-suite gtk-multi-sorter)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkMultiSorter

(test gtk-multi-sorter-class
  ;; Type check
  (is (g:type-is-object "GtkMultiSorter"))
  ;; Check the registered name
  (is (eq 'gtk:multi-sorter
          (glib:symbol-for-gtype "GtkMultiSorter")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkMultiSorter")
          (g:gtype (cffi:foreign-funcall "gtk_multi_sorter_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkSorter")
          (g:type-parent "GtkMultiSorter")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkMultiSorter")))
  ;; Check the interfaces
  (is (equal '("GListModel" "GtkBuildable")
             (list-interfaces "GtkMultiSorter")))
  ;; Check the properties
  (is (equal '("item-type" "n-items")
             (list-properties "GtkMultiSorter")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkMultiSorter")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkMultiSorter" GTK-MULTI-SORTER
                               (:SUPERCLASS GTK-SORTER :EXPORT T :INTERFACES
                                ("GListModel" "GtkBuildable") :TYPE-INITIALIZER
                                "gtk_multi_sorter_get_type")
                               ((ITEM-TYPE GTK-MULTI-SORTER-ITEM-TYPE
                                 "item-type" "GType" T NIL)
                                (N-ITEMS GTK-MULTI-SORTER-N-ITEMS "n-items"
                                 "guint" T NIL)))
             (gobject:get-g-type-definition "GtkMultiSorter"))))

;;; --- Properties -------------------------------------------------------------

;;;     item-type                                          Since 4.8
;;;     n-items                                            Since 4.8

;;; --- Functions --------------------------------------------------------------

;;;     gtk_multi_sorter_new
;;;     gtk_multi_sorter_append
;;;     gtk_multi_sorter_remove

;;; --- 2023-9-5 ---------------------------------------------------------------

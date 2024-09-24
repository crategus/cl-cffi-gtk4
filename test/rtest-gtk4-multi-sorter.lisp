(in-package :gtk-test)

(def-suite gtk-multi-sorter :in gtk-suite)
(in-suite gtk-multi-sorter)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkMultiSorter

(test gtk-multi-sorter-class
  ;; Check type
  (is (g:type-is-object "GtkMultiSorter"))
  ;; Check registered name
  (is (eq 'gtk:multi-sorter
          (glib:symbol-for-gtype "GtkMultiSorter")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkMultiSorter")
          (g:gtype (cffi:foreign-funcall "gtk_multi_sorter_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkSorter")
          (g:type-parent "GtkMultiSorter")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkMultiSorter")))
  ;; Check interfaces
  (is (equal '("GListModel" "GtkBuildable")
             (glib-test:list-interfaces "GtkMultiSorter")))
  ;; Check properties
  (is (equal '("item-type" "n-items")
             (glib-test:list-properties "GtkMultiSorter")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkMultiSorter")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkMultiSorter" GTK:MULTI-SORTER
                       (:SUPERCLASS GTK:SORTER
                        :EXPORT T
                        :INTERFACES ("GListModel" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_multi_sorter_get_type")
                       ((ITEM-TYPE MULTI-SORTER-ITEM-TYPE
                         "item-type" "GType" T NIL)
                        (N-ITEMS MULTI-SORTER-N-ITEMS "n-items" "guint" T NIL)))
             (gobject:get-gtype-definition "GtkMultiSorter"))))

;;; --- Properties -------------------------------------------------------------

;;;     item-type                                          Since 4.8
;;;     n-items                                            Since 4.8

;;; --- Functions --------------------------------------------------------------

;;;     gtk_multi_sorter_new
;;;     gtk_multi_sorter_append
;;;     gtk_multi_sorter_remove

;;; 2024-9-19

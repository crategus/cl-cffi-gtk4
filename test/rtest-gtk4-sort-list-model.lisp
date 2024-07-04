(in-package :gtk-test)

(def-suite gtk-sort-list-model :in gtk-suite)
(in-suite gtk-sort-list-model)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkSortListModel

(test gtk-sort-list-model-class
  ;; Check type
  (is (g:type-is-object "GtkSortListModel"))
  ;; Check registered name
  (is (eq 'gtk:sort-list-model
          (glib:symbol-for-gtype "GtkSortListModel")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkSortListModel")
          (g:gtype (cffi:foreign-funcall "gtk_sort_list_model_get_type"
                                         :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkSortListModel")))
  ;; Check children
  (is (equal '()
             (gtk-test:list-children "GtkSortListModel")))
  ;; Check interfaces
  (is (equal '("GListModel" "GtkSectionModel")
             (gtk-test:list-interfaces "GtkSortListModel")))
  ;; Check properties
  (is (equal '("incremental" "item-type" "model" "n-items" "pending"
               "section-sorter" "sorter")
             (gtk-test:list-properties "GtkSortListModel")))
  ;; Check signals
  (is (equal '()
             (gtk-test:list-signals "GtkSortListModel")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkSortListModel"
                                             GTK-SORT-LIST-MODEL
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES
                                ("GListModel" "GtkSectionModel")
                                :TYPE-INITIALIZER
                                "gtk_sort_list_model_get_type")
                               ((INCREMENTAL GTK-SORT-LIST-MODEL-INCREMENTAL
                                 "incremental" "gboolean" T T)
                                (ITEM-TYPE GTK-SORT-LIST-MODEL-ITEM-TYPE
                                 "item-type" "GType" T NIL)
                                (MODEL GTK-SORT-LIST-MODEL-MODEL "model"
                                 "GListModel" T T)
                                (N-ITEMS GTK-SORT-LIST-MODEL-N-ITEMS "n-items"
                                 "guint" T NIL)
                                (PENDING GTK-SORT-LIST-MODEL-PENDING "pending"
                                 "guint" T NIL)
                                (SECTION-SORTER
                                 GTK-SORT-LIST-MODEL-SECTION-SORTER
                                 "section-sorter" "GtkSorter" T T)
                                (SORTER GTK-SORT-LIST-MODEL-SORTER "sorter"
                                 "GtkSorter" T T)))
             (gobject:get-g-type-definition "GtkSortListModel"))))

;;; --- Properties -------------------------------------------------------------

;;;     incremental
;;;     item-type                                          Since 4.8
;;;     model
;;;     n-items                                            Since 4.8
;;;     pending
;;;     section-sorter                                     Since 4.12
;;;     sorter

;;; --- Functions --------------------------------------------------------------

;;;     gtk_sort_list_model_new

;;;     gtk_sort_list_model_set_sorter
;;;     gtk_sort_list_model_get_sorter
;;;     gtk_sort_list_model_set_model
;;;     gtk_sort_list_model_get_model
;;;     gtk_sort_list_model_set_incremental
;;;     gtk_sort_list_model_get_incremental
;;;     gtk_sort_list_model_get_pending
;;;     gtk_sort_list_model_get_section_sorter             Since 4.12
;;;     gtk_sort_list_model_set_section_sorter             Since 4.12

;;; 2024-7-4

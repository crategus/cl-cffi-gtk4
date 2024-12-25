(in-package :gtk-test)

(def-suite gtk-tree-sortable :in gtk-deprecated)
(in-suite gtk-tree-sortable)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkTreeSortable

(test gtk-tree-sortable-interface
  ;; Check type
  (is (g:type-is-interface "GtkTreeSortable"))
  ;; Check registered name
  (is (eq 'gtk:tree-sortable
          (glib:symbol-for-gtype "GtkTreeSortable")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkTreeSortable")
          (g:gtype (cffi:foreign-funcall "gtk_tree_sortable_get_type" :size))))
  ;; Check interface prerequisites
  (is (equal '("GtkTreeModel" "GObject")
             (glib-test:list-interface-prerequisites "GtkTreeSortable")))
  ;; Check interface properties
  (is (equal '()
             (glib-test:list-interface-properties "GtkTreeSortable")))
  ;; Check interface signals
  (is (equal '("sort-column-changed")
             (glib-test:list-signals "GtkTreeSortable")))
  ;; Get interface definition
  (is (equal '(GOBJECT:DEFINE-GINTERFACE "GtkTreeSortable" GTK:TREE-SORTABLE
                      (:EXPORT T
                       :TYPE-INITIALIZER "gtk_tree_sortable_get_type"))
             (gobject:get-gtype-definition "GtkTreeSortable"))))

;;;     GTK_TREE_SORTABLE_DEFAULT_SORT_COLUMN_ID

(test gtk-tree-sortable-default-sort-column-id
  (is (= -1 gtk:+tree-sortable-default-sort-column-id+)))

;;;     GTK_TREE_SORTABLE_UNSORTED_SORT_COLUMN_ID

(test gtk-tree-sortable-unsorted-sort-column-id
  (is (= -2 gtk:+tree-sortable-unsorted-sort-column-id+)))

;;; --- Signals ----------------------------------------------------------------

;;;     sort-column-changed

;;; --- Functions --------------------------------------------------------------

;;;     GtkTreeIterCompareFunc

;;;     gtk_tree_sortable_sort_column_changed
;;;     gtk_tree_sortable_get_sort_column_id
;;;     gtk_tree_sortable_set_sort_column_id
;;;     gtk_tree_sortable_set_sort_func
;;;     gtk_tree_sortable_set_default_sort_func
;;;     gtk_tree_sortable_has_default_sort_func

;;; 2024-9-20

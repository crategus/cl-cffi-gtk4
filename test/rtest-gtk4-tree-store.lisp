(in-package :gtk-test)

(def-suite gtk-tree-store :in gtk-suite)
(in-suite gtk-tree-store)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkTreeStore

(test gtk-tree-store-class
  ;; Check type
  (is (g:type-is-object "GtkTreeStore"))
  ;; Check registered name
  (is (eq 'gtk:tree-store
          (glib:symbol-for-gtype "GtkTreeStore")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkTreeStore")
          (g:gtype (cffi:foreign-funcall "gtk_tree_store_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkTreeStore")))
  ;; Check children
  (is (equal '()
             (gtk-test:list-children "GtkTreeStore")))
  ;; Check interfaces
  (is (equal '("GtkTreeModel" "GtkTreeDragSource" "GtkTreeDragDest"
               "GtkTreeSortable" "GtkBuildable")
             (gtk-test:list-interfaces "GtkTreeStore")))
  ;; Check properties
  (is (equal '()
             (gtk-test:list-properties "GtkTreeStore")))
  ;; Check signals
  (is (equal '()
             (gtk-test:list-signals "GtkTreeStore")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkTreeStore" GTK-TREE-STORE
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES
                                ("GtkBuildable" "GtkTreeDragDest"
                                 "GtkTreeDragSource" "GtkTreeModel"
                                 "GtkTreeSortable")
                                :TYPE-INITIALIZER "gtk_tree_store_get_type")
                               NIL)
             (gobject:get-g-type-definition "GtkTreeStore"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_tree_store_new

(test gtk-tree-store-new
  (let ((gtk-init:*gtk-warn-deprecated* nil))
    (is (typep (gtk:tree-store-new "gint" "gchararray" "GdkPixbuf")
               'gtk:tree-store))))

;;;     gtk_tree_store_newv
;;;     gtk_tree_store_set_column_types
;;;     gtk_tree_store_set_value
;;;     gtk_tree_store_set
;;;     gtk_tree_store_set_valist
;;;     gtk_tree_store_set_valuesv
;;;     gtk_tree_store_remove
;;;     gtk_tree_store_insert
;;;     gtk_tree_store_insert_before
;;;     gtk_tree_store_insert_after
;;;     gtk_tree_store_insert_with_values
;;;     gtk_tree_store_insert_with_valuesv
;;;     gtk_tree_store_prepend
;;;     gtk_tree_store_append
;;;     gtk_tree_store_is_ancestor
;;;     gtk_tree_store_iter_depth
;;;     gtk_tree_store_clear
;;;     gtk_tree_store_iter_is_valid
;;;     gtk_tree_store_reorder
;;;     gtk_tree_store_swap
;;;     gtk_tree_store_move_before
;;;     gtk_tree_store_move_after

;;; 2024-5-16

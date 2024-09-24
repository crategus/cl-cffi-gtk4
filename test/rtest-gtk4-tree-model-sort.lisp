(in-package :gtk-test)

(def-suite gtk-tree-model-sort :in gtk-suite)
(in-suite gtk-tree-model-sort)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkTreeModelSort

(test gtk-tree-model-sort-class
  ;; Check type
  (is (g:type-is-object "GtkTreeModelSort"))
  ;; Check registered name
  (is (eq 'gtk:tree-model-sort
          (glib:symbol-for-gtype "GtkTreeModelSort")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkTreeModelSort")
          (g:gtype (cffi:foreign-funcall "gtk_tree_model_sort_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkTreeModelSort")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkTreeModelSort")))
  ;; Check interfaces
  (is (equal '("GtkTreeModel" "GtkTreeSortable" "GtkTreeDragSource")
             (glib-test:list-interfaces "GtkTreeModelSort")))
  ;; Check properties
  (is (equal '("model")
             (glib-test:list-properties "GtkTreeModelSort")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkTreeModelSort")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkTreeModelSort" GTK:TREE-MODEL-SORT
                      (:SUPERCLASS G:OBJECT
                       :EXPORT T
                       :INTERFACES
                       ("GtkTreeDragSource" "GtkTreeModel" "GtkTreeSortable")
                       :TYPE-INITIALIZER "gtk_tree_model_sort_get_type")
                      ((MODEL TREE-MODEL-SORT-MODEL
                        "model" "GtkTreeModel" T NIL)))
             (gobject:get-gtype-definition "GtkTreeModelSort"))))

;;; --- Properties -------------------------------------------------------------

;;;     model

(test gtk-tree-model-sort-properties
  (let* ((gtk-init:*gtk-warn-deprecated* nil)
         (model (make-instance 'gtk:tree-model-sort)))
    (is-false (gtk:tree-model-sort-model model))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_tree_model_sort_new_with_model

(test gtk-tree-model-sort-new-with-model
  (let* ((gtk-init:*gtk-warn-deprecated* nil)
         (model (create-and-fill-gtk-list-store))
         (sortmodel nil))
    (is (typep (gtk:tree-model-sort-new-with-model nil) 'gtk:tree-model-sort))
    (is (typep (setf sortmodel
                     (gtk:tree-model-sort-new-with-model model))
               'gtk:tree-model-sort))
    (is (eq model (gtk:tree-model-sort-model sortmodel)))))

;;;     gtk_tree_model_sort_convert_child_path_to_path
;;;     gtk_tree_model_sort_convert_child_iter_to_iter
;;;     gtk_tree_model_sort_convert_path_to_child_path
;;;     gtk_tree_model_sort_convert_iter_to_child_iter
;;;     gtk_tree_model_sort_reset_default_sort_func
;;;     gtk_tree_model_sort_clear_cache
;;;     gtk_tree_model_sort_iter_is_valid

;;; 2024-9-20

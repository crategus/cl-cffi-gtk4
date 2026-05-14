(in-package :gtk-test)

(def-suite gtk-tree-model-sort :in gtk-deprecated)
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
  (let ((gtk-init:*warn-deprecated* nil))
    (glib-test:with-check-memory (model)
      (setf model (make-instance 'gtk:tree-model-sort))
      (is-false (gtk:tree-model-sort-model model)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_tree_model_sort_new_with_model

(test gtk-tree-model-sort-new-with-model
  (let ((gtk-init:*warn-deprecated* nil))
    (when *first-run-testsuite*
      (let ((gtk-init:*warn-deprecated* nil))
        (glib-test:with-check-memory ((model 2) sortmodel :strong 1)
          (setf model (create-list-store-for-package))
          (is (typep (setf sortmodel
                           (gtk:tree-model-sort-new-with-model model))
                     'gtk:tree-model-sort))
          (is (eq model (gtk:tree-model-sort-model sortmodel))))))))

;;;     gtk_tree_model_sort_convert_child_path_to_path

(test gtk-tree-model-sort-convert-child-path-to-path
  (let ((gtk-init:*warn-deprecated* nil))
    (when *first-run-testsuite*
      (glib-test:with-check-memory ((store 2) model :strong 1)
        (let* ((column 1)
               (path1 (gtk:tree-path-new-first))
               (path2 (gtk:tree-path-new-first))
               value1 value2 value3)
          ;; Create store and model
          (setf store (create-list-store-for-package "GTK"))
          (setf model (gtk:tree-model-sort-new-with-model store))
          ;; Column with symbols is sorted
          (setf (gtk:tree-sortable-sort-column-id model) column)
          ;; Retrieve fist value from store and model
          (setf value1
                (gtk:tree-model-value store
                                      (gtk:tree-model-iter store path1) column))
          (setf value2
                (gtk:tree-model-value model
                                      (gtk:tree-model-iter model path2) column))
          ;; The strings are not equal
          (is (not (string= value1 value2)))
          ;; Get the path for value1 in model
          (setf path2 (gtk:tree-model-sort-convert-child-path-to-path model path1))
          ;; Retrieve again the value
          (setf value3
                (gtk:tree-model-value model
                                      (gtk:tree-model-iter model path2) column))
          ;; Check values again
          (is (string= value1 value3)))))))

;;;     gtk_tree_model_sort_convert_child_iter_to_iter

(test gtk-tree-model-sort-convert-child-iter-to-iter
  (let ((gtk-init:*warn-deprecated* nil))
    (when *first-run-testsuite*
      (glib-test:with-check-memory ((store 2) model :strong 1)
        (let* ((column 1)
               iter1 iter2
               value1 value2 value3)
          ;; Create store and model
          (setf store (create-list-store-for-package "GTK"))
          (setf model (gtk:tree-model-sort-new-with-model store))
          ;; Column with symbols is sorted
          (setf (gtk:tree-sortable-sort-column-id model) column)

          (is (typep (setf iter1 (gtk:tree-model-iter-first store)) 'gtk:tree-iter))
          (is (typep (setf iter2 (gtk:tree-model-iter-first model)) 'gtk:tree-iter))

          ;; Retrieve fist value from store and model
          (setf value1 (gtk:tree-model-value store iter1 column))
          (setf value2 (gtk:tree-model-value model iter2 column))
          ;; The strings are not equal
          (is (not (string= value1 value2)))
          ;; Get the iter for value1 in model
          (setf iter2 (gtk:tree-model-sort-convert-child-iter-to-iter model iter1))
          ;; Retrieve again the value
          (setf value3 (gtk:tree-model-value model iter2 column))
          ;; Check values again
          (is (string= value1 value3)))))))

;;;     gtk_tree_model_sort_convert_path_to_child_path

(test gtk-tree-model-sort-convert-path-to-child-path
  (let ((gtk-init:*warn-deprecated* nil))
    (when *first-run-testsuite*
      (glib-test:with-check-memory ((store 2) model :strong 1)
        (let* ((column 1)
               (path1 (gtk:tree-path-new-first))
               (path2 (gtk:tree-path-new-first))
               value1 value2 value3)
          ;; Create store and model
          (setf store (create-list-store-for-package "GTK"))
          (setf model (gtk:tree-model-sort-new-with-model store))
          ;; Column with symbols is sorted
          (setf (gtk:tree-sortable-sort-column-id model) column)
          ;; Retrieve fist value from store and model
          (setf value1
                (gtk:tree-model-value store
                                      (gtk:tree-model-iter store path1) column))
          (setf value2
                (gtk:tree-model-value model
                                      (gtk:tree-model-iter model path2) column))
          ;; The strings are not equal
          (is (not (string= value1 value2)))
          ;; Get the path for value2 in sort
          (setf path1 (gtk:tree-model-sort-convert-path-to-child-path model path2))
          ;; Retrieve again the value
          (setf value3
                (gtk:tree-model-value store
                                      (gtk:tree-model-iter store path1) column))
          ;; Check values again
          (is (string= value2 value3)))))))

;;;     gtk_tree_model_sort_convert_iter_to_child_iter

(test gtk-tree-model-sort-convert-iter-to-child-iter
  (let ((gtk-init:*warn-deprecated* nil))
    (when *first-run-testsuite*
      (glib-test:with-check-memory ((store 2) model :strong 1)
        (let* ((column 1)
               iter1 iter2
               value1 value2 value3)
          ;; Create store and model
          (setf store (create-list-store-for-package "GTK"))
          (setf model (gtk:tree-model-sort-new-with-model store))
          ;; Column with symbols is sorted
          (setf (gtk:tree-sortable-sort-column-id model) column)

          (is (typep (setf iter1 (gtk:tree-model-iter-first store)) 'gtk:tree-iter))
          (is (typep (setf iter2 (gtk:tree-model-iter-first model)) 'gtk:tree-iter))

          ;; Retrieve fist value from store and model
          (setf value1 (gtk:tree-model-value store iter1 column))
          (setf value2 (gtk:tree-model-value model iter2 column))
          ;; The strings are not equal
          (is (not (string= value1 value2)))
          ;; Get the iter for value2 in store
          (setf iter1 (gtk:tree-model-sort-convert-iter-to-child-iter model iter2))
          ;; Retrieve again the value
          (setf value3 (gtk:tree-model-value store iter1 column))
          ;; Check values again
          (is (string= value2 value3)))))))

;;;     gtk_tree_model_sort_reset_default_sort_func
;;;     gtk_tree_model_sort_clear_cache
;;;     gtk_tree_model_sort_iter_is_valid

;;; 2026-05-08

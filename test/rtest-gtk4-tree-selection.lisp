(in-package :gtk-test)

(def-suite gtk-tree-selection :in gtk-suite)
(in-suite gtk-tree-selection)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkTreeSelection

;;; --- Properties -------------------------------------------------------------

;;;     mode

;;; --- Signals ----------------------------------------------------------------

;;;     changed

;;; --- Functions --------------------------------------------------------------

;;;     GtkTreeSelectionFunc
;;;     GtkTreeSelectionForeachFunc

;;;     gtk_tree_selection_set_select_function
;;;     gtk_tree_selection_get_select_function
;;;     gtk_tree_selection_get_user_data

;;;     gtk_tree_selection_get_tree_view

(test gtk-tree-selection-tree-view
  (let* ((view (gtk:tree-view-new))
         (selection (gtk:tree-view-selection view)))
    (is (eq view (gtk:tree-selection-tree-view selection)))))

;;;     gtk_tree_selection_get_selected
;;;     gtk_tree_selection_selected_foreach
;;;     gtk_tree_selection_get_selected_rows
;;;     gtk_tree_selection_count_selected_rows
;;;     gtk_tree_selection_select_path
;;;     gtk_tree_selection_unselect_path
;;;     gtk_tree_selection_path_is_selected
;;;     gtk_tree_selection_select_iter
;;;     gtk_tree_selection_unselect_iter
;;;     gtk_tree_selection_iter_is_selected
;;;     gtk_tree_selection_select_all
;;;     gtk_tree_selection_unselect_all
;;;     gtk_tree_selection_select_range
;;;     gtk_tree_selection_unselect_range

;;; 2024-2-22

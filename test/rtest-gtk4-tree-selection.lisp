(in-package :gtk-test)

(def-suite gtk-tree-selection :in gtk-deprecated)
(in-suite gtk-tree-selection)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkTreeSelection

(test gtk-tree-selection-class
  ;; Check type
  (is (g:type-is-object "GtkTreeSelection"))
  ;; Check registered name
  (is (eq 'gtk:tree-selection
          (glib:symbol-for-gtype "GtkTreeSelection")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkTreeSelection")
          (g:gtype (cffi:foreign-funcall "gtk_tree_selection_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkTreeSelection")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkTreeSelection")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkTreeSelection")))
  ;; Check properties
  (is (equal '("mode")
             (glib-test:list-properties "GtkTreeSelection")))
  ;; Check signals
  (is (equal '("changed")
             (glib-test:list-signals "GtkTreeSelection")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkTreeSelection" GTK:TREE-SELECTION
                      (:SUPERCLASS G:OBJECT
                       :EXPORT T
                       :INTERFACES NIL
                       :TYPE-INITIALIZER "gtk_tree_selection_get_type")
                      ((MODE TREE-SELECTION-MODE
                        "mode" "GtkSelectionMode" T T)))
             (gobject:get-gtype-definition "GtkTreeSelection"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-tree-selection-properties
  (let ((gtk-init:*warn-deprecated* nil))
    (glib-test:with-check-memory (selection)
      (setf selection (make-instance 'gtk:tree-selection))
      (is (eq :single (gtk:tree-selection-mode selection))))))

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
  (when *first-run-testsuite*
    (let ((gtk-init:*warn-deprecated* nil))
      (glib-test:with-check-memory (view (selection 2) :strong 1)
        (setf view (gtk:tree-view-new))
        (setf selection (gtk:tree-view-selection view))
        (is (eq view (gtk:tree-selection-tree-view selection)))))))

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

;;; 2024-12-24

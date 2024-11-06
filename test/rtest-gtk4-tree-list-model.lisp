(in-package :gtk-test)

(def-suite gtk-tree-list-model :in gtk-tree-support)
(in-suite gtk-tree-list-model)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkTreeListRow

(test gtk-tree-list-row-class
  ;; Check type
  (is (g:type-is-object "GtkTreeListRow"))
  ;; Check registered name
  (is (eq 'gtk:tree-list-row
          (glib:symbol-for-gtype "GtkTreeListRow")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkTreeListRow")
          (g:gtype (cffi:foreign-funcall "gtk_tree_list_row_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkTreeListRow")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkTreeListRow")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkTreeListRow")))
  ;; Check properties
  (is (equal '("children" "depth" "expandable" "expanded" "item")
             (glib-test:list-properties "GtkTreeListRow")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkTreeListRow")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkTreeListRow" GTK:TREE-LIST-ROW
                       (:SUPERCLASS GOBJECT:OBJECT
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_tree_list_row_get_type")
                       ((CHILDREN TREE-LIST-ROW-CHILDREN "children"
                         "GListModel" T NIL)
                        (DEPTH TREE-LIST-ROW-DEPTH "depth" "guint" T NIL)
                        (EXPANDABLE TREE-LIST-ROW-EXPANDABLE "expandable"
                         "gboolean" T NIL)
                        (EXPANDED TREE-LIST-ROW-EXPANDED "expanded" "gboolean"
                         T T)
                        (ITEM TREE-LIST-ROW-ITEM "item" "GObject" T NIL)))
             (gobject:get-gtype-definition "GtkTreeListRow"))))

;;; --- Properties -------------------------------------------------------------

;; TODO: Make a test with a complete initialized GtkTreeListRow

(test gtk-tree-list-row-properties
  (let ((listrow (make-instance 'gtk:tree-list-row)))
    (is-false (gtk:tree-list-row-children listrow))
    (is (= 0 (gtk:tree-list-row-depth listrow)))
    (is-false (gtk:tree-list-row-expandable listrow))
    (is-false (gtk:tree-list-row-expanded listrow))
;; TODO: Gives an GLib error:
;;     g_object_ref: assertion 'G_IS_OBJECT (object)' failed
;   (is-false (gtk:tree-list-row-item listrow))
))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_tree_list_row_get_child_row
;;;     gtk_tree_list_row_get_parent
;;;     gtk_tree_list_row_get_position
;;;     gtk_tree_list_row_is_expandable

;;; --- Types and Values -------------------------------------------------------

;;;     GtkTreeListModel

(test gtk-tree-list-model-class
  ;; Check type
  (is (g:type-is-object "GtkTreeListModel"))
  ;; Check registered name
  (is (eq 'gtk:tree-list-model
          (glib:symbol-for-gtype "GtkTreeListModel")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkTreeListModel")
          (g:gtype (cffi:foreign-funcall "gtk_tree_list_model_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkTreeListModel")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkTreeListModel")))
  ;; Check interfaces
  (is (equal '("GListModel")
             (glib-test:list-interfaces "GtkTreeListModel")))
  ;; Check properties
  (is (equal '("autoexpand" "item-type" "model" "n-items" "passthrough")
             (glib-test:list-properties "GtkTreeListModel")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkTreeListModel")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkTreeListModel" GTK:TREE-LIST-MODEL
                       (:SUPERCLASS GOBJECT:OBJECT
                        :EXPORT T
                        :INTERFACES ("GListModel")
                        :TYPE-INITIALIZER "gtk_tree_list_model_get_type")
                       ((AUTOEXPAND TREE-LIST-MODEL-AUTOEXPAND "autoexpand"
                         "gboolean" T T)
                        (ITEM-TYPE TREE-LIST-MODEL-ITEM-TYPE "item-type"
                         "GType" T NIL)
                        (MODEL TREE-LIST-MODEL-MODEL "model" "GListModel" T
                         NIL)
                        (N-ITEMS TREE-LIST-MODEL-N-ITEMS "n-items" "guint" T
                         NIL)
                        (PASSTHROUGH TREE-LIST-MODEL-PASSTHROUGH "passthrough"
                         "gboolean" T NIL)))
             (gobject:get-gtype-definition "GtkTreeListModel"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-tree-list-model-properties
  (let ((model (make-instance 'gtk:tree-list-model)))
    (is-false (gtk:tree-list-model-autoexpand model))
    (is (g:gtype "GtkTreeListRow") (gtk:tree-list-model-item-type model))
    (is-false (gtk:tree-list-model-model model))
    (is (= 0 (gtk:tree-list-model-n-items model)))
    (is-false (gtk:tree-list-model-passthrough model))))

;;; --- Functions --------------------------------------------------------------

;;;     GtkTreeListModelCreateModelFunc

;;;     gtk_tree_list_model_new
;;;     gtk_tree_list_model_get_row
;;;     gtk_tree_list_model_get_child_row

;;; 2024-10-17

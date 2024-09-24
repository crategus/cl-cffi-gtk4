(in-package :gtk-test)

(def-suite gtk-tree-view-dnd :in gtk-suite)
(in-suite gtk-tree-view-dnd)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkTreeDragSource

(test gtk-tree-drag-source-interface
  ;; Check type
  (is (g:type-is-interface "GtkTreeDragSource"))
  ;; Check registered name
  (is (eq 'gtk:tree-drag-source
          (glib:symbol-for-gtype "GtkTreeDragSource")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkTreeDragSource")
          (g:gtype (cffi:foreign-funcall "gtk_tree_drag_source_get_type" :size))))
  ;; Check interface prerequisites
  (is (equal '()
             (glib-test:list-interface-prerequisites "GtkTreeDragSource")))
  ;; Check interface properties
  (is (equal '()
             (glib-test:list-interface-properties "GtkTreeDragSource")))
  ;; Check interface signals
  (is (equal '()
             (glib-test:list-signals "GtkTreeDragSource")))
  ;; Get interface definition
  (is (equal '(GOBJECT:DEFINE-GINTERFACE "GtkTreeDragSource"
                                         GTK:TREE-DRAG-SOURCE
                      (:EXPORT T
                       :TYPE-INITIALIZER "gtk_tree_drag_source_get_type"))
             (gobject:get-gtype-definition "GtkTreeDragSource"))))

;;;     GtkTreeDragDest

(test gtk-tree-drag-dest-interface
  ;; Check type
  (is (g:type-is-interface "GtkTreeDragDest"))
  ;; Check registered name
  (is (eq 'gtk:tree-drag-dest
          (glib:symbol-for-gtype "GtkTreeDragDest")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkTreeDragDest")
          (g:gtype (cffi:foreign-funcall "gtk_tree_drag_dest_get_type" :size))))
  ;; Check interface prerequisites
  (is (equal '()
             (glib-test:list-interface-prerequisites "GtkTreeDragDest")))
  ;; Check interface properties
  (is (equal '()
             (glib-test:list-interface-properties "GtkTreeDragDest")))
  ;; Check interface signals
  (is (equal '()
             (glib-test:list-signals "GtkTreeDragDest")))
  ;; Get interface definition
  (is (equal '(GOBJECT:DEFINE-GINTERFACE "GtkTreeDragDest" GTK:TREE-DRAG-DEST
                       (:EXPORT T
                        :TYPE-INITIALIZER "gtk_tree_drag_dest_get_type"))
             (gobject:get-gtype-definition "GtkTreeDragDest"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_tree_drag_source_drag_data_delete
;;;     gtk_tree_drag_source_drag_data_get
;;;     gtk_tree_drag_source_row_draggable

;;;     gtk_tree_drag_dest_drag_data_received
;;;     gtk_tree_drag_dest_row_drop_possible
;;;     gtk_tree_create_row_drag_content
;;;     gtk_tree_get_row_drag_data

;;; 2024-9-20

(in-package :gtk-test)

(def-suite gtk-tree-view :in gtk-suite)
(in-suite gtk-tree-view)

(defun create-and-fill-model-simple ()
  (let ((*gtk-warn-deprecated* nil))
    (let ((model (gtk:tree-store-new "gchararray" "gchararray" "guint")))
      ;; First Book
      (let ((iter (gtk:tree-store-append model nil))) ; Toplevel iterator
        ;; Set the toplevel row
        (gtk:tree-store-set model
                            iter
                            "The Art of Computer Programming"
                            "Donald E. Knuth"
                            2011)
        ;; Append and set child rows
        (gtk:tree-store-set model
                            (gtk:tree-store-append model iter) ; Child iterator
                            "Volume 1: Fundamental Algorithms"
                            ""
                            1997)
        (gtk:tree-store-set model
                            (gtk:tree-store-append model iter) ; Child iterator
                            "Volume 2: Seminumerical Algorithms"
                            ""
                            1998)
        (gtk:tree-store-set model
                            (gtk:tree-store-append model iter) ; Child iterator
                            "Volume 3: Sorting and Searching"
                            ""
                            1998))
      ;; Second Book
      (let ((iter (gtk:tree-store-append model nil))) ; Toplevel iterator
        (gtk:tree-store-set model
                            iter
                            "Let Over Lambda"
                            "Doug Hoyte"
                            2008))
      ;; Third Book
      (let ((iter (gtk:tree-store-append model nil))) ; Toplevel iterator
        (gtk:tree-store-set model
                            iter
                            "On Lisp"
                            "Paul Graham"
                            1993))
      model)))

;;; --- Types and Values -------------------------------------------------------

;;;     GtkTreeViewDropPosition

(test gtk-tree-view-dop-position
  ;; Check the type
  (is (g:type-is-enum "GtkTreeViewDropPosition"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkTreeViewDropPosition")
          (g:gtype (cffi:foreign-funcall "gtk_tree_view_drop_position_get_type"
                                         :size))))
  ;; Check the registered name
  (is (eq 'gtk:tree-view-drop-position
          (glib:symbol-for-gtype "GtkTreeViewDropPosition")))
  ;; Check the names
  (is (equal '("GTK_TREE_VIEW_DROP_BEFORE" "GTK_TREE_VIEW_DROP_AFTER"
               "GTK_TREE_VIEW_DROP_INTO_OR_BEFORE"
               "GTK_TREE_VIEW_DROP_INTO_OR_AFTER")
             (list-enum-item-name "GtkTreeViewDropPosition")))
  ;; Check the values
  (is (equal '(0 1 2 3)
             (list-enum-item-value "GtkTreeViewDropPosition")))
  ;; Check the nick names
  (is (equal '("before" "after" "into-or-before" "into-or-after")
             (list-enum-item-nick "GtkTreeViewDropPosition")))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkTreeViewDropPosition"
                                     GTK-TREE-VIEW-DROP-POSITION
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "gtk_tree_view_drop_position_get_type")
                                     (:BEFORE 0)
                                     (:AFTER 1)
                                     (:INTO-OR-BEFORE 2)
                                     (:INTO-OR-AFTER 3))
             (gobject:get-g-type-definition "GtkTreeViewDropPosition"))))

;;;     GtkTreeViewGridLines

(test gtk-tree-view-grid-lines
  ;; Check the type
  (is (g:type-is-enum "GtkTreeViewGridLines"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkTreeViewGridLines")
          (g:gtype (cffi:foreign-funcall "gtk_tree_view_grid_lines_get_type"
                                         :size))))
  ;; Check the registered name
  (is (eq 'gtk:tree-view-grid-lines
          (glib:symbol-for-gtype "GtkTreeViewGridLines")))
  ;; Check the names
  (is (equal '("GTK_TREE_VIEW_GRID_LINES_NONE"
               "GTK_TREE_VIEW_GRID_LINES_HORIZONTAL"
               "GTK_TREE_VIEW_GRID_LINES_VERTICAL"
               "GTK_TREE_VIEW_GRID_LINES_BOTH")
             (list-enum-item-name "GtkTreeViewGridLines")))
  ;; Check the values
  (is (equal '(0 1 2 3)
             (list-enum-item-value "GtkTreeViewGridLines")))
  ;; Check the nick names
  (is (equal '("none" "horizontal" "vertical" "both")
             (list-enum-item-nick "GtkTreeViewGridLines")))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkTreeViewGridLines"
                                     GTK-TREE-VIEW-GRID-LINES
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "gtk_tree_view_grid_lines_get_type")
                                     (:NONE 0)
                                     (:HORIZONTAL 1)
                                     (:VERTICAL 2)
                                     (:BOTH 3))
             (gobject:get-g-type-definition "GtkTreeViewGridLines"))))

;;;     GtkTreeView

(test gtk-tree-view-class
  ;; Type check
  (is (g:type-is-object "GtkTreeView"))
  ;; Check the registered name
  (is (eq 'gtk:tree-view
          (glib:symbol-for-gtype "GtkTreeView")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkTreeView")
          (g:gtype (cffi:foreign-funcall "gtk_tree_view_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkTreeView")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkTreeView")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkScrollable")
             (list-interfaces "GtkTreeView")))
  ;; Check the properties
  (is (equal '("activate-on-single-click" "enable-grid-lines" "enable-search"
               "enable-tree-lines" "expander-column" "fixed-height-mode"
               "hadjustment" "headers-clickable" "headers-visible"
               "hover-expand" "hover-selection" "hscroll-policy"
               "level-indentation" "model" "reorderable" "rubber-banding"
               "search-column" "show-expanders" "tooltip-column" "vadjustment"
               "vscroll-policy")
             (list-properties "GtkTreeView")))
  ;; Check the signals
  (is (equal '("columns-changed" "cursor-changed" "expand-collapse-cursor-row"
               "move-cursor" "row-activated" "row-collapsed" "row-expanded"
               "select-all" "select-cursor-parent" "select-cursor-row"
               "start-interactive-search" "test-collapse-row" "test-expand-row"
               "toggle-cursor-row" "unselect-all")
             (list-signals "GtkTreeView")))
  ;; CSS name
  (is (string= "treeview"
               (gtk:widget-class-css-name "GtkTreeView")))
  ;; CSS classes
  (let ((*gtk-warn-deprecated* nil))
    (is (equal '("view")
               (gtk:widget-css-classes (make-instance 'gtk:tree-view)))))
  ;; Accessible role
  (is (eq :widget (gtk:widget-class-accessible-role "GtkTreeView")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkTreeView" GTK-TREE-VIEW
                               (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                                ("GtkAccessible" "GtkBuildable"
                                 "GtkConstraintTarget" "GtkScrollable")
                                :TYPE-INITIALIZER "gtk_tree_view_get_type")
                               ((ACTIVATE-ON-SINGLE-CLICK
                                 GTK-TREE-VIEW-ACTIVATE-ON-SINGLE-CLICK
                                 "activate-on-single-click" "gboolean" T T)
                                (ENABLE-GRID-LINES
                                 GTK-TREE-VIEW-ENABLE-GRID-LINES
                                 "enable-grid-lines" "GtkTreeViewGridLines" T
                                 T)
                                (ENABLE-SEARCH GTK-TREE-VIEW-ENABLE-SEARCH
                                 "enable-search" "gboolean" T T)
                                (ENABLE-TREE-LINES
                                 GTK-TREE-VIEW-ENABLE-TREE-LINES
                                 "enable-tree-lines" "gboolean" T T)
                                (EXPANDER-COLUMN GTK-TREE-VIEW-EXPANDER-COLUMN
                                 "expander-column" "GtkTreeViewColumn" T T)
                                (FIXED-HEIGHT-MODE
                                 GTK-TREE-VIEW-FIXED-HEIGHT-MODE
                                 "fixed-height-mode" "gboolean" T T)
                                (HEADERS-CLICKABLE
                                 GTK-TREE-VIEW-HEADERS-CLICKABLE
                                 "headers-clickable" "gboolean" T T)
                                (HEADERS-VISIBLE GTK-TREE-VIEW-HEADERS-VISIBLE
                                 "headers-visible" "gboolean" T T)
                                (HOVER-EXPAND GTK-TREE-VIEW-HOVER-EXPAND
                                 "hover-expand" "gboolean" T T)
                                (HOVER-SELECTION GTK-TREE-VIEW-HOVER-SELECTION
                                 "hover-selection" "gboolean" T T)
                                (LEVEL-INDENTATION
                                 GTK-TREE-VIEW-LEVEL-INDENTATION
                                 "level-indentation" "gint" T T)
                                (MODEL GTK-TREE-VIEW-MODEL "model"
                                 "GtkTreeModel" T T)
                                (REORDERABLE GTK-TREE-VIEW-REORDERABLE
                                 "reorderable" "gboolean" T T)
                                (RUBBER-BANDING GTK-TREE-VIEW-RUBBER-BANDING
                                 "rubber-banding" "gboolean" T T)
                                (SEARCH-COLUMN GTK-TREE-VIEW-SEARCH-COLUMN
                                 "search-column" "gint" T T)
                                (SHOW-EXPANDERS GTK-TREE-VIEW-SHOW-EXPANDERS
                                 "show-expanders" "gboolean" T T)
                                (TOOLTIP-COLUMN GTK-TREE-VIEW-TOOLTIP-COLUMN
                                 "tooltip-column" "gint" T T)))
             (gobject:get-g-type-definition "GtkTreeView"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-tree-view-properties
  (let ((*gtk-warn-deprecated* nil))
    (let ((view (make-instance 'gtk:tree-view)))
      (is-false (gtk:tree-view-activate-on-single-click view))
      (is (eq :none (gtk:tree-view-enable-grid-lines view)))
      (is-true (gtk:tree-view-enable-search view))
      (is-false (gtk:tree-view-enable-tree-lines view))
      (is-false (gtk:tree-view-expander-column view))
      (is-false (gtk:tree-view-fixed-height-mode view))
      (is-true (gtk:tree-view-headers-clickable view))
      (is-true (gtk:tree-view-headers-visible view))
      (is-false (gtk:tree-view-hover-expand view))
      (is-false (gtk:tree-view-hover-selection view))
      (is (= 0 (gtk:tree-view-level-indentation view)))
      (is-false (gtk:tree-view-model view))
      (is-false (gtk:tree-view-reorderable view))
      (is-false (gtk:tree-view-rubber-banding view))
      (is (= -1 (gtk:tree-view-search-column view)))
      (is-true (gtk:tree-view-show-expanders view))
      (is (= -1 (gtk:tree-view-tooltip-column view))))))

;;; --- Signals ----------------------------------------------------------------

;;;     columns-changed
;;;     cursor-changed
;;;     expand-collapse-cursor-row
;;;     move-cursor
;;;     row-activated
;;;     row-collapsed
;;;     row-expanded
;;;     select-all
;;;     select-cursor-parent
;;;     select-cursor-row
;;;     start-interactive-search
;;;     test-collapse-row
;;;     test-expand-row
;;;     toggle-cursor-row
;;;     unselect-all

;;; --- Functions --------------------------------------------------------------

;;;     gtk_tree_view_new

(test gtk-tree-view-new
  (let ((*gtk-warn-deprecated* nil))
    (is (typep (gtk:tree-view-new) 'gtk:tree-view))))

;;;     gtk_tree_view_new_with_model

(test gtk-tree-view-new-with-model
  (let ((*gtk-warn-deprecated* nil))
    (let ((model (gtk:list-store-new "gint" "gchararray" "GdkPixbuf")))
      (is (typep (gtk:tree-view-new-with-model model) 'gtk:tree-view)))))

;;;     gtk_tree_view_get_selection

(test gtk-tree-view-selection
  (let ((*gtk-warn-deprecated* nil))
    (let ((view (gtk:tree-view-new)))
      (is (typep (gtk:tree-view-selection view) 'gtk:tree-selection)))))

;;;     gtk_tree_view_columns_autosize

;;;     gtk_tree_view_append_column
;;;     gtk_tree_view_remove_column
;;;     gtk_tree_view_get_n_columns

(test gtk-tree-view-append-column
  (let ((*gtk-warn-deprecated* nil))
    (let ((view (gtk:tree-view-new))
          (column (gtk:tree-view-column-new)))
      (is (= 0 (gtk:tree-view-n-columns view)))
      (is (= 1 (gtk:tree-view-append-column view column)))
      (is (= 1 (gtk:tree-view-n-columns view)))
      (is (= 0 (gtk:tree-view-remove-column view column)))
      (is (= 0 (gtk:tree-view-n-columns view))))))

;;;     gtk_tree_view_insert_column
;;;     gtk_tree_view_get_column
;;;     gtk_tree_view_get_columns

(test gtk-tree-view-insert-column
  (let ((*gtk-warn-deprecated* nil))
    (let ((view (gtk:tree-view-new))
          (column0 (gtk:tree-view-column-new))
          (column1 (gtk:tree-view-column-new))
          (column2 (gtk:tree-view-column-new)))
      (is (= 1 (gtk:tree-view-insert-column view column0 0)))
      (is (= 2 (gtk:tree-view-insert-column view column1 0)))
      (is (= 3 (gtk:tree-view-insert-column view column2 0)))
      (is (eq column2 (gtk:tree-view-column view 0)))
      (is (eq column1 (gtk:tree-view-column view 1)))
      (is (eq column0 (gtk:tree-view-column view 2)))
      (is (= 3 (length (gtk:tree-view-columns view))))
      (is (every (lambda (x) (typep x 'gtk:tree-view-column))
                 (gtk:tree-view-columns view))))))

;;;     gtk_tree_view_insert_column_with_attributes

(test gtk-tree-view-insert-column-with-attributes
  (let ((*gtk-warn-deprecated* nil))
    (let* ((model (create-and-fill-model-simple))
           (view (gtk:tree-view-new-with-model model)))
      (let ((renderer (gtk:cell-renderer-text-new)))
        (is (= 1 (gtk:tree-view-insert-column-with-attributes
                         view
                         0
                         "Title"
                         renderer
                         "text" 0))))
      (let ((renderer (gtk:cell-renderer-text-new)))
        (is (= 2 (gtk:tree-view-insert-column-with-attributes
                         view
                         1
                         "Author"
                         renderer
                         "text" 1))))
      (let ((renderer (gtk:cell-renderer-text-new)))
        (is (= 3 (gtk:tree-view-insert-column-with-attributes
                         view
                         2
                         "Year"
                         renderer
                         "text" 2))))
      (is (equal '("Title" "Author" "Year")
                 (mapcar #'gtk:tree-view-column-title
                         (gtk:tree-view-columns view)))))))

;;;     gtk_tree_view_insert_column_with_data_func

;; TODO: GtkTreeCellDataFunc is not called. What is the problem!?

(test gtk-tree-view-insert-column-with-data-func
  (let ((*gtk-warn-deprecated* nil))
    (let* ((model (create-and-fill-model-simple))
           (view (gtk:tree-view-new-with-model model)))
      (let ((renderer (gtk:cell-renderer-text-new)))
        (is (= 1 (gtk:tree-view-insert-column-with-data-func
                         view
                         0
                         "Title"
                         renderer
                         (lambda (column renderer model iter)
                           (format t "in GtkTreeCellDataFunc~%")
                           (format t "   column : ~a~%" column)
                           (format t " renderer : ~a~%" renderer)
                           (format t "    model : ~a~%" model)
                           (format t "     iter : ~a~%" iter))))))
                         )))

;;;     gtk_tree_view_move_column_after

(test gtk-tree-view-move-column-after.1
  (let ((*gtk-warn-deprecated* nil))
    (let ((view (gtk:tree-view-new))
          (column0 (make-instance 'gtk:tree-view-column))
          (column1 (make-instance 'gtk:tree-view-column))
          (column2 (make-instance 'gtk:tree-view-column)))
      ;; Add columns to the tree view
      (is (= 1 (gtk:tree-view-append-column view column0)))
      (is (= 2 (gtk:tree-view-append-column view column1)))
      (is (= 3 (gtk:tree-view-append-column view column2)))
      ;; Move columns
      (is-false (gtk:tree-view-move-column-after view column0 column2))
      (is (eq column1 (gtK:tree-view-column view 0)))
      (is (eq column2 (gtk:tree-view-column view 1)))
      (is (eq column0 (gtk:tree-view-column view 2))))))

(test gtk-tree-view-move-column-after.2
  (let ((*gtk-warn-deprecated* nil))
    (let ((view (gtk:tree-view-new))
          (column0 (make-instance 'gtk:tree-view-column))
          (column1 (make-instance 'gtk:tree-view-column))
          (column2 (make-instance 'gtk:tree-view-column)))
      ;; Add columns to the tree view
      (is (= 1 (gtk:tree-view-append-column view column0)))
      (is (= 2 (gtk:tree-view-append-column view column1)))
      (is (= 3 (gtk:tree-view-append-column view column2)))
      ;; Move columns
      (is-false (gtk:tree-view-move-column-after view column2 nil))
      (is (eq column2 (gtK:tree-view-column view 0)))
      (is (eq column0 (gtk:tree-view-column view 1)))
      (is (eq column1 (gtk:tree-view-column view 2))))))

;;;     GtkTreeViewColumnDropFunc
;;;     gtk_tree_view_set_column_drag_function

(test gtk-tree-view-set-column-drag-function
  (let ((*gtk-warn-deprecated* nil))
    (let ((view (gtk:tree-view-new)))
      (is-false (gtk:tree-view-set-column-drag-function view
                        (lambda (view column prev next)
                          (declare (ignore view column prev next))
                          t)))
      (is-false (gtk:tree-view-set-column-drag-function view nil)))))

;;;     gtk_tree_view_scroll_to_point
;;;     gtk_tree_view_scroll_to_cell
;;;     gtk_tree_view_set_cursor
;;;     gtk_tree_view_set_cursor_on_cell

;;;     gtk_tree_view_get_cursor

(test gtk-tree-view-get-cursor
  (let ((*gtk-warn-deprecated* nil))
    (let ((view (gtk:tree-view-new)))
      (is (equal '(nil nil)
                 (multiple-value-list (gtk:tree-view-get-cursor view)))))))

;;;     gtk_tree_view_row_activated
;;;     gtk_tree_view_expand_all
;;;     gtk_tree_view_collapse_all
;;;     gtk_tree_view_expand_to_path
;;;     gtk_tree_view_expand_row
;;;     gtk_tree_view_collapse_row
;;;
;;;     GtkTreeViewMappingFunc
;;;
;;;     gtk_tree_view_map_expanded_rows
;;;     gtk_tree_view_row_expanded
;;;     gtk_tree_view_get_path_at_pos
;;;     gtk_tree_view_is_blank_at_pos
;;;     gtk_tree_view_get_cell_area
;;;     gtk_tree_view_get_background_area
;;;     gtk_tree_view_get_visible_rect
;;;     gtk_tree_view_get_visible_range
;;;     gtk_tree_view_convert_bin_window_to_tree_coords
;;;     gtk_tree_view_convert_bin_window_to_widget_coords
;;;     gtk_tree_view_convert_tree_to_bin_window_coords
;;;     gtk_tree_view_convert_tree_to_widget_coords
;;;     gtk_tree_view_convert_widget_to_bin_window_coords
;;;     gtk_tree_view_convert_widget_to_tree_coords
;;;     gtk_tree_view_enable_model_drag_dest
;;;     gtk_tree_view_enable_model_drag_source
;;;     gtk_tree_view_unset_rows_drag_source
;;;     gtk_tree_view_unset_rows_drag_dest
;;;     gtk_tree_view_set_drag_dest_row
;;;     gtk_tree_view_get_drag_dest_row
;;;     gtk_tree_view_get_dest_row_at_pos
;;;     gtk_tree_view_create_row_drag_icon
;;;
;;;     GtkTreeViewSearchEqualFunc
;;;
;;;     gtk_tree_view_get_search_equal_func
;;;     gtk_tree_view_set_search_equal_func
;;;     gtk_tree_view_get_search_entry
;;;     gtk_tree_view_set_search_entry
;;;
;;;     GtkTreeViewRowSeparatorFunc
;;;
;;;     gtk_tree_view_get_row_separator_func
;;;     gtk_tree_view_set_row_separator_func
;;;     gtk_tree_view_is_rubber_banding_active
;;;     gtk_tree_view_get_grid_lines
;;;     gtk_tree_view_set_grid_lines
;;;     gtk_tree_view_set_tooltip_row
;;;     gtk_tree_view_set_tooltip_cell
;;;     gtk_tree_view_get_tooltip_context

;;; 2024-4-27

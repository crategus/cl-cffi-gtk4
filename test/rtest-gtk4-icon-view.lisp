(in-package :gtk-test)

(def-suite gtk-icon-view :in gtk-deprecated)
(in-suite gtk-icon-view)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkIconViewDropPosition

(test gtk-icon-view-drop-position
  ;; Check type
  (is (g:type-is-enum "GtkIconViewDropPosition"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkIconViewDropPosition")
          (g:gtype (cffi:foreign-funcall "gtk_icon_view_drop_position_get_type"
                                         :size))))
  ;; Check registered name
  (is (eq 'gtk:icon-view-drop-position
          (glib:symbol-for-gtype "GtkIconViewDropPosition")))
  ;; Check names
  (is (equal '("GTK_ICON_VIEW_NO_DROP" "GTK_ICON_VIEW_DROP_INTO"
               "GTK_ICON_VIEW_DROP_LEFT" "GTK_ICON_VIEW_DROP_RIGHT"
               "GTK_ICON_VIEW_DROP_ABOVE" "GTK_ICON_VIEW_DROP_BELOW")
             (glib-test:list-enum-item-names "GtkIconViewDropPosition")))
  ;; Check values
  (is (equal '(0 1 2 3 4 5)
             (glib-test:list-enum-item-values "GtkIconViewDropPosition")))
  ;; Check nick names
  (is (equal '("no-drop" "drop-into" "drop-left" "drop-right" "drop-above"
               "drop-below")
             (glib-test:list-enum-item-nicks "GtkIconViewDropPosition")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkIconViewDropPosition"
                                    GTK:ICON-VIEW-DROP-POSITION
                       (:EXPORT T
                        :TYPE-INITIALIZER
                        "gtk_icon_view_drop_position_get_type")
                       (:NO-DROP 0)
                       (:DROP-INTO 1)
                       (:DROP-LEFT 2)
                       (:DROP-RIGHT 3)
                       (:DROP-ABOVE 4)
                       (:DROP-BELOW 5))
             (gobject:get-gtype-definition "GtkIconViewDropPosition"))))

;;;     GtkIconView

(test gtk-icon-view-class
  ;; Check type
  (is (g:type-is-object "GtkIconView"))
  ;; Check registered name
  (is (eq 'gtk:icon-view
          (glib:symbol-for-gtype "GtkIconView")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkIconView")
          (g:gtype (cffi:foreign-funcall "gtk_icon_view_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkIconView")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkIconView")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkCellLayout" "GtkScrollable")
             (glib-test:list-interfaces "GtkIconView")))
  ;; Check properties
  (is (equal '("activate-on-single-click" "cell-area" "column-spacing" "columns"
               "hadjustment" "hscroll-policy" "item-orientation" "item-padding"
               "item-width" "margin" "markup-column" "model" "pixbuf-column"
               "reorderable" "row-spacing" "selection-mode" "spacing"
               "text-column" "tooltip-column" "vadjustment" "vscroll-policy")
             (glib-test:list-properties "GtkIconView")))
  ;; Check signals
  (is (equal '("activate-cursor-item" "item-activated" "move-cursor"
               "select-all" "select-cursor-item" "selection-changed"
               "toggle-cursor-item" "unselect-all")
             (glib-test:list-signals "GtkIconView")))
  ;; Check CSS name
  (is (string= "iconview"
               (gtk:widget-class-css-name "GtkIconView")))
  ;; Check accessible role
  (is (eq :widget (gtk:widget-class-accessible-role "GtkIconView")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkIconView" GTK:ICON-VIEW
                      (:SUPERCLASS GTK:WIDGET
                       :EXPORT T
                       :INTERFACES
                       ("GtkAccessible" "GtkBuildable" "GtkCellLayout"
                        "GtkConstraintTarget" "GtkScrollable")
                       :TYPE-INITIALIZER "gtk_icon_view_get_type")
                      ((ACTIVATE-ON-SINGLE-CLICK
                        ICON-VIEW-ACTIVATE-ON-SINGLE-CLICK
                        "activate-on-single-click" "gboolean" T T)
                       (CELL-AREA ICON-VIEW-CELL-AREA
                        "cell-area" "GtkCellArea" T NIL)
                       (COLUMN-SPACING ICON-VIEW-COLUMN-SPACING
                        "column-spacing" "gint" T T)
                       (COLUMNS ICON-VIEW-COLUMNS "columns" "gint" T T)
                       (ITEM-ORIENTATION ICON-VIEW-ITEM-ORIENTATION
                        "item-orientation" "GtkOrientation" T T)
                       (ITEM-PADDING ICON-VIEW-ITEM-PADDING
                        "item-padding" "gint" T T)
                       (ITEM-WIDTH ICON-VIEW-ITEM-WIDTH
                        "item-width" "gint" T T)
                       (MARGIN ICON-VIEW-MARGIN "margin" "gint" T T)
                       (MARKUP-COLUMN ICON-VIEW-MARKUP-COLUMN
                        "markup-column" "gint" T T)
                       (MODEL ICON-VIEW-MODEL "model" "GtkTreeModel" T T)
                       (PIXBUF-COLUMN ICON-VIEW-PIXBUF-COLUMN
                        "pixbuf-column" "gint" T T)
                       (REORDERABLE ICON-VIEW-REORDERABLE
                        "reorderable" "gboolean" T T)
                       (ROW-SPACING ICON-VIEW-ROW-SPACING
                        "row-spacing" "gint" T T)
                       (SELECTION-MODE ICON-VIEW-SELECTION-MODE
                        "selection-mode" "GtkSelectionMode" T T)
                       (SPACING ICON-VIEW-SPACING "spacing" "gint" T T)
                       (TEXT-COLUMN ICON-VIEW-TEXT-COLUMN
                        "text-column" "gint" T T)
                       (TOOLTIP-COLUMN ICON-VIEW-TOOLTIP-COLUMN
                        "tooltip-column" "gint" T T)))
             (gobject:get-gtype-definition "GtkIconView"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-icon-view-properties
  (when *first-run-testsuite*
    (let ((gtk-init:*warn-deprecated* nil))
      (glib-test:with-check-memory (view :strong 1)
        (setf view (make-instance 'gtk:icon-view))
        (is-false (gtk:icon-view-activate-on-single-click view))
        (is (typep (gtk:icon-view-cell-area view) 'gtk:cell-area-box))
        (is (= 6 (gtk:icon-view-column-spacing view)))
        (is (= -1 (gtk:icon-view-columns view)))
        (is (eq :vertical (gtk:icon-view-item-orientation view)))
        (is (= 6 (gtk:icon-view-item-padding view)))
        (is (= -1 (gtk:icon-view-item-width view)))
        (is (= 6 (gtk:icon-view-margin view)))
        (is (= -1 (gtk:icon-view-markup-column view)))
        (is-false (gtk:icon-view-model view))
        (is (= -1 (gtk:icon-view-pixbuf-column view)))
        (is-false (gtk:icon-view-reorderable view))
        (is (= 6 (gtk:icon-view-row-spacing view)))
        (is (eq :single (gtk:icon-view-selection-mode view)))
        (is (= 0 (gtk:icon-view-spacing view)))
        (is (= -1 (gtk:icon-view-text-column view)))
        (is (= -1 (gtk:icon-view-tooltip-column view)))))))

;;; --- Signals ----------------------------------------------------------------

;;;     activate-cursor-item
;;;     item-activated
;;;     move-cursor
;;;     select-all
;;;     select-cursor-item
;;;     selection-changed
;;;     toggle-cursor-item
;;;     unselect-all

;;; --- Functions --------------------------------------------------------------

;;;     GtkIconViewForeachFunc

;;;     gtk_icon_view_new
;;;     gtk_icon_view_new_with_area
;;;     gtk_icon_view_new_with_model
;;;     gtk_icon_view_get_path_at_pos
;;;     gtk_icon_view_get_item_at_pos
;;;     gtk_icon_view_set_cursor
;;;     gtk_icon_view_get_cursor
;;;     gtk_icon_view_selected_foreach
;;;     gtk_icon_view_get_cell_rect
;;;     gtk_icon_view_select_path
;;;     gtk_icon_view_unselect_path
;;;     gtk_icon_view_path_is_selected
;;;     gtk_icon_view_get_selected_items
;;;     gtk_icon_view_select_all
;;;     gtk_icon_view_unselect_all
;;;     gtk_icon_view_item_activated
;;;     gtk_icon_view_scroll_to_path
;;;     gtk_icon_view_get_visible_range
;;;     gtk_icon_view_set_tooltip_item
;;;     gtk_icon_view_set_tooltip_cell
;;;     gtk_icon_view_get_tooltip_context
;;;     gtk_icon_view_get_item_row
;;;     gtk_icon_view_get_item_column
;;;     gtk_icon_view_enable_model_drag_source
;;;     gtk_icon_view_enable_model_drag_dest
;;;     gtk_icon_view_unset_model_drag_source
;;;     gtk_icon_view_unset_model_drag_dest
;;;     gtk_icon_view_set_drag_dest_item
;;;     gtk_icon_view_get_drag_dest_item
;;;     gtk_icon_view_get_dest_item_at_pos
;;;     gtk_icon_view_create_drag_icon

;;; 2024-9-21

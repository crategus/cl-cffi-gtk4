(in-package :gtk-test)

(def-suite gtk-list-box :in gtk-suite)
(in-suite gtk-list-box)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkListBoxRow

(test gtk-list-box-row-class
  ;; Type check
  (is (g:type-is-object "GtkListBoxRow"))
  ;; Check the registered name
  (is (eq 'gtk:list-box-row
          (glib:symbol-for-gtype "GtkListBoxRow")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkListBoxRow")
          (g:gtype (cffi:foreign-funcall "gtk_list_box_row_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkListBoxRow")))
  ;; Check the children
  (is (equal '("GtkPlacesViewRow" "GtkSidebarRow")
             (list-children "GtkListBoxRow")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget" 
               "GtkActionable")
             (list-interfaces "GtkListBoxRow")))
  ;; Check the properties
  (is (equal '("action-name" "action-target" "activatable" "child" "selectable")
             (list-properties "GtkListBoxRow")))
  ;; Check the signals
  (is (equal '("activate")
             (list-signals "GtkListBoxRow")))
  ;; CSS name
  (is (string= "row"
               (gtk:widget-class-css-name "GtkListBoxRow")))
  ;; Accessible role
  (is (eq :list-item (gtk:widget-class-accessible-role "GtkListBoxRow")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkListBoxRow" GTK-LIST-BOX-ROW
                               (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                                ("GtkAccessible" "GtkActionable" "GtkBuildable"
                                 "GtkConstraintTarget")
                                :TYPE-INITIALIZER "gtk_list_box_row_get_type")
                               ((ACTIVATABLE GTK-LIST-BOX-ROW-ACTIVATABLE
                                 "activatable" "gboolean" T T)
                                (CHILD GTK-LIST-BOX-ROW-CHILD "child"
                                 "GtkWidget" T T)
                                (SELECTABLE GTK-LIST-BOX-ROW-SELECTABLE
                                 "selectable" "gboolean" T T)))
             (gobject:get-g-type-definition "GtkListBoxRow"))))

;;; --- Properties -------------------------------------------------------------

;;;     activatable
;;;     child
;;;     selectable

;;; --- Signals ----------------------------------------------------------------

;;;     activate

;;; --- Types and Values -------------------------------------------------------

;;;     GtkListBox

(test gtk-list-box-class
  ;; Type check
  (is (g:type-is-object "GtkListBox"))
  ;; Check the registered name
  (is (eq 'gtk:list-box
          (glib:symbol-for-gtype "GtkListBox")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkListBox")
          (g:gtype (cffi:foreign-funcall "gtk_list_box_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkListBox")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkListBox")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (list-interfaces "GtkListBox")))
  ;; Check the properties
  (is (equal '("accept-unpaired-release" "activate-on-single-click" 
               "selection-mode" "show-separators")
             (list-properties "GtkListBox")))
  ;; Check the signals
  (is (equal '("activate-cursor-row" "move-cursor" "row-activated" 
               "row-selected" "select-all" "selected-rows-changed" 
               "toggle-cursor-row" "unselect-all")
             (list-signals "GtkListBox")))
  ;; CSS name
  (is (string= "list"
               (gtk:widget-class-css-name "GtkListBox")))
  ;; Accessible role
  (is (eq :list (gtk:widget-class-accessible-role "GtkListBox")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkListBox" GTK-LIST-BOX
                               (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                                ("GtkAccessible" "GtkBuildable"
                                 "GtkConstraintTarget")
                                :TYPE-INITIALIZER "gtk_list_box_get_type")
                               ((ACCEPT-UNPAIRED-RELEASE
                                 GTK-LIST-BOX-ACCEPT-UNPAIRED-RELEASE
                                 "accept-unpaired-release" "gboolean" T T)
                                (ACTIVATE-ON-SINGLE-CLICK
                                 GTK-LIST-BOX-ACTIVATE-ON-SINGLE-CLICK
                                 "activate-on-single-click" "gboolean" T T)
                                (SELECTION-MODE GTK-LIST-BOX-SELECTION-MODE
                                 "selection-mode" "GtkSelectionMode" T T)
                                (SHOW-SEPARATORS GTK-LIST-BOX-SHOW-SEPARATORS
                                 "show-separators" "gboolean" T T)))
             (gobject:get-g-type-definition "GtkListBox"))))

;;; --- Properties -------------------------------------------------------------

;;;     accept-unpaired-release
;;;     activate-on-single-click
;;;     selection-mode
;;;     show-separators

;;; --- Signals ----------------------------------------------------------------

;;;     activate-cursor-row
;;;     move-cursor
;;;     row-activated
;;;     row-selected
;;;     select-all
;;;     selected-rows-changed
;;;     toggle-cursor-row
;;;     unselect-all

;;; --- Functions --------------------------------------------------------------
;;;
;;;     GtkListBoxFilterFunc
;;;     GtkListBoxSortFunc
;;;     GtkListBoxUpdateHeaderFunc

;;;     gtk_list_box_new
;;;     gtk_list_box_prepend
;;;     gtk_list_box_append
;;;     gtk_list_box_insert
;;;     gtk_list_box_remove
;;;     gtk_list_box_remove_all                            Since 4.12
;;;     gtk_list_box_select_row
;;;     gtk_list_box_unselect_row
;;;     gtk_list_box_select_all
;;;     gtk_list_box_unselect_all
;;;     gtk_list_box_get_selected_row

;;;     GtkListBoxForeachFunc

;;;     gtk_list_box_selected_foreach
;;;     gtk_list_box_get_selected_rows
;;;     gtk_list_box_get_adjustment
;;;     gtk_list_box_set_adjustment
;;;     gtk_list_box_set_placeholder
;;;     gtk_list_box_get_row_at_index
;;;     gtk_list_box_get_row_at_y
;;;     gtk_list_box_invalidate_filter
;;;     gtk_list_box_invalidate_headers
;;;     gtk_list_box_invalidate_sort
;;;     gtk_list_box_set_filter_func
;;;     gtk_list_box_set_header_func
;;;     gtk_list_box_set_sort_func
;;;     gtk_list_box_drag_highlight_row
;;;     gtk_list_box_drag_unhighlight_row

;;;     GtkListBoxCreateWidgetFunc

;;;     gtk_list_box_bind_model

;;;     gtk_list_box_row_new
;;;     gtk_list_box_row_changed
;;;     gtk_list_box_row_is_selected
;;;     gtk_list_box_row_get_header
;;;     gtk_list_box_row_set_header
;;;     gtk_list_box_row_get_index

;;; --- 2023-8-31 --------------------------------------------------------------

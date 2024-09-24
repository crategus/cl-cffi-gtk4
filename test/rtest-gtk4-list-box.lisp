(in-package :gtk-test)

(def-suite gtk-list-box :in gtk-suite)
(in-suite gtk-list-box)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkListBoxRow

(test gtk-list-box-row-class
  ;; Check type
  (is (g:type-is-object "GtkListBoxRow"))
  ;; Check registered name
  (is (eq 'gtk:list-box-row
          (glib:symbol-for-gtype "GtkListBoxRow")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkListBoxRow")
          (g:gtype (cffi:foreign-funcall "gtk_list_box_row_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkListBoxRow")))
  ;; Check children
  #-windows
  (is (equal '("GtkPlacesViewRow" "GtkSidebarRow")
             (glib-test:list-children "GtkListBoxRow")))
  #+windows
  (if *first-run-gtk-test*
      (is (equal '()
                 (glib-test:list-children "GtkListBoxRow"))))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkActionable")
             (glib-test:list-interfaces "GtkListBoxRow")))
  ;; Check properties
  (is (equal '("action-name" "action-target" "activatable" "child" "selectable")
             (glib-test:list-properties "GtkListBoxRow")))
  ;; Check signals
  (is (equal '("activate")
             (glib-test:list-signals "GtkListBoxRow")))
  ;; Check CSS name
  (is (string= "row"
               (gtk:widget-class-css-name "GtkListBoxRow")))
  ;; Check accessible role
  (is (eq :list-item (gtk:widget-class-accessible-role "GtkListBoxRow")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkListBoxRow" GTK:LIST-BOX-ROW
                      (:SUPERCLASS GTK:WIDGET
                       :EXPORT T
                       :INTERFACES
                       ("GtkAccessible" "GtkActionable" "GtkBuildable"
                        "GtkConstraintTarget")
                       :TYPE-INITIALIZER "gtk_list_box_row_get_type")
                      ((ACTIVATABLE LIST-BOX-ROW-ACTIVATABLE
                        "activatable" "gboolean" T T)
                       (CHILD LIST-BOX-ROW-CHILD "child" "GtkWidget" T T)
                       (SELECTABLE LIST-BOX-ROW-SELECTABLE
                        "selectable" "gboolean" T T)))
             (gobject:get-gtype-definition "GtkListBoxRow"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-list-box-row-properties
  (let ((row (make-instance 'gtk:list-box-row)))
    (is-true (gtk:list-box-row-activatable row))
    (is-false (gtk:list-box-row-child row))
    (is-true (gtk:list-box-row-selectable row))))

;;; --- Signals ----------------------------------------------------------------

;;;     activate

;;; --- Functions --------------------------------------------------------------

;;;     gtk_list_box_row_new

(test gtk-list-box-row-new
  (is (typep (gtk:list-box-row-new) 'gtk:list-box-row)))

;;;     gtk_list_box_row_changed
;;;     gtk_list_box_row_is_selected
;;;     gtk_list_box_row_get_header
;;;     gtk_list_box_row_set_header
;;;     gtk_list_box_row_get_index

;;; --- Types and Values -------------------------------------------------------

;;;     GtkListBox

(test gtk-list-box-class
  ;; Check type
  (is (g:type-is-object "GtkListBox"))
  ;; Check registered name
  (is (eq 'gtk:list-box
          (glib:symbol-for-gtype "GtkListBox")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkListBox")
          (g:gtype (cffi:foreign-funcall "gtk_list_box_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkListBox")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkListBox")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (glib-test:list-interfaces "GtkListBox")))
  ;; Check properties
  (is (equal '("accept-unpaired-release" "activate-on-single-click"
               "selection-mode" "show-separators")
             (glib-test:list-properties "GtkListBox")))
  ;; Check signals
  (is (equal '("activate-cursor-row" "move-cursor" "row-activated"
               "row-selected" "select-all" "selected-rows-changed"
               "toggle-cursor-row" "unselect-all")
             (glib-test:list-signals "GtkListBox")))
  ;; Check CSS name
  (is (string= "list"
               (gtk:widget-class-css-name "GtkListBox")))
  ;; Check accessible role
  (is (eq :list (gtk:widget-class-accessible-role "GtkListBox")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkListBox" GTK:LIST-BOX
                      (:SUPERCLASS GTK:WIDGET
                       :EXPORT T
                       :INTERFACES
                       ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
                       :TYPE-INITIALIZER "gtk_list_box_get_type")
                      ((ACCEPT-UNPAIRED-RELEASE LIST-BOX-ACCEPT-UNPAIRED-RELEASE
                        "accept-unpaired-release" "gboolean" T T)
                       (ACTIVATE-ON-SINGLE-CLICK
                        LIST-BOX-ACTIVATE-ON-SINGLE-CLICK
                        "activate-on-single-click" "gboolean" T T)
                       (SELECTION-MODE LIST-BOX-SELECTION-MODE
                        "selection-mode" "GtkSelectionMode" T T)
                       (SHOW-SEPARATORS LIST-BOX-SHOW-SEPARATORS
                        "show-separators" "gboolean" T T)))
             (gobject:get-gtype-definition "GtkListBox"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-list-box-properties
  (let ((listbox (make-instance 'gtk:list-box)))
    (is-false (gtk:list-box-accept-unpaired-release listbox))
    (is-true (gtk:list-box-activate-on-single-click listbox))
    (is (eq :single (gtk:list-box-selection-mode listbox)))
    (is-false (gtk:list-box-show-separators listbox))))

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

(test gtk-list-box-new
  (is (typep (gtk:list-box-new) 'gtk:list-box)))

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

;;; 2024-9-19

(in-package :gtk-test)

(def-suite gtk-notebook :in gtk-suite)
(in-suite gtk-notebook)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkNotebookTab

(test gtk-notebook-tab
  ;; Check the type
  (is (g:type-is-enum "GtkNotebookTab"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkNotebookTab")
          (g:gtype (cffi:foreign-funcall "gtk_notebook_tab_get_type" 
                                         :size))))
  ;; Check the registered name
  (is (eq 'gtk:notebook-tab
          (glib:symbol-for-gtype "GtkNotebookTab")))
  ;; Check the names
  (is (equal '("GTK_NOTEBOOK_TAB_FIRST" "GTK_NOTEBOOK_TAB_LAST")
             (list-enum-item-name "GtkNotebookTab")))
  ;; Check the values
  (is (equal '(0 1)
             (list-enum-item-value "GtkNotebookTab")))
  ;; Check the nick names
  (is (equal '("first" "last")
             (list-enum-item-nick "GtkNotebookTab")))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkNotebookTab" GTK-NOTEBOOK-TAB
                                     (:EXPORT T 
                                      :TYPE-INITIALIZER 
                                      "gtk_notebook_tab_get_type")
                                     (:FIRST 0)
                                     (:LAST 1))
             (gobject:get-g-type-definition "GtkNotebookTab"))))

;;;     GtkNotebookPage

(test gtk-notebook-page-class
  ;; Type check
  (is (g:type-is-object "GtkNotebookPage"))
  ;; Check the registered name
  (is (eq 'gtk:notebook-page
          (glib:symbol-for-gtype "GtkNotebookPage")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkNotebookPage")
          (g:gtype (cffi:foreign-funcall "gtk_notebook_page_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkNotebookPage")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkNotebookPage")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkNotebookPage")))
  ;; Check the properties
  (is (equal '("child" "detachable" "menu" "menu-label" "position" "reorderable" 
               "tab" "tab-expand" "tab-fill" "tab-label")
             (list-properties "GtkNotebookPage")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkNotebookPage")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkNotebookPage" GTK-NOTEBOOK-PAGE
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                                :TYPE-INITIALIZER "gtk_notebook_page_get_type")
                               ((CHILD GTK-NOTEBOOK-PAGE-CHILD "child"
                                 "GtkWidget" T NIL)
                                (DETACHABLE GTK-NOTEBOOK-PAGE-DETACHABLE
                                 "detachable" "gboolean" T T)
                                (MENU GTK-NOTEBOOK-PAGE-MENU "menu" "GtkWidget"
                                 T NIL)
                                (MENU-LABEL GTK-NOTEBOOK-PAGE-MENU-LABEL
                                 "menu-label" "gchararray" T T)
                                (POSITION GTK-NOTEBOOK-PAGE-POSITION "position"
                                          "gint" T T)
                                (REORDERABLE GTK-NOTEBOOK-PAGE-REORDERABLE
                                 "reorderable" "gboolean" T T)
                                (TAB GTK-NOTEBOOK-PAGE-TAB "tab" "GtkWidget" T
                                 NIL)
                                (TAB-EXPAND GTK-NOTEBOOK-PAGE-TAB-EXPAND
                                 "tab-expand" "gboolean" T T)
                                (TAB-FILL GTK-NOTEBOOK-PAGE-TAB-FILL "tab-fill"
                                 "gboolean" T T)
                                (TAB-LABEL GTK-NOTEBOOK-PAGE-TAB-LABEL
                                 "tab-label" "gchararray" T T)))
             (gobject:get-g-type-definition "GtkNotebookPage"))))

;;; --- Properties for GtkNotebookPage -----------------------------------------

;;;     child
;;;     detachable
;;;     menu
;;;     menu-label
;;;     position
;;;     reorderable
;;;     tab
;;;     tab-expand
;;;     tab-fill
;;;     tab-label

;;; --- Types and Values -------------------------------------------------------

;;;     GtkNotebook

(test gtk-notebook-class
  ;; Type check
  (is (g:type-is-object "GtkNotebook"))
  ;; Check the registered name
  (is (eq 'gtk:notebook
          (glib:symbol-for-gtype "GtkNotebook")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkNotebook")
          (g:gtype (cffi:foreign-funcall "gtk_notebook_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkNotebook")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkNotebook")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (list-interfaces "GtkNotebook")))
  ;; Check the properties
  (is (equal '("enable-popup" "group-name" "page" "pages" "scrollable" 
               "show-border" "show-tabs" "tab-pos")
             (list-properties "GtkNotebook")))
  ;; Check the signals
  (is (equal '("change-current-page" "create-window" "focus-tab" 
               "move-focus-out" "page-added" "page-removed" "page-reordered" 
               "reorder-tab" "select-page" "switch-page")
             (list-signals "GtkNotebook")))
  ;; CSS name
  (is (string= "notebook"
               (gtk:widget-class-css-name "GtkNotebook")))
  ;; Accessible role
  (is (eq :GROUP (gtk:widget-class-accessible-role "GtkNotebook")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkNotebook" GTK-NOTEBOOK
                               (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                                ("GtkAccessible" "GtkBuildable"
                                 "GtkConstraintTarget")
                                :TYPE-INITIALIZER "gtk_notebook_get_type")
                               ((ENABLE-POPUP GTK-NOTEBOOK-ENABLE-POPUP
                                 "enable-popup" "gboolean" T T)
                                (GROUP-NAME GTK-NOTEBOOK-GROUP-NAME
                                 "group-name" "gchararray" T T)
                                (PAGE GTK-NOTEBOOK-PAGE "page" "gint" T T)
                                (PAGES GTK-NOTEBOOK-PAGES "pages" "GListModel"
                                 T NIL)
                                (SCROLLABLE GTK-NOTEBOOK-SCROLLABLE
                                 "scrollable" "gboolean" T T)
                                (SHOW-BORDER GTK-NOTEBOOK-SHOW-BORDER
                                 "show-border" "gboolean" T T)
                                (SHOW-TABS GTK-NOTEBOOK-SHOW-TABS "show-tabs"
                                 "gboolean" T T)
                                (TAB-POS GTK-NOTEBOOK-TAB-POS "tab-pos"
                                 "GtkPositionType" T T)))
             (gobject:get-g-type-definition "GtkNotebook"))))

;;; --- Properties for GtkNotebook ---------------------------------------------

;;;     enable-popup
;;;     group-name
;;;     page
;;;     pages
;;;     scrollable
;;;     show-border
;;;     show-tabs
;;;     tab-pos

;;; --- Signals ----------------------------------------------------------------

;;;     change-current-page
;;;     create-window
;;;     focus-tab
;;;     move-focus-out
;;;     page-added
;;;     page-removed
;;;     page-reordered
;;;     reorder-tab
;;;     select-page
;;;     switch-page

;;; --- Functions --------------------------------------------------------------

;;;     gtk_notebook_new
;;;     gtk_notebook_append_page
;;;     gtk_notebook_append_page_menu
;;;     gtk_notebook_prepend_page
;;;     gtk_notebook_prepend_page_menu
;;;     gtk_notebook_insert_page
;;;     gtk_notebook_insert_page_menu
;;;     gtk_notebook_remove_page
;;;     gtk_notebook_detach_tab
;;;     gtk_notebook_page_num
;;;     gtk_notebook_next_page
;;;     gtk_notebook_prev_page
;;;     gtk_notebook_reorder_child
;;;     gtk_notebook_popup_enable
;;;     gtk_notebook_popup_disable
;;;     gtk_notebook_get_current_page
;;;     gtk_notebook_get_menu_label
;;;     gtk_notebook_get_nth_page
;;;     gtk_notebook_get_n_pages
;;;     gtk_notebook_get_tab_label
;;;     gtk_notebook_set_menu_label
;;;     gtk_notebook_set_menu_label_text
;;;     gtk_notebook_set_tab_label
;;;     gtk_notebook_set_tab_label_text
;;;     gtk_notebook_set_tab_reorderable
;;;     gtk_notebook_set_tab_detachable
;;;     gtk_notebook_get_menu_label_text
;;;     gtk_notebook_get_tab_label_text
;;;     gtk_notebook_get_tab_reorderable
;;;     gtk_notebook_get_tab_detachable
;;;     gtk_notebook_set_current_page
;;;     gtk_notebook_set_action_widget
;;;     gtk_notebook_get_action_widget

;;; --- 2023-8-31 --------------------------------------------------------------

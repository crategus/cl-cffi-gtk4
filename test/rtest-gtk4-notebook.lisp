(in-package :gtk-test)

(def-suite gtk-notebook :in gtk-suite)
(in-suite gtk-notebook)

;; Ensure initialization of the GtkNotebookPage type.
;; This should remove a warning from the testsuite, but it does not.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (g:type-ensure "GtkNotebookPages"))

;;; --- Types and Values -------------------------------------------------------

;;;     GtkNotebookTab

(test gtk-notebook-tab
  ;; Check type
  (is (g:type-is-enum "GtkNotebookTab"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkNotebookTab")
          (g:gtype (cffi:foreign-funcall "gtk_notebook_tab_get_type"
                                         :size))))
  ;; Check registered name
  (is (eq 'gtk:notebook-tab
          (glib:symbol-for-gtype "GtkNotebookTab")))
  ;; Check names
  (is (equal '("GTK_NOTEBOOK_TAB_FIRST" "GTK_NOTEBOOK_TAB_LAST")
             (gtk-test:list-enum-item-name "GtkNotebookTab")))
  ;; Check values
  (is (equal '(0 1)
             (gtk-test:list-enum-item-value "GtkNotebookTab")))
  ;; Check nick names
  (is (equal '("first" "last")
             (gtk-test:list-enum-item-nick "GtkNotebookTab")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkNotebookTab" GTK-NOTEBOOK-TAB
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "gtk_notebook_tab_get_type")
                                     (:FIRST 0)
                                     (:LAST 1))
             (gobject:get-g-type-definition "GtkNotebookTab"))))

;;;     GtkNotebookPage

(test gtk-notebook-page-class
  ;; Check type
  (is (g:type-is-object "GtkNotebookPage"))
  ;; Check registered name
  (is (eq 'gtk:notebook-page
          (glib:symbol-for-gtype "GtkNotebookPage")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkNotebookPage")
          (g:gtype (cffi:foreign-funcall "gtk_notebook_page_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkNotebookPage")))
  ;; Check children
  (is (equal '()
             (gtk-test:list-children "GtkNotebookPage")))
  ;; Check interfaces
  (is (equal '()
             (gtk-test:list-interfaces "GtkNotebookPage")))
  ;; Check properties
  (is (equal '("child" "detachable" "menu" "menu-label" "position" "reorderable"
               "tab" "tab-expand" "tab-fill" "tab-label")
             (gtk-test:list-properties "GtkNotebookPage")))
  ;; Check signals
  (is (equal '()
             (gtk-test:list-signals "GtkNotebookPage")))
  ;; Check class definition
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

(test gtk-notebook-page-properties
  (let ((page (make-instance 'gtk:notebook-page
                             :child (make-instance 'gtk:box))))
    (is (typep (gtk:notebook-page-child page) 'gtk:box))
    (is-false (gtk:notebook-page-detachable page))
    (is-false (gtk:notebook-page-menu page))
    (is-false (gtk:notebook-page-menu-label page))
    (is (= 0 (gtk:notebook-page-position page)))
    (is-false (gtk:notebook-page-reorderable page))
    (is-false (gtk:notebook-page-tab page))
    (is-false (gtk:notebook-page-tab-expand page))
    (is-true (gtk:notebook-page-tab-fill page))
    (is-false (gtk:notebook-page-tab-label page))))

;;; --- Types and Values -------------------------------------------------------

;;;     GtkNotebook

(test gtk-notebook-class
  ;; Check type
  (is (g:type-is-object "GtkNotebook"))
  ;; Check registered name
  (is (eq 'gtk:notebook
          (glib:symbol-for-gtype "GtkNotebook")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkNotebook")
          (g:gtype (cffi:foreign-funcall "gtk_notebook_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkNotebook")))
  ;; Check children
  (is (equal '()
             (gtk-test:list-children "GtkNotebook")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (gtk-test:list-interfaces "GtkNotebook")))
  ;; Check properties
  (is (equal '("enable-popup" "group-name" "page" "pages" "scrollable"
               "show-border" "show-tabs" "tab-pos")
             (gtk-test:list-properties "GtkNotebook")))
  ;; Check signals
  (is (equal '("change-current-page" "create-window" "focus-tab"
               "move-focus-out" "page-added" "page-removed" "page-reordered"
               "reorder-tab" "select-page" "switch-page")
             (gtk-test:list-signals "GtkNotebook")))
  ;; Check CSS name
  (is (string= "notebook"
               (gtk:widget-class-css-name "GtkNotebook")))
  ;; Check accessible role
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

(test gtk-notebook-properties
  (let ((notebook (make-instance 'gtk:notebook)))
    (is-false (gtk:notebook-enable-popup notebook))
    (is-false (gtk:notebook-group-name notebook))
    (is (= -1 (gtk:notebook-page notebook)))
    (is (typep (gtk:notebook-pages notebook) 'g:object))
    (is-false (gtk:notebook-scrollable notebook))
    (is-true (gtk:notebook-show-border notebook))
    (is-true (gtk:notebook-show-tabs notebook))
    (is (eq :top (gtk:notebook-tab-pos notebook)))))

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

(test gtk-notebook-new
  (is (typep (gtk:notebook-new) 'gtk:notebook)))

;;;     gtk_notebook_append_page
;;;     gtk_notebook_append_page_menu
;;;     gtk_notebook_prepend_page
;;;     gtk_notebook_prepend_page_menu
;;;     gtk_notebook_insert_page
;;;     gtk_notebook_insert_page_menu

;;;     gtk_notebook_remove_page
;;;     gtk_notebook_get_nth_page

(test gkt-notebook-remove-page
  (let ((notebook (gtk:notebook-new))
        (page1 (gtk:frame-new))
        (page2 (gtk:frame-new))
        (label (gtk:label-new "label")))

    (is (= 0 (gtk:notebook-add-page notebook page1 nil)))
    (is (= 1 (gtk:notebook-add-page notebook page2 label)))

    (is (eq page1 (gtk:notebook-nth-page notebook 0)))
    (is (eq page2 (gtk:notebook-nth-page notebook 1)))

    (is-false (gtk:notebook-remove-page notebook 0))
    (is-false (gtk:notebook-remove-page notebook page2))))

;;;     gtk_notebook_detach_tab

;;;     gtk_notebook_page_num

(test gkt-notebook-page-num
  (let ((notebook (gtk:notebook-new))
        (page1 (gtk:frame-new))
        (page2 (gtk:frame-new))
        (label (gtk:label-new "label")))

    (is (= 0 (gtk:notebook-add-page notebook page1 nil)))
    (is (= 1 (gtk:notebook-add-page notebook page2 label)))

    (is (= 0 (gtk:notebook-page-num notebook page1)))
    (is (= 1 (gtk:notebook-page-num notebook page2)))))

;;;     gtk_notebook_next_page
;;;     gtk_notebook_prev_page

;;;     gtk_notebook_reorder_child

(test gkt-notebook-reorder-child
  (let ((notebook (gtk:notebook-new))
        (page1 (gtk:frame-new))
        (page2 (gtk:frame-new))
        (label (gtk:label-new "label")))

    (is (= 0 (gtk:notebook-add-page notebook page1 nil)))
    (is (= 1 (gtk:notebook-add-page notebook page2 label)))

    (is-false (gtk:notebook-reorder-child notebook page2 0))

    (is (= 1 (gtk:notebook-page-num notebook page1)))
    (is (= 0 (gtk:notebook-page-num notebook page2)))))

;;;     gtk_notebook_popup_enable
;;;     gtk_notebook_popup_disable
;;;     gtk_notebook_get_current_page
;;;     gtk_notebook_get_menu_label
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

;;; 2024-6-30

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
          (g:gtype (cffi:foreign-funcall "gtk_notebook_tab_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:notebook-tab
          (glib:symbol-for-gtype "GtkNotebookTab")))
  ;; Check names
  (is (equal '("GTK_NOTEBOOK_TAB_FIRST" "GTK_NOTEBOOK_TAB_LAST")
             (glib-test:list-enum-item-names "GtkNotebookTab")))
  ;; Check values
  (is (equal '(0 1)
             (glib-test:list-enum-item-values "GtkNotebookTab")))
  ;; Check nick names
  (is (equal '("first" "last")
             (glib-test:list-enum-item-nicks "GtkNotebookTab")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkNotebookTab" GTK:NOTEBOOK-TAB
                       (:EXPORT T
                        :TYPE-INITIALIZER "gtk_notebook_tab_get_type")
                       (:FIRST 0)
                       (:LAST 1))
             (gobject:get-gtype-definition "GtkNotebookTab"))))

;;;     GtkNotebookPages

;; TODO: GtkNotebookPages is not exported from the C library. In a first run
;; of the tests for GtkNotebookPages does not work.

#+nil
(test gtk-notebook-pages-class
  ;; Check type
  (is (g:type-is-object "GtkNotebookPages"))
  ;; Check registered name
  (is (eq 'gtk:notebook-pages
          (glib:symbol-for-gtype "GtkNotebookPages")))
  ;; Check type initializer
  #+nil
  (is (eq (g:gtype "GtkNotebookPages")
          (g:gtype (cffi:foreign-funcall "gtk_notebook_pages_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkNotebookPages")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkNotebookPages")))
  ;; Check interfaces
  (is (equal '("GListModel" "GtkSelectionModel")
             (glib-test:list-interfaces "GtkNotebookPages")))
  ;; Check properties
  (is (equal '()
             (glib-test:list-properties "GtkNotebookPages")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkNotebookPages")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkNotebookPages" GTK:NOTEBOOK-PAGES
                       (:SUPERCLASS GOBJECT:OBJECT
                        :EXPORT T
                        :INTERFACES ("GListModel" "GtkSelectionModel"))
                       NIL)
             (gobject:get-gtype-definition "GtkNotebookPages"))))

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
             (glib-test:list-children "GtkNotebookPage")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkNotebookPage")))
  ;; Check properties
  (is (equal '("child" "detachable" "menu" "menu-label" "position" "reorderable"
               "tab" "tab-expand" "tab-fill" "tab-label")
             (glib-test:list-properties "GtkNotebookPage")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkNotebookPage")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkNotebookPage" GTK:NOTEBOOK-PAGE
                       (:SUPERCLASS GOBJECT:OBJECT
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_notebook_page_get_type")
                       ((CHILD NOTEBOOK-PAGE-CHILD "child" "GtkWidget" T NIL)
                        (DETACHABLE NOTEBOOK-PAGE-DETACHABLE
                         "detachable" "gboolean" T T)
                        (MENU NOTEBOOK-PAGE-MENU "menu" "GtkWidget" T NIL)
                        (MENU-LABEL NOTEBOOK-PAGE-MENU-LABEL
                         "menu-label" "gchararray" T T)
                        (POSITION NOTEBOOK-PAGE-POSITION "position" "gint" T T)
                        (REORDERABLE NOTEBOOK-PAGE-REORDERABLE
                         "reorderable" "gboolean" T T)
                        (TAB NOTEBOOK-PAGE-TAB "tab" "GtkWidget" T NIL)
                        (TAB-EXPAND NOTEBOOK-PAGE-TAB-EXPAND
                         "tab-expand" "gboolean" T T)
                        (TAB-FILL NOTEBOOK-PAGE-TAB-FILL
                         "tab-fill" "gboolean" T T)
                        (TAB-LABEL NOTEBOOK-PAGE-TAB-LABEL
                         "tab-label" "gchararray" T T)))
             (gobject:get-gtype-definition "GtkNotebookPage"))))

;;; --- Properties for GtkNotebookPage -----------------------------------------

(test gtk-notebook-page-properties
  (let* ((box (make-instance 'gtk:box))
         (page (make-instance 'gtk:notebook-page
                              :child box)))
    (is (eq box (gtk:notebook-page-child page)))
    (is-false (gtk:notebook-page-detachable page))
    (is-false (gtk:notebook-page-menu page))
    (is-false (gtk:notebook-page-menu-label page))
    (is (= 0 (gtk:notebook-page-position page)))
    (is-false (gtk:notebook-page-reorderable page))
    (is-false (gtk:notebook-page-tab page))
    (is-false (gtk:notebook-page-tab-expand page))
    (is-true (gtk:notebook-page-tab-fill page))
    (is-false (gtk:notebook-page-tab-label page))
    ;; Check memory management
    (is (= 1 (g:object-ref-count page)))
    (is (= 2 (g:object-ref-count box)))))

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
             (glib-test:list-children "GtkNotebook")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (glib-test:list-interfaces "GtkNotebook")))
  ;; Check properties
  (is (equal '("enable-popup" "group-name" "page" "pages" "scrollable"
               "show-border" "show-tabs" "tab-pos")
             (glib-test:list-properties "GtkNotebook")))
  ;; Check signals
  (is (equal '("change-current-page" "create-window" "focus-tab"
               "move-focus-out" "page-added" "page-removed" "page-reordered"
               "reorder-tab" "select-page" "switch-page")
             (glib-test:list-signals "GtkNotebook")))
  ;; Check CSS name
  (is (string= "notebook"
               (gtk:widget-class-css-name "GtkNotebook")))
  ;; Check accessible role
  (is (eq :GROUP (gtk:widget-class-accessible-role "GtkNotebook")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkNotebook" GTK:NOTEBOOK
                       (:SUPERCLASS GTK:WIDGET
                        :EXPORT T
                        :INTERFACES
                        ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
                        :TYPE-INITIALIZER "gtk_notebook_get_type")
                       ((ENABLE-POPUP NOTEBOOK-ENABLE-POPUP
                         "enable-popup" "gboolean" T T)
                        (GROUP-NAME NOTEBOOK-GROUP-NAME
                         "group-name" "gchararray" T T)
                        (PAGE NOTEBOOK-PAGE "page" "gint" T T)
                        (PAGES NOTEBOOK-PAGES "pages" "GListModel" T NIL)
                        (SCROLLABLE NOTEBOOK-SCROLLABLE
                         "scrollable" "gboolean" T T)
                        (SHOW-BORDER NOTEBOOK-SHOW-BORDER
                         "show-border" "gboolean" T T)
                        (SHOW-TABS NOTEBOOK-SHOW-TABS "show-tabs" "gboolean" T T)
                        (TAB-POS NOTEBOOK-TAB-POS
                         "tab-pos" "GtkPositionType" T T)))
             (gobject:get-gtype-definition "GtkNotebook"))))

;;; --- Signals ----------------------------------------------------------------

;;;     change-current-page

(test gtk-notebook-change-current-page-signal
  (let* ((name "change-current-page")
         (gtype (g:gtype "GtkNotebook"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:ACTION :RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "gboolean") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("gint")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     create-window

(test gtk-notebook-create-window-signal
  (let* ((name "create-window")
         (gtype (g:gtype "GtkNotebook"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "GtkNotebook") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("GtkWidget")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     focus-tab

(test gtk-notebook-focus-tab-signal
  (let* ((name "focus-tab")
         (gtype (g:gtype "GtkNotebook"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:ACTION :RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "gboolean") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("GtkNotebookTab")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     move-focus-out

(test gtk-notebook-move-focus-out-signal
  (let* ((name "move-focus-out")
         (gtype (g:gtype "GtkNotebook"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:ACTION :RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("GtkDirectionType")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     page-added

(test gtk-notebook-page-added-signal
  (let* ((name "page-added")
         (gtype (g:gtype "GtkNotebook"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(::RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("GtkWidget" "guint")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     page-removed

(test gtk-notebook-page-removed-signal
  (let* ((name "page-removed")
         (gtype (g:gtype "GtkNotebook"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("GtkWidget" "guint")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     page-reordered

(test gtk-notebook-page-reordered-signal
  (let* ((name "page-reordered")
         (gtype (g:gtype "GtkNotebook"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("GtkWidget" "guint")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     reorder-tab

(test gtk-notebook-reorder-tab-signal
  (let* ((name "reorder-tab")
         (gtype (g:gtype "GtkNotebook"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:ACTION :RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "gboolean") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("GtkDirectionType" "gboolean")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     select-page

(test gtk-notebook-select-page-signal
  (let* ((name "select-page")
         (gtype (g:gtype "GtkNotebook"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:ACTION :RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "gboolean") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("gboolean")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     switch-page

(test gtk-notebook-switch-page-signal
  (let* ((name "switch-page")
         (gtype (g:gtype "GtkNotebook"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("GtkWidget" "guint")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Properties for GtkNotebook ---------------------------------------------

(test gtk-notebook-properties
  (let ((notebook (make-instance 'gtk:notebook))
        (page (make-instance 'gtk:box)))
    ;; Add a page to notebook
    (is (= 0 (gtk:notebook-add-page notebook page nil)))
    ;; Read the properties
    (is-false (gtk:notebook-enable-popup notebook))
    (is-false (gtk:notebook-group-name notebook))
    (is (= 0 (gtk:notebook-page notebook)))
    #+nil
    (is (typep (gtk:notebook-pages notebook) 'gtk:notebook-pages))
    (is-false (gtk:notebook-scrollable notebook))
    (is-true (gtk:notebook-show-border notebook))
    (is-true (gtk:notebook-show-tabs notebook))
    (is (eq :top (gtk:notebook-tab-pos notebook)))
    ;; Remove page from notebook
    (is-false (gtk:notebook-remove-page notebook page))))

(test gtk-notebook-pages
  (let ((notebook (make-instance 'gtk:notebook))
        (page1 (gtk:box-new))
        (page2 (gtk:frame-new))
        (page3 (gtk:paned-new))
        (pages nil))
    ;; Add first page to notebook
    (is (= 1 (g:object-ref-count page1)))
    (is (= 0 (gtk:notebook-add-page notebook page1 nil)))
    (is (= 4 (g:object-ref-count page1))) ; Value is 4 and not 2. Why?
    ;; Add more pages to notebook
    (is (= 1 (gtk:notebook-add-page notebook page2 nil)))
    (is (= 2 (gtk:notebook-add-page notebook page3 nil)))
    ;; Retrieve list model
    (is (typep (setf pages (gtk:notebook-pages notebook)) 'gtk:notebook-pages))
    (is (= 2 (g:object-ref-count pages)))
    (is (eq (g:gtype "GtkNotebookPage") (g:list-model-item-type pages)))
    (is (= 3 (g:list-model-n-items pages)))
    ;; Retrieve notebook page from list model
    (is (typep (g:list-model-object pages 0) 'gtk:notebook-page))
    (is (= 2 (g:object-ref-count (g:list-model-object pages 0))))
    (is (typep (gtk:notebook-page-child (g:list-model-object pages 0)) 'gtk:box))
    (is (eq page1 (gtk:notebook-page-child (g:list-model-object pages 0))))
    (is (= 4 (g:object-ref-count
                 (gtk:notebook-page-child (g:list-model-object pages 0)))))
    (is (= 4 (g:object-ref-count page1)))
    ;; Retrieve more notebook pages
    (is (typep (g:list-model-object pages 1) 'gtk:notebook-page))
    (is (typep (g:list-model-object pages 2) 'gtk:notebook-page))

    (is-false (gtk:notebook-remove-page notebook page1))
    (is-false (gtk:notebook-remove-page notebook page2))
    (is-false (gtk:notebook-remove-page notebook page3))

    (is (= 0 (g:list-model-n-items pages)))
    ;; TODO: We have a strong reference to PAGES and PAGE1, PAGE2, PAGE3.
    ;; Can we remove the strong references?
    (is (= 2 (g:object-ref-count page1)))
    (is (= 2 (g:object-ref-count page2)))
    (is (= 2 (g:object-ref-count page3)))
    (is (= 2 (g:object-ref-count pages)))
    (is (= 1 (g:object-ref-count notebook)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_notebook_new

(test gtk-notebook-new
  (is (typep (gtk:notebook-new) 'gtk:notebook))
  (is (= 1 (g:object-ref-count (gtk:notebook-new))))
  ;; Create with make-instance
  (is (typep (make-instance 'gtk:notebook) 'gtk:notebook))
  (is (= 1 (g:object-ref-count (make-instance 'gtk:notebook))))
  ;; Create with g:object-new
  (is (typep (g:object-new "GtkNotebook") 'gtk:notebook))
  (is (= 1 (g:object-ref-count (g:object-new "GtkNotebook")))))

;;;     gtk_notebook_get_n_pages
;;;     gtk_notebook_get_nth_page

(test gtk-notebook-n-pages/nth-page
  (let ((notebook (gtk:notebook-new))
        (page1 (gtk:frame-new))
        (page2 (gtk:box-new))
        (page3 (gtk:paned-new)))
    ;; Add pages to notebook
    (is (= 0 (gtk:notebook-add-page notebook page1 nil)))
    (is (= 1 (gtk:notebook-add-page notebook page2 nil)))
    (is (= 2 (gtk:notebook-add-page notebook page3 nil)))
    ;; Get number of pages
    (is (= 3 (gtk:notebook-n-pages notebook)))
    ;; Retrieve widgets from index
    (is (eq page1 (gtk:notebook-nth-page notebook 0)))
    (is (eq page2 (gtk:notebook-nth-page notebook 1)))
    (is (eq page3 (gtk:notebook-nth-page notebook 2)))
    ;; Check memory management
    (is-false (gtk:notebook-remove-page notebook page1))
    (is-false (gtk:notebook-remove-page notebook page2))
    (is-false (gtk:notebook-remove-page notebook page3))
    (is (= 1 (g:object-ref-count page1)))
    (is (= 1 (g:object-ref-count page2)))
    (is (= 1 (g:object-ref-count page3)))
    (is (= 1 (g:object-ref-count notebook)))))

;;;     gtk_notebook_page_num

(test gtk-notebook-page-num
  (let ((notebook (gtk:notebook-new))
        (page1 (gtk:frame-new))
        (page2 (gtk:frame-new))
        (label (gtk:label-new "label")))
    ;; Add pages to notebook
    (is (= 0 (gtk:notebook-add-page notebook page1 nil)))
    (is (= 1 (gtk:notebook-add-page notebook page2 label)))
    ;; Retrieve index from child widget
    (is (= 0 (gtk:notebook-page-num notebook page1)))
    (is (= 1 (gtk:notebook-page-num notebook page2)))
    ;; Check memory management
    (is-false (gtk:notebook-remove-page notebook page1))
    (is-false (gtk:notebook-remove-page notebook page2))
    (is (= 2 (g:object-ref-count label)))
    (is (= 1 (g:object-ref-count page1)))
    (is (= 1 (g:object-ref-count page2)))
    (is (= 1 (g:object-ref-count notebook)))))

;;;     gtk_notebook_get_current_page
;;;     gtk_notebook_set_current_page

(test gtk-notebook-current-page
  (let ((notebook (gtk:notebook-new))
        (page1 (gtk:frame-new))
        (page2 (gtk:box-new))
        (page3 (gtk:paned-new)))
    ;; Add pages to notebook
    (is (= 0 (gtk:notebook-add-page notebook page1 nil)))
    (is (= 1 (gtk:notebook-add-page notebook page2 nil)))
    (is (= 2 (gtk:notebook-add-page notebook page3 nil)))
    (is (= 0 (gtk:notebook-current-page notebook)))
    (is (= 2 (setf (gtk:notebook-current-page notebook) 2)))
    (is (eq page3
            (gtk:notebook-nth-page notebook
                                   (gtk:notebook-current-page notebook))))
    (is (= 1 (setf (gtk:notebook-current-page notebook) 1)))
    (is (eq page2
            (gtk:notebook-nth-page notebook
                                   (gtk:notebook-current-page notebook))))
    ;; Check memory management
    (is-false (gtk:notebook-remove-page notebook page1))
    (is-false (gtk:notebook-remove-page notebook page2))
    (is-false (gtk:notebook-remove-page notebook page3))))

;;;     gtk_notebook_next_page
;;;     gtk_notebook_prev_page

(test gtk-notebook-next/prev-page
  (let ((notebook (gtk:notebook-new))
        (page1 (gtk:frame-new))
        (page2 (gtk:box-new))
        (page3 (gtk:paned-new)))
    ;; Add pages to notebook
    (is (= 0 (gtk:notebook-add-page notebook page1 nil)))
    (is (= 1 (gtk:notebook-add-page notebook page2 nil)))
    (is (= 2 (gtk:notebook-add-page notebook page3 nil)))
    (is (= 0 (gtk:notebook-current-page notebook)))
    (is-false (gtk:notebook-next-page notebook))
    (is (= 1 (gtk:notebook-current-page notebook)))
    (is-false (gtk:notebook-next-page notebook))
    (is (= 2 (gtk:notebook-current-page notebook)))
    (is-false (gtk:notebook-prev-page notebook))
    (is (= 1 (gtk:notebook-current-page notebook)))
    (is-false (gtk:notebook-prev-page notebook))
    (is (= 0 (gtk:notebook-current-page notebook)))
    ;; Check memory management
    (is-false (gtk:notebook-remove-page notebook page1))
    (is-false (gtk:notebook-remove-page notebook page2))
    (is-false (gtk:notebook-remove-page notebook page3))))

;;;     gtk_notebook_append_page
;;;     gtk_notebook_append_page_menu
;;;     gtk_notebook_prepend_page
;;;     gtk_notebook_prepend_page_menu
;;;     gtk_notebook_insert_page
;;;     gtk_notebook_insert_page_menu

(test gtk-notebook-add-page
  (let ((notebook (gtk:notebook-new))
        (page1 (gtk:frame-new))
        (page2 (gtk:box-new))
        (page3 (gtk:paned-new))
        (label1 (gtk:label-new "page1"))
        (label2 (gtk:label-new "page2"))
        (label3 (gtk:label-new "page3")))
    (is (= 0 (gtk:notebook-add-page notebook page1 label1)))
    (is (= 0 (gtk:notebook-add-page notebook page2 label2 :pos :start)))
    (is (= 2 (gtk:notebook-add-page notebook page3 label3 :pos :end)))
    (is (typep (gtk:notebook-nth-page notebook 0) 'gtk:box))
    (is (typep (gtk:notebook-nth-page notebook 1) 'gtk:frame))
    (is (typep (gtk:notebook-nth-page notebook 2) 'gtk:paned))
    ;; Check memory management
    (is-false (gtk:notebook-remove-page notebook page1))
    (is-false (gtk:notebook-remove-page notebook page2))
    (is-false (gtk:notebook-remove-page notebook page3))))

;;;     gtk_notebook_remove_page

(test gtk-notebook-remove-page
  (let ((notebook (gtk:notebook-new))
        (page1 (gtk:frame-new))
        (page2 (gtk:frame-new))
        (label (gtk:label-new "label")))
    (is (= 1 (g:object-ref-count page1)))
    (is (= 0 (gtk:notebook-add-page notebook page1 nil)))
    (is (= 4 (g:object-ref-count page1)))
    (is (= 1 (gtk:notebook-add-page notebook page2 label)))
    (is (eq page1 (gtk:notebook-nth-page notebook 0)))
    (is (eq page2 (gtk:notebook-nth-page notebook 1)))
    ;; Remove with index
    (is-false (gtk:notebook-remove-page notebook 0))
    (is (= 1 (g:object-ref-count page1)))
    ;; Remove with child widget
    (is-false (gtk:notebook-remove-page notebook page2))))

;;;     gtk_notebook_detach_tab

(test gtk-notebook-detach-tab
  (let ((notebook (gtk:notebook-new))
         (page1 (gtk:frame-new))
         (page2 (gtk:box-new)))
    (is (= 1 (g:object-ref-count page1)))
    (is (= 0 (gtk:notebook-add-page notebook page1 nil)))
    (is (= 1 (gtk:notebook-add-page notebook page2 nil)))
    ;; Detach with index
    (is-false (gtk:notebook-detach-tab notebook 0))
    (is (= 1 (g:object-ref-count page1)))
    ;; Detach with widget
    (is-false (gtk:notebook-detach-tab notebook page2))
    (is (= 1 (g:object-ref-count page2)))))

;;;     gtk_notebook_reorder_child

(test gtk-notebook-reorder-child
  (let ((notebook (gtk:notebook-new))
        (page1 (gtk:frame-new))
        (page2 (gtk:frame-new))
        (label (gtk:label-new "label")))
    (is (= 0 (gtk:notebook-add-page notebook page1 nil)))
    (is (= 1 (gtk:notebook-add-page notebook page2 label)))
    (is-false (gtk:notebook-reorder-child notebook page2 0))
    (is (= 1 (gtk:notebook-page-num notebook page1)))
    (is (= 0 (gtk:notebook-page-num notebook page2)))
    ;; Check memory management
    (is-false (gtk:notebook-remove-page notebook page1))
    (is-false (gtk:notebook-remove-page notebook page2))))

;;;     gtk_notebook_popup_enable
;;;     gtk_notebook_popup_disable

(test gtk-notebook-popup-enable/disable
  (let ((notebook (gtk:notebook-new))
        (page (gtk:box-new)))
    (is (= 0 (gtk:notebook-add-page notebook page nil)))
    (is-false (gtk:notebook-enable-popup notebook))
    (is-true (gtk:notebook-popup-enable notebook))
    (is-true (gtk:notebook-enable-popup notebook))
    (is-false (gtk:notebook-popup-disable notebook))
    (is-false (gtk:notebook-enable-popup notebook))
    (is-false (gtk:notebook-remove-page notebook page))))

;;;     gtk_notebook_get_tab_detachable
;;;     gtk_notebook_set_tab_detachable

(test gtk-notebook-tab-detachable
  (let ((notebook (gtk:notebook-new))
        (page (gtk:box-new))
        (pages nil))
    (is (= 0 (gtk:notebook-add-page notebook page nil)))
    (is (typep (setf pages (gtk:notebook-pages notebook)) 'gtk:notebook-pages))
    (is (typep (g:list-model-object pages 0) 'gtk:notebook-page))
    (is (typep (gtk:notebook-page-child (g:list-model-object pages 0)) 'gtk:box))
    (is-false (gtk:notebook-tab-detachable notebook page))
    (is-false (gtk:notebook-page-detachable (g:list-model-object pages 0)))
    (is-true (setf (gtk:notebook-tab-detachable notebook page) t))
    (is-true (gtk:notebook-tab-detachable notebook page))
    (is-true (gtk:notebook-page-detachable (g:list-model-object pages 0)))

    (is-false (gtk:notebook-remove-page notebook page))))

;;;     gtk_notebook_get_tab_reorderable
;;;     gtk_notebook_set_tab_reorderable

(test gtk-notebook-tab-reorderable
  (let ((notebook (gtk:notebook-new))
        (page (gtk:box-new))
        (pages nil))
    (is (= 0 (gtk:notebook-add-page notebook page nil)))
    (is (typep (setf pages (gtk:notebook-pages notebook)) 'gtk:notebook-pages))
    (is (typep (g:list-model-object pages 0) 'gtk:notebook-page))
    (is (typep (gtk:notebook-page-child (g:list-model-object pages 0)) 'gtk:box))
    (is-false (gtk:notebook-tab-reorderable notebook page))
    (is-false (gtk:notebook-page-reorderable (g:list-model-object pages 0)))
    (is-true (setf (gtk:notebook-tab-reorderable notebook page) t))
    (is-true (gtk:notebook-tab-reorderable notebook page))
    (is-true (gtk:notebook-page-reorderable (g:list-model-object pages 0)))

    (is-false (gtk:notebook-remove-page notebook page))))

;;;     gtk_notebook_get_menu_label
;;;     gtk_notebook_set_menu_label

(test gtk-notebook-menu-label
  (let ((notebook (gtk:notebook-new))
        (page (gtk:box-new))
        (label (gtk:label-new "menu"))
        (pages nil))
    (is (= 0 (gtk:notebook-add-page notebook page nil)))
    (is (typep (setf pages (gtk:notebook-pages notebook)) 'gtk:notebook-pages))
    (is (eq label (setf (gtk:notebook-menu-label notebook page) label)))
    (is (eq label (gtk:notebook-menu-label notebook page)))
    (is (eq label (gtk:notebook-page-menu (g:list-model-object pages 0))))

    (is-false (gtk:notebook-remove-page notebook page))))

;;;     gtk_notebook_get_menu_label_text
;;;     gtk_notebook_set_menu_label_text

(test gtk-notebook-menu-label-text
  (let ((notebook (gtk:notebook-new))
        (page (gtk:box-new))
        (label (gtk:label-new "no label"))
        (pages nil))
    (is (= 0 (gtk:notebook-add-page notebook page nil :menu label)))
    (is (typep (setf pages (gtk:notebook-pages notebook)) 'gtk:notebook-pages))
    (is (string= "menu"
                 (setf (gtk:notebook-menu-label-text notebook page) "menu")))
    (is (string= "menu" (gtk:notebook-menu-label-text notebook page)))
    (is (string= "menu"
                 (gtk:label-label
                     (gtk:notebook-page-menu (g:list-model-object pages 0)))))
    (is-false (gtk:notebook-remove-page notebook page))))

(test gtk-notebook-menu-label-text.2
  (let ((notebook (gtk:notebook-new))
        (page (gtk:box-new))
        (pages nil))
    (is (= 0 (gtk:notebook-add-page notebook page nil)))
    (is (typep (setf pages (gtk:notebook-pages notebook)) 'gtk:notebook-pages))
    (is (string= "menu"
                 (setf (gtk:notebook-menu-label-text notebook page) "menu")))
    (is (string= "menu" (gtk:notebook-menu-label-text notebook page)))
    ;; FIXME: Should return the string, but returns nil
    (is-false (gtk:notebook-page-menu-label (g:list-model-object pages 0)))
    (is-false (gtk:notebook-remove-page notebook page))))

;;;     gtk_notebook_get_tab_label
;;;     gtk_notebook_set_tab_label

(test gtk-notebook-tab-label
  (let ((notebook (gtk:notebook-new))
        (page (gtk:box-new))
        (label (gtk:label-new "tab"))
        (pages nil))
    (is (= 0 (gtk:notebook-add-page notebook page nil)))
    (is (typep (setf pages (gtk:notebook-pages notebook)) 'gtk:notebook-pages))
    (is (eq label (setf (gtk:notebook-menu-label notebook page) label)))
    (is (eq label (gtk:notebook-menu-label notebook page)))
    (is (eq label (gtk:notebook-page-menu (g:list-model-object pages 0))))
    (is-false (gtk:notebook-remove-page notebook page))))

;;;     gtk_notebook_set_tab_label_text
;;;     gtk_notebook_get_tab_label_text

(test gtk-notebook-tab-label-text
  (let ((notebook (gtk:notebook-new))
        (page (gtk:box-new))
        (pages nil))
    (is (= 0 (gtk:notebook-add-page notebook page nil)))
    (is (typep (setf pages (gtk:notebook-pages notebook)) 'gtk:notebook-pages))
    (is (string= "tab"
                 (setf (gtk:notebook-tab-label-text notebook page) "tab")))
    (is (string= "tab" (gtk:notebook-tab-label-text notebook page)))
    ;; FIXME: Should return the string, but returns nil
    (is-false (gtk:notebook-page-tab-label (g:list-model-object pages 0)))
    (is-false (gtk:notebook-remove-page notebook page))))

;;;     gtk_notebook_set_action_widget
;;;     gtk_notebook_get_action_widget

(test gtk-notebook-action-widget
  (let ((notebook (gtk:notebook-new))
        (widget (gtk:button-new)))
    (is-false (gtk:notebook-action-widget notebook :start))
    (is-false (gtk:notebook-action-widget notebook :end))

    (is (eq widget (setf (gtk:notebook-action-widget notebook :start) widget)))
    (is (eq widget (gtk:notebook-action-widget notebook :start)))

    (is-false (setf (gtk:notebook-action-widget notebook :start) nil))))

;;; 2024-11-10

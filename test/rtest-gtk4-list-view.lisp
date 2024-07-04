(in-package :gtk-test)

(def-suite gtk-list-view :in gtk-suite)
(in-suite gtk-list-view)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkListTabBehavior

(test gtk-list-tab-behavior
  ;; Check type
  (is (g:type-is-enum "GtkListTabBehavior"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkListTabBehavior")
          (g:gtype (cffi:foreign-funcall "gtk_list_tab_behavior_get_type"
                                         :size))))
  ;; Check registered name
  (is (eq 'gtk:list-tab-behavior
          (glib:symbol-for-gtype "GtkListTabBehavior")))
  ;; Check names
  (is (equal '("GTK_LIST_TAB_ALL" "GTK_LIST_TAB_ITEM" "GTK_LIST_TAB_CELL")
             (gtk-test:list-enum-item-name "GtkListTabBehavior")))
  ;; Check values
  (is (equal '(0 1 2)
             (gtk-test:list-enum-item-value "GtkListTabBehavior")))
  ;; Check nick names
  (is (equal '("all" "item" "cell")
             (gtk-test:list-enum-item-nick "GtkListTabBehavior")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkListTabBehavior"
                                     GTK-LIST-TAB-BEHAVIOR
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "gtk_list_tab_behavior_get_type")
                                     (:ALL 0)
                                     (:ITEM 1)
                                     (:CELL 2))
             (gobject:get-g-type-definition "GtkListTabBehavior"))))

;;;     GtkListScrollFlags

(test gtk-list-scroll-flags
  ;; Check type
  (is (g:type-is-flags "GtkListScrollFlags"))
  ;; Check registered name
  (is (eq 'gtk:list-scroll-flags
          (glib:symbol-for-gtype "GtkListScrollFlags")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkListScrollFlags")
          (g:gtype (cffi:foreign-funcall "gtk_list_scroll_flags_get_type"
                                         :size))))
  ;; Check names
  (is (equal '("GTK_LIST_SCROLL_NONE" "GTK_LIST_SCROLL_FOCUS"
               "GTK_LIST_SCROLL_SELECT")
             (gtk-test:list-flags-item-name "GtkListScrollFlags")))
  ;; Check values
  (is (equal '(0 1 2)
             (gtk-test:list-flags-item-value "GtkListScrollFlags")))
  ;; Check nick names
  (is (equal '("none" "focus" "select")
             (gtk-test:list-flags-item-nick "GtkListScrollFlags")))
  ;; Check flags definition
  (is (equal '(GOBJECT:DEFINE-G-FLAGS "GtkListScrollFlags"
                                      GTK-LIST-SCROLL-FLAGS
                                      (:EXPORT T
                                       :TYPE-INITIALIZER
                                       "gtk_list_scroll_flags_get_type")
                                      (:NONE 0)
                                      (:FOCUS 1)
                                      (:SELECT 2))
             (gobject:get-g-type-definition "GtkListScrollFlags"))))

;;;     GtkListBase

(test gtk-list-base-class
  ;; Check type
  (is (g:type-is-object "GtkListBase"))
  ;; Check registered name
  (is (eq 'gtk:list-base
          (glib:symbol-for-gtype "GtkListBase")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkListBase")
          (g:gtype (cffi:foreign-funcall "gtk_list_base_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkListBase")))
  ;; Check children
  (is (equal '("GtkGridView" "GtkListView")
             (gtk-test:list-children "GtkListBase")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkOrientable" "GtkScrollable")
             (gtk-test:list-interfaces "GtkListBase")))
  ;; Check properties
  (is (equal '("hadjustment" "hscroll-policy" "orientation" "vadjustment"
               "vscroll-policy")
             (gtk-test:list-properties "GtkListBase")))
  ;; Check signals
  (is (equal '()
             (gtk-test:list-signals "GtkListBase")))
  ;; Check CSS name
  (is (string= "widget"
               (gtk:widget-class-css-name "GtkListBase")))
  ;; Check accessible role
  (is (eq :widget (gtk:widget-class-accessible-role "GtkListBase")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkListBase" GTK-LIST-BASE
                               (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                                ("GtkAccessible" "GtkBuildable"
                                 "GtkConstraintTarget" "GtkOrientable"
                                 "GtkScrollable")
                                :TYPE-INITIALIZER "gtk_list_base_get_type")
                               ((ORIENTATION GTK-LIST-BASE-ORIENTATION
                                 "orientation" "GtkOrientation" T T)))
             (gobject:get-g-type-definition "GtkListBase"))))

;;;     GtkListView

(test gtk-list-view-class
  ;; Check type
  (is (g:type-is-object "GtkListView"))
  ;; Check registered name
  (is (eq 'gtk:list-view
          (glib:symbol-for-gtype "GtkListView")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkListView")
          (g:gtype (cffi:foreign-funcall "gtk_list_view_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkListBase")
          (g:type-parent "GtkListView")))
  ;; Check children
  #-windows
  (if *first-run-gtk-test*
      (is (equal '()
                 (gtk-test:list-children "GtkListView")))
      (is (equal '("GtkColumnListView")
                 (gtk-test:list-children "GtkListView"))))
  #+windows
  (is (equal '()
             (gtk-test:list-children "GtkListView")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkOrientable" "GtkScrollable")
             (gtk-test:list-interfaces "GtkListView")))
  ;; Check properties
  (is (equal '("enable-rubberband" "factory" "header-factory" "model"
               "show-separators" "single-click-activate" "tab-behavior")
             (gtk-test:list-properties "GtkListView")))
  ;; Check signals
  (is (equal '("activate")
             (gtk-test:list-signals "GtkListView")))
  ;; Check CSS name
  (is (string= "listview"
               (gtk:widget-class-css-name "GtkListView")))
  ;; Check accessible role
  (is (eq :list (gtk:widget-class-accessible-role "GtkListView")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkListView" GTK-LIST-VIEW
                               (:SUPERCLASS GTK-LIST-BASE :EXPORT T :INTERFACES
                                ("GtkAccessible" "GtkBuildable"
                                 "GtkConstraintTarget" "GtkOrientable"
                                 "GtkScrollable")
                                :TYPE-INITIALIZER "gtk_list_view_get_type")
                               ((ENABLE-RUBBERBAND
                                 GTK-LIST-VIEW-ENABLE-RUBBERBAND
                                 "enable-rubberband" "gboolean" T T)
                                (FACTORY GTK-LIST-VIEW-FACTORY "factory"
                                 "GtkListItemFactory" T T)
                                (HEADER-FACTORY GTK-LIST-VIEW-HEADER-FACTORY
                                 "header-factory" "GtkListItemFactory" T T)
                                (MODEL GTK-LIST-VIEW-MODEL "model"
                                 "GtkSelectionModel" T T)
                                (SHOW-SEPARATORS GTK-LIST-VIEW-SHOW-SEPARATORS
                                 "show-separators" "gboolean" T T)
                                (SINGLE-CLICK-ACTIVATE
                                 GTK-LIST-VIEW-SINGLE-CLICK-ACTIVATE
                                 "single-click-activate" "gboolean" T T)
                                (TAB-BEHAVIOR GTK-LIST-VIEW-TAB-BEHAVIOR
                                 "tab-behavior" "GtkListTabBehavior" T T)))
             (gobject:get-g-type-definition "GtkListView"))))

;;; --- Properties (GtkListView) -----------------------------------------------

(test gtk-list-view-properties
  (let ((listview (make-instance 'gtk:list-view)))
    (is (eq :vertical (gtk:list-base-orientation listview)))
    (is-false (gtk:list-view-enable-rubberband listview))
    (is-false (gtk:list-view-factory listview))
    (is-false (gtk:list-view-header-factory listview))
    (is-false (gtk:list-view-model listview))
    (is-false (gtk:list-view-show-separators listview))
    (is-false (gtk:list-view-single-click-activate listview))
    (is (eq :all (gtk:list-view-tab-behavior listview)))))

;;; --- Signals ----------------------------------------------------------------

(test gtk-list-view-signals
  (let ((query (g:signal-query (g:signal-lookup "activate" "GtkListView"))))
    (is (string= "activate" (g:signal-query-signal-name query)))
    (is (string= "GtkListView" (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '("guint")
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_list_view_new

(test gtk-list-view-new
  (is (typep (gtk:list-view-new nil nil) 'gtk:list-view))
  (is (typep (gtk:list-view-new (gtk:no-selection-new nil) nil) 'gtk:list-view))
  (is (typep (gtk:list-view-new nil
                                (gtk:signal-list-item-factory-new))
             'gtk:list-view))
  (is (typep (gtk:list-view-new (gtk:no-selection-new nil)
                                (gtk:signal-list-item-factory-new))
             'gtk:list-view)))

;;;     gtk_list_view_scroll_to                            Since 4.12

;;; 2024-4-11

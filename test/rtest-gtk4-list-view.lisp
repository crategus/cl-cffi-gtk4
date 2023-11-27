(in-package :gtk-test)

(def-suite gtk-list-view :in gtk-suite)
(in-suite gtk-list-view)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkListTabBehavior

(test gtk-list-tab-behavior
  ;; Check the type
  (is (g:type-is-enum "GtkListTabBehavior"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkListTabBehavior")
          (g:gtype (cffi:foreign-funcall "gtk_list_tab_behavior_get_type" 
                                         :size))))
  ;; Check the registered name
  (is (eq 'gtk:list-tab-behavior
          (glib:symbol-for-gtype "GtkListTabBehavior")))
  ;; Check the names
  (is (equal '("GTK_LIST_TAB_ALL" "GTK_LIST_TAB_ITEM" "GTK_LIST_TAB_CELL")
             (list-enum-item-name "GtkListTabBehavior")))
  ;; Check the values
  (is (equal '(0 1 2)
             (list-enum-item-value "GtkListTabBehavior")))
  ;; Check the nick names
  (is (equal '("all" "item" "cell")
             (list-enum-item-nick "GtkListTabBehavior")))
  ;; Check the enum definition
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
  ;; Check the type
  (is (g:type-is-flags "GtkListScrollFlags"))
  ;; Check the registered name
  (is (eq 'gtk:list-scroll-flags
          (glib:symbol-for-gtype "GtkListScrollFlags")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkListScrollFlags")
          (g:gtype (cffi:foreign-funcall "gtk_list_scroll_flags_get_type" 
                                         :size))))
  ;; Check the names
  (is (equal '("GTK_LIST_SCROLL_NONE" "GTK_LIST_SCROLL_FOCUS" 
               "GTK_LIST_SCROLL_SELECT")
             (list-flags-item-name "GtkListScrollFlags")))
  ;; Check the values
  (is (equal '(0 1 2)
             (list-flags-item-value "GtkListScrollFlags")))
  ;; Check the nick names
  (is (equal '("none" "focus" "select")
             (list-flags-item-nick "GtkListScrollFlags")))
  ;; Check the flags definition
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
  ;; Type check
  (is (g:type-is-object "GtkListBase"))
  ;; Check the registered name
  (is (eq 'gtk:list-base
          (glib:symbol-for-gtype "GtkListBase")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkListBase")
          (g:gtype (cffi:foreign-funcall "gtk_list_base_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkListBase")))
  ;; Check the children
  (is (equal '("GtkGridView" "GtkListView")
             (list-children "GtkListBase")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget" 
               "GtkOrientable" "GtkScrollable")
             (list-interfaces "GtkListBase")))
  ;; Check the properties
  (is (equal '("hadjustment" "hscroll-policy" "orientation" "vadjustment" 
               "vscroll-policy")
             (list-properties "GtkListBase")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkListBase")))
  ;; CSS name
  (is (string= "widget"
               (gtk:widget-class-css-name "GtkListBase")))
  ;; CSS classes
  ;; GtkListBase is an abstract class
  ;; Accessible role
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
  ;; Type check
  (is (g:type-is-object "GtkListView"))
  ;; Check the registered name
  (is (eq 'gtk:list-view
          (glib:symbol-for-gtype "GtkListView")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkListView")
          (g:gtype (cffi:foreign-funcall "gtk_list_view_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkListBase")
          (g:type-parent "GtkListView")))
  ;; Check the children
  (if *first-run-gtk-test*
      (is (equal '()
                 (list-children "GtkListView")))
      (is (equal '("GtkColumnListView")
                 (list-children "GtkListView"))))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget" 
               "GtkOrientable" "GtkScrollable")
             (list-interfaces "GtkListView")))
  ;; Check the properties
  (is (equal '("enable-rubberband" "factory" "header-factory" "model" 
               "show-separators" "single-click-activate" "tab-behavior")
             (list-properties "GtkListView")))
  ;; Check the signals
  (is (equal '("activate")
             (list-signals "GtkListView")))
  ;; CSS name
  (is (string= "listview"
               (gtk:widget-class-css-name "GtkListView")))
  ;; CSS classes
  (is (equal '("view")
             (gtk:widget-css-classes (make-instance 'gtk:list-view))))
  ;; Accessible role
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

;;;     orientation

;;;     enable-rubberband
;;;     factory
;;;     header-factory                                     Since 4.12
;;;     model
;;;     show-separators
;;;     single-click-activate
;;;     tab-behavior                                       Since 4.12

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

;;;     activate

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

;;; --- 2023-11-25 -------------------------------------------------------------

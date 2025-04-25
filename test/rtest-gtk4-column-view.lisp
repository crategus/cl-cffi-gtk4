(in-package :gtk-test)

(def-suite gtk-column-view :in gtk-list-widgets)
(in-suite gtk-column-view)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkColumnView

(test gtk-column-view-class
  ;; Check type
  (is (g:type-is-object "GtkColumnView"))
  ;; Check registered name
  (is (eq 'gtk:column-view
          (glib:symbol-for-gtype "GtkColumnView")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkColumnView")
          (g:gtype (cffi:foreign-funcall "gtk_column_view_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkColumnView")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkColumnView")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkScrollable")
             (glib-test:list-interfaces "GtkColumnView")))
  ;; Check properties
  (is (equal '("columns" "enable-rubberband" "hadjustment" "header-factory"
               "hscroll-policy" "model" "reorderable" "row-factory"
               "show-column-separators" "show-row-separators"
               "single-click-activate" "sorter" "tab-behavior" "vadjustment"
               "vscroll-policy")
             (glib-test:list-properties "GtkColumnView")))
  ;; Check signals
  (is (equal '("activate")
             (glib-test:list-signals "GtkColumnView")))
  ;; Check CSS name
  (is (string= "columnview"
               (gtk:widget-class-css-name "GtkColumnView")))
  ;; Check accessible role
  (is (eq :tree-grid
          (gtk:widget-class-accessible-role "GtkColumnView")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkColumnView" GTK:COLUMN-VIEW
                      (:SUPERCLASS GTK:WIDGET
                       :EXPORT T
                       :INTERFACES
                       ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
                        "GtkScrollable")
                       :TYPE-INITIALIZER "gtk_column_view_get_type")
                      ((COLUMNS COLUMN-VIEW-COLUMNS "columns" "GListModel" T
                        NIL)
                       (ENABLE-RUBBERBAND COLUMN-VIEW-ENABLE-RUBBERBAND
                        "enable-rubberband" "gboolean" T T)
                       (HEADER-FACTORY COLUMN-VIEW-HEADER-FACTORY
                        "header-factory" "GtkListItemFactory" T T)
                       (MODEL COLUMN-VIEW-MODEL "model" "GtkSelectionModel" T
                        T)
                       (REORDERABLE COLUMN-VIEW-REORDERABLE "reorderable"
                        "gboolean" T T)
                       (ROW-FACTORY COLUMN-VIEW-ROW-FACTORY "row-factory"
                        "GtkListItemFactory" T T)
                       (SHOW-COLUMN-SEPARATORS
                        COLUMN-VIEW-SHOW-COLUMN-SEPARATORS
                        "show-column-separators" "gboolean" T T)
                       (SHOW-ROW-SEPARATORS COLUMN-VIEW-SHOW-ROW-SEPARATORS
                        "show-row-separators" "gboolean" T T)
                       (SINGLE-CLICK-ACTIVATE
                        COLUMN-VIEW-SINGLE-CLICK-ACTIVATE
                        "single-click-activate" "gboolean" T T)
                       (SORTER COLUMN-VIEW-SORTER "sorter" "GtkSorter" T NIL)
                       (TAB-BEHAVIOR COLUMN-VIEW-TAB-BEHAVIOR "tab-behavior"
                        "GtkListTabBehavior" T T)))
             (gobject:get-gtype-definition "GtkColumnView"))))

;;; --- Signals ----------------------------------------------------------------

;;;     activate

(test gtk-column-view-activate-signal
  (let* ((name "activate")
         (gtype (g:gtype "GtkColumnView"))
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
    (is (equal '("guint")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Properties -------------------------------------------------------------

;;;     columns
;;;     enable-rubberband
;;;     header-factory                                      Since 4.12
;;;     model
;;;     reorderable
;;;     row-factory                                         Since 4.12
;;;     show-column-separators
;;;     show-row-separators
;;;     single-click-activate
;;;     sorter
;;;     tab-behavior                                        Since 4.12

(test gtk-column-view-properties
  (glib-test:with-check-memory (view :strong 2)
    (is (typep (setf view (make-instance 'gtk:column-view)) 'gtk:column-view))
    (is (typep (gtk:column-view-columns view) 'g:list-store))
    (is-false (gtk:column-view-enable-rubberband view))
    (is-false (gtk:column-view-header-factory view))
    (is-false (gtk:column-view-model view))
    (is-true (gtk:column-view-reorderable view))
    (is-false (gtk:column-view-row-factory view))
    (is-false (gtk:column-view-show-column-separators view))
    (is-false (gtk:column-view-show-row-separators view))
    (is-false (gtk:column-view-single-click-activate view))
    (is (typep (gtk:column-view-sorter view) 'gtk:column-view-sorter))
    (is (eq :all (gtk:column-view-tab-behavior view)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_column_view_new

(test gtk-column-view-new
  (glib-test:with-check-memory (view model)
    (setf model (gtk:no-selection-new))
    (is (typep (setf view (gtk:column-view-new)) 'gtk:column-view))
    (is (typep (setf view (gtk:column-view-new model)) 'gtk:column-view))
    ;; Remove reference to model
    (is-false (setf (gtk:column-view-model view) nil))))

;;;     gtk_column_view_append_column
;;;     gtk_column_view_insert_column
;;;     gtk_column_view_remove_column

(test gtk-column-view-append/insert/remove-column
  (glib-test:with-check-memory (view column1 column2 :strong 1)
    (is (typep (setf view (gtk:column-view-new)) 'gtk:column-view))
    (is (typep (setf column1
                     (gtk:column-view-column-new)) 'gtk:column-view-column))
    (is (typep (setf column2
                     (gtk:column-view-column-new)) 'gtk:column-view-column))
    (is-false (gtk:column-view-append-column view column1))
    (is-false (gtk:column-view-insert-column view 0 column2))
    (is (eq column2
            (g:list-model-item (gtk:column-view-columns view) 0)))
    (is (eq column1
            (g:list-model-item (gtk:column-view-columns view) 1)))
    (is-false (gtk:column-view-remove-column view column1))
    (is-false (gtk:column-view-remove-column view column2))))

;;;     gtk_column_view_sort_by_column

;;; 2025-4-13

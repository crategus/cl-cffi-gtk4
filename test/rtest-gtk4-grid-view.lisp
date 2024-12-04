(in-package :gtk-test)

(def-suite gtk-grid-view :in gtk-list-widgets)
(in-suite gtk-grid-view)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkGridView

(test gtk-grid-view-class
  ;; Check type
  (is (g:type-is-object "GtkGridView"))
  ;; Check registered name
  (is (eq 'gtk:grid-view
          (glib:symbol-for-gtype "GtkGridView")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkGridView")
          (g:gtype (cffi:foreign-funcall "gtk_grid_view_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkListBase")
          (g:type-parent "GtkGridView")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkGridView")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkOrientable" "GtkScrollable")
             (glib-test:list-interfaces "GtkGridView")))
  ;; Check properties
  (is (equal '("enable-rubberband" "factory" "max-columns" "min-columns" "model"
               "single-click-activate" "tab-behavior")
             (glib-test:list-properties "GtkGridView")))
  ;; Check signals
  (is (equal '("activate")
             (glib-test:list-signals "GtkGridView")))
  ;; Check CSS name
  (is (string= "gridview"
               (gtk:widget-class-css-name "GtkGridView")))
  ;; Check accessible role
  (is (eq :GRID
          (gtk:widget-class-accessible-role "GtkGridView")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkGridView" GTK:GRID-VIEW
                       (:SUPERCLASS GTK:LIST-BASE
                        :EXPORT T
                        :INTERFACES
                        ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
                         "GtkOrientable" "GtkScrollable")
                        :TYPE-INITIALIZER "gtk_grid_view_get_type")
                       ((ENABLE-RUBBERBAND GRID-VIEW-ENABLE-RUBBERBAND
                         "enable-rubberband" "gboolean" T T)
                        (FACTORY GRID-VIEW-FACTORY "factory"
                         "GtkListItemFactory" T T)
                        (MAX-COLUMNS GRID-VIEW-MAX-COLUMNS "max-columns"
                         "guint" T T)
                        (MIN-COLUMNS GRID-VIEW-MIN-COLUMNS "min-columns"
                         "guint" T T)
                        (MODEL GRID-VIEW-MODEL "model" "GtkSelectionModel" T
                         T)
                        (SINGLE-CLICK-ACTIVATE GRID-VIEW-SINGLE-CLICK-ACTIVATE
                         "single-click-activate" "gboolean" T T)
                        (TAB-BEHAVIOR GRID-VIEW-TAB-BEHAVIOR "tab-behavior"
                         "GtkListTabBehavior" T T)))
             (gobject:get-gtype-definition "GtkGridView"))))

;;; --- Signals ----------------------------------------------------------------

;;;     activate

(test gtk-grid-view-activate-signal
  (let* ((name "activate")
         (gtype (g:gtype "GtkGridView"))
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

;;;     enable-rubberband
;;;     factory
;;;     max-columns
;;;     min-columns
;;;     model
;;;     single-click-activate
;;;     tab-behavior                                       Since 4.12

(test gtk-grid-view-properties
  (let ((view (make-instance 'gtk:grid-view)))
    (is-false (gtk:grid-view-enable-rubberband view))
    (is-false (gtk:grid-view-factory view))
    (is (= 7 (gtk:grid-view-max-columns view)))
    (is (= 1 (gtk:grid-view-min-columns view)))
    (is-false (gtk:grid-view-model view))
    (is-false (gtk:grid-view-single-click-activate view))
    (is (eq :all (gtk:grid-view-tab-behavior view)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_grid_view_new

(test gtk-grid-view-new
  (let ((view nil)
        (factory (gtk:signal-list-item-factory-new))
        (model (gtk:no-selection-new)))
    (is (typep (setf view (gtk:grid-view-new)) 'gtk:grid-view))
    ;; Remove reference from MODEL
    (is-false (setf (gtk:grid-view-model view) nil))
    (is (typep (setf view (gtk:grid-view-new model)) 'gtk:grid-view))
    ;; Remove reference from MODEL
    (is-false (setf (gtk:grid-view-model view) nil))
    (is (typep (setf view (gtk:grid-view-new model factory)) 'gtk:grid-view))
    ;; Remove reference from MODEL
    (is-false (setf (gtk:grid-view-model view) nil))
    ;; Check memory management
    (is-false (setf (gtk:grid-view-factory view) nil))
    (is (= 1 (g:object-ref-count factory)))
    (is (= 1 (g:object-ref-count model)))
    (is (= 1 (g:object-ref-count view)))))

;;; gtk_grid_view_scroll_to

;;; 2024-11-27

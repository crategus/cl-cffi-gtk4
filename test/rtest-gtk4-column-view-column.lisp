(in-package :gtk-test)

(def-suite gtk-column-view-column :in gtk-list-widgets)
(in-suite gtk-column-view-column)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkColumnViewColumn

(test gtk-column-view-column-class
  ;; Check type
  (is (g:type-is-object "GtkColumnViewColumn"))
  ;; Check registered name
  (is (eq 'gtk:column-view-column
          (glib:symbol-for-gtype "GtkColumnViewColumn")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkColumnViewColumn")
          (g:gtype (cffi:foreign-funcall "gtk_column_view_column_get_type"
                                         :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkColumnViewColumn")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkColumnViewColumn")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkColumnViewColumn")))
  ;; Check properties
  (is (equal '("column-view" "expand" "factory" "fixed-width" "header-menu"
               "id" "resizable" "sorter" "title" "visible")
             (glib-test:list-properties "GtkColumnViewColumn")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkColumnViewColumn")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkColumnViewColumn" GTK:COLUMN-VIEW-COLUMN
                      (:SUPERCLASS GOBJECT:OBJECT
                       :EXPORT T
                       :INTERFACES NIL
                       :TYPE-INITIALIZER "gtk_column_view_column_get_type")
                      ((COLUMN-VIEW COLUMN-VIEW-COLUMN-COLUMN-VIEW
                        "column-view" "GtkColumnView" T NIL)
                       (EXPAND COLUMN-VIEW-COLUMN-EXPAND
                        "expand" "gboolean" T T)
                       (FACTORY COLUMN-VIEW-COLUMN-FACTORY
                        "factory" "GtkListItemFactory" T T)
                       (FIXED-WIDTH COLUMN-VIEW-COLUMN-FIXED-WIDTH
                        "fixed-width" "gint" T T)
                       (HEADER-MENU COLUMN-VIEW-COLUMN-HEADER-MENU
                        "header-menu" "GMenuModel" T T)
                       (ID COLUMN-VIEW-COLUMN-ID "id" "gchararray" T T)
                       (RESIZABLE COLUMN-VIEW-COLUMN-RESIZABLE
                        "resizable" "gboolean" T T)
                       (SORTER COLUMN-VIEW-COLUMN-SORTER
                        "sorter" "GtkSorter" T T)
                       (TITLE COLUMN-VIEW-COLUMN-TITLE
                        "title" "gchararray" T T)
                       (VISIBLE COLUMN-VIEW-COLUMN-VISIBLE
                        "visible" "gboolean" T T)))
             (gobject:get-gtype-definition "GtkColumnViewColumn"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-column-view-column-properties
  (glib-test:with-check-memory (column)
    (is (typep (setf column
                     (make-instance 'gtk:column-view-column))
               'gtk:column-view-column))
    (is-false (gtk:column-view-column-column-view column))
    (is-false (gtk:column-view-column-expand column))
    (is-false (gtk:column-view-column-factory column))
    (is (= -1 (gtk:column-view-column-fixed-width column)))
    (is-false (gtk:column-view-column-header-menu column))
    (is-false (gtk:column-view-column-id column))
    (is-false (gtk:column-view-column-resizable column))
    (is-false (gtk:column-view-column-sorter column))
    (is-false (gtk:column-view-column-title column))
    (is-true (gtk:column-view-column-visible column))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_column_view_column_new

(test gtk-column-view-column-new
  (glib-test:with-check-memory (column factory)
    (setf factory (gtk:signal-list-item-factory-new))
    (is (typep (setf column (gtk:column-view-column-new))
               'gtk:column-view-column))
    (is (typep (setf column (gtk:column-view-column-new "title"))
               'gtk:column-view-column))
    (is (string= "title"
                 (gtk:column-view-column-title column)))
    (is (typep (setf column (gtk:column-view-column-new "title" factory))
               'gtk:column-view-column))
    (is (string= "title"
                 (gtk:column-view-column-title column)))
    (is (eq factory (gtk:column-view-column-factory column)))
    ;; Remove references
    (is-false (setf (gtk:column-view-column-factory column) nil))))

;;; 2025-4-13

(in-package :gtk-test)

(def-suite gtk-column-view-cell :in gtk-suite)
(in-suite gtk-column-view-cell)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkColumnViewCell

(test gtk-column-view-cell-class
  ;; Check type
  (is (g:type-is-object "GtkColumnViewCell"))
  ;; Check registered name
  (is (eq 'gtk:column-view-cell
          (glib:symbol-for-gtype "GtkColumnViewCell")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkColumnViewCell")
          (g:gtype (cffi:foreign-funcall "gtk_column_view_cell_get_type"
                                         :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkListItem")
          (g:type-parent "GtkColumnViewCell")))
  ;; Check children
  (is (equal '()
             (gtk-test:list-children "GtkColumnViewCell")))
  ;; Check interfaces
  (is (equal '()
             (gtk-test:list-interfaces "GtkColumnViewCell")))
  ;; Check properties
  (is (equal '()
             (gtk-test:list-properties "GtkColumnViewCell")))
  ;; Check signals
  (is (equal '()
             (gtk-test:list-signals "GtkColumnViewCell")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkColumnViewCell"
                                             GTK-COLUMN-VIEW-CELL
                               (:SUPERCLASS GTK-LIST-ITEM :EXPORT T :INTERFACES
                                NIL :TYPE-INITIALIZER
                                "gtk_column_view_cell_get_type")
                               ((CHILD GTK-COLUMN-VIEW-CELL-CHILD "child"
                                 "GtkWidget" T T)
                                (FOCUSABLE GTK-COLUMN-VIEW-CELL-FOCUSABLE
                                 "focusable" "gboolean" T T)
                                (ITEM GTK-COLUMN-VIEW-CELL-ITEM "item"
                                 "GObject" T NIL)
                                (POSITION GTK-COLUMN-VIEW-CELL-POSITION
                                          "position" "guint" T NIL)
                                (SELECTED GTK-COLUMN-VIEW-CELL-SELECTED
                                 "selected" "gboolean" T NIL)))
             (gobject:get-g-type-definition "GtkColumnViewCell"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-column-view-cell-properties
  (let ((cell (make-instance 'gtk:column-view-cell)))
    (is-false (gtk:column-view-cell-child cell))
    (is-false (gtk:column-view-cell-focusable cell))
    (is-false (gtk:column-view-cell-item cell))
    (is (= gtk:+invalid-list-position+
           (gtk:column-view-cell-position cell)))
    (is-false (gtk:column-view-cell-selected cell))))

;;; 2024-7-4


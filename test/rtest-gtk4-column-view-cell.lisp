(in-package :gtk-test)

(def-suite gtk-column-view-cell :in gtk-list-widgets)
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
             (glib-test:list-children "GtkColumnViewCell")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkColumnViewCell")))
  ;; Check properties
  (is (equal '()
             (glib-test:list-properties "GtkColumnViewCell")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkColumnViewCell")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkColumnViewCell" GTK:COLUMN-VIEW-CELL
                      (:SUPERCLASS GTK:LIST-ITEM
                       :EXPORT T
                       :INTERFACES NIL
                       :TYPE-INITIALIZER "gtk_column_view_cell_get_type")
                      ((CHILD COLUMN-VIEW-CELL-CHILD "child" "GtkWidget" T T)
                       (FOCUSABLE COLUMN-VIEW-CELL-FOCUSABLE
                        "focusable" "gboolean" T T)
                       (ITEM COLUMN-VIEW-CELL-ITEM "item" "GObject" T NIL)
                       (POSITION COLUMN-VIEW-CELL-POSITION
                        "position" "guint" T NIL)
                       (SELECTED COLUMN-VIEW-CELL-SELECTED
                        "selected" "gboolean" T NIL)))
             (gobject:get-gtype-definition "GtkColumnViewCell"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-column-view-cell-properties
  (glib-test:with-check-memory (cell)
    (is (typep (setf cell
                     (make-instance 'gtk:column-view-cell))
               'gtk:column-view-cell))
    (is-false (gtk:column-view-cell-child cell))
    (is-false (gtk:column-view-cell-focusable cell))
    (is-false (gtk:column-view-cell-item cell))
    (is (= gtk:+invalid-list-position+
           (gtk:column-view-cell-position cell)))
    (is-false (gtk:column-view-cell-selected cell))))

;;; 2025-4-13

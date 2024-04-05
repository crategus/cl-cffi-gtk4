(in-package :gtk-test)

(def-suite gtk-column-view-cell :in gtk-suite)
(in-suite gtk-column-view-cell)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkColumnViewCell

(test gtk-column-view-cell-class
  ;; Type check
  (is (g:type-is-object "GtkColumnViewCell"))
  ;; Check the registered name
  (is (eq 'gtk:column-view-cell
          (glib:symbol-for-gtype "GtkColumnViewCell")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkColumnViewCell")
          (g:gtype (cffi:foreign-funcall "gtk_column_view_cell_get_type"
                                         :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkListItem")
          (g:type-parent "GtkColumnViewCell")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkColumnViewCell")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkColumnViewCell")))
  ;; Check the properties
  (is (equal '()
             (list-properties "GtkColumnViewCell")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkColumnViewCell")))
  ;; Check the class definition
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

;;;     child
;;;     focusable
;;;     item
;;;     position
;;;     selected

(test gtk-column-view-cell-properties
  (let ((cell (make-instance 'gtk:column-view-cell)))
    (is-false (gtk:column-view-cell-child cell))
    (is-false (gtk:column-view-cell-focusable cell))
    (is-false (gtk:column-view-cell-item cell))
    (is (= gtk:+invalid-list-position+
           (gtk:column-view-cell-position cell)))
    (is-false (gtk:column-view-cell-selected cell))))

;;; --- 2023-11-27 -------------------------------------------------------------


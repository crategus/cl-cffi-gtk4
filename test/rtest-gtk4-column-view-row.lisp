(in-package :gtk-test)

(def-suite gtk-column-view-row :in gtk-suite)
(in-suite gtk-column-view-row)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkColumnViewRow

(test gtk-column-view-row-class
  ;; Type check
  (is (g:type-is-object "GtkColumnViewRow"))
  ;; Check the registered name
  (is (eq 'gtk:column-view-row
          (glib:symbol-for-gtype "GtkColumnViewRow")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkColumnViewRow")
          (g:gtype (cffi:foreign-funcall "gtk_column_view_row_get_type"
                                         :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkColumnViewRow")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkColumnViewRow")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkColumnViewRow")))
  ;; Check the properties
  (is (equal '("accessible-description" "accessible-label" "activatable"
               "focusable" "item" "position" "selectable" "selected")
             (list-properties "GtkColumnViewRow")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkColumnViewRow")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkColumnViewRow"
                                             GTK-COLUMN-VIEW-ROW
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                                :TYPE-INITIALIZER
                                "gtk_column_view_row_get_type")
                               ((ACCESSIBLE-DESCRIPTION
                                 GTK-COLUMN-VIEW-ROW-ACCESSIBLE-DESCRIPTION
                                 "accessible-description" "gchararray" T T)
                                (ACCESSIBLE-LABEL
                                 GTK-COLUMN-VIEW-ROW-ACCESSIBLE-LABEL
                                 "accessible-label" "gchararray" T T)
                                (ACTIVATABLE GTK-COLUMN-VIEW-ROW-ACTIVATABLE
                                 "activatable" "gboolean" T T)
                                (FOCUSABLE GTK-COLUMN-VIEW-ROW-FOCUSABLE
                                 "focusable" "gboolean" T T)
                                (ITEM GTK-COLUMN-VIEW-ROW-ITEM "item" "GObject"
                                 T NIL)
                                (POSITION GTK-COLUMN-VIEW-ROW-POSITION
                                          "position" "guint" T NIL)
                                (SELECTABLE GTK-COLUMN-VIEW-ROW-SELECTABLE
                                 "selectable" "gboolean" T T)
                                (SELECTED GTK-COLUMN-VIEW-ROW-SELECTED
                                 "selected" "gboolean" T NIL)))
             (gobject:get-g-type-definition "GtkColumnViewRow"))))

;;; --- Properties -------------------------------------------------------------

;;;     accessible-description
;;;     accessible-label
;;;     activatable
;;;     focusable
;;;     item
;;;     position
;;;     selectable
;;;     selected

(test gtk-column-view-row-properties
  (let ((row (make-instance 'gtk:column-view-row)))
    (is-false (gtk:column-view-row-accessible-description row))
    (is-false (gtk:column-view-row-accessible-label row))
    (is-true (gtk:column-view-row-activatable row))
    (is-true (gtk:column-view-row-focusable row))
    (is-false (gtk:column-view-row-item row))
    (is (= gtk:+gtk-invalid-list-position+ (gtk:column-view-row-position row)))
    (is-true (gtk:column-view-row-selectable row))
    (is-false (gtk:column-view-row-selected row))))

;;; --- 2023-11-27 -------------------------------------------------------------


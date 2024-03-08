(in-package :gtk-test)

(def-suite gtk-cell-renderer-combo :in gtk-suite)
(in-suite gtk-cell-renderer-combo)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkCellRendererCombo

(test gtk-cell-renderer-combo-class
  ;; Type check
  (is (g:type-is-object "GtkCellRendererCombo"))
  ;; Check the registered name
  (is (eq 'gtk:cell-renderer-combo
          (glib:symbol-for-gtype "GtkCellRendererCombo")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkCellRendererCombo")
          (g:gtype (cffi:foreign-funcall "gtk_cell_renderer_combo_get_type"
                                         :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkCellRendererText")
          (g:type-parent "GtkCellRendererCombo")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkCellRendererCombo")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkCellRendererCombo")))
  ;; Check the properties
  (is (equal '("has-entry" "model" "text-column")
             (list-properties "GtkCellRendererCombo")))
  ;; Check the signals
  (is (equal '("changed")
             (list-signals "GtkCellRendererCombo")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkCellRendererCombo"
                                             GTK-CELL-RENDERER-COMBO
                               (:SUPERCLASS GTK-CELL-RENDERER-TEXT :EXPORT T
                                :INTERFACES NIL :TYPE-INITIALIZER
                                "gtk_cell_renderer_combo_get_type")
                               ((HAS-ENTRY GTK-CELL-RENDERER-COMBO-HAS-ENTRY
                                 "has-entry" "gboolean" T T)
                                (MODEL GTK-CELL-RENDERER-COMBO-MODEL "model"
                                 "GtkTreeModel" T T)
                                (TEXT-COLUMN
                                 GTK-CELL-RENDERER-COMBO-TEXT-COLUMN
                                 "text-column" "gint" T T)))
             (gobject:get-g-type-definition "GtkCellRendererCombo"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-cell-renderer-combo-properties
  (let ((renderer (make-instance 'gtk:cell-renderer-combo)))
    (is-true (gtk:cell-renderer-combo-has-entry renderer))
    (is-false (gtk:cell-renderer-combo-model renderer))
    (is (= -1 (gtk:cell-renderer-combo-text-column renderer)))))

;;; --- Signals ----------------------------------------------------------------

;;;     changed

;;; --- Functions --------------------------------------------------------------

;;;     gtk_cell_renderer_combo_new

(test gtk-cell-renderer-combo-new
  (is (typep (gtk:cell-renderer-combo-new) 'gtk:cell-renderer-combo)))

;;; 2024-2-21

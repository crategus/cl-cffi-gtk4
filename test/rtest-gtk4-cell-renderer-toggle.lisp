(in-package :gtk-test)

(def-suite gtk-cell-renderer-toggle :in gtk-suite)
(in-suite gtk-cell-renderer-toggle)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkCellRendererToggle

(test gtk-cell-renderer-toggle-class
  ;; Type check
  (is (g:type-is-object "GtkCellRendererToggle"))
  ;; Check the registered name
  (is (eq 'gtk:cell-renderer-toggle
          (glib:symbol-for-gtype "GtkCellRendererToggle")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkCellRendererToggle")
          (g:gtype (cffi:foreign-funcall "gtk_cell_renderer_toggle_get_type"
                                         :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkCellRenderer")
          (g:type-parent "GtkCellRendererToggle")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkCellRendererToggle")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkCellRendererToggle")))
  ;; Check the properties
  (is (equal '("activatable" "active" "inconsistent" "radio")
             (list-properties "GtkCellRendererToggle")))
  ;; Check the signals
  (is (equal '("toggled")
             (list-signals "GtkCellRendererToggle")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkCellRendererToggle"
                               GTK-CELL-RENDERER-TOGGLE
                               (:SUPERCLASS GTK-CELL-RENDERER :EXPORT T
                                :INTERFACES NIL :TYPE-INITIALIZER
                                "gtk_cell_renderer_toggle_get_type")
                               ((ACTIVATABLE
                                 GTK-CELL-RENDERER-TOGGLE-ACTIVATABLE
                                 "activatable" "gboolean" T T)
                                (ACTIVE GTK-CELL-RENDERER-TOGGLE-ACTIVE
                                 "active" "gboolean" T T)
                                (INCONSISTENT
                                 GTK-CELL-RENDERER-TOGGLE-INCONSISTENT
                                 "inconsistent" "gboolean" T T)
                                (RADIO GTK-CELL-RENDERER-TOGGLE-RADIO "radio"
                                 "gboolean" T T)))
             (gobject:get-g-type-definition "GtkCellRendererToggle"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-cell-renderer-toggle-properties
  (let ((renderer (make-instance 'gtk:cell-renderer-toggle)))
    (is-true (gtk:cell-renderer-toggle-activatable renderer))
    (is-false (gtk:cell-renderer-toggle-active renderer))
    (is-false (gtk:cell-renderer-toggle-inconsistent renderer))
    (is-false (gtk:cell-renderer-toggle-radio renderer))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_cell_renderer_toggle_new

(test gtk-cell-renderer-toggle-new
  (is (typep (gtk:cell-renderer-toggle-new) 'gtk:cell-renderer-toggle)))

;;; 2024-2-22

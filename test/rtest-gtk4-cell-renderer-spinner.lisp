(in-package :gtk-test)

(def-suite gtk-cell-renderer-spinner :in gtk-suite)
(in-suite gtk-cell-renderer-spinner)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkCellRendererSpinner

(test gtk-cell-renderer-spinner-class
  ;; Type check
  (is (g:type-is-object "GtkCellRendererSpinner"))
  ;; Check the registered name
  (is (eq 'gtk:cell-renderer-spinner
          (glib:symbol-for-gtype "GtkCellRendererSpinner")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkCellRendererSpinner")
          (g:gtype (cffi:foreign-funcall "gtk_cell_renderer_spinner_get_type"
                                         :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkCellRenderer")
          (g:type-parent "GtkCellRendererSpinner")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkCellRendererSpinner")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkCellRendererSpinner")))
  ;; Check the properties
  (is (equal '("active" "pulse" "size")
             (list-properties "GtkCellRendererSpinner")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkCellRendererSpinner")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkCellRendererSpinner"
                               GTK-CELL-RENDERER-SPINNER
                               (:SUPERCLASS GTK-CELL-RENDERER :EXPORT T
                                :INTERFACES NIL :TYPE-INITIALIZER
                                "gtk_cell_renderer_spinner_get_type")
                               ((ACTIVE GTK-CELL-RENDERER-SPINNER-ACTIVE
                                 "active" "gboolean" T T)
                                (PULSE GTK-CELL-RENDERER-SPINNER-PULSE "pulse"
                                 "guint" T T)
                                (SIZE GTK-CELL-RENDERER-SPINNER-SIZE "size"
                                 "GtkIconSize" T T)))
             (gobject:get-g-type-definition "GtkCellRendererSpinner"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-cell-renderer-spinner-properties
  (let ((renderer (make-instance 'gtk:cell-renderer-spinner)))
    (is-false (gtk:cell-renderer-spinner-active renderer))
    (is (= 0 (gtk:cell-renderer-spinner-pulse renderer)))
    (is (eq :inherit (gtk:cell-renderer-spinner-size renderer)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_cell_renderer_spinner_new

(test gtk-cell-renderer-spinner-new
  (is (typep (gtk:cell-renderer-spinner-new) 'gtk:cell-renderer-spinner)))

;;; 2024-2-22

(in-package :gtk-test)

(def-suite gtk-cell-renderer-spinner :in gtk-deprecated)
(in-suite gtk-cell-renderer-spinner)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkCellRendererSpinner

(test gtk-cell-renderer-spinner-class
  ;; Check type
  (is (g:type-is-object "GtkCellRendererSpinner"))
  ;; Check registered name
  (is (eq 'gtk:cell-renderer-spinner
          (glib:symbol-for-gtype "GtkCellRendererSpinner")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkCellRendererSpinner")
          (g:gtype (cffi:foreign-funcall "gtk_cell_renderer_spinner_get_type"
                                         :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkCellRenderer")
          (g:type-parent "GtkCellRendererSpinner")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkCellRendererSpinner")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkCellRendererSpinner")))
  ;; Check properties
  (is (equal '("active" "pulse" "size")
             (glib-test:list-properties "GtkCellRendererSpinner")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkCellRendererSpinner")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkCellRendererSpinner"
                                      GTK:CELL-RENDERER-SPINNER
                      (:SUPERCLASS GTK:CELL-RENDERER
                       :EXPORT T
                       :INTERFACES NIL
                       :TYPE-INITIALIZER "gtk_cell_renderer_spinner_get_type")
                      ((ACTIVE CELL-RENDERER-SPINNER-ACTIVE
                        "active" "gboolean" T T)
                       (PULSE CELL-RENDERER-SPINNER-PULSE "pulse" "guint" T T)
                       (SIZE CELL-RENDERER-SPINNER-SIZE
                        "size" "GtkIconSize" T T)))
             (gobject:get-gtype-definition "GtkCellRendererSpinner"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-cell-renderer-spinner-properties
  (let* ((gtk-init:*gtk-warn-deprecated* nil)
         (renderer (make-instance 'gtk:cell-renderer-spinner)))
    (is-false (gtk:cell-renderer-spinner-active renderer))
    (is (= 0 (gtk:cell-renderer-spinner-pulse renderer)))
    (is (eq :inherit (gtk:cell-renderer-spinner-size renderer)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_cell_renderer_spinner_new

(test gtk-cell-renderer-spinner-new
  (let* ((gtk-init:*gtk-warn-deprecated* nil))
    (is (typep (gtk:cell-renderer-spinner-new) 'gtk:cell-renderer-spinner))))

;;; 2024-9-20

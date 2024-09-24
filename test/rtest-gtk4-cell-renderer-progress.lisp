(in-package :gtk-test)

(def-suite gtk-cell-renderer-progress :in gtk-suite)
(in-suite gtk-cell-renderer-progress)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkCellRendererProgress

(test gtk-cell-renderer-progress-class
  ;; Check type
  (is (g:type-is-object "GtkCellRendererProgress"))
  ;; Check registered name
  (is (eq 'gtk:cell-renderer-progress
          (glib:symbol-for-gtype "GtkCellRendererProgress")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkCellRendererProgress")
          (g:gtype (cffi:foreign-funcall "gtk_cell_renderer_progress_get_type"
                                         :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkCellRenderer")
          (g:type-parent "GtkCellRendererProgress")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkCellRendererProgress")))
  ;; Check interfaces
  (is (equal '("GtkOrientable")
             (glib-test:list-interfaces "GtkCellRendererProgress")))
  ;; Check properties
  (is (equal '("inverted" "orientation" "pulse" "text" "text-xalign"
               "text-yalign" "value")
             (glib-test:list-properties "GtkCellRendererProgress")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkCellRendererProgress")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkCellRendererProgress"
                                      GTK:CELL-RENDERER-PROGRESS
                      (:SUPERCLASS GTK:CELL-RENDERER
                       :EXPORT T
                       :INTERFACES ("GtkOrientable")
                       :TYPE-INITIALIZER "gtk_cell_renderer_progress_get_type")
                      ((INVERTED CELL-RENDERER-PROGRESS-INVERTED
                        "inverted" "gboolean" T T)
                       (PULSE CELL-RENDERER-PROGRESS-PULSE "pulse" "gint" T T)
                       (TEXT CELL-RENDERER-PROGRESS-TEXT "text" "gchararray" T T)
                       (TEXT-XALIGN CELL-RENDERER-PROGRESS-TEXT-XALIGN
                        "text-xalign" "gfloat" T T)
                       (TEXT-YALIGN CELL-RENDERER-PROGRESS-TEXT-YALIGN
                        "text-yalign" "gfloat" T T)
                       (VALUE CELL-RENDERER-PROGRESS-VALUE "value" "gint" T T)))
             (gobject:get-gtype-definition "GtkCellRendererProgress"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-cell-renderer-progress-properties
  (let* ((gtk-init:*gtk-warn-deprecated* nil)
         (renderer (make-instance 'gtk:cell-renderer-progress)))
    (is-false (gtk:cell-renderer-progress-inverted renderer))
    (is (= -1 (gtk:cell-renderer-progress-pulse renderer)))
    (is-false (gtk:cell-renderer-progress-text renderer))
    (is (= 0.5 (gtk:cell-renderer-progress-text-xalign renderer)))
    (is (= 0.5 (gtk:cell-renderer-progress-text-yalign renderer)))
    (is (= 0 (gtk:cell-renderer-progress-value renderer)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_cell_renderer_progress_new

(test gtk:cell-renderer-progress-new
  (let* ((gtk-init:*gtk-warn-deprecated* nil))
    (is (typep (gtk:cell-renderer-progress-new) 'gtk:cell-renderer-progress))))

;;; 2024-9-20

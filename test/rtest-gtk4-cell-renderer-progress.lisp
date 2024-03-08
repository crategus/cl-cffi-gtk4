(in-package :gtk-test)

(def-suite gtk-cell-renderer-progress :in gtk-suite)
(in-suite gtk-cell-renderer-progress)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkCellRendererProgress

(test gtk-cell-renderer-progress-class
  ;; Type check
  (is (g:type-is-object "GtkCellRendererProgress"))
  ;; Check the registered name
  (is (eq 'gtk:cell-renderer-progress
          (glib:symbol-for-gtype "GtkCellRendererProgress")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkCellRendererProgress")
          (g:gtype (cffi:foreign-funcall "gtk_cell_renderer_progress_get_type"
                                         :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkCellRenderer")
          (g:type-parent "GtkCellRendererProgress")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkCellRendererProgress")))
  ;; Check the interfaces
  (is (equal '("GtkOrientable")
             (list-interfaces "GtkCellRendererProgress")))
  ;; Check the properties
  (is (equal '("inverted" "orientation" "pulse" "text" "text-xalign"
               "text-yalign" "value")
             (list-properties "GtkCellRendererProgress")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkCellRendererProgress")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkCellRendererProgress"
                               GTK-CELL-RENDERER-PROGRESS
                               (:SUPERCLASS GTK-CELL-RENDERER :EXPORT T
                                :INTERFACES ("GtkOrientable") :TYPE-INITIALIZER
                                "gtk_cell_renderer_progress_get_type")
                               ((INVERTED GTK-CELL-RENDERER-PROGRESS-INVERTED
                                 "inverted" "gboolean" T T)
                                (PULSE GTK-CELL-RENDERER-PROGRESS-PULSE "pulse"
                                 "gint" T T)
                                (TEXT GTK-CELL-RENDERER-PROGRESS-TEXT "text"
                                 "gchararray" T T)
                                (TEXT-XALIGN
                                 GTK-CELL-RENDERER-PROGRESS-TEXT-XALIGN
                                 "text-xalign" "gfloat" T T)
                                (TEXT-YALIGN
                                 GTK-CELL-RENDERER-PROGRESS-TEXT-YALIGN
                                 "text-yalign" "gfloat" T T)
                                (VALUE GTK-CELL-RENDERER-PROGRESS-VALUE "value"
                                 "gint" T T)))
             (gobject:get-g-type-definition "GtkCellRendererProgress"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-cell-renderer-progress-properties
  (let ((renderer (make-instance 'gtk:cell-renderer-progress)))
    (is-false (gtk:cell-renderer-progress-inverted renderer))
    (is (= -1 (gtk:cell-renderer-progress-pulse renderer)))
    (is-false (gtk:cell-renderer-progress-text renderer))
    (is (= 0.5 (gtk:cell-renderer-progress-text-xalign renderer)))
    (is (= 0.5 (gtk:cell-renderer-progress-text-yalign renderer)))
    (is (= 0 (gtk:cell-renderer-progress-value renderer)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_cell_renderer_progress_new

(test gtk:cell-renderer-progress-new
  (is (typep (gtk:cell-renderer-progress-new) 'gtk:cell-renderer-progress)))

;;; 2024-2-21

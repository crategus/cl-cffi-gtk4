(in-package :gtk-test)

(def-suite gtk-cell-renderer-spin :in gtk-suite)
(in-suite gtk-cell-renderer-spin)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkCellRendererSpin

(test gtk-cell-renderer-spin-class
  ;; Type check
  (is (g:type-is-object "GtkCellRendererSpin"))
  ;; Check the registered name
  (is (eq 'gtk:cell-renderer-spin
          (glib:symbol-for-gtype "GtkCellRendererSpin")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkCellRendererSpin")
          (g:gtype (cffi:foreign-funcall "gtk_cell_renderer_spin_get_type"
                                         :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkCellRendererText")
          (g:type-parent "GtkCellRendererSpin")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkCellRendererSpin")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkCellRendererSpin")))
  ;; Check the properties
  (is (equal '("adjustment" "climb-rate" "digits")
             (list-properties "GtkCellRendererSpin")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkCellRendererSpin")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkCellRendererSpin"
                                             GTK-CELL-RENDERER-SPIN
                               (:SUPERCLASS GTK-CELL-RENDERER-TEXT :EXPORT T
                                :INTERFACES NIL :TYPE-INITIALIZER
                                "gtk_cell_renderer_spin_get_type")
                               ((ADJUSTMENT GTK-CELL-RENDERER-SPIN-ADJUSTMENT
                                 "adjustment" "GtkAdjustment" T T)
                                (CLIMB-RATE GTK-CELL-RENDERER-SPIN-CLIMB-RATE
                                 "climb-rate" "gdouble" T T)
                                (DIGITS GTK-CELL-RENDERER-SPIN-DIGITS "digits"
                                 "guint" T T)))
             (gobject:get-g-type-definition "GtkCellRendererSpin"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-cell-renderer-spin-properties
  (let ((renderer (make-instance 'gtk:cell-renderer-spin)))
    (is-false (gtk:cell-renderer-spin-adjustment renderer))
    (is (= 0.0d0 (gtk:cell-renderer-spin-climb-rate renderer)))
    (is (= 0 (gtk:cell-renderer-spin-digits renderer)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_cell_renderer_spin_new

(test gtk-cell-renderer-spin-new
  (is (typep (gtk:cell-renderer-spin-new) 'gtk:cell-renderer-spin)))

;;; 2024-2-22

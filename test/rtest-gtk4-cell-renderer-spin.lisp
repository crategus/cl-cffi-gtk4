(in-package :gtk-test)

(def-suite gtk-cell-renderer-spin :in gtk-suite)
(in-suite gtk-cell-renderer-spin)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkCellRendererSpin

(test gtk-cell-renderer-spin-class
  ;; Check type
  (is (g:type-is-object "GtkCellRendererSpin"))
  ;; Check registered name
  (is (eq 'gtk:cell-renderer-spin
          (glib:symbol-for-gtype "GtkCellRendererSpin")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkCellRendererSpin")
          (g:gtype (cffi:foreign-funcall "gtk_cell_renderer_spin_get_type"
                                         :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkCellRendererText")
          (g:type-parent "GtkCellRendererSpin")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkCellRendererSpin")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkCellRendererSpin")))
  ;; Check properties
  (is (equal '("adjustment" "climb-rate" "digits")
             (glib-test:list-properties "GtkCellRendererSpin")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkCellRendererSpin")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkCellRendererSpin"
                                      GTK:CELL-RENDERER-SPIN
                      (:SUPERCLASS GTK:CELL-RENDERER-TEXT
                       :EXPORT T
                       :INTERFACES NIL
                       :TYPE-INITIALIZER "gtk_cell_renderer_spin_get_type")
                      ((ADJUSTMENT CELL-RENDERER-SPIN-ADJUSTMENT
                        "adjustment" "GtkAdjustment" T T)
                       (CLIMB-RATE CELL-RENDERER-SPIN-CLIMB-RATE
                        "climb-rate" "gdouble" T T)
                       (DIGITS CELL-RENDERER-SPIN-DIGITS "digits" "guint" T T)))
             (gobject:get-gtype-definition "GtkCellRendererSpin"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-cell-renderer-spin-properties
  (let* ((gtk-init:*gtk-warn-deprecated* nil)
         (renderer (make-instance 'gtk:cell-renderer-spin)))
    (is-false (gtk:cell-renderer-spin-adjustment renderer))
    (is (= 0.0d0 (gtk:cell-renderer-spin-climb-rate renderer)))
    (is (= 0 (gtk:cell-renderer-spin-digits renderer)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_cell_renderer_spin_new

(test gtk-cell-renderer-spin-new
  (let* ((gtk-init:*gtk-warn-deprecated* nil))
    (is (typep (gtk:cell-renderer-spin-new) 'gtk:cell-renderer-spin))))

;;; 2024-9-20

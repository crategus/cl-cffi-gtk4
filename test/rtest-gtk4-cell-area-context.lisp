(in-package :gtk-test)

(def-suite gtk-cell-area-context :in gtk-suite)
(in-suite gtk-cell-area-context)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkCellAreaContext

(test gtk-cell-area-context-class
  ;; Check type
  (is (g:type-is-object "GtkCellAreaContext"))
  ;; Check registered name
  (is (eq 'gtk:cell-area-context
          (glib:symbol-for-gtype "GtkCellAreaContext")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkCellAreaContext")
          (g:gtype (cffi:foreign-funcall "gtk_cell_area_context_get_type"
                                         :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkCellAreaContext")))
  ;; Check children
  (is (equal '("GtkCellAreaBoxContext")
             (glib-test:list-children "GtkCellAreaContext")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkCellAreaContext")))
  ;; Check class properties
  (is (equal '("area" "minimum-height" "minimum-width"
                      "natural-height" "natural-width")
             (glib-test:list-properties "GtkCellAreaContext")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkCellAreaContext")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkCellAreaContext" GTK:CELL-AREA-CONTEXT
                       (:SUPERCLASS G:OBJECT
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_cell_area_context_get_type")
                       ((AREA CELL-AREA-CONTEXT-AREA "area" "GtkCellArea" T NIL)
                        (MINIMUM-HEIGHT CELL-AREA-CONTEXT-MINIMUM-HEIGHT
                         "minimum-height" "gint" T NIL)
                        (MINIMUM-WIDTH CELL-AREA-CONTEXT-MINIMUM-WIDTH
                         "minimum-width" "gint" T NIL)
                        (NATURAL-HEIGHT CELL-AREA-CONTEXT-NATURAL-HEIGHT
                         "natural-height" "gint" T NIL)
                        (NATURAL-WIDTH CELL-AREA-CONTEXT-NATURAL-WIDTH
                         "natural-width" "gint" T NIL)))
             (gobject:get-gtype-definition "GtkCellAreaContext"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-cell-area-context-properties
  (let ((context (make-instance 'gtk:cell-area-context)))
    (is-false (gtk:cell-area-context-area context))
    (is (= 0 (gtk:cell-area-context-minimum-height context)))
    (signals (error) (setf (gtk:cell-area-context-minimum-height context) 10))
    (is (= 0 (gtk:cell-area-context-minimum-width context)))
    (signals (error) (setf (gtk:cell-area-context-minimum-width context) 10))
    (is (= 0 (gtk:cell-area-context-natural-height context)))
    (signals (error) (setf (gtk:cell-area-context-natural-height context) 10))
    (is (= 0 (gtk:cell-area-context-natural-width context)))
    (signals (error) (setf (gtk:cell-area-context-natural-width context) 10))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_cell_area_context_allocate
;;;     gtk_cell_area_context_reset
;;;     gtk_cell_area_context_get_preferred_width
;;;     gtk_cell_area_context_get_preferred_height
;;;     gtk_cell_area_context_get_preferred_height_for_width
;;;     gtk_cell_area_context_get_preferred_width_for_height
;;;     gtk_cell_area_context_get_allocation
;;;     gtk_cell_area_context_push_preferred_width
;;;     gtk_cell_area_context_push_preferred_height

;;; 2024-9-20

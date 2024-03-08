(in-package :gtk-test)

(def-suite gtk-cell-area-context :in gtk-suite)
(in-suite gtk-cell-area-context)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkCellAreaContext

(test gtk-cell-area-context-class
  ;; Type check
  (is (g:type-is-object "GtkCellAreaContext"))
  ;; Check the registered name
  (is (eq 'gtk:cell-area-context
          (glib:symbol-for-gtype "GtkCellAreaContext")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkCellAreaContext")
          (g:gtype (cffi:foreign-funcall "gtk_cell_area_context_get_type"
                                         :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkCellAreaContext")))
  ;; Check the children
  (is (equal '("GtkCellAreaBoxContext")
             (list-children "GtkCellAreaContext")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkCellAreaContext")))
  ;; Check the class properties
  (is (equal '("area" "minimum-height" "minimum-width"
                      "natural-height" "natural-width")
             (list-properties "GtkCellAreaContext")))
  ;; Check the list of signals
  (is (equal '()
             (list-signals "GtkCellAreaContext")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkCellAreaContext" GTK-CELL-AREA-CONTEXT
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_cell_area_context_get_type")
                       ((AREA GTK-CELL-AREA-CONTEXT-AREA "area" "GtkCellArea" T
                         NIL)
                        (MINIMUM-HEIGHT GTK-CELL-AREA-CONTEXT-MINIMUM-HEIGHT
                         "minimum-height" "gint" T NIL)
                        (MINIMUM-WIDTH GTK-CELL-AREA-CONTEXT-MINIMUM-WIDTH
                         "minimum-width" "gint" T NIL)
                        (NATURAL-HEIGHT GTK-CELL-AREA-CONTEXT-NATURAL-HEIGHT
                         "natural-height" "gint" T NIL)
                        (NATURAL-WIDTH GTK-CELL-AREA-CONTEXT-NATURAL-WIDTH
                         "natural-width" "gint" T NIL)))
             (gobject:get-g-type-definition "GtkCellAreaContext"))))

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

;;; 2024-2-21

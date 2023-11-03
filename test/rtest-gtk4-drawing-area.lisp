(in-package :gtk-test)

(def-suite gtk-drawing-area :in gtk-suite)
(in-suite gtk-drawing-area)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkDrawingArea

(test gtk-drawing-area-class
  ;; Type check
  (is (g:type-is-object "GtkDrawingArea"))
  ;; Check the registered name
  (is (eq 'gtk:drawing-area
          (glib:symbol-for-gtype "GtkDrawingArea")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkDrawingArea")
          (g:gtype (cffi:foreign-funcall "gtk_drawing_area_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkDrawingArea")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkDrawingArea")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (list-interfaces "GtkDrawingArea")))
  ;; Check the properties
  (is (equal '("content-height" "content-width")
             (list-properties "GtkDrawingArea")))
  ;; Check the list of signals
  (is (equal '("resize")
             (list-signals "GtkDrawingArea")))
  ;; CSS name
  (is (string= "widget"
               (gtk:widget-class-css-name "GtkDrawingArea")))
  ;; CSS classes
  (is (equal '()
             (gtk:widget-css-classes (make-instance 'gtk:drawing-area))))
  ;; Accessible role
  (is (eq :widget (gtk:widget-class-accessible-role "GtkDrawingArea")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkDrawingArea" GTK-DRAWING-AREA
                       (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                        ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
                        :TYPE-INITIALIZER "gtk_drawing_area_get_type")
                       ((CONTENT-HEIGHT GTK-DRAWING-AREA-CONTENT-HEIGHT
                         "content-height" "gint" T T)
                        (CONTENT-WIDTH GTK-DRAWING-AREA-CONTENT-WIDTH
                         "content-width" "gint" T T)))
             (gobject:get-g-type-definition "GtkDrawingArea"))))

;;; --- Properties -------------------------------------------------------------

;;;     content-height
;;;     content-width

(test gtk-drawing-area-properties
  (let ((area (make-instance 'gtk:drawing-area)))
    (is (= 0 (gtk:drawing-area-content-height area)))
    (is (= 0 (gtk:drawing-area-content-width area)))))

;;; --- Signals ----------------------------------------------------------------

;;;     resize

;;; --- Callbacks --------------------------------------------------------------

;;;     GtkDrawingAreaDrawFunc

;;; --- Functions --------------------------------------------------------------

;;;     gtk_drawing_area_new

(test gtk-drawing-area-new
  (let ((area (gtk:drawing-area-new)))
    (is (= 0 (gtk:drawing-area-content-height area)))
    (is (= 0 (gtk:drawing-area-content-width area)))))

;;;     gtk_drawing_area_set_draw_func

;;; --- 2023-11-1 --------------------------------------------------------------

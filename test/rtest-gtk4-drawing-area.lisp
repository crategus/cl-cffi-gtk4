(in-package :gtk-test)

(def-suite gtk-drawing-area :in gtk-suite)
(in-suite gtk-drawing-area)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkDrawingArea

(test gtk-drawing-area-class
  ;; Check type
  (is (g:type-is-object "GtkDrawingArea"))
  ;; Check registered name
  (is (eq 'gtk:drawing-area
          (glib:symbol-for-gtype "GtkDrawingArea")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkDrawingArea")
          (g:gtype (cffi:foreign-funcall "gtk_drawing_area_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkDrawingArea")))
  ;; Check children
  (is (equal '()
             (gtk-test:list-children "GtkDrawingArea")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (gtk-test:list-interfaces "GtkDrawingArea")))
  ;; Check properties
  (is (equal '("content-height" "content-width")
             (gtk-test:list-properties "GtkDrawingArea")))
  ;; Check signals
  (is (equal '("resize")
             (gtk-test:list-signals "GtkDrawingArea")))
  ;; Check CSS name
  (is (string= "widget"
               (gtk:widget-class-css-name "GtkDrawingArea")))
  ;; Check accessible role
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

;;; 2024-7-4

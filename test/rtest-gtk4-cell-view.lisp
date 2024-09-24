(in-package :gtk-test)

(def-suite gtk-cell-view :in gtk-suite)
(in-suite gtk-cell-view)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkCellView

(test gtk-cell-view-class
  ;; Check type
  (is (g:type-is-object "GtkCellView"))
  ;; Check registered name
  (is (eq 'gtk:cell-view
          (glib:symbol-for-gtype "GtkCellView")))
  ;; Check initializer
  (is (eq (g:gtype "GtkCellView")
          (g:gtype (cffi:foreign-funcall "gtk_cell_view_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkCellView")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkCellView")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkCellLayout" "GtkOrientable")
             (glib-test:list-interfaces "GtkCellView")))
  ;; Check properties
  (is (equal '("cell-area" "cell-area-context" "draw-sensitive" "fit-model"
               "model" "orientation")
             (glib-test:list-properties "GtkCellView")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkCellView")))
  ;; Check CSS name
  (is (string= "cellview"
               (gtk:widget-class-css-name "GtkCellView")))
  ;; Check accessible role
  (is (eq :widget (gtk:widget-class-accessible-role "GtkCellView")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkCellView" GTK:CELL-VIEW
                       (:SUPERCLASS GTK:WIDGET
                        :EXPORT T
                        :INTERFACES
                        ("GtkAccessible" "GtkBuildable" "GtkCellLayout"
                         "GtkConstraintTarget" "GtkOrientable")
                        :TYPE-INITIALIZER "gtk_cell_view_get_type")
                       ((CELL-AREA CELL-VIEW-CELL-AREA
                         "cell-area" "GtkCellArea" T NIL)
                        (CELL-AREA-CONTEXT CELL-VIEW-CELL-AREA-CONTEXT
                         "cell-area-context" "GtkCellAreaContext" T NIL)
                        (DRAW-SENSITIVE CELL-VIEW-DRAW-SENSITIVE
                         "draw-sensitive" "gboolean" T T)
                        (FIT-MODEL CELL-VIEW-FIT-MODEL
                         "fit-model" "gboolean" T T)
                        (MODEL CELL-VIEW-MODEL "model" "GtkTreeModel" T T)))
             (gobject:get-gtype-definition "GtkCellView"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-cell-view-properties
  (let* ((gtk-init:*gtk-warn-deprecated* nil)
         (view (make-instance 'gtk:cell-view)))
    (is (typep (gtk:cell-view-cell-area view) 'gtk:cell-area-box))
    (is (typep (gtk:cell-view-cell-area-context view) 'gtk:cell-area-context))
    (is-false (gtk:cell-view-draw-sensitive view))
    (is-false (gtk:cell-view-fit-model view))
    (is-false (gtk:cell-view-model view))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_cell_view_new

(test gtk-cell-view-new
  (let ((gtk-init:*gtk-warn-deprecated* nil))
    (is (typep (gtk:cell-view-new) 'gtk:cell-view))))

;;;     gtk_cell_view_new_with_context

(test gtk-cell-view-new-with-context
  (let* ((gtk-init:*gtk-warn-deprecated* nil)
         (area (make-instance 'gtk:cell-area-box))
         (context (make-instance 'gtk:cell-area-context))
         view)
    (is (typep (setf view
                     (gtk:cell-view-new-with-context area context))
               'gtk:cell-view))
    (is (eq area (gtk:cell-view-cell-area view)))
    (is (eq context (gtk:cell-view-cell-area-context view)))))

;;;     gtk_cell_view_new_with_text

(test gtk-cell-view-new-with-text
  (let ((gtk-init:*gtk-warn-deprecated* nil))
    (is (typep (gtk:cell-view-new-with-text "text") 'gtk:cell-view))))

;;;     gtk_cell_view_new_with_markup

(test gtk-cell-view-new-with-markup
  (let ((gtk-init:*gtk-warn-deprecated* nil))
    (is (typep (gtk:cell-view-new-with-text "<b>text</b>") 'gtk:cell-view))))

;;;     gtk_cell_view_new_with_texture

(test gtk-cell-view-new-with-textture
  (let* ((gtk-init:*gtk-warn-deprecated* nil)
         (path (glib-sys:sys-path "test/resource/ducky.png"))
         (texture (gdk:texture-new-from-filename path)))
    (is (typep (gtk:cell-view-new-with-texture texture) 'gtk:cell-view))))

;;;     gtk_cell_view_set_displayed_row
;;;     gtk_cell_view_get_displayed_row

;;; 2024-9-20

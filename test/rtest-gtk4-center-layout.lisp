(in-package :gtk-test)

(def-suite gtk-center-layout :in gtk-suite)
(in-suite gtk-center-layout)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkCenterLayout

(test gtk-center-layout-class
  ;; Check type
  (is (g:type-is-object "GtkCenterLayout"))
  ;; Check registered name
  (is (eq 'gtk:center-layout
          (glib:symbol-for-gtype "GtkCenterLayout")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkCenterLayout")
          (g:gtype (cffi:foreign-funcall "gtk_center_layout_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkLayoutManager")
          (g:type-parent "GtkCenterLayout")))
  ;; Check children
  (is (equal '()
             (gtk-test:list-children "GtkCenterLayout")))
  ;; Check interfaces
  (is (equal '()
             (gtk-test:list-interfaces "GtkCenterLayout")))
  ;; Check properties
  (is (equal '("shrink-center-last")
             (gtk-test:list-properties "GtkCenterLayout")))
  ;; Check signals
  (is (equal '()
             (gtk-test:list-signals "GtkCenterLayout")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkCenterLayout" GTK-CENTER-LAYOUT
                               (:SUPERCLASS GTK-LAYOUT-MANAGER :EXPORT T
                                :INTERFACES NIL :TYPE-INITIALIZER
                                "gtk_center_layout_get_type")
                               ((SHRINK-CENTER-LAST
                                 GTK-CENTER-LAYOUT-SHRINK-CENTER-LAST
                                 "shrink-center-last" "gboolean" T T)))
             (gobject:get-g-type-definition "GtkCenterLayout"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-center-layout-shrink-center-last
  (let ((manager (make-instance 'gtk:center-layout)))
    (is-true (gtk:center-layout-shrink-center-last manager))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_center_layout_new

(test gtk:center-layout-new
  (is (typep (gtk:center-layout-new) 'gtk:center-layout)))

;;;     gtk_center_layout_set_orientation
;;;     gtk_center_layout_get_orientation
;;;     gtk_center_layout_set_baseline_position
;;;     gtk_center_layout_get_baseline_position
;;;     gtk_center_layout_set_start_widget
;;;     gtk_center_layout_get_start_widget
;;;     gtk_center_layout_set_center_widget
;;;     gtk_center_layout_get_center_widget
;;;     gtk_center_layout_set_end_widget
;;;     gtk_center_layout_get_end_widget

(test gtk-center-layout-widget
  (let* ((start (make-instance 'gtk:button))
         (center (make-instance 'gtk:button))
         (end (make-instance 'gtk:button))
         (box (make-instance 'gtk:center-box
                             :start-widget start
                             :center-widget center
                             :end-widget end))
         (layout (gtk:widget-layout-manager box)))
    (is (typep layout 'gtk:center-layout))
    (is (eq :horizontal (gtk:center-layout-orientation layout)))
    (is (eq :center (gtk:center-layout-baseline-position layout)))
    (is (eq start (gtk:center-layout-start-widget layout)))
    (is (eq center (gtk:center-layout-center-widget layout)))
    (is (eq end (gtk:center-layout-end-widget layout)))))

;;; 2024-4-19

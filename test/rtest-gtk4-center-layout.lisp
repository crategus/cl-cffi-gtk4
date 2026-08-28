(in-package :gtk-test)

(def-suite gtk-center-layout :in gtk-layout-managers)
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
             (glib-test:list-children "GtkCenterLayout")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkCenterLayout")))
  ;; Check properties
  (is (equal '("shrink-center-last")
             (glib-test:list-properties "GtkCenterLayout")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkCenterLayout")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkCenterLayout" GTK:CENTER-LAYOUT
                      (:SUPERCLASS GTK:LAYOUT-MANAGER
                       :EXPORT T
                       :INTERFACES NIL
                       :TYPE-INITIALIZER "gtk_center_layout_get_type")
                      ((SHRINK-CENTER-LAST CENTER-LAYOUT-SHRINK-CENTER-LAST
                        "shrink-center-last" "gboolean" T T)))
             (gobject:get-gtype-definition "GtkCenterLayout"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-center-layout-shrink-center-last
  (glib-test:with-check-memory (manager)
    (is (typep (setf manager
                     (make-instance 'gtk:center-layout)) 'gtk:center-layout))
    (is-true (gtk:center-layout-shrink-center-last manager))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_center_layout_new

(test gtk-center-layout-new
  (glib-test:with-check-memory (manager)
    (is (typep (setf manager
                     (gtk:center-layout-new)) 'gtk:center-layout))))

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
  (glib-test:with-check-memory (layout box start center end)
    (is (typep (setf start (make-instance 'gtk:button)) 'gtk:button))
    (is (typep (setf center (make-instance 'gtk:button)) 'gtk:button))
    (is (typep (setf end (make-instance 'gtk:button)) 'gtk:button))
    (is (typep (setf box (make-instance 'gtk:center-box
                                        :start-widget start
                                        :center-widget center
                                        :end-widget end)) 'gtk:center-box))
    (is (typep (setf layout
                     (gtk:widget-layout-manager box)) 'gtk:center-layout))
    (is (eq :horizontal (gtk:center-layout-orientation layout)))
    (is (eq :center (gtk:center-layout-baseline-position layout)))
    (is (eq start (gtk:center-layout-start-widget layout)))
    (is (eq center (gtk:center-layout-center-widget layout)))
    (is (eq end (gtk:center-layout-end-widget layout)))

    (is-false (setf (gtk:center-box-start-widget box) nil))
    (is-false (setf (gtk:center-box-center-widget box) nil))
    (is-false (setf (gtk:center-box-end-widget box) nil))
    (is-false (setf (gtk:widget-layout-manager box) nil))))

;;; 2026-07-20

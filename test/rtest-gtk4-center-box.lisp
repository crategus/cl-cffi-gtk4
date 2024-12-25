(in-package :gtk-test)

(def-suite gtk-center-box :in gtk-layout-widgets)
(in-suite gtk-center-box)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkCenterBox

(test gtk-center-box-class
  ;; Check type
  (is (g:type-is-object "GtkCenterBox"))
  ;; Check registered name
  (is (eq 'gtk:center-box
          (glib:symbol-for-gtype "GtkCenterBox")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkCenterBox")
          (g:gtype (cffi:foreign-funcall "gtk_center_box_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkCenterBox")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkCenterBox")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkOrientable")
             (glib-test:list-interfaces "GtkCenterBox")))
  ;; Check properties
  (is (equal '("baseline-position" "center-widget" "end-widget" "orientation"
               "shrink-center-last" "start-widget")
             (glib-test:list-properties "GtkCenterBox")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkCenterBox")))
  ;; Check CSS name
  (is (string= "box"
               (gtk:widget-class-css-name "GtkCenterBox")))
  ;; Check accessible role
  (is (eq :generic (gtk:widget-class-accessible-role "GtkCenterBox")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkCenterBox" GTK:CENTER-BOX
                       (:SUPERCLASS GTK:WIDGET
                        :EXPORT T
                        :INTERFACES
                        ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
                         "GtkOrientable")
                        :TYPE-INITIALIZER "gtk_center_box_get_type")
                       ((BASELINE-POSITION CENTER-BOX-BASELINE-POSITION
                         "baseline-position" "GtkBaselinePosition" T T)
                        (CENTER-WIDGET CENTER-BOX-CENTER-WIDGET
                         "center-widget" "GtkWidget" T T)
                        (END-WIDGET CENTER-BOX-END-WIDGET
                         "end-widget" "GtkWidget" T T)
                        (SHRINK-CENTER-LAST CENTER-BOX-SHRINK-CENTER-LAST
                         "shrink-center-last" "gboolean" T T)
                        (START-WIDGET CENTER-BOX-START-WIDGET
                         "start-widget" "GtkWidget" T T)))
             (gobject:get-gtype-definition "GtkCenterBox"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-center-box-properties.1
  (glib-test:with-check-memory (box)
    (setf box (gtk:center-box-new))
    (is (eq :center (gtk:center-box-baseline-position box)))
    (is-false (gtk:center-box-center-widget box))
    (is-false (gtk:center-box-end-widget box))
    #+gtk-4-12
    (is-true (gtk:center-box-shrink-center-last box))
    (is-false (gtk:center-box-start-widget box))))

(test gtk-center-box-properties.2
  (glib-test:with-check-memory (box center start end)
    (setf box (gtk:center-box-new))
    (setf center (gtk:button-new))
    (setf start (gtk:button-new))
    (setf end (gtk:button-new))
    (is (eq :top (setf (gtk:center-box-baseline-position box) :top)))
    (is (eq :top (gtk:center-box-baseline-position box)))
    (is (typep (setf (gtk:center-box-center-widget box) center) 'gtk:button))
    (is (eq center (gtk:center-box-center-widget box)))
    (is (typep (setf (gtk:center-box-end-widget box) end) 'gtk:button))
    (is (eq end (gtk:center-box-end-widget box)))
    (is (typep (setf (gtk:center-box-start-widget box) start) 'gtk:button))
    (is (eq start (gtk:center-box-start-widget box)))
    ;; Remove buttons from center box
    (is-false (setf (gtk:center-box-start-widget box) nil))
    (is-false (setf (gtk:center-box-end-widget box) nil))
    (is-false (setf (gtk:center-box-center-widget box) nil))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_center_box_new

(test gtk-center-box-new
  (glib-test:with-check-memory ()
    (is (typep (gtk:center-box-new) 'gtk:center-box))
    (is (= 1 (g:object-ref-count (make-instance 'gtk:center-box))))
    (is (= 1 (g:object-ref-count (gtk:center-box-new))))))

;;; 2024-12-23

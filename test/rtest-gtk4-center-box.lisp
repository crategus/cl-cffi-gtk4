(in-package :gtk-test)

(def-suite gtk-center-box :in gtk-suite)
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
             (gtk-test:list-children "GtkCenterBox")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkOrientable")
             (gtk-test:list-interfaces "GtkCenterBox")))
  ;; Check properties
  (is (equal '("baseline-position" "center-widget" "end-widget" "orientation"
               "shrink-center-last" "start-widget")
             (gtk-test:list-properties "GtkCenterBox")))
  ;; Check signals
  (is (equal '()
             (gtk-test:list-signals "GtkCenterBox")))
  ;; Check CSS name
  (is (string= "box"
               (gtk:widget-class-css-name "GtkCenterBox")))
  ;; Check accessible role
  (is (eq :generic (gtk:widget-class-accessible-role "GtkCenterBox")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkCenterBox" GTK-CENTER-BOX
                               (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                                ("GtkAccessible" "GtkBuildable"
                                 "GtkConstraintTarget" "GtkOrientable")
                                :TYPE-INITIALIZER "gtk_center_box_get_type")
                               ((BASELINE-POSITION
                                 GTK-CENTER-BOX-BASELINE-POSITION
                                 "baseline-position" "GtkBaselinePosition" T T)
                                (CENTER-WIDGET GTK-CENTER-BOX-CENTER-WIDGET
                                 "center-widget" "GtkWidget" T T)
                                (END-WIDGET GTK-CENTER-BOX-END-WIDGET
                                 "end-widget" "GtkWidget" T T)
                                (SHRINK-CENTER-LAST
                                 GTK-CENTER-BOX-SHRINK-CENTER-LAST
                                 "shrink-center-last" "gboolean" T T)
                                (START-WIDGET GTK-CENTER-BOX-START-WIDGET
                                 "start-widget" "GtkWidget" T T)))
             (gobject:get-g-type-definition "GtkCenterBox"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-center-box-properties.1
  (let ((box (gtk:center-box-new)))
    (is (eq :center (gtk:center-box-baseline-position box)))
    (is-false (gtk:center-box-center-widget box))
    (is-false (gtk:center-box-end-widget box))
    #+gtk-4-12
    (is-true (gtk:center-box-shrink-center-last box))
    (is-false (gtk:center-box-start-widget box))))

(test gtk-center-box-properties.2
  (let ((box (gtk:center-box-new))
        (center (gtk:button-new))
        (start (gtk:button-new))
        (end (gtk:button-new)))
    (is (eq :top (setf (gtk:center-box-baseline-position box) :top)))
    (is (eq :top (gtk:center-box-baseline-position box)))
    (is (typep (setf (gtk:center-box-center-widget box) center) 'gtk:button))
    (is (eq center (gtk:center-box-center-widget box)))
    (is (typep (setf (gtk:center-box-end-widget box) end) 'gtk:button))
    (is (eq end (gtk:center-box-end-widget box)))
    (is (typep (setf (gtk:center-box-start-widget box) start) 'gtk:button))
    (is (eq start (gtk:center-box-start-widget box)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_center_box_new

(test gtk-center-box-new
  (is (typep (gtk:center-box-new) 'gtk:center-box)))

;;; 2024-4-11

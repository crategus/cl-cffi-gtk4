(in-package :gtk-test)

(def-suite gtk-center-box :in gtk-suite)
(in-suite gtk-center-box)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkCenterBox

(test gtk-center-box-class
  ;; Type check
  (is (g:type-is-object "GtkCenterBox"))
  ;; Check the registered name
  (is (eq 'gtk:center-box
          (gobject:symbol-for-gtype "GtkCenterBox")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkCenterBox")
          (g:gtype (cffi:foreign-funcall "gtk_center_box_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkCenterBox")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkCenterBox")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkOrientable")
             (list-interfaces "GtkCenterBox")))
  ;; Check the properties
  (is (equal '("baseline-position" "center-widget" "end-widget" "orientation" 
               "start-widget")
             (list-properties "GtkCenterBox")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkCenterBox")))
  ;; CSS information
  (is (string= "box"
               (gtk:widget-class-css-name "GtkCenterBox")))
  (is (string=
"box:dir(ltr)
"
               (gtk:style-context-to-string
                   (gtk:widget-style-context (make-instance 'gtk:center-box))
                   :none)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkCenterBox" GTK-CENTER-BOX
                       (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                        ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
                         "GtkOrientable")
                        :TYPE-INITIALIZER "gtk_center_box_get_type")
                       ((BASELINE-POSITION GTK-CENTER-BOX-BASELINE-POSITION
                         "baseline-position" "GtkBaselinePosition" T T)
                        (CENTER-WIDGET GTK-CENTER-BOX-CENTER-WIDGET
                         "center-widget" "GtkWidget" T T)
                        (END-WIDGET GTK-CENTER-BOX-END-WIDGET "end-widget"
                         "GtkWidget" T T)
                        (START-WIDGET GTK-CENTER-BOX-START-WIDGET
                         "start-widget" "GtkWidget" T T)))
             (gobject:get-g-type-definition "GtkCenterBox"))))

;;; --- Properties -------------------------------------------------------------

;;;     baseline-position
;;;     center-widget                                      Since 4.10
;;;     end-widget                                         Since 4.10
;;;     start-widget                                       Since 4.10

(test gtk-center-box-properties.1
  (let ((box (gtk:center-box-new)))
    (is (eq :center (gtk:center-box-baseline-position box)))
    (is-false (gtk:center-box-center-widget box))
    (is-false (gtk:center-box-end-widget box))
    (is-false (gtk:center-box-start-widget box))))

(test gtk-center-box-properties.2
  (let ((box (gtk:center-box-new))
        (button (gtk:button-new)))
    (is (eq :top (setf (gtk:center-box-baseline-position box) :top)))
    (is (eq :top (gtk:center-box-baseline-position box)))
    (is (typep (setf (gtk:center-box-center-widget box) button) 'gtk:button))
    (is (eq button (gtk:center-box-center-widget box)))
    (is (typep (setf (gtk:center-box-end-widget box) button) 'gtk:button))
    (is (eq button (gtk:center-box-end-widget box)))
    (is (typep (setf (gtk:center-box-start-widget box) button) 'gtk:button))
    (is (eq button (gtk:center-box-start-widget box)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_center_box_new

(test gtk-center-box-new
  (is (typep (gtk:center-box-new) 'gtk:center-box)))

;;; --- 2023-5-2 ---------------------------------------------------------------

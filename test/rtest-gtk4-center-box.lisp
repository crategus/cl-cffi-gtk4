(in-package :gtk-test)

(def-suite gtk-center-box :in gtk-suite)
(in-suite gtk-center-box)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkCenterBox

(test center-box-class
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
  (is (equal '("baseline-position" "orientation")
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
                         "baseline-position" "GtkBaselinePosition" T T)))
             (gobject:get-g-type-definition "GtkCenterBox"))))

;;; --- Properties -------------------------------------------------------------

;;;     baseline-position

;;; --- Functions --------------------------------------------------------------

;;;     gtk_center_box_new
;;;     gtk_center_box_set_start_widget
;;;     gtk_center_box_set_center_widget
;;;     gtk_center_box_set_end_widget
;;;     gtk_center_box_get_start_widget
;;;     gtk_center_box_get_center_widget
;;;     gtk_center_box_get_end_widget

;;; --- 2023-3-18 --------------------------------------------------------------

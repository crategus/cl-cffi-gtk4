(in-package :gtk-test)

(def-suite gtk-box :in gtk-suite)
(in-suite gtk-box)

;; GtkPrinterOptionWidget is a child of GtkBox
#-windows
(eval-when (:compile-toplevel :load-toplevel :execute)
  (cffi:foreign-funcall "gtk_printer_option_widget_get_type" :size))

;;; --- Types and Values -------------------------------------------------------

;;;     GtkBox

(test gtk-box-class
  ;; Type check
  (is (g:type-is-object "GtkBox"))
  ;; Check the registered name
  (is (eq 'gtk:box
          (glib:symbol-for-gtype "GtkBox")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkBox")
          (g:gtype (cffi:foreign-funcall "gtk_box_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkBox")))
  ;; Check the children
  #-windows
  (is (equal '("GtkColorEditor" "GtkPlacesView" "GtkPrinterOptionWidget"
               "GtkShortcutsGroup" "GtkShortcutsSection")
             (list-children "GtkBox")))
  #+windows
  (is (equal '("GtkColorEditor" "GtkPlacesView" "GtkShortcutsGroup"
               "GtkShortcutsSection")
             (list-children "GtkBox")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkOrientable")
             (list-interfaces "GtkBox")))
  ;; Check the properties
  (is (equal '("baseline-child" "baseline-position" "homogeneous" "orientation"
               "spacing")
             (list-properties "GtkBox")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkBox")))
  ;; CSS name
  (is (string= "box"
               (gtk:widget-class-css-name "GtkBox")))
  ;; CSS classes
  (is (equal '("horizontal")
             (gtk:widget-css-classes (make-instance 'gtk:box))))
  ;; Accessible role
  (is (eq :generic (gtk:widget-class-accessible-role "GtkBox")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkBox" GTK-BOX
                               (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                                ("GtkAccessible" "GtkBuildable"
                                 "GtkConstraintTarget" "GtkOrientable")
                                :TYPE-INITIALIZER "gtk_box_get_type")
                               ((BASELINE-CHILD GTK-BOX-BASELINE-CHILD
                                 "baseline-child" "gint" T T)
                                (BASELINE-POSITION GTK-BOX-BASELINE-POSITION
                                 "baseline-position" "GtkBaselinePosition" T T)
                                (HOMOGENEOUS GTK-BOX-HOMOGENEOUS "homogeneous"
                                 "gboolean" T T)
                                (SPACING GTK-BOX-SPACING "spacing" "gint" T T)))
             (gobject:get-g-type-definition "GtkBox"))))

;;; --- Properties -------------------------------------------------------------

;;;     baseline-child                                     Since 4.12
;;;     baseline-position
;;;     homogeneous
;;;     spacing

(test gtk-box-properties
  (let ((box (make-instance 'gtk:box :orientation :vertical :spacing 12)))
    (is (eq :vertical (gtk:orientable-orientation box)))
    (is (eq :center (gtk:box-baseline-position box)))
    (is-false (gtk:box-homogeneous box))
    (is (= 12 (gtk:box-spacing box)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_box_new

(test gtk-box-new
  ;; Create a box
  (let ((box (gtk:box-new :vertical 12)))
    (is (eq :vertical (gtk:orientable-orientation box)))
    (is (eq :center (gtk:box-baseline-position box)))
    (is-false (gtk:box-homogeneous box))
    (is (= 12 (gtk:box-spacing box))))
  ;; Create a box with the default value for spacing
  (let ((box (gtk:box-new :horizontal)))
    (is (eq :horizontal (gtk:orientable-orientation box)))
    (is (eq :center (gtk:box-baseline-position box)))
    (is-false (gtk:box-homogeneous box))
    (is (= 0 (gtk:box-spacing box))))
  ;; Use make-instance with default values
  (let ((box (make-instance 'gtk:box)))
    (is (eq :horizontal (gtk:orientable-orientation box)))
    (is (eq :center (gtk:box-baseline-position box)))
    (is-false (gtk:box-homogeneous box))
    (is (= 0 (gtk:box-spacing box))))
  ;; Use make-instance and set some properties
  (let ((box (make-instance 'gtk:box
                            :orientation :vertical
                            :baseline-position :top
                            :homogeneous t
                            :spacing 12)))
    (is (eq :vertical (gtk:orientable-orientation box)))
    (is (eq :top (gtk:box-baseline-position box)))
    (is-true (gtk:box-homogeneous box))
    (is (= 12 (gtk:box-spacing box)))))

;;;     gtk_box_append
;;;     gtk_box_prepend
;;;     gtk_box_remove
;;;     gtk_box_insert_child_after
;;;     gtk_box_reorder_child_after

;;; --- 2023-11-4 --------------------------------------------------------------

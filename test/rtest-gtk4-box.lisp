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
  ;; Check type
  (is (g:type-is-object "GtkBox"))
  ;; Check registered name
  (is (eq 'gtk:box
          (glib:symbol-for-gtype "GtkBox")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkBox")
          (g:gtype (cffi:foreign-funcall "gtk_box_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkBox")))
  ;; Check children
  #-windows
  (is (equal '("GtkColorEditor" "GtkPlacesView" "GtkPrinterOptionWidget"
               "GtkShortcutsGroup" "GtkShortcutsSection")
             (glib-test:list-children "GtkBox")))
  #+windows
  (if *first-run-gtk-test*
      (is (equal '("GtkShortcutsGroup" "GtkShortcutsSection")
                 (glib-test:list-children "GtkBox"))))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkOrientable")
             (glib-test:list-interfaces "GtkBox")))
  ;; Check properties
  (is (equal '("baseline-child" "baseline-position" "homogeneous" "orientation"
               "spacing")
             (glib-test:list-properties "GtkBox")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkBox")))
  ;; Check CSS name
  (is (string= "box"
               (gtk:widget-class-css-name "GtkBox")))
  ;; Check accessible role
  (is (eq :generic (gtk:widget-class-accessible-role "GtkBox")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkBox" GTK:BOX
                      (:SUPERCLASS GTK:WIDGET
                       :EXPORT T
                       :INTERFACES
                       ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
                        "GtkOrientable")
                       :TYPE-INITIALIZER "gtk_box_get_type")
                      ((BASELINE-CHILD BOX-BASELINE-CHILD
                        "baseline-child" "gint" T T)
                       (BASELINE-POSITION BOX-BASELINE-POSITION
                        "baseline-position" "GtkBaselinePosition" T T)
                       (HOMOGENEOUS BOX-HOMOGENEOUS
                        "homogeneous" "gboolean" T T)
                       (SPACING BOX-SPACING "spacing" "gint" T T)))
             (gobject:get-gtype-definition "GtkBox"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-box-properties
  (let ((box (make-instance 'gtk:box :orientation :vertical :spacing 12)))
    (is (eq :vertical (gtk:orientable-orientation box)))
    (is (= -1 (gtk:box-baseline-child box)))
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

(test gtk-box-append/prepend/remove
  (let ((box (make-instance 'gtk:box))
        (child1 (make-instance 'gtk:button))
        (child2 (make-instance 'gtk:button))
        (child3 (make-instance 'gtk:button)))

    (is-false (gtk:box-append box child1))
    (is-false (gtk:box-prepend box child2))
    (is-false (gtk:box-append box child3))
    ;; The order is child2, child1, child3
    (is (eq child2 (gtk:widget-first-child box)))
    (is (eq child1 (gtk:widget-next-sibling child2)))
    (is (eq child3 (gtk:widget-last-child box)))

    (is-false (gtk:box-remove box child3))
    (is (eq child1 (gtk:widget-last-child box)))))

;;;     gtk_box_insert_child_after
;;;     gtk_box_reorder_child_after

(test gtk-box-insert/reorder-child-after
  (let ((box (make-instance 'gtk:box))
        (child1 (make-instance 'gtk:button))
        (child2 (make-instance 'gtk:button))
        (child3 (make-instance 'gtk:button)))
    (is-false (gtk:box-append box child1))
    (is-false (gtk:box-append box child2))

    (is-false (gtk:box-insert-child-after box child3 child1))
    (is (eq child1 (gtk:widget-first-child box)))
    (is (eq child2 (gtk:widget-last-child box)))

    (is-false (gtk:box-reorder-child-after box child3 child2))
    (is (eq child3 (gtk:widget-last-child box)))
    (is-false (gtk:box-reorder-child-after box child3 nil))
    (is (eq child3 (gtk:widget-first-child box)))))

;;; 2024-4-11

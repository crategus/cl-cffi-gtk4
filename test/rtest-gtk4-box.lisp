(in-package :gtk-test)

(def-suite gtk-box :in gtk-layout-widgets)
(in-suite gtk-box)

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
  (is (or (equal '("GtkShortcutsGroup" "GtkShortcutsSection")
                 (glib-test:list-children "GtkBox"))
          (equal '("GtkColorEditor" "GtkPlacesView" "GtkShortcutsGroup"
                   "GtkShortcutsSection")
                 (glib-test:list-children "GtkBox"))))
  #+windows
  (if *first-run-testsuite*
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
  (glib-test:with-check-memory (box)
    (setf box (make-instance 'gtk:box :orientation :vertical :spacing 12))
    (is (eq :vertical (gtk:orientable-orientation box)))
    (is (= -1 (gtk:box-baseline-child box)))
    (is (eq :center (gtk:box-baseline-position box)))
    (is-false (gtk:box-homogeneous box))
    (is (= 12 (gtk:box-spacing box)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_box_new

(test gtk-box-new
  ;; Create a box
  (glib-test:with-check-memory (box)
    (setf box (gtk:box-new :vertical 12))
    (is (eq :vertical (gtk:orientable-orientation box)))
    (is (eq :center (gtk:box-baseline-position box)))
    (is-false (gtk:box-homogeneous box))
    (is (= 12 (gtk:box-spacing box))))
  ;; Create a box with the default value for spacing
  (glib-test:with-check-memory (box)
    (setf box (gtk:box-new :horizontal))
    (is (eq :horizontal (gtk:orientable-orientation box)))
    (is (eq :center (gtk:box-baseline-position box)))
    (is-false (gtk:box-homogeneous box))
    (is (= 0 (gtk:box-spacing box))))
  ;; Use make-instance with default values
  (glib-test:with-check-memory (box)
    (setf box (make-instance 'gtk:box))
    (is (eq :horizontal (gtk:orientable-orientation box)))
    (is (eq :center (gtk:box-baseline-position box)))
    (is-false (gtk:box-homogeneous box))
    (is (= 0 (gtk:box-spacing box))))
  ;; Use make-instance and set some properties
  (glib-test:with-check-memory (box)
    (setf box (make-instance 'gtk:box
                             :orientation :vertical
                             :baseline-position :top
                             :homogeneous t
                             :spacing 12))
    (is (eq :vertical (gtk:orientable-orientation box)))
    (is (eq :top (gtk:box-baseline-position box)))
    (is-true (gtk:box-homogeneous box))
    (is (= 12 (gtk:box-spacing box)))))

;;;     gtk_box_append
;;;     gtk_box_prepend
;;;     gtk_box_remove

(test gtk-box-append/prepend/remove
  (glib-test:with-check-memory (box child1 child2 child3)
    (setf box (make-instance 'gtk:box))
    (setf child1 (make-instance 'gtk:button))
    (setf child2 (make-instance 'gtk:button))
    (setf child3 (make-instance 'gtk:button))

    (is (= 1 (g:object-ref-count child1)))
    (is (= 1 (g:object-ref-count child2)))
    (is (= 1 (g:object-ref-count child3)))

    (is-false (gtk:box-append box child1))
    (is-false (gtk:box-prepend box child2))
    (is-false (gtk:box-append box child3))

    (is (= 2 (g:object-ref-count child1)))
    (is (= 2 (g:object-ref-count child2)))
    (is (= 2 (g:object-ref-count child3)))

    ;; The order is child2, child1, child3
    (is (eq child2 (gtk:widget-first-child box)))
    (is (eq child1 (gtk:widget-next-sibling child2)))
    (is (eq child3 (gtk:widget-last-child box)))

    (is (= 2 (g:object-ref-count child1)))
    (is (= 2 (g:object-ref-count child2)))
    (is (= 2 (g:object-ref-count child3)))

    (is-false (gtk:box-remove box child3))

    (is (= 2 (g:object-ref-count child1)))
    (is (= 2 (g:object-ref-count child2)))
    (is (= 1 (g:object-ref-count child3)))

    (is (eq child1 (gtk:widget-last-child box)))

    (is-false (gtk:box-remove box child1))
    (is-false (gtk:box-remove box child2))))

;;;     gtk_box_insert_child_after
;;;     gtk_box_reorder_child_after

(test gtk-box-insert/reorder-child-after
  (glib-test:with-check-memory (box child1 child2 child3)
    (setf box (make-instance 'gtk:box))
    (setf child1 (make-instance 'gtk:button))
    (setf child2 (make-instance 'gtk:button))
    (setf child3 (make-instance 'gtk:button))
    (is-false (gtk:box-append box child1))
    (is-false (gtk:box-append box child2))

    (is-false (gtk:box-insert-child-after box child3 child1))
    (is (eq child1 (gtk:widget-first-child box)))
    (is (eq child2 (gtk:widget-last-child box)))

    (is-false (gtk:box-reorder-child-after box child3 child2))
    (is (eq child3 (gtk:widget-last-child box)))
    (is-false (gtk:box-reorder-child-after box child3 nil))
    (is (eq child3 (gtk:widget-first-child box)))

    (is-false (gtk:box-remove box child1))
    (is-false (gtk:box-remove box child2))
    (is-false (gtk:box-remove box child3))))

;;; 2025-4-26

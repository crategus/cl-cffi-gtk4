(in-package :gtk-test)

(def-suite gtk-range :in gtk-suite)
(in-suite gtk-range)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkRange

(test gtk-range-class
  ;; Type check
  (is (g:type-is-object "GtkRange"))
  ;; Check the registered name
  (is (eq 'gtk:range
          (glib:symbol-for-gtype "GtkRange")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkRange")
          (g:gtype (cffi:foreign-funcall "gtk_range_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkRange")))
  ;; Check the children
  (is (equal '("GtkScale")
             (list-children "GtkRange")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkAccessibleRange" "GtkOrientable")
             (list-interfaces "GtkRange")))
  ;; Check the properties
  (is (equal '("adjustment" "fill-level" "inverted" "orientation"
               "restrict-to-fill-level" "round-digits" "show-fill-level")
             (list-properties "GtkRange")))
  ;; Check the signals
  (is (equal '("adjust-bounds" "change-value" "move-slider" "value-changed")
             (list-signals "GtkRange")))
  ;; CSS name
  (is (string= "range"
               (gtk:widget-class-css-name "GtkRange")))
  ;; CSS classes
  (is (equal '("horizontal")
             (gtk:widget-css-classes (make-instance 'gtk:range))))
  ;; Accessible role
  (is (eq :generic (gtk:widget-class-accessible-role "GtkRange")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkRange" GTK-RANGE
                       (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                        ("GtkAccessible" "GtkAccessibleRange" "GtkBuildable"
                         "GtkConstraintTarget" "GtkOrientable")
                        :TYPE-INITIALIZER "gtk_range_get_type")
                       ((ADJUSTMENT GTK-RANGE-ADJUSTMENT "adjustment"
                         "GtkAdjustment" T T)
                        (FILL-LEVEL GTK-RANGE-FILL-LEVEL "fill-level" "gdouble"
                         T T)
                        (INVERTED GTK-RANGE-INVERTED "inverted" "gboolean" T T)
                        (RESTRICT-TO-FILL-LEVEL
                         GTK-RANGE-RESTRICT-TO-FILL-LEVEL
                         "restrict-to-fill-level" "gboolean" T T)
                        (ROUND-DIGITS GTK-RANGE-ROUND-DIGITS "round-digits"
                         "gint" T T)
                        (SHOW-FILL-LEVEL GTK-RANGE-SHOW-FILL-LEVEL
                         "show-fill-level" "gboolean" T T)))
             (gobject:get-g-type-definition "GtkRange"))))

;;; --- Properties -------------------------------------------------------------

;;;     adjustment
;;;     fill-level
;;;     inverted
;;;     restrict-to-fill-level
;;;     round-digits
;;;     show-fill-level

(test gtk-range-properties
  (let ((range (make-instance 'gtk:range)))
    (is (typep (gtk:range-adjustment range) 'gtk:adjustment))
    (is (= 1.7976931348623157d308 (gtk:range-fill-level range)))
    (is-false (gtk:range-inverted range))
    (is-true (gtk:range-restrict-to-fill-level range))
    (is (= -1 (gtk:range-round-digits range)))
    (is-false (gtk:range-show-fill-level range))))

;;; --- Signals ----------------------------------------------------------------

;;;     adjust-bounds
;;;     change-value
;;;     move-slider
;;;     value-changed

;;; --- Functions --------------------------------------------------------------

;;;     gtk_range_get_value
;;;     gtk_range_set_value

(test gtk-range-value
  (let ((range (make-instance 'gtk:range
                              :adjustment
                              (make-instance 'gtk:adjustment
                                             :lower 10.0
                                             :upper 20.00))))
    ;; Default value
    (is (=  0.0 (gtk:range-value range)))
    (is (=  0.0 (gtk:adjustment-value (gtk:range-adjustment range))))
    ;; Value in the range of the adjustment
    (is (= 15.0 (setf (gtk:range-value range) 15.0)))
    (is (= 15.0 (gtk:range-value range)))
    ;; Clamp greater value
    (is (= 30.0 (setf (gtk:range-value range) 30.0)))
    (is (= 20.0 (gtk:range-value range)))
    ;; Clamp lower value
    (is (=  5.0 (setf (gtk:range-value range) 5.0)))
    (is (= 10.0 (gtk:range-value range)))))

;;;     gtk_range_set_increments
;;;     gtk_range_set_range
;;;     gtk_range_get_flippable
;;;     gtk_range_set_flippable
;;;     gtk_range_get_range_rect
;;;     gtk_range_get_slider_range
;;;     gtk_range_get_slider_size_fixed
;;;     gtk_range_set_slider_size_fixed

;;; --- 2023-11-4 --------------------------------------------------------------

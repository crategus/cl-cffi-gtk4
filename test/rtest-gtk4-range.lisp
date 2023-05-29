(in-package :gtk-test)

(def-suite gtk-range :in gtk-suite)
(in-suite gtk-range)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkRange

(test range-class
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
  ;; CSS information
  (is (string= "range"
               (gtk:widget-class-css-name "GtkRange")))
  (is (string=
"range.horizontal:dir(ltr)
"
               (gtk:style-context-to-string
                   (gtk:widget-style-context (make-instance 'gtk:range))
                   :none)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkRange" GTK-RANGE
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

;;; --- Signals ----------------------------------------------------------------

;;;     adjust-bounds
;;;     change-value
;;;     move-slider
;;;     value-changed

;;; --- Functions --------------------------------------------------------------

;;;     gtk_range_get_value
;;;     gtk_range_set_value
;;;     gtk_range_set_increments
;;;     gtk_range_set_range
;;;     gtk_range_get_flippable
;;;     gtk_range_set_flippable
;;;     gtk_range_get_range_rect
;;;     gtk_range_get_slider_range
;;;     gtk_range_get_slider_size_fixed
;;;     gtk_range_set_slider_size_fixed

;;; --- 2023-5-29 --------------------------------------------------------------

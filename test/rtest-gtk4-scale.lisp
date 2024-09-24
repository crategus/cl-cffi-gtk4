(in-package :gtk-test)

(def-suite gtk-scale :in gtk-suite)
(in-suite gtk-scale)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkScale

(test gtk-scale-class
  ;; Check type
  (is (g:type-is-object "GtkScale"))
  ;; Check registered name
  (is (eq 'gtk:scale
          (glib:symbol-for-gtype "GtkScale")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkScale")
          (g:gtype (cffi:foreign-funcall "gtk_scale_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkRange")
          (g:type-parent "GtkScale")))
  ;; Check children
  #-windows
  (is (equal '("GtkColorScale")
             (glib-test:list-children "GtkScale")))
  #+windows
  (if *first-run-gtk-test*
    (is (equal '()
               (glib-test:list-children "GtkScale"))))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkAccessibleRange" "GtkOrientable")
             (glib-test:list-interfaces "GtkScale")))
  ;; Check properties
  (is (equal '("digits" "draw-value" "has-origin" "value-pos")
             (glib-test:list-properties "GtkScale")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkScale")))
  ;; Check CSS name
  (is (string= "scale"
               (gtk:widget-class-css-name "GtkScale")))
  ;; Check accessible role
  (is (eq :slider (gtk:widget-class-accessible-role "GtkScale")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkScale" GTK:SCALE
                       (:SUPERCLASS GTK:RANGE
                        :EXPORT T
                        :INTERFACES
                        ("GtkAccessible" "GtkAccessibleRange" "GtkBuildable"
                         "GtkConstraintTarget" "GtkOrientable")
                        :TYPE-INITIALIZER "gtk_scale_get_type")
                       ((DIGITS SCALE-DIGITS "digits" "gint" T T)
                        (DRAW-VALUE SCALE-DRAW-VALUE
                         "draw-value" "gboolean" T T)
                        (HAS-ORIGIN SCALE-HAS-ORIGIN
                         "has-origin" "gboolean" T T)
                        (VALUE-POS SCALE-VALUE-POS
                         "value-pos" "GtkPositionType" T T)))
             (gobject:get-gtype-definition "GtkScale"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-scale-properties
  (let ((scale (make-instance 'gtk:scale)))
    (is (= 1 (gtk:scale-digits scale)))
    (is-false (gtk:scale-draw-value scale))
    (is-true (gtk:scale-has-origin scale))
    (is (eq :top (gtk:scale-value-pos scale)))))

;;; --- Functions --------------------------------------------------------------

;;;     GtkScaleFormatValueFunc

;;;     gtk_scale_new

(test gtk-scale-new
  (is (typep (gtk:scale-new :vertical (make-instance 'gtk:adjustment))
             'gtk:scale)))

;;;     gtk_scale_new_with_range

(test gtk-scale-new-with-range
  (is (typep (gtk:scale-new-with-range :vertical 5.0 10.0 1.0) 'gtk:scale)))

;;;     gtk_scale_set_format_value_func
;;;     gtk_scale_get_layout
;;;     gtk_scale_get_layout_offsets
;;;     gtk_scale_add_mark
;;;     gtk_scale_clear_marks

;;; 2024-4-11

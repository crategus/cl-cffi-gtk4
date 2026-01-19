(in-package :gtk-test)

(def-suite gtk-range :in gtk-abstract-widgets)
(in-suite gtk-range)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkRange

(test gtk-range-class
  ;; Check type
  (is (g:type-is-object "GtkRange"))
  ;; Check registered name
  (is (eq 'gtk:range
          (glib:symbol-for-gtype "GtkRange")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkRange")
          (g:gtype (cffi:foreign-funcall "gtk_range_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkRange")))
  ;; Check children
  (is (equal '("GtkScale")
             (glib-test:list-children "GtkRange")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkAccessibleRange" "GtkOrientable")
             (glib-test:list-interfaces "GtkRange")))
  ;; Check properties
  (is (equal '("adjustment" "fill-level" "inverted" "orientation"
               "restrict-to-fill-level" "round-digits" "show-fill-level")
             (glib-test:list-properties "GtkRange")))
  ;; Check signals
  (is (equal '("adjust-bounds" "change-value" "move-slider" "value-changed")
             (glib-test:list-signals "GtkRange")))
  ;; Check CSS name
  (is (string= "range"
               (gtk:widget-class-css-name "GtkRange")))
  ;; Check accessible role
  (is (eq :generic (gtk:widget-class-accessible-role "GtkRange")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkRange" GTK:RANGE
                      (:SUPERCLASS GTK:WIDGET
                       :EXPORT T
                       :INTERFACES
                       ("GtkAccessible" "GtkAccessibleRange" "GtkBuildable"
                        "GtkConstraintTarget" "GtkOrientable")
                       :TYPE-INITIALIZER "gtk_range_get_type")
                      ((ADJUSTMENT RANGE-ADJUSTMENT
                        "adjustment" "GtkAdjustment" T T)
                       (FILL-LEVEL RANGE-FILL-LEVEL "fill-level" "gdouble" T T)
                       (INVERTED RANGE-INVERTED "inverted" "gboolean" T T)
                       (RESTRICT-TO-FILL-LEVEL RANGE-RESTRICT-TO-FILL-LEVEL
                        "restrict-to-fill-level" "gboolean" T T)
                       (ROUND-DIGITS RANGE-ROUND-DIGITS
                        "round-digits" "gint" T T)
                       (SHOW-FILL-LEVEL RANGE-SHOW-FILL-LEVEL
                        "show-fill-level" "gboolean" T T)))
             (gobject:get-gtype-definition "GtkRange"))))

;;; --- Properties -------------------------------------------------------------

;;;     adjustment
;;;     fill-level
;;;     inverted
;;;     restrict-to-fill-level
;;;     round-digits
;;;     show-fill-level

(test gtk-range-properties
  (glib-test:with-check-memory (range)
    (is (typep (setf range (make-instance 'gtk:range)) 'gtk:range))
    (is (typep (gtk:range-adjustment range) 'gtk:adjustment))
    (is (= 1.7976931348623157d308 (gtk:range-fill-level range)))
    (is-false (gtk:range-inverted range))
    (is-true (gtk:range-restrict-to-fill-level range))
    (is (= -1 (gtk:range-round-digits range)))
    (is-false (gtk:range-show-fill-level range))
    (is-false (setf (gtk:range-adjustment range) nil))
    (is (= 1 (g:object-ref-count range)))))

;;; --- Signals ----------------------------------------------------------------

;;;     adjust-bounds

(test gtk-range-adjust-bounds-signal
  (let* ((name "adjust-bounds")
         (gtype (g:gtype "GtkRange"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("gdouble")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     change-value

(test gtk-range-change-value-signal
  (let* ((name "change-value")
         (gtype (g:gtype "GtkRange"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "gboolean") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("GtkScrollType" "gdouble")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     move-slider

(test gtk-range-move-slider-signal
  (let* ((name "move-slider")
         (gtype (g:gtype "GtkRange"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:ACTION :RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("GtkScrollType")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     value-changed

(test gtk-range-value-changed-signal
  (let* ((name "value-changed")
         (gtype (g:gtype "GtkRange"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

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
    (is (= 10.0 (gtk:range-value range)))

    (is-false (setf (gtk:range-adjustment range) nil))
    (is (= 1 (g:object-ref-count range)))))

;;;     gtk_range_set_increments

(test gtk-range-set-increments
  (when *first-run-testsuite*
    (glib-test:with-check-memory (range :strong 1)
      (is (typep (setf range (make-instance 'gtk:range)) 'gtk:range))
      (is-false (gtk:range-set-increments range 1 2))
      (is (= 1.0d0 (gtk:adjustment-step-increment (gtk:range-adjustment range))))
      (is (= 2.0d0 (gtk:adjustment-page-increment (gtk:range-adjustment range)))))))

;;;     gtk_range_set_range

(test gtk-range-set-range.1
  (when *first-run-testsuite*
    (glib-test:with-check-memory (range :strong 1)
      (is (typep (setf range (make-instance 'gtk:range)) 'gtk:range))
      (is-false (gtk:range-set-range range 1 2))
      (is (= 1.0d0 (gtk:adjustment-lower (gtk:range-adjustment range))))
      (is (= 2.0d0 (gtk:adjustment-upper (gtk:range-adjustment range)))))))

(test gtk-range-set-range.2
  (when *first-run-testsuite*
    (glib-test:with-check-memory (range :strong 1)
      (is (typep (setf range
                       (make-instance 'gtk:range
                                      :adjustment
                                      (make-instance 'gtk:adjustment
                                                     :lower 10
                                                     :upper 100
                                                     :page-size 10)))
                 'gtk:range))
      (is-false (gtk:range-set-range range 20 80))
      (is (= 10.0d0 (gtk:adjustment-page-size (gtk:range-adjustment range))))
      (is (= 20.0d0 (gtk:adjustment-lower (gtk:range-adjustment range))))
      (is (= 80.0d0 (gtk:adjustment-upper (gtk:range-adjustment range)))))))

;;;     gtk_range_get_flippable
;;;     gtk_range_set_flippable

(test gtk-range-flippable
  (glib-test:with-check-memory (range)
    (is (typep (setf range (make-instance 'gtk:range)) 'gtk:range))
    (is-false (gtk:range-flippable range))
    (is-true (setf (gtk:range-flippable range) t))
    (is-true (gtk:range-flippable range))))

;;;     gtk_range_get_range_rect

(test gtk-range-range-rect
  (glib-test:with-check-memory (range)
    (is (typep (setf range (make-instance 'gtk:range)) 'gtk:range))
    (is (typep (gtk:range-range-rect range) 'gdk:rectangle))))

;;;     gtk_range_get_slider_range

(test gtk-range-slider-range
  (glib-test:with-check-memory (range)
    (is (typep (setf range (make-instance 'gtk:range)) 'gtk:range))
    (is (equal '(0 0) (multiple-value-list (gtk:range-slider-range range))))))

;;;     gtk_range_get_slider_size_fixed
;;;     gtk_range_set_slider_size_fixed

(test gtk-range-slider-size-fixed
  (glib-test:with-check-memory (range)
    (is (typep (setf range (make-instance 'gtk:range)) 'gtk:range))
    (is-false (gtk:range-slider-size-fixed range))
    (is-true (setf (gtk:range-slider-size-fixed range) t))
    (is-true (gtk:range-slider-size-fixed range))))

;;; 2026-01-10

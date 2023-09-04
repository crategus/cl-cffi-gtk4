(in-package :gtk-test)

(def-suite gtk-adjustment :in gtk-suite)
(in-suite gtk-adjustment)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkAdjustment

(test gtk-adjustment-class
  ;; Type check
  (is (g:type-is-object "GtkAdjustment"))
  ;; Check the registered name
  (is (eq 'gtk:adjustment
          (glib:symbol-for-gtype "GtkAdjustment")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkAdjustment")
          (g:gtype (cffi:foreign-funcall "gtk_adjustment_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GInitiallyUnowned")
          (g:type-parent "GtkAdjustment")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkAdjustment")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkAdjustment")))
  ;; Check the properties
  (is (equal '("lower" "page-increment" "page-size" "step-increment" "upper"
               "value")
             (list-properties "GtkAdjustment")))
  ;; Check the list of signals
  (is (equal '("changed" "value-changed")
             (list-signals "GtkAdjustment")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkAdjustment" GTK-ADJUSTMENT
                       (:SUPERCLASS G-INITIALLY-UNOWNED :EXPORT T :INTERFACES
                        NIL :TYPE-INITIALIZER "gtk_adjustment_get_type")
                       ((LOWER GTK-ADJUSTMENT-LOWER "lower" "gdouble" T T)
                        (PAGE-INCREMENT GTK-ADJUSTMENT-PAGE-INCREMENT
                         "page-increment" "gdouble" T T)
                        (PAGE-SIZE GTK-ADJUSTMENT-PAGE-SIZE "page-size"
                         "gdouble" T T)
                        (STEP-INCREMENT GTK-ADJUSTMENT-STEP-INCREMENT
                         "step-increment" "gdouble" T T)
                        (UPPER GTK-ADJUSTMENT-UPPER "upper" "gdouble" T T)
                        (VALUE GTK-ADJUSTMENT-VALUE "value" "gdouble" T T)))
             (gobject:get-g-type-definition "GtkAdjustment"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-adjustment-properties
  (let ((adjustment (make-instance 'gtk:adjustment)))
    ;; adjustment-lower
    (is (= 0.0d0 (gtk:adjustment-lower adjustment)))
    (is (= 1.0d0 (setf (gtk:adjustment-lower adjustment) 1.0d0)))
    ;; adjustment-page-increment
    (is (= 0.0d0 (gtk:adjustment-page-increment adjustment)))
    (is (= 2.0d0 (setf (gtk:adjustment-page-increment adjustment) 2)))
    ;; adjustment-page-size
    (is (= 0.0d0 (gtk:adjustment-page-size adjustment)))
    (is (= 0.5d0 (setf (gtk:adjustment-page-size adjustment) 1/2)))
    ;; adjustment-step-increment
    (is (= 0.0d0 (gtk:adjustment-step-increment adjustment)))
    (is (= 3.0d0 (setf (gtk:adjustment-step-increment adjustment) 3.0)))
    ;; adjustment-upper
    (is (= 0.0d0 (gtk:adjustment-upper adjustment)))
    (is (= 1.0d0 (setf (gtk:adjustment-upper adjustment) 1.0d0)))
    ;; adjustment-value
    (is (= 0.0d0 (gtk:adjustment-value adjustment)))
    (is (= 1.0d0 (setf (gtk:adjustment-value adjustment) 1.0d0)))))

;;; --- Signals ----------------------------------------------------------------

(test gtk-adjustment-changed-signal
  (let ((query (g:signal-query (g:signal-lookup "changed" "GtkAdjustment"))))
    (is (string= "changed" (g:signal-query-signal-name query)))
    (is (string= "GtkAdjustment"
                 (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:NO-RECURSE :RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

(test gtk-adjustment-value-changed-signal
  (let ((query (g:signal-query (g:signal-lookup "value-changed"
                                                "GtkAdjustment"))))
    (is (string= "value-changed" (g:signal-query-signal-name query)))
    (is (string= "GtkAdjustment"
                 (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:NO-RECURSE :RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_adjustment_new

(test gtk-adjustment-new
  (let ((adjustment (gtk:adjustment-new 10.0 1 20 1/2 3/4 2.5d0)))
    (is (= 10.00d0 (gtk:adjustment-value adjustment)))
    (is (=  1.00d0 (gtk:adjustment-lower adjustment)))
    (is (= 20.00d0 (gtk:adjustment-upper adjustment)))
    (is (=  0.5d00 (gtk:adjustment-step-increment adjustment)))
    (is (=  0.75d0 (gtk:adjustment-page-increment adjustment)))
    (is (=  2.50d0 (gtk:adjustment-page-size adjustment)))))

;;;     gtk_adjustment_clamp_page

(test gtk-adjustment-clamp-page
  (let ((adjustment (gtk:adjustment-new 10 0 20 0 0 0)))
    (is (= 10 (gtk:adjustment-value adjustment)))
    (is (=  0 (gtk:adjustment-lower adjustment)))
    (is (= 20 (gtk:adjustment-upper adjustment)))
    (is-false (gtk:adjustment-clamp-page adjustment 0 20))
    (is (=  0 (gtk:adjustment-value adjustment)))
    (is (=  0 (gtk:adjustment-lower adjustment)))
    (is (= 20 (gtk:adjustment-upper adjustment)))))

;;;     gtk_adjustment_configure
;;;     gtk_adjustment_get_minimum_increment

;;; --- 2023-9-2 ---------------------------------------------------------------

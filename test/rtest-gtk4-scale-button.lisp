(in-package :gtk-test)

(def-suite gtk-scale-button :in gtk-suite)
(in-suite gtk-scale-button)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkScaleButton

(test scale-button-class
  ;; Check type
  (is (g:type-is-object "GtkScaleButton"))
  ;; Check registered name
  (is (eq 'gtk:scale-button
          (glib:symbol-for-gtype "GtkScaleButton")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkScaleButton")
          (g:gtype (cffi:foreign-funcall "gtk_scale_button_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkScaleButton")))
  ;; Check children
  (is (equal '("GtkVolumeButton")
             (list-children "GtkScaleButton")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkAccessibleRange" "GtkOrientable")
             (list-interfaces "GtkScaleButton")))
  ;; Check properties
  (is (equal '("active" "adjustment" "icons" "orientation" "value")
             (list-properties "GtkScaleButton")))
  ;; Check signals
  (is (equal '("popdown" "popup" "value-changed")
             (list-signals "GtkScaleButton")))
  ;; Check CSS name
  (is (string= "scalebutton"
               (gtk:widget-class-css-name "GtkScaleButton")))
  ;; Check CSS classes
  (is (equal '("scale")
             (gtk:widget-css-classes (make-instance 'gtk:scale-button))))
  ;; Check accessible role
  (is (eq :group (gtk:widget-class-accessible-role "GtkScaleButton")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkScaleButton" GTK-SCALE-BUTTON
                       (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                        ("GtkAccessible" "GtkAccessibleRange" "GtkBuildable"
                         "GtkConstraintTarget" "GtkOrientable")
                        :TYPE-INITIALIZER "gtk_scale_button_get_type")
                       ((ACTIVE GTK-SCALE-BUTTON-ACTIVE "active" "gboolean" T
                         NIL)
                        (ADJUSTMENT GTK-SCALE-BUTTON-ADJUSTMENT "adjustment"
                         "GtkAdjustment" T T)
                        (ICONS GTK-SCALE-BUTTON-ICONS "icons" "GStrv" T T)
                        (VALUE GTK-SCALE-BUTTON-VALUE "value" "gdouble" T T)))
             (gobject:get-g-type-definition "GtkScaleButton"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-scale-button-properties
  (let ((button (make-instance 'gtk:scale-button)))
    (is-false (gtk:scale-button-active button))
    (is (typep (gtk:scale-button-adjustment button) 'gtk:adjustment))
    (is-false (gtk:scale-button-icons button))
    (is (= 0.0d0 (gtk:scale-button-value button)))))

;;; --- Signals ----------------------------------------------------------------

;;;     popdown

(test gtk-scale-button-popdown-signal
  (let ((query (g:signal-query (g:signal-lookup "popdown" "GtkScaleButton"))))
    (is (string= "popdown" (g:signal-query-signal-name query)))
    (is (string= "GtkScaleButton" (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:ACTION :RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

;;;     popup

(test gtk-scale-button-popup-signal
  (let ((query (g:signal-query (g:signal-lookup "popup" "GtkScaleButton"))))
    (is (string= "popup" (g:signal-query-signal-name query)))
    (is (string= "GtkScaleButton" (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:ACTION :RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

;;;     value-changed

(test gtk-scale-button-value-changed-signal
  (let ((query (g:signal-query (g:signal-lookup "value-changed" "GtkScaleButton"))))
    (is (string= "value-changed" (g:signal-query-signal-name query)))
    (is (string= "GtkScaleButton" (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '("gdouble")
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_scale_button_new

(test gtk-scale-button-new
  (let* ((button (gtk:scale-button-new 0 10 2 nil))
         (adjustment (gtk:scale-button-adjustment button)))
    (is (typep button 'gtk:scale-button))
    (is (= 0.0d0 (gtk:scale-button-value button)))
    (is (= 0.0d0 (gtk:adjustment-value adjustment)))
    (is (= 0.0d0 (gtk:adjustment-lower adjustment)))
    (is (= 10.0d0 (gtk:adjustment-upper adjustment)))
    (is (= 2.0d0 (gtk:adjustment-step-increment adjustment)))
    (is (= 20.0d0 (gtk:adjustment-page-increment adjustment)))
    (is (= 0.0d0 (gtk:adjustment-page-size adjustment)))))

;;;     gtk_scale_button_get_popup
;;;     gtk_scale_button_get_plus_button
;;;     gtk_scale_button_get_minus_button

(test gtk-scale-button-get
  (let ((button (gtk:scale-button-new 1 10 0.5 nil)))
    (is (typep (gtk:scale-button-popup button) 'gtk:popover))
    (is (typep (gtk:scale-button-plus-button button) 'gtk:button))
    (is (typep (gtk:scale-button-minus-button button) 'gtk:button))))

;;; 2024-5-4

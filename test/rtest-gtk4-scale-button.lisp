(in-package :gtk-test)

(def-suite gtk-scale-button :in gtk-buttons-and-toggles)
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
             (glib-test:list-children "GtkScaleButton")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkAccessibleRange" "GtkOrientable")
             (glib-test:list-interfaces "GtkScaleButton")))
  ;; Check properties
  (is (equal '("active" "adjustment" "has-frame" "icons" "orientation" "value")
             (glib-test:list-properties "GtkScaleButton")))
  ;; Check signals
  (is (equal '("popdown" "popup" "value-changed")
             (glib-test:list-signals "GtkScaleButton")))
  ;; Check CSS name
  (is (string= "scalebutton"
               (gtk:widget-class-css-name "GtkScaleButton")))
  ;; Check accessible role
  (is (eq :group (gtk:widget-class-accessible-role "GtkScaleButton")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkScaleButton" GTK:SCALE-BUTTON
                      (:SUPERCLASS GTK:WIDGET
                       :EXPORT T
                       :INTERFACES
                       ("GtkAccessible" "GtkAccessibleRange" "GtkBuildable"
                        "GtkConstraintTarget" "GtkOrientable")
                       :TYPE-INITIALIZER "gtk_scale_button_get_type")
                      ((ACTIVE SCALE-BUTTON-ACTIVE "active" "gboolean" T NIL)
                       (ADJUSTMENT SCALE-BUTTON-ADJUSTMENT
                        "adjustment" "GtkAdjustment" T T)
                       (HAS-FRAME SCALE-BUTTON-HAS-FRAME
                        "has-frame" "gboolean" T T)
                       (ICONS SCALE-BUTTON-ICONS "icons" "GStrv" T T)
                       (VALUE SCALE-BUTTON-VALUE "value" "gdouble" T T)))
             (gobject:get-gtype-definition "GtkScaleButton"))))

;;; --- Signals ----------------------------------------------------------------

;;;     popdown

(test gtk-scale-button-popdown-signal
  (let* ((name "popdown")
         (gtype (g:gtype "GtkScaleButton"))
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
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     popup

(test gtk-scale-button-popup-signal
  (let* ((name "popup")
         (gtype (g:gtype "GtkScaleButton"))
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
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     value-changed

(test gtk-scale-button-value-changed-signal
  (let* ((name "value-changed")
         (gtype (g:gtype "GtkScaleButton"))
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

;;; --- Properties -------------------------------------------------------------

(test gtk-scale-button-properties
  (glib-test:with-check-memory (button)
    (is (typep (setf button (make-instance 'gtk:scale-button)) 'gtk:scale-button))
    (is-false (gtk:scale-button-active button))
    (is (typep (gtk:scale-button-adjustment button) 'gtk:adjustment))
    (is-false (gtk:scale-button-has-frame button))
    (is-false (gtk:scale-button-icons button))
    (is (= 0.0d0 (gtk:scale-button-value button)))
    ;; Remove reference
    (is-false (setf (gtk:scale-button-adjustment button) nil))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_scale_button_new

(test gtk-scale-button-new
  (glib-test:with-check-memory (button adjustment)
    (is (typep (setf button (gtk:scale-button-new 0 10 2 nil)) 'gtk:scale-button))
    (is (typep (setf adjustment (gtk:scale-button-adjustment button)) 'gtk:adjustment))
    (is (= 0.0d0 (gtk:scale-button-value button)))
    (is (= 0.0d0 (gtk:adjustment-value adjustment)))
    (is (= 0.0d0 (gtk:adjustment-lower adjustment)))
    (is (= 10.0d0 (gtk:adjustment-upper adjustment)))
    (is (= 2.0d0 (gtk:adjustment-step-increment adjustment)))
    (is (= 20.0d0 (gtk:adjustment-page-increment adjustment)))
    (is (= 0.0d0 (gtk:adjustment-page-size adjustment)))
    ;; Remove reference
    (is-false (setf (gtk:scale-button-adjustment button) nil))))

;;;     gtk_scale_button_get_popup
;;;     gtk_scale_button_get_plus_button
;;;     gtk_scale_button_get_minus_button

(test gtk-scale-button-get
  (glib-test:with-check-memory (button (button1 3) (button2 3) (popup 3) :strong 3)
    (is (typep (setf button (gtk:scale-button-new 1 10 0.5 nil)) 'gtk:scale-button))
    (is (typep (setf popup (gtk:scale-button-popup button)) 'gtk:popover))
    (is (typep (setf button1 (gtk:scale-button-plus-button button)) 'gtk:button))
    (is (typep (setf button2 (gtk:scale-button-minus-button button)) 'gtk:button))
    ;; Remove reference
    (is-false (setf (gtk:scale-button-adjustment button) nil))))

;;; 2026-07-28

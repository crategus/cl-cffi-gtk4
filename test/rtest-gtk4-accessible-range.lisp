(in-package :gtk-test)

(def-suite gtk-accessible-range :in gtk-suite)
(in-suite gtk-accessible-range)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkAccessibleRange

(test gtk-accessible-range-interface
  ;; Check type
  (is (g:type-is-interface "GtkAccessibleRange"))
  ;; Check registered name
  (is (eq 'gtk:accessible-range
          (glib:symbol-for-gtype "GtkAccessibleRange")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkAccessibleRange")
          (g:gtype (cffi:foreign-funcall "gtk_accessible_range_get_type"
                                         :size))))
  ;; Check interface prerequisites
  (is (equal '("GtkAccessible" "GObject")
             (glib-test:list-interface-prerequisites "GtkAccessibleRange")))
  ;; Check interface properties
  (is (equal '()
             (glib-test:list-interface-properties "GtkAccessibleRange")))
  ;; Check interface signals
  (is (equal '()
             (glib-test:list-signals "GtkAccessibleRange")))
  ;; Check interface definition
  (is (equal '(GOBJECT:DEFINE-GINTERFACE "GtkAccessibleRange" GTK:ACCESSIBLE-RANGE
                       (:EXPORT T
                        :TYPE-INITIALIZER "gtk_accessible_range_get_type"))
             (gobject:get-gtype-definition "GtkAccessibleRange"))))

;;;     gtk_accessible_range_set_current_value

;; TODO: Windows does not support this function

#-windows
(test gtk-accessible-range-set-current-value
  (let ((range (make-instance 'gtk:range)))
    (is (= 0 (gtk:range-value range)))
    (is-false (gtk:accessible-range-set-current-value range 100))
    (is (= 0 (gtk:range-value range)))
    (is (= 1 (g:object-ref-count range)))))

;;; 2024-11-5

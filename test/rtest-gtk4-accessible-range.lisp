(in-package :gtk-test)

(def-suite gtk-accessible-range :in gtk-suite)
(in-suite gtk-accessible-range)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkAccessibleRange

(test gtk-accessible-range-interface
  ;; Type check
  (is (g:type-is-interface "GtkAccessibleRange"))
  ;; Check the registered name
  (is (eq 'gtk:accessible-range
          (glib:symbol-for-gtype "GtkAccessibleRange")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkAccessibleRange")
          (g:gtype (cffi:foreign-funcall "gtk_accessible_range_get_type"
                                         :size))))
  ;; Check the interface prerequisites
  (is (equal '("GtkAccessible" "GObject")
             (list-interface-prerequisites "GtkAccessibleRange")))
  ;; Check the interface properties
  (is (equal '()
             (list-interface-properties "GtkAccessibleRange")))
  ;; Check the interface signals
  (is (equal '()
             (list-signals "GtkAccessibleRange")))
  ;; Get the interface definition
  (is (equal '(GOBJECT:DEFINE-G-INTERFACE "GtkAccessibleRange"
                                          GTK-ACCESSIBLE-RANGE
                            (:EXPORT T :TYPE-INITIALIZER
                             "gtk_accessible_range_get_type"))
             (gobject:get-g-type-definition "GtkAccessibleRange"))))

;;; --- 2023-8-24 --------------------------------------------------------------

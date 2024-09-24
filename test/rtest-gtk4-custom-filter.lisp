(in-package :gtk-test)

(def-suite gtk-custom-filter :in gtk-suite)
(in-suite gtk-custom-filter)

;;; --- Types and Values  ------------------------------------------------------

;;;     GtkCustomFilter

(test gtk-custom-filter-class
  ;; Check type
  (is (g:type-is-object "GtkCustomFilter"))
  ;; Check registered name
  (is (eq 'gtk:custom-filter
          (glib:symbol-for-gtype "GtkCustomFilter")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkCustomFilter")
          (g:gtype (cffi:foreign-funcall "gtk_custom_filter_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkFilter")
          (g:type-parent "GtkCustomFilter")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkCustomFilter")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkCustomFilter")))
  ;; Check properties
  (is (equal '()
             (glib-test:list-properties "GtkCustomFilter")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkCustomFilter")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkCustomFilter" GTK:CUSTOM-FILTER
                       (:SUPERCLASS GTK:FILTER
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_custom_filter_get_type")
                       NIL)
             (gobject:get-gtype-definition "GtkCustomFilter"))))

;;; --- Functions  -------------------------------------------------------------

;;;     gtk_custom_filter_new
;;;     gtk_custom_filter_set_filter_func

;;; 2024-9-19

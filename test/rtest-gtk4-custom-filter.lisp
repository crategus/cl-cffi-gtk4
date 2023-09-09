(in-package :gtk-test)

(def-suite gtk-custom-filter :in gtk-suite)
(in-suite gtk-custom-filter)

;;; --- Types and Values  ------------------------------------------------------

;;;     GtkCustomFilter

(test gtk-custom-filter-class
  ;; Type check
  (is (g:type-is-object "GtkCustomFilter"))
  ;; Check the registered name
  (is (eq 'gtk:custom-filter
          (glib:symbol-for-gtype "GtkCustomFilter")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkCustomFilter")
          (g:gtype (cffi:foreign-funcall "gtk_custom_filter_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkFilter")
          (g:type-parent "GtkCustomFilter")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkCustomFilter")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkCustomFilter")))
  ;; Check the properties
  (is (equal '()
             (list-properties "GtkCustomFilter")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkCustomFilter")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkCustomFilter" GTK-CUSTOM-FILTER
                               (:SUPERCLASS GTK-FILTER :EXPORT T :INTERFACES
                                NIL :TYPE-INITIALIZER
                                "gtk_custom_filter_get_type")
                               NIL)
             (gobject:get-g-type-definition "GtkCustomFilter"))))

;;; --- Functions  -------------------------------------------------------------

;;;     gtk_custom_filter_new
;;;     gtk_custom_filter_set_filter_func

;;; --- 2023-9-6 ---------------------------------------------------------------

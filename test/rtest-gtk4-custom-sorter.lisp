(in-package :gtk-test)

(def-suite gtk-custom-sorter :in gtk-suite)
(in-suite gtk-custom-sorter)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkCustomSorter

(test gtk-custom-sorter-class
  ;; Type check
  (is (g:type-is-object "GtkCustomSorter"))
  ;; Check the registered name
  (is (eq 'gtk:custom-sorter
          (glib:symbol-for-gtype "GtkCustomSorter")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkCustomSorter")
          (g:gtype (cffi:foreign-funcall "gtk_custom_sorter_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkSorter")
          (g:type-parent "GtkCustomSorter")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkCustomSorter")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkCustomSorter")))
  ;; Check the properties
  (is (equal '()
             (list-properties "GtkCustomSorter")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkCustomSorter")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkCustomSorter" GTK-CUSTOM-SORTER
                               (:SUPERCLASS GTK-SORTER :EXPORT T :INTERFACES
                                NIL :TYPE-INITIALIZER
                                "gtk_custom_sorter_get_type")
                               NIL)
             (gobject:get-g-type-definition "GtkCustomSorter"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_custom_sorter_new
;;;     gtk_custom_sorter_set_sort_func

;;; --- 2023-9-5 ---------------------------------------------------------------

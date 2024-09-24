(in-package :gtk-test)

(def-suite gtk-custom-sorter :in gtk-suite)
(in-suite gtk-custom-sorter)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkCustomSorter

(test gtk-custom-sorter-class
  ;; Check type
  (is (g:type-is-object "GtkCustomSorter"))
  ;; Check registered name
  (is (eq 'gtk:custom-sorter
          (glib:symbol-for-gtype "GtkCustomSorter")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkCustomSorter")
          (g:gtype (cffi:foreign-funcall "gtk_custom_sorter_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkSorter")
          (g:type-parent "GtkCustomSorter")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkCustomSorter")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkCustomSorter")))
  ;; Check properties
  (is (equal '()
             (glib-test:list-properties "GtkCustomSorter")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkCustomSorter")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkCustomSorter" GTK:CUSTOM-SORTER
                       (:SUPERCLASS GTK:SORTER
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_custom_sorter_get_type")
                       NIL)
             (gobject:get-gtype-definition "GtkCustomSorter"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_custom_sorter_new

(test gtk-custom-sorter-new
  (is (typep (gtk:custom-sorter-new) 'gtk:custom-sorter))
  (is (typep (gtk:custom-sorter-new nil) 'gtk:custom-sorter))
  (is (typep (gtk:custom-sorter-new (lambda (item1 item2)
                                      (cond ((< item1 item2) -1)
                                            ((> item1 item2) +1)
                                            (t 0))))
             'gtk:custom-sorter)))

;;;     gtk_custom_sorter_set_sort_func

(test gtk-custom-sorter-set-sort-func
  (let ((sorter (gtk:custom-sorter-new)))
    (is-false (gtk:custom-sorter-set-sort-func sorter nil))
    (is-false (gtk:custom-sorter-set-sort-func sorter
                                               (lambda (item1 item2)
                                                 (cond ((< item1 item2) -1)
                                                       ((> item1 item2) +1)
                                                       (t 0)))))))

;;; 2024-9-19

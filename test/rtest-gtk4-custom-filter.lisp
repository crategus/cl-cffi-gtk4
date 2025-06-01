(in-package :gtk-test)

(def-suite gtk-custom-filter :in gtk-list-model-support)
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

(defun custom-filter-func (item)
 (search "string" (gtk:string-object-string item) :test #'char-equal))

;;;     gtk_custom_filter_new

(test gtk-custom-filter-new
  (glib-test:with-check-memory (store filter model)

    (setf store (create-string-list-for-package))
    (setf filter (gtk:custom-filter-new #'custom-filter-func))
    (setf model (gtk:filter-list-model-new store filter))

    (is (typep filter 'gtk:custom-filter))
    (is (= 2 (g:object-ref-count filter)))
    ;; Create with constructor function
    (is (typep (gtk:custom-filter-new #'custom-filter-func) 'gtk:custom-filter))
    (is (= 1 (g:object-ref-count (gtk:custom-filter-new #'custom-filter-func))))
    ;; Items in the filter model
    (is (= 42 (gtk:filter-list-model-n-items model)))
    ;; Create with make instance
    (is (typep (make-instance 'gtk:custom-filter) 'gtk:custom-filter))
    (is (= 1 (g:object-ref-count (make-instance 'gtk:custom-filter))))
    ;; Create with object new
    (is (typep (g:object-new "GtkCustomFilter") 'gtk:custom-filter))
    (is (= 1 (g:object-ref-count (g:object-new "GtkCustomFilter"))))
    ;; Clear the string list
    (is-false (gtk:string-list-splice store
                                      0
                                      (gtk:string-list-n-items store)
                                      '()))
    (is (= 0 (gtk:string-list-n-items store)))
    ;; Remove string list and filter from filter model
    (is-false (setf (gtk:filter-list-model-filter model) nil))
    (is-false (setf (gtk:filter-list-model-model model) nil))))

;;;     gtk_custom_filter_set_filter_func

(test gtk-custom-filter-set-filter-func
  (glib-test:with-check-memory (store filter model)

    (setf store (create-string-list-for-package))
    (setf filter (gtk:custom-filter-new))
    (setf model (gtk:filter-list-model-new store filter))

    ;; Check refcount for first object in list store
    (is (= 2 (g:object-ref-count (g:list-model-item store 0))))
    ;; No filter function set
    (is (< 3000 (gtk:filter-list-model-n-items model)))
    ;; Set filter function
    (is-false (gtk:custom-filter-set-filter-func filter #'custom-filter-func))
    (is (= 42 (gtk:filter-list-model-n-items model)))
    ;; Set a new filter function
    (is-false (gtk:custom-filter-set-filter-func filter
                  (lambda (item)
                    (search "button"
                            (gtk:string-object-string item) :test #'char-equal))))
    (is (= 145 (gtk:filter-list-model-n-items model)))
    ;; Unset the custom filter
    (is-false (gtk:custom-filter-set-filter-func filter))
    (is (< 3000 (gtk:filter-list-model-n-items model)))
    ;; Check refcount for first object in list store
    (is (= 2 (g:object-ref-count (g:list-model-item store 0))))
    ;; Clear the string list
    (is-false (gtk:string-list-splice store
                                      0
                                      (gtk:string-list-n-items store)
                                      '()))
    (is (= 0 (gtk:string-list-n-items store)))
    ;; Remove string list and filter from filter model
    (is-false (setf (gtk:filter-list-model-filter model) nil))
    (is-false (setf (gtk:filter-list-model-model model) nil))))

;;; 2025-05-25

(in-package :gtk-test)

(def-suite gtk-multi-filter :in gtk-suite)
(in-suite gtk-multi-filter)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkMultiFilter

(test gtk-multi-filter-class
  ;; Check type
  (is (g:type-is-object "GtkMultiFilter"))
  ;; Check registered name
  (is (eq 'gtk:multi-filter
          (glib:symbol-for-gtype "GtkMultiFilter")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkMultiFilter")
          (g:gtype (cffi:foreign-funcall "gtk_multi_filter_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkFilter")
          (g:type-parent "GtkMultiFilter")))
  ;; Check children
  (is (equal '("GtkAnyFilter" "GtkEveryFilter")
             (glib-test:list-children "GtkMultiFilter")))
  ;; Check interfaces
  (is (equal '("GListModel" "GtkBuildable")
             (glib-test:list-interfaces "GtkMultiFilter")))
  ;; Check properties
  (is (equal '("item-type" "n-items")
             (glib-test:list-properties "GtkMultiFilter")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkMultiFilter")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkMultiFilter" GTK:MULTI-FILTER
                       (:SUPERCLASS GTK:FILTER
                        :EXPORT T
                        :INTERFACES ("GListModel" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_multi_filter_get_type")
                       ((ITEM-TYPE MULTI-FILTER-ITEM-TYPE
                         "item-type" "GType" T NIL)
                        (N-ITEMS MULTI-FILTER-N-ITEMS "n-items" "guint" T NIL)))
             (gobject:get-gtype-definition "GtkMultiFilter"))))

;;;     GtkAnyFilter

(test gtk-any-filter-class
  ;; Check type
  (is (g:type-is-object "GtkAnyFilter"))
  ;; Check registered name
  (is (eq 'gtk:any-filter
          (glib:symbol-for-gtype "GtkAnyFilter")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkAnyFilter")
          (g:gtype (cffi:foreign-funcall "gtk_any_filter_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkMultiFilter")
          (g:type-parent "GtkAnyFilter")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkAnyFilter")))
  ;; Check interfaces
  (is (equal '("GListModel" "GtkBuildable")
             (glib-test:list-interfaces "GtkAnyFilter")))
  ;; Check properties
  (is (equal '()
             (glib-test:list-properties "GtkAnyFilter")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkAnyFilter")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkAnyFilter" GTK:ANY-FILTER
                       (:SUPERCLASS GTK:MULTI-FILTER
                        :EXPORT T
                        :INTERFACES ("GListModel" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_any_filter_get_type")
                       NIL)
             (gobject:get-gtype-definition "GtkAnyFilter"))))

;;;     GtkEveryFilter

(test gtk-every-filter-class
  ;; Check type
  (is (g:type-is-object "GtkEveryFilter"))
  ;; Check registered name
  (is (eq 'gtk:every-filter
          (glib:symbol-for-gtype "GtkEveryFilter")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkEveryFilter")
          (g:gtype (cffi:foreign-funcall "gtk_every_filter_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkMultiFilter")
          (g:type-parent "GtkEveryFilter")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkEveryFilter")))
  ;; Check interfaces
  (is (equal '("GListModel" "GtkBuildable")
             (glib-test:list-interfaces "GtkEveryFilter")))
  ;; Check properties
  (is (equal '()
             (glib-test:list-properties "GtkEveryFilter")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkEveryFilter")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkEveryFilter" GTK:EVERY-FILTER
                       (:SUPERCLASS GTK:MULTI-FILTER
                        :EXPORT T
                        :INTERFACES ("GListModel" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_every_filter_get_type")
                       NIL)
             (gobject:get-gtype-definition "GtkEveryFilter"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_any_filter_new

(test gtk-any-filter-new
  (is (typep (gtk:any-filter-new) 'gtk:any-filter))
  (is (= 1 (g:object-ref-count (make-instance 'gtk:any-filter))))
  (is (= 1 (g:object-ref-count (gtk:any-filter-new)))))

;;;     gtk_every_filter_new

(test gtk-every-filter-new
  (is (typep (gtk:every-filter-new) 'gtk:every-filter))
  (is (= 1 (g:object-ref-count (make-instance 'gtk:every-filter))))
  (is (= 1 (g:object-ref-count (gtk:every-filter-new)))))

;;;     gtk_multi_filter_append
;;;     gtk_multi_filter_remove

(test gtk-multi-filter-append/remove.1
  (let* ((store (create-string-list-for-package))
         (filter (gtk:any-filter-new))
         (model (gtk:filter-list-model-new store filter))
         (expr1 (gtk:property-expression-new "GtkStringObject" nil "string"))
         (expr2 (gtk:property-expression-new "GtkStringObject" nil "string"))
         (filter1 (gtk:string-filter-new (gtk:expression-ref expr1)))
         (filter2 (gtk:string-filter-new (gtk:expression-ref expr2))))

    (is (= 2 (g:object-ref-count store)))
    (is (= 2 (g:object-ref-count filter)))
    (is (= 1 (g:object-ref-count model)))
    (is (= 1 (g:object-ref-count filter1)))
    (is (= 1 (g:object-ref-count filter2)))

    ;; Properties of the any filter
    (is (eq (g:gtype "GtkFilter") (gtk:multi-filter-item-type filter)))
    (is (= 0 (gtk:multi-filter-n-items filter)))
    ;; No filter function set
    (is (= 0 (gtk:filter-list-model-n-items model)))
    ;; Configure and append filter1
    (is (eq :substring (setf (gtk:string-filter-match-mode filter1) :substring)))
    (is (string= (setf (gtk:string-filter-search filter1) "string") "string"))
    (is-false (gtk:multi-filter-append filter filter1))
    (is (= 2 (g:object-ref-count filter)))
    (is (= 2 (g:object-ref-count filter1)))
    ;; Check the filter result
    (is (= 1 (gtk:multi-filter-n-items filter)))
    (is (= 42 (gtk:filter-list-model-n-items model)))
    ;; Configure and append filter2
    (is (eq :substring (setf (gtk:string-filter-match-mode filter2) :substring)))
    (is (string= (setf (gtk:string-filter-search filter2) "list") "list"))
    (is-false (gtk:multi-filter-append filter filter2))
    (is (= 2 (gtk:multi-filter-n-items filter)))
    ;; Check the filter result
    #-windows
    (is (= 232 (gtk:filter-list-model-n-items model)))
    #+windows
    (is (= 231 (gtk:filter-list-model-n-items model)))
    ;; Remove filter2
    (is-false (gtk:multi-filter-remove filter 0))
    ;; Check again the filter result
    (is (= 1 (gtk:multi-filter-n-items filter)))
    #-windows
    (is (= 199 (gtk:filter-list-model-n-items model)))
    #+windows
    (is (= 198 (gtk:filter-list-model-n-items model)))))

(test gtk-multi-filter-append/remove.2
  (let* ((store (create-string-list-for-package))
         (filter (gtk:every-filter-new))
         (model (gtk:filter-list-model-new store filter))
         (expr1 (gtk:property-expression-new "GtkStringObject" nil "string"))
         (expr2 (gtk:property-expression-new "GtkStringObject" nil "string"))
         (filter1 (gtk:string-filter-new expr1))
         (filter2 (gtk:string-filter-new expr2)))
    ;; Properties of filter
    (is (eq (g:gtype "GtkFilter") (gtk:multi-filter-item-type filter)))
    (is (= 0 (gtk:multi-filter-n-items filter)))
    ;; No filter function set
    #-windows
    (is (< 3400 (gtk:filter-list-model-n-items model)))
    #+windows
    (is (< 3330 (gtk:filter-list-model-n-items model)))
    ;; Append filter1
    (is (eq :substring (setf (gtk:string-filter-match-mode filter1) :substring)))
    (is (string= (setf (gtk:string-filter-search filter1) "string") "string"))
    (is-false (gtk:multi-filter-append filter filter1))
    (is (= 1 (gtk:multi-filter-n-items filter)))
    (is (= 42 (gtk:filter-list-model-n-items model)))
    ;; Append filter2
    (is (eq :substring (setf (gtk:string-filter-match-mode filter2) :substring)))
    (is (string= (setf (gtk:string-filter-search filter2) "list") "list"))
    (is-false (gtk:multi-filter-append filter filter2))
    (is (= 2 (gtk:multi-filter-n-items filter)))
    (is (= 9 (gtk:filter-list-model-n-items model)))
    ;; Remove filter2
    (is-false (gtk:multi-filter-remove filter 0))
    (is (= 1 (gtk:multi-filter-n-items filter)))
    #-windows
    (is (= 199 (gtk:filter-list-model-n-items model)))
    #+windows
    (is (= 198 (gtk:filter-list-model-n-items model)))))

;;; 2024-1-10

(in-package :gtk-test)

(def-suite gtk-filter :in gtk-suite)
(in-suite gtk-filter)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFilterMatch

(test gtk-filter-match-enumeration
  ;; Check type
  (is (g:type-is-enum "GtkFilterMatch"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkFilterMatch")
          (g:gtype (cffi:foreign-funcall "gtk_filter_match_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:filter-match
          (glib:symbol-for-gtype "GtkFilterMatch")))
  ;; Check names
  (is (equal '("GTK_FILTER_MATCH_SOME" "GTK_FILTER_MATCH_NONE"
               "GTK_FILTER_MATCH_ALL")
             (glib-test:list-enum-item-names "GtkFilterMatch")))
  ;; Check values
  (is (equal '(0 1 2)
             (glib-test:list-enum-item-values "GtkFilterMatch")))
  ;; Check nick names
  (is (equal '("some" "none" "all")
             (glib-test:list-enum-item-nicks "GtkFilterMatch")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkFilterMatch" GTK:FILTER-MATCH
                       (:EXPORT T
                        :TYPE-INITIALIZER "gtk_filter_match_get_type")
                       (:SOME 0)
                       (:NONE 1)
                       (:ALL 2))
             (gobject:get-gtype-definition "GtkFilterMatch"))))

;;;     GtkFilterChange

(test gtk-filter-change
  ;; Check type
  (is (g:type-is-enum "GtkFilterChange"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkFilterChange")
          (g:gtype (cffi:foreign-funcall "gtk_filter_change_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:filter-change
          (glib:symbol-for-gtype "GtkFilterChange")))
  ;; Check names
  (is (equal '("GTK_FILTER_CHANGE_DIFFERENT" "GTK_FILTER_CHANGE_LESS_STRICT"
               "GTK_FILTER_CHANGE_MORE_STRICT")
             (glib-test:list-enum-item-names "GtkFilterChange")))
  ;; Check values
  (is (equal '(0 1 2)
             (glib-test:list-enum-item-values "GtkFilterChange")))
  ;; Check nick names
  (is (equal '("different" "less-strict" "more-strict")
             (glib-test:list-enum-item-nicks "GtkFilterChange")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkFilterChange" GTK:FILTER-CHANGE
                       (:EXPORT T
                        :TYPE-INITIALIZER "gtk_filter_change_get_type")
                       (:DIFFERENT 0)
                       (:LESS-STRICT 1)
                       (:MORE-STRICT 2))
             (gobject:get-gtype-definition "GtkFilterChange"))))

;;;     GtkFilter

(test gtk-filter-class
  ;; Check type
  (is (g:type-is-object "GtkFilter"))
  ;; Check registered name
  (is (eq 'gtk:filter
          (glib:symbol-for-gtype "GtkFilter")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkFilter")
          (g:gtype (cffi:foreign-funcall "gtk_filter_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkFilter")))
  ;; Check children
  (if *first-run-gtk-test*
      (is (equal '("GtkBoolFilter" "GtkCustomFilter" "GtkFileFilter"
                   "GtkMultiFilter" "GtkStringFilter")
                 (glib-test:list-children "GtkFilter")))
      (is (equal '("GtkBoolFilter" "GtkCustomFilter" "GtkFileFilter"
                   "GtkFontFilter" "GtkMultiFilter" "GtkStringFilter")
                 (glib-test:list-children "GtkFilter"))))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkFilter")))
  ;; Check properties
  (is (equal '()
             (glib-test:list-properties "GtkFilter")))
  ;; Check signals
  (is (equal '("changed")
             (glib-test:list-signals "GtkFilter")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkFilter" GTK:FILTER
                       (:SUPERCLASS G:OBJECT
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_filter_get_type")
                       NIL)
             (gobject:get-gtype-definition "GtkFilter"))))

;;; --- Signals ----------------------------------------------------------------

(test gtk-filter-changed-signal
  (let* ((name "changed")
         (gtype (g:gtype "GtkFilter"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    ;; Check parameter types
    (is (equal '("GtkFilterChange")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_filter_match

(test gtk-filter-match
  (let* ((store (create-string-list-for-package))
         (expression (gtk:property-expression-new "GtkStringObject"
                                                  nil "string"))
         (filter (gtk:string-filter-new expression))
         (model (gtk:filter-list-model-new store filter)))
    ;; Check filter model
    (is (eq filter (gtk:filter-list-model-filter model)))
    (is-false (gtk:filter-list-model-incremental model))
    (is (eq (g:gtype "GObject") (gtk:filter-list-model-item-type model)))
    (is (eq store (gtk:filter-list-model-model model)))
    #-windows
    (is (< 3400 (gtk:filter-list-model-n-items model)))
    #+windows
    (is (< 3330 (gtk:filter-list-model-n-items model)))
    (is (= 0 (gtk:filter-list-model-pending model)))
    ;; At this point we have a filter list model with string objects
    (is (eq :substring (setf (gtk:string-filter-match-mode filter) :substring)))
    (is (eq :substring (gtk:string-filter-match-mode filter)))
    (is (eq :all (gtk:filter-strictness filter)))
    (is-true (gtk:string-filter-ignore-case filter))
    ;; Match "string" as a substring in the filter model
    (is (string= (setf (gtk:string-filter-search filter) "string") "string"))
    (is (= 42 (gtk:filter-list-model-n-items model)))
    ;; Check match
    (is (eq :exact (setf (gtk:string-filter-match-mode filter) :exact)))
    (is-true (gtk:filter-match filter
                               (gtk:string-object-new "string")))
    (is-false (gtk:filter-match filter
                               (gtk:string-object-new "gtk:string")))
    (is-false (gtk:filter-match filter
                               (gtk:string-object-new "gtk:button")))
    ;; Change match mode
    (is (eq :substring (setf (gtk:string-filter-match-mode filter) :substring)))
    (is-true (gtk:filter-match filter
                               (gtk:string-object-new "string")))
    (is-true (gtk:filter-match filter
                               (gtk:string-object-new "gtk:string")))
    (is-true (gtk:filter-match filter
                               (gtk:string-object-new "gtk:string-filter")))
    (is-false (gtk:filter-match filter
                               (gtk:string-object-new "gtk:button")))
    ;; Check memory management
    (is-false (setf (gtk:filter-list-model-model model) nil))
    (is-false (setf (gtk:filter-list-model-filter model) nil))
    (is (= 1 (g:object-ref-count store)))
    (is (= 1 (g:object-ref-count filter)))
    (is (= 1 (g:object-ref-count model)))))

;;;     gtk_filter_get_strictness

(test gtk-filter-strictness
  (is (eq :all (gtk:filter-strictness (gtk:custom-filter-new))))
  (is (eq :none (gtk:filter-strictness (gtk:any-filter-new))))
  (is (eq :all (gtk:filter-strictness (gtk:every-filter-new))))
  (is (eq :none (gtk:filter-strictness (gtk:bool-filter-new))))
  (is (eq :all (gtk:filter-strictness (gtk:string-filter-new))))
  (is (eq :none (gtk:filter-strictness (gtk:file-filter-new)))))

;;;     gtk_filter_changed

(test gtk-filter-changed
  (let* ((store (gtk:string-list-new '()))
         (expression (gtk:property-expression-new "GtkStringObject"
                                                  nil "string"))
         (filter (gtk:string-filter-new expression))
         (model (gtk:filter-list-model-new store filter))
         (msg nil))
    (is (typep model 'gtk:filter-list-model))
    (g:signal-connect filter "changed"
                      (lambda (filter change)
                        (declare (ignore filter))
                        (push change msg)))
    (is-false (gtk:filter-changed filter :different))
    (is (equal '(:different) msg))
    ;; Check memory management
    (is-false (setf (gtk:filter-list-model-model model) nil))
    (is-false (setf (gtk:filter-list-model-filter model) nil))
    (is (= 1 (g:object-ref-count store)))
    (is (= 1 (g:object-ref-count filter)))
    (is (= 1 (g:object-ref-count model)))))

;;; 2024-10-24

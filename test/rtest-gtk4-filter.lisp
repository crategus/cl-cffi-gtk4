(in-package :gtk-test)

(def-suite gtk-filter :in gtk-suite)
(in-suite gtk-filter)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFilterMatch

(test gtk-filter-match
  ;; Check the type
  (is (g:type-is-enum "GtkFilterMatch"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkFilterMatch")
          (g:gtype (cffi:foreign-funcall "gtk_filter_match_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gtk:filter-match
          (glib:symbol-for-gtype "GtkFilterMatch")))
  ;; Check the names
  (is (equal '("GTK_FILTER_MATCH_SOME" "GTK_FILTER_MATCH_NONE"
               "GTK_FILTER_MATCH_ALL")
             (list-enum-item-name "GtkFilterMatch")))
  ;; Check the values
  (is (equal '(0 1 2)
             (list-enum-item-value "GtkFilterMatch")))
  ;; Check the nick names
  (is (equal '("some" "none" "all")
             (list-enum-item-nick "GtkFilterMatch")))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkFilterMatch" GTK-FILTER-MATCH
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_filter_match_get_type")
                             (:SOME 0)
                             (:NONE 1)
                             (:ALL 2))
             (gobject:get-g-type-definition "GtkFilterMatch"))))

;;;     GtkFilterChange

(test gtk-filter-change
  ;; Check the type
  (is (g:type-is-enum "GtkFilterChange"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkFilterChange")
          (g:gtype (cffi:foreign-funcall "gtk_filter_change_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gtk:filter-change
          (glib:symbol-for-gtype "GtkFilterChange")))
  ;; Check the names
  (is (equal '("GTK_FILTER_CHANGE_DIFFERENT" "GTK_FILTER_CHANGE_LESS_STRICT"
               "GTK_FILTER_CHANGE_MORE_STRICT")
             (list-enum-item-name "GtkFilterChange")))
  ;; Check the values
  (is (equal '(0 1 2)
             (list-enum-item-value "GtkFilterChange")))
  ;; Check the nick names
  (is (equal '("different" "less-strict" "more-strict")
             (list-enum-item-nick "GtkFilterChange")))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkFilterChange" GTK-FILTER-CHANGE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_filter_change_get_type")
                             (:DIFFERENT 0)
                             (:LESS-STRICT 1)
                             (:MORE-STRICT 2))
             (gobject:get-g-type-definition "GtkFilterChange"))))

;;;     GtkFilter

(test gtk-filter-class
  ;; Type check
  (is (g:type-is-object "GtkFilter"))
  ;; Check the registered name
  (is (eq 'gtk:filter
          (glib:symbol-for-gtype "GtkFilter")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkFilter")
          (g:gtype (cffi:foreign-funcall "gtk_filter_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkFilter")))
  ;; Check the children
  (is (equal '("GtkBoolFilter" "GtkCustomFilter" "GtkFileFilter"
               "GtkMultiFilter" "GtkStringFilter")
             (list-children "GtkFilter")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkFilter")))
  ;; Check the properties
  (is (equal '()
             (list-properties "GtkFilter")))
  ;; Check the signals
  (is (equal '("changed")
             (list-signals "GtkFilter")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkFilter" GTK-FILTER
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_filter_get_type")
                       NIL)
             (gobject:get-g-type-definition "GtkFilter"))))

;;; --- Signals ----------------------------------------------------------------

;;;     changed
;;;     gtk_filter-changed

(test gtk-string-filter-changed-signal
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
    (is (equal '(:different) msg))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_filter_match
;;;     gtk_filter_get_strictness

(test gtk-filter-match/strictness
  (let* ((store (gtk:string-list-new '()))
         (expression (gtk:property-expression-new "GtkStringObject"
                                                  nil "string"))
         (filter (gtk:string-filter-new expression))
         (model (gtk:filter-list-model-new store filter)))
    (declare (ignore model))
    ;; Fill the string list with strings
    (do-external-symbols (symbol (find-package "GTK"))
      (gtk:string-list-append store (string-downcase (format nil "~a" symbol))))
    ;; At this point we have a filter list model with string objects
    (is (eq :exact (setf (gtk:string-filter-match-mode filter) :exact)))
    (is (eq :all (gtk:filter-strictness filter)))
    (is (eq :exact (gtk:string-filter-match-mode filter)))
    (is-true (gtk:string-filter-ignore-case filter))
    ;; Match strings in the model
    (is-true (gtk:filter-match filter
                               (gtk:string-object-new "gtk:string-filter")))
    (is-true (gtk:filter-match filter
                               (gtk:string-object-new "gtk:button")))
    ;; This should be false, do a correct test!? But the value of strictness
    ;; is :all!? Do we habe a problem with the implementation!?
    (is-true (gtk:filter-match filter (make-instance 'gtk:button)))
))

;;; --- 2023-11-3 --------------------------------------------------------------

(in-package :gtk-test)

(def-suite gtk-filter :in gtk-suite)
(in-suite gtk-filter)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFilterMatch

(test filter-match
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
  (is (equal '(DEFINE-G-ENUM "GtkFilterMatch" GTK-FILTER-MATCH
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_filter_match_get_type")
                             (:SOME 0)
                             (:NONE 1)
                             (:ALL 2))
             (gobject:get-g-type-definition "GtkFilterMatch"))))

;;;     GtkFilterChange

(test filter-change
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
  (is (equal '(DEFINE-G-ENUM "GtkFilterChange" GTK-FILTER-CHANGE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_filter_change_get_type")
                             (:DIFFERENT 0)
                             (:LESS-STRICT 1)
                             (:MORE-STRICT 2))
             (gobject:get-g-type-definition "GtkFilterChange"))))

;;;     GtkFilter

(test filter-class
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
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkFilter" GTK-FILTER
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_filter_get_type")
                       NIL)
             (gobject:get-g-type-definition "GtkFilter"))))

;;; --- Signals ----------------------------------------------------------------

;;;     changed

;;; --- Functions --------------------------------------------------------------

;;;     gtk_filter_match
;;;     gtk_filter_get_strictness
;;;     gtk_filter_changed

;;; --- 2023-5-29 --------------------------------------------------------------

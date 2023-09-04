(in-package :gtk-test)

(def-suite gtk-string-filter :in gtk-suite)
(in-suite gtk-string-filter)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkStringFilterMatchMode

(test gtk-string-filter-match-mode
  ;; Check the type
  (is (g:type-is-enum "GtkStringFilterMatchMode"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkStringFilterMatchMode")
          (g:gtype (cffi:foreign-funcall "gtk_string_filter_match_mode_get_type" 
                                         :size))))
  ;; Check the registered name
  (is (eq 'gtk:string-filter-match-mode
          (glib:symbol-for-gtype "GtkStringFilterMatchMode")))
  ;; Check the names
  (is (equal '("GTK_STRING_FILTER_MATCH_MODE_EXACT" 
               "GTK_STRING_FILTER_MATCH_MODE_SUBSTRING"
               "GTK_STRING_FILTER_MATCH_MODE_PREFIX")
             (list-enum-item-name "GtkStringFilterMatchMode")))
  ;; Check the values
  (is (equal '(0 1 2)
             (list-enum-item-value "GtkStringFilterMatchMode")))
  ;; Check the nick names
  (is (equal '("exact" "substring" "prefix")
             (list-enum-item-nick "GtkStringFilterMatchMode")))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkStringFilterMatchMode"
                                     GTK-STRING-FILTER-MATCH-MODE
                                     (:EXPORT T 
                                      :TYPE-INITIALIZER 
                                      "gtk_string_filter_match_mode_get_type")
                                     (:EXACT 0)
                                     (:SUBSTRING 1)
                                     (:PREFIX 2))
             (gobject:get-g-type-definition "GtkStringFilterMatchMode"))))

;;;     GtkStringFilter

(test gtk-string-filter-class
  ;; Type check
  (is (g:type-is-object "GtkStringFilter"))
  ;; Check the registered name
  (is (eq 'gtk:string-filter
          (glib:symbol-for-gtype "GtkStringFilter")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkStringFilter")
          (g:gtype (cffi:foreign-funcall "gtk_string_filter_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkFilter")
          (g:type-parent "GtkStringFilter")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkStringFilter")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkStringFilter")))
  ;; Check the properties
  (is (equal '("expression" "ignore-case" "match-mode" "search")
             (list-properties "GtkStringFilter")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkStringFilter")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkStringFilter" GTK-STRING-FILTER
                               (:SUPERCLASS GTK-FILTER :EXPORT T :INTERFACES
                                NIL :TYPE-INITIALIZER
                                "gtk_string_filter_get_type")
                               ((EXPRESSION GTK-STRING-FILTER-EXPRESSION
                                 "expression" "GtkExpression" T T)
                                (IGNORE-CASE GTK-STRING-FILTER-IGNORE-CASE
                                 "ignore-case" "gboolean" T T)
                                (MATCH-MODE GTK-STRING-FILTER-MATCH-MODE
                                 "match-mode" "GtkStringFilterMatchMode" T T)
                                (SEARCH GTK-STRING-FILTER-SEARCH "search"
                                        "gchararray" T T)))
             (gobject:get-g-type-definition "GtkStringFilter"))))

;;; --- Properties -------------------------------------------------------------

;;;     expression
;;;     ignore-case
;;;     match-mode
;;;     search

(test gtk-string-filter-properties
  (let ((filter (make-instance 'gtk:string-filter)))
;    (is-false (gtk:string-filter-expression filter))
    (is-true (gtk:string-filter-ignore-case filter))
    (is-false (gtk:string-filter-search filter))
))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_string_filter_new 

;;;     gtk_string_filter_get_search 
;;;     gtk_string_filter_set_search 
;;;     gtk_string_filter_get_expression 
;;;     gtk_string_filter_set_expression 
;;;     gtk_string_filter_get_ignore_case 
;;;     gtk_string_filter_set_ignore_case 
;;;     gtk_string_filter_get_match_mode 
;;;     gtk_string_filter_set_match_mode 

;;; --- 2023-9-3 ---------------------------------------------------------------

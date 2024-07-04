(in-package :gtk-test)

(def-suite gtk-string-filter :in gtk-suite)
(in-suite gtk-string-filter)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkStringFilterMatchMode

(test gtk-string-filter-match-mode
  ;; Check type
  (is (g:type-is-enum "GtkStringFilterMatchMode"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkStringFilterMatchMode")
          (g:gtype (cffi:foreign-funcall "gtk_string_filter_match_mode_get_type"
                                         :size))))
  ;; Check registered name
  (is (eq 'gtk:string-filter-match-mode
          (glib:symbol-for-gtype "GtkStringFilterMatchMode")))
  ;; Check names
  (is (equal '("GTK_STRING_FILTER_MATCH_MODE_EXACT"
               "GTK_STRING_FILTER_MATCH_MODE_SUBSTRING"
               "GTK_STRING_FILTER_MATCH_MODE_PREFIX")
             (gtk-test:list-enum-item-name "GtkStringFilterMatchMode")))
  ;; Check values
  (is (equal '(0 1 2)
             (gtk-test:list-enum-item-value "GtkStringFilterMatchMode")))
  ;; Check nick names
  (is (equal '("exact" "substring" "prefix")
             (gtk-test:list-enum-item-nick "GtkStringFilterMatchMode")))
  ;; Check enum definition
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
  ;; Check type
  (is (g:type-is-object "GtkStringFilter"))
  ;; Check registered name
  (is (eq 'gtk:string-filter
          (glib:symbol-for-gtype "GtkStringFilter")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkStringFilter")
          (g:gtype (cffi:foreign-funcall "gtk_string_filter_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkFilter")
          (g:type-parent "GtkStringFilter")))
  ;; Check children
  (is (equal '()
             (gtk-test:list-children "GtkStringFilter")))
  ;; Check interfaces
  (is (equal '()
             (gtk-test:list-interfaces "GtkStringFilter")))
  ;; Check properties
  (is (equal '("expression" "ignore-case" "match-mode" "search")
             (gtk-test:list-properties "GtkStringFilter")))
  ;; Check signals
  (is (equal '()
             (gtk-test:list-signals "GtkStringFilter")))
  ;; Check class definition
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
    (is (cffi:pointer-eq (cffi:null-pointer)
                         (gtk:string-filter-expression filter)))
    (is-true (gtk:string-filter-ignore-case filter))
    (is (eq :substring (gtk:string-filter-match-mode filter)))
    (is-false (gtk:string-filter-search filter))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_string_filter_new

(test gtk-string-filter-new
  (let* ((expression (gtk:property-expression-new "GtkStringObject"
                                                  nil "string"))
         (filter (gtk:string-filter-new expression)))
    (is (cffi:pointerp (gtk:string-filter-expression filter)))
    (is-true (gtk:string-filter-ignore-case filter))
    (is (eq :substring (gtk:string-filter-match-mode filter)))
    (is (string= "search" (setf (gtk:string-filter-search filter) "search")))
    (is (string= "search" (gtk:string-filter-search filter)))
    (is-false (gtk:expression-unref expression))))

;;; 2024-7-4

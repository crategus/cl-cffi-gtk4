(in-package :gtk-test)

(def-suite gtk-string-filter :in gtk-list-model-support)
(in-suite gtk-string-filter)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkStringFilterMatchMode

(test gtk-string-filter-match-mode-enumeration
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
             (glib-test:list-enum-item-names "GtkStringFilterMatchMode")))
  ;; Check values
  (is (equal '(0 1 2)
             (glib-test:list-enum-item-values "GtkStringFilterMatchMode")))
  ;; Check nick names
  (is (equal '("exact" "substring" "prefix")
             (glib-test:list-enum-item-nicks "GtkStringFilterMatchMode")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkStringFilterMatchMode"
                                    GTK:STRING-FILTER-MATCH-MODE
                       (:EXPORT T
                        :TYPE-INITIALIZER
                        "gtk_string_filter_match_mode_get_type")
                       (:EXACT 0)
                       (:SUBSTRING 1)
                       (:PREFIX 2))
             (gobject:get-gtype-definition "GtkStringFilterMatchMode"))))

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
             (glib-test:list-children "GtkStringFilter")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkStringFilter")))
  ;; Check properties
  (is (equal '("expression" "ignore-case" "match-mode" "search")
             (glib-test:list-properties "GtkStringFilter")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkStringFilter")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkStringFilter" GTK:STRING-FILTER
                       (:SUPERCLASS GTK:FILTER
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_string_filter_get_type")
                       ((EXPRESSION STRING-FILTER-EXPRESSION
                         "expression" "GtkExpression" T T)
                        (IGNORE-CASE STRING-FILTER-IGNORE-CASE
                         "ignore-case" "gboolean" T T)
                        (MATCH-MODE STRING-FILTER-MATCH-MODE
                         "match-mode" "GtkStringFilterMatchMode" T T)
                        (SEARCH STRING-FILTER-SEARCH
                         "search" "gchararray" T T)))
             (gobject:get-gtype-definition "GtkStringFilter"))))

;;; --- Properties -------------------------------------------------------------

;;;     gtk:string-filter-expression

(test gtk-string-filter-expression
  (glib-test:with-check-memory (filter)
    (let ((expression (gtk:constant-expression-new "gchararray" "string")))
      (is (typep (setf filter (make-instance 'gtk:string-filter))
                 'gtk:string-filter))
      (is (cffi:null-pointer-p (gtk:string-filter-expression filter)))
      (is (cffi:pointer-eq expression
                           (setf (gtk:string-filter-expression filter)
                                 expression)))
      (is (cffi:pointer-eq expression
                           (gtk:string-filter-expression filter))))))

;;;     gtk:string-filter-ignore-case

(test gtk-string-filter-ignore-case
  (glib-test:with-check-memory (filter)
    (is (typep (setf filter (make-instance 'gtk:string-filter))
               'gtk:string-filter))
    (is-true (gtk:string-filter-ignore-case filter))
    (is-false (setf (gtk:string-filter-ignore-case filter) nil))
    (is-false (gtk:string-filter-ignore-case filter))))

;;;     gtk:string-filter-match-mode

(test gtk-string-filter-match-mode
  (glib-test:with-check-memory (filter)
    (is (typep (setf filter (make-instance 'gtk:string-filter))
               'gtk:string-filter))
    (is (eq :substring (gtk:string-filter-match-mode filter)))
    (is (eq :prefix (setf (gtk:string-filter-match-mode filter) :prefix)))
    (is (eq :prefix (gtk:string-filter-match-mode filter)))))

;;;     gtk:string-filter-search

(test gtk-string-filter-search
  (glib-test:with-check-memory (filter)
    (is (typep (setf filter (make-instance 'gtk:string-filter))
               'gtk:string-filter))
    (is-false (gtk:string-filter-search filter))
    (is (string= "search" (setf (gtk:string-filter-search filter) "search")))
    (is (string= "search" (gtk:string-filter-search filter)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_string_filter_new

(test gtk-string-filter-new
  (glib-test:with-check-memory (filter)
    (let ((expression (gtk:property-expression-new "GtkStringObject"
                                                    nil "string")))
    (is (typep (setf filter (gtk:string-filter-new expression))
               'gtk:string-filter))
    (is (cffi:pointerp (gtk:string-filter-expression filter)))
    (is-true (gtk:string-filter-ignore-case filter))
    (is (eq :substring (gtk:string-filter-match-mode filter)))
    (is (string= "search" (setf (gtk:string-filter-search filter) "search")))
    (is (string= "search" (gtk:string-filter-search filter)))
    (is-false (gtk:expression-unref expression)))))

;;; 2024-12-16

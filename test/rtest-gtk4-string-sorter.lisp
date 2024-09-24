(in-package :gtk-test)

(def-suite gtk-string-sorter :in gtk-suite)
(in-suite gtk-string-sorter)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkCollation                                       Since 4.10

(test gtk-collation
  ;; Check type
  (is (g:type-is-enum "GtkCollation"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkCollation")
          (g:gtype (cffi:foreign-funcall "gtk_collation_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:collation
          (glib:symbol-for-gtype "GtkCollation")))
  ;; Check names
  (is (equal '("GTK_COLLATION_NONE" "GTK_COLLATION_UNICODE"
               "GTK_COLLATION_FILENAME")
             (glib-test:list-enum-item-names "GtkCollation")))
  ;; Check values
  (is (equal '(0 1 2)
             (glib-test:list-enum-item-values "GtkCollation")))
  ;; Check nick names
  (is (equal '("none" "unicode" "filename")
             (glib-test:list-enum-item-nicks "GtkCollation")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkCollation" GTK:COLLATION
                       (:EXPORT T
                        :TYPE-INITIALIZER "gtk_collation_get_type")
                       (:NONE 0)
                       (:UNICODE 1)
                       (:FILENAME 2))
             (gobject:get-gtype-definition "GtkCollation"))))

;;;     GtkStringSorter

(test gtk-string-sorter-class
  ;; Check type
  (is (g:type-is-object "GtkStringSorter"))
  ;; Check registered name
  (is (eq 'gtk:string-sorter
          (glib:symbol-for-gtype "GtkStringSorter")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkStringSorter")
          (g:gtype (cffi:foreign-funcall "gtk_string_sorter_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkSorter")
          (g:type-parent "GtkStringSorter")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkStringSorter")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkStringSorter")))
  ;; Check properties
  (is (equal '("collation" "expression" "ignore-case")
             (glib-test:list-properties "GtkStringSorter")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkStringSorter")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkStringSorter" GTK:STRING-SORTER
                       (:SUPERCLASS GTK:SORTER
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_string_sorter_get_type")
                       ((COLLATION STRING-SORTER-COLLATION
                         "collation" "GtkCollation" T T)
                        (EXPRESSION STRING-SORTER-EXPRESSION
                         "expression" "GtkExpression" T T)
                        (IGNORE-CASE STRING-SORTER-IGNORE-CASE
                         "ignore-case" "gboolean" T T)))
             (gobject:get-gtype-definition "GtkStringSorter"))))

;;; --- Properties -------------------------------------------------------------

;;;     collation                                          Since 4.10
;;;     expression
;;;     ignore-case

;;; --- Functions --------------------------------------------------------------

;;;     gtk_string_sorter_new

;;; 2024-9-19

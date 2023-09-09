(in-package :gtk-test)

(def-suite gtk-string-sorter :in gtk-suite)
(in-suite gtk-string-sorter)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkCollation                                       Since 4.10

(test gtk-collation
  ;; Check the type
  (is (g:type-is-enum "GtkCollation"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkCollation")
          (g:gtype (cffi:foreign-funcall "gtk_collation_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gtk:collation
          (glib:symbol-for-gtype "GtkCollation")))
  ;; Check the names
  (is (equal '("GTK_COLLATION_NONE" "GTK_COLLATION_UNICODE"
               "GTK_COLLATION_FILENAME")
             (list-enum-item-name "GtkCollation")))
  ;; Check the values
  (is (equal '(0 1 2)
             (list-enum-item-value "GtkCollation")))
  ;; Check the nick names
  (is (equal '("none" "unicode" "filename")
             (list-enum-item-nick "GtkCollation")))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkCollation" GTK-COLLATION
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "gtk_collation_get_type")
                                     (:NONE 0)
                                     (:UNICODE 1)
                                     (:FILENAME 2))
             (gobject:get-g-type-definition "GtkCollation"))))

;;;     GtkStringSorter

(test gtk-string-sorter-class
  ;; Type check
  (is (g:type-is-object "GtkStringSorter"))
  ;; Check the registered name
  (is (eq 'gtk:string-sorter
          (glib:symbol-for-gtype "GtkStringSorter")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkStringSorter")
          (g:gtype (cffi:foreign-funcall "gtk_string_sorter_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkSorter")
          (g:type-parent "GtkStringSorter")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkStringSorter")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkStringSorter")))
  ;; Check the properties
  (is (equal '("collation" "expression" "ignore-case")
             (list-properties "GtkStringSorter")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkStringSorter")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkStringSorter" GTK-STRING-SORTER
                               (:SUPERCLASS GTK-SORTER :EXPORT T :INTERFACES
                                NIL :TYPE-INITIALIZER
                                "gtk_string_sorter_get_type")
                               ((COLLATION GTK-STRING-SORTER-COLLATION
                                 "collation" "GtkCollation" T T)
                                (EXPRESSION GTK-STRING-SORTER-EXPRESSION
                                 "expression" "GtkExpression" T T)
                                (IGNORE-CASE GTK-STRING-SORTER-IGNORE-CASE
                                 "ignore-case" "gboolean" T T)))
             (gobject:get-g-type-definition "GtkStringSorter"))))

;;; --- Properties -------------------------------------------------------------

;;;     collation                                          Since 4.10
;;;     expression
;;;     ignore-case

;;; --- Functions --------------------------------------------------------------

;;;     gtk_string_sorter_new

;;; --- 2023-9-5 ---------------------------------------------------------------

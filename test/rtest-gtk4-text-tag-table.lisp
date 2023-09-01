(in-package :gtk-test)

(def-suite gtk-text-tag-table :in gtk-suite)
(in-suite gtk-text-tag-table)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkTextTagTable

(test gtk-text-tag-table-class
  ;; Type check
  (is (g:type-is-object "GtkTextTagTable"))
  ;; Check the registered name
  (is (eq 'gtk:text-tag-table
          (glib:symbol-for-gtype "GtkTextTagTable")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkTextTagTable")
          (g:gtype (cffi:foreign-funcall "gtk_text_tag_table_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkTextTagTable")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkTextTagTable")))
  ;; Check the interfaces
  (is (equal '("GtkBuildable")
             (list-interfaces "GtkTextTagTable")))
  ;; Check the properties
  (is (equal '()
             (list-properties "GtkTextTagTable")))
  ;; Check the signals
  (is (equal '("tag-added" "tag-changed" "tag-removed")
             (list-signals "GtkTextTagTable")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkTextTagTable"
                                             GTK-TEXT-TAG-TABLE
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES
                                ("GtkBuildable") :TYPE-INITIALIZER
                                "gtk_text_tag_table_get_type")
                               NIL)
             (gobject:get-g-type-definition "GtkTextTagTable"))))

;;; --- Signals ----------------------------------------------------------------

;;;     tag-added
;;;     tag-changed
;;;     tag-removed

;;; --- Functions --------------------------------------------------------------

;;;     GtkTextTagTableForeach

;;;     gtk_text_tag_table_new

(test gtk-text-tag-table-new
  (is (typep (gtk:text-tag-table-new) 'gtk:text-tag-table)))

;;;     gtk_text_tag_table_add
;;;     gtk_text_tag_table_remove
;;;     gtk_text_tag_table_lookup
;;;     gtk_text_tag_table_foreach
;;;     gtk_text_tag_table_get_size

;;; --- 2023-8-26 --------------------------------------------------------------

(in-package :gtk-test)

(def-suite gtk-text-tag-table :in gtk-suite)
(in-suite gtk-text-tag-table)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkTextTagTable

(test gtk-text-tag-table-class
  ;; Check type
  (is (g:type-is-object "GtkTextTagTable"))
  ;; Check registered name
  (is (eq 'gtk:text-tag-table
          (glib:symbol-for-gtype "GtkTextTagTable")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkTextTagTable")
          (g:gtype (cffi:foreign-funcall "gtk_text_tag_table_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkTextTagTable")))
  ;; Check children
  (is (equal '()
             (gtk-test:list-children "GtkTextTagTable")))
  ;; Check the interfaces
  (is (equal '("GtkBuildable")
             (gtk-test:list-interfaces "GtkTextTagTable")))
  ;; Check the properties
  (is (equal '()
             (gtk-test:list-properties "GtkTextTagTable")))
  ;; Check the signals
  (is (equal '("tag-added" "tag-changed" "tag-removed")
             (gtk-test:list-signals "GtkTextTagTable")))
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

(test gtk-tag-table-tag-added-signal
  (let* ((name "tag-added")
         (gtype (g:gtype "GtkTextTagTable"))
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
    (is (equal '("GtkTextTag")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     tag-changed

(test gtk-tag-table-tag-changed-signal
  (let* ((name "tag-changed")
         (gtype (g:gtype "GtkTextTagTable"))
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
    (is (equal '("GtkTextTag" "gboolean")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     tag-removed

(test gtk-tag-table-tag-removed-signal
  (let* ((name "tag-removed")
         (gtype (g:gtype "GtkTextTagTable"))
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
    (is (equal '("GtkTextTag")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Functions --------------------------------------------------------------

;;;     GtkTextTagTableForeach

;;;     gtk_text_tag_table_new

(test gtk-text-tag-table-new
  (is (typep (gtk:text-tag-table-new) 'gtk:text-tag-table)))

;;;     gtk_text_tag_table_add
;;;     gtk_text_tag_table_remove
;;;     gtk_text_tag_table_lookup
;;;     gtk_text_tag_table_get_size

(test gtk-text-tag-table-add/remove
  (let ((table (gtk:text-tag-table-new)))
    (is-true (gtk:text-tag-table-add table
                                     (gtk:text-tag-new "tag1" :font "fixed")))
    (is-true (gtk:text-tag-table-add table
                                     (gtk:text-tag-new "tag2" :weight 700)))
    (is (= 2 (gtk:text-tag-table-size table)))
    (is-false (gtk:text-tag-table-remove table
                                         (gtk:text-tag-table-lookup table
                                                                    "tag1")))
    (is (= 1 (gtk:text-tag-table-size table)))
    (is-false (gtk:text-tag-table-remove table
                                         (gtk:text-tag-table-lookup table
                                                                    "tag2")))
    (is (= 0 (gtk:text-tag-table-size table)))))

;;;     gtk_text_tag_table_foreach

(test gtk-test-tag-table-foreach
  (let ((table (gtk:text-tag-table-new))
        (count 0))
    (dotimes (i 1000)
      (gtk:text-tag-table-add table
                              (gtk:text-tag-new (format nil "tag~a" i)
                                                :weight i)))
    (is (= 1000 (gtk:text-tag-table-size table)))
    (is-false (gtk:text-tag-table-foreach table
                                          (lambda (tag)
                                            (is (typep tag 'gtk:text-tag))
                                            (incf count))))
    (is (= count (gtk:text-tag-table-size table)))))

;;; 2024-7-2

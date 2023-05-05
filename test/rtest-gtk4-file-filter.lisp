(in-package :gtk-test)

(def-suite gtk-file-filter :in gtk-suite)
(in-suite gtk-file-filter)

(cffi:foreign-funcall "gtk_bool_filter_get_type" :size)
(cffi:foreign-funcall "gtk_custom_filter_get_type" :size)
(cffi:foreign-funcall "gtk_multi_filter_get_type" :size)
(cffi:foreign-funcall "gtk_string_filter_get_type" :size)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFileFilter

(test file-filter-class
  ;; Type check
  (is (g:type-is-object "GtkFileFilter"))
  ;; Check the registered name
  (is (eq 'gtk:file-filter
          (gobject:symbol-for-gtype "GtkFileFilter")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkFileFilter")
          (g:gtype (cffi:foreign-funcall "gtk_file_filter_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkFilter")
          (g:type-parent "GtkFileFilter")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkFileFilter")))
  ;; Check the interfaces
  (is (equal '("GtkBuildable")
             (list-interfaces "GtkFileFilter")))
  ;; Check the properties
  (is (equal '("mime-types" "name" "patterns" "suffixes")
             (list-properties "GtkFileFilter")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkFileFilter")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkFileFilter" GTK-FILE-FILTER
                       (:SUPERCLASS GTK-FILTER :EXPORT T :INTERFACES
                        ("GtkBuildable") :TYPE-INITIALIZER
                        "gtk_file_filter_get_type")
                       ((MIME-TYPES GTK-FILE-FILTER-MIME-TYPES "mime-types"
                         "GStrv" NIL NIL)
                        (NAME GTK-FILE-FILTER-NAME "name" "gchararray" T T)
                        (PATTERNS GTK-FILE-FILTER-PATTERNS "patterns" "GStrv"
                         NIL NIL)
                        (SUFFIXES GTK-FILE-FILTER-SUFFIXES "suffixes" "GStrv"
                         NIL NIL)))
             (gobject:get-g-type-definition "GtkFileFilter"))))

;;; --- Properties -------------------------------------------------------------

(test file-filter-name
  (let ((filter (make-instance 'gtk:file-filter)))
    (is (string= "Filter" (setf (gtk:file-filter-name filter) "Filter")))
    (is (string= "Filter" (gtk:file-filter-name filter)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_file_filter_new

(test file-filter-new
  (is (eq 'gtk:file-filter
          (type-of (gtk:file-filter-new)))))

;;;     gtk_file_filter_add_mime_type


(test file-filter-add-mime-type
  (let ((filter (gtk:file-filter-new)))
    (is-false (gtk:file-filter-add-mime-type filter "text/plain"))))

;;;     gtk_file_filter_add_pattern

(test file-filter-add-pattern
  (let ((filter (gtk:file-filter-new)))
    (is-false (gtk:file-filter-add-pattern filter "*.txt"))))

;;;     gtk_file_filter_add_pixbuf_formats

(test file-filter-add-pixbuf-formats
  (let ((filter (gtk:file-filter-new)))
    (is-false (gtk:file-filter-add-pixbuf-formats filter))))

;;;     gtk_file_filter_add_suffix                         Since 4.4

(test file-filter-add-suffix
  (let ((filter (gtk:file-filter-new)))
    (is-false (gtk:file-filter-add-suffix filter "txt"))))

;;;     gtk_file_filter_get_attributes

(test file-filter-attributes.1
  (let ((filter (gtk:file-filter-new)))
    (is-false (gtk:file-filter-attributes filter))
    (is-false (gtk:file-filter-add-mime-type filter "text/plain"))
    (is (equal '("standard::content-type")
               (gtk:file-filter-attributes filter)))))

(test file-filter-attributes.2
  (let ((filter (gtk:file-filter-new)))
    (is-false (gtk:file-filter-attributes filter))
    (is-false (gtk:file-filter-add-pattern filter "*"))
    (is (equal '("standard::display-name")
               (gtk:file-filter-attributes filter)))))

(test file-filter-attributes.3
  (let ((filter (gtk:file-filter-new)))
    (is-false (gtk:file-filter-attributes filter))
    (is-false (gtk:file-filter-add-pixbuf-formats filter))
    (is (equal '("standard::content-type")
               (gtk:file-filter-attributes filter)))))

;;;     gtk_file_filter_new_from_gvariant
;;;     gtk_file_filter_to_gvariant

(test gtk-file-filter-to-gvariant
  (let ((filter (gtk:file-filter-new))
        (variant nil))
    (is-false (gtk:file-filter-add-mime-type filter "text/plain"))
    (is (cffi:pointerp (setf variant (gtk:file-filter-to-gvariant filter))))
    #-windows
    (is (string= "('Einfaches Textdokument', [(1, 'text/plain')])"
                 (g:variant-print variant)))
    #+windows
    (is (string= "('.mhjl-Dateityp', [(1, '.mhjl')])"
                 (g:variant-print variant)))

    (is (eq 'gtk:file-filter
            (type-of (setf filter
                           (gtk:file-filter-new-from-gvariant variant)))))
    (is (cffi:pointerp (setf variant (gtk:file-filter-to-gvariant filter))))
    #-windows
    (is (string= "('Einfaches Textdokument', [(1, 'text/plain')])"
                 (g:variant-print variant)))
    #+windows
    (is (string= "('.mhjl-Dateityp', [(1, '*')])"
                 (g:variant-print variant)))))

;;; --- 2023-5-2 ---------------------------------------------------------------

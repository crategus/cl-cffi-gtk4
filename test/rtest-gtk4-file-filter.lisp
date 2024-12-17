(in-package :gtk-test)

(def-suite gtk-file-filter :in gtk-list-model-support)
(in-suite gtk-file-filter)

(cffi:foreign-funcall "gtk_bool_filter_get_type" :size)
(cffi:foreign-funcall "gtk_custom_filter_get_type" :size)
(cffi:foreign-funcall "gtk_multi_filter_get_type" :size)
(cffi:foreign-funcall "gtk_string_filter_get_type" :size)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFileFilter

(test gtk-file-filter-class
  ;; Check type
  (is (g:type-is-object "GtkFileFilter"))
  ;; Check registered name
  (is (eq 'gtk:file-filter
          (glib:symbol-for-gtype "GtkFileFilter")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkFileFilter")
          (g:gtype (cffi:foreign-funcall "gtk_file_filter_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkFilter")
          (g:type-parent "GtkFileFilter")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkFileFilter")))
  ;; Check interfaces
  (is (equal '("GtkBuildable")
             (glib-test:list-interfaces "GtkFileFilter")))
  ;; Check properties
  (is (equal '("mime-types" "name" "patterns" "suffixes")
             (glib-test:list-properties "GtkFileFilter")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkFileFilter")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkFileFilter" GTK:FILE-FILTER
                       (:SUPERCLASS GTK:FILTER
                        :EXPORT T
                        :INTERFACES ("GtkBuildable")
                        :TYPE-INITIALIZER "gtk_file_filter_get_type")
                       ((MIME-TYPES FILE-FILTER-MIME-TYPES
                         "mime-types" "GStrv" NIL NIL)
                        (NAME FILE-FILTER-NAME "name" "gchararray" T T)
                        (PATTERNS FILE-FILTER-PATTERNS
                         "patterns" "GStrv" NIL NIL)
                        (SUFFIXES FILE-FILTER-SUFFIXES
                         "suffixes" "GStrv" NIL NIL)))
             (gobject:get-gtype-definition "GtkFileFilter"))))

;;; --- Properties -------------------------------------------------------------

;;;     gtk:file-filter-name

(test gtk-file-filter-name
  (glib-test:with-check-memory (filter)
    (is (typep (setf filter (make-instance 'gtk:file-filter)) 'gtk:file-filter))
    (is-false (gtk:file-filter-name filter))
    (is (string= "Filter" (setf (gtk:file-filter-name filter) "Filter")))
    (is (string= "Filter" (gtk:file-filter-name filter)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_file_filter_new

(test gtk-file-filter-new
  (glib-test:with-check-memory (nil)
    (is (eq 'gtk:file-filter
            (type-of (gtk:file-filter-new))))))

;;;     gtk_file_filter_add_mime_type

(test gtk-file-filter-add-mime-type
  (glib-test:with-check-memory (filter)
    (is (typep (setf filter (gtk:file-filter-new)) 'gtk:file-filter))
    (is-false (gtk:file-filter-add-mime-type filter "text/plain"))))

;;;     gtk_file_filter_add_pattern

(test gtk-file-filter-add-pattern
  (glib-test:with-check-memory (filter)
    (is (typep (setf filter (gtk:file-filter-new)) 'gtk:file-filter))
    (is-false (gtk:file-filter-add-pattern filter "*.txt"))))

;;;     gtk_file_filter_add_pixbuf_formats

(test gtk-file-filter-add-pixbuf-formats
  (glib-test:with-check-memory (filter)
    (is (typep (setf filter (gtk:file-filter-new)) 'gtk:file-filter))
    (is-false (gtk:file-filter-add-pixbuf-formats filter))))

;;;     gtk_file_filter_add_suffix                         Since 4.4

(test gtk-file-filter-add-suffix
  (glib-test:with-check-memory (filter)
    (is (typep (setf filter (gtk:file-filter-new)) 'gtk:file-filter))
    (is-false (gtk:file-filter-add-suffix filter "txt"))))

;;;     gtk_file_filter_get_attributes

(test gtk-file-filter-attributes.1
  (glib-test:with-check-memory (filter)
    (is (typep (setf filter (gtk:file-filter-new)) 'gtk:file-filter))
    (is-false (gtk:file-filter-attributes filter))
    (is-false (gtk:file-filter-add-mime-type filter "text/plain"))
    (is (equal '("standard::content-type")
               (gtk:file-filter-attributes filter)))))

(test gtk-file-filter-attributes.2
  (glib-test:with-check-memory (filter)
    (is (typep (setf filter (gtk:file-filter-new)) 'gtk:file-filter))
    (is-false (gtk:file-filter-attributes filter))
    (is-false (gtk:file-filter-add-pattern filter "*"))
    (is (equal '("standard::display-name")
               (gtk:file-filter-attributes filter)))))

(test gtk-file-filter-attributes.3
  (glib-test:with-check-memory (filter)
    (is (typep (setf filter (gtk:file-filter-new)) 'gtk:file-filter))
    (is-false (gtk:file-filter-attributes filter))
    (is-false (gtk:file-filter-add-pixbuf-formats filter))
    (is (equal '("standard::content-type")
               (gtk:file-filter-attributes filter)))))

;;;     gtk_file_filter_new_from_gvariant
;;;     gtk_file_filter_to_gvariant

(test gtk-file-filter-to-gvariant
  (glib-test:with-check-memory (filter)
    (let ((variant nil))
      (is (typep (setf filter (gtk:file-filter-new)) 'gtk:file-filter))
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
                   (g:variant-print variant))))))

;;; 2024-12-16

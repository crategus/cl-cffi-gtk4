(in-package :gtk-test)

(def-suite gtk-css-provider :in gtk-theming)
(in-suite gtk-css-provider)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkCssLocation

(test gtk-css-location-properties
  (cffi:with-foreign-object (location '(:struct gtk:css-location))

    (cffi:with-foreign-slots ((gtk::bytes gtk::chars gtk::lines
                               gtk::line-bytes gtk::line-chars)
                              location
                              (:struct gtk:css-location))
      ;; Clear slots
      (setf gtk::bytes 0
            gtk::chars 0
            gtk::lines 0
            gtk::line-bytes 0
            gtk::line-chars 0))
    ;; Get values from CssLocation structure
    (is (= 0 (gtk:css-location-bytes location)))
    (is (= 0 (gtk:css-location-chars location)))
    (is (= 0 (gtk:css-location-lines location)))
    (is (= 0 (gtk:css-location-line-bytes location)))
    (is (= 0 (gtk:css-location-line-chars location)))))

;;;     GtkCssSection

(test gtk-css-section-boxed
  ;; Check type
  (is (g:type-is-boxed "GtkCssSection"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkCssSection")
          (g:gtype (cffi:foreign-funcall "gtk_css_section_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:css-section
          (glib:symbol-for-gtype "GtkCssSection"))))

;;;     gtk_css_section_new

;; TODO: The gtk:css-section-new function creates an g:file object with one
;; reference. The gtk:css-section instance holds a second reference. Is this
;; the correct implementation of the memory management?

(test gtk-css-section-new
  (when *first-run-testsuite*
    (glib-test:with-check-memory (:strong 1)
      (let* ((path (glib-sys:sys-path "test/resource/css-accordion.css"))
             (filename (namestring path))
            section)
        (cffi:with-foreign-objects ((start '(:struct gtk:css-location))
                                    (end '(:struct gtk:css-location)))
          ;; Set start slots
          (cffi:with-foreign-slots ((gtk::bytes gtk::chars gtk::lines
                                     gtk::line-bytes gtk::line-chars)
                                    start
                                    (:struct gtk:css-location))
            (setf gtk::bytes 0
                  gtk::chars 0
                  gtk::lines 0
                  gtk::line-bytes 0
                  gtk::line-chars 0))
          ;; Set end slots
          (cffi:with-foreign-slots ((gtk::bytes gtk::chars gtk::lines
                                     gtk::line-bytes gtk::line-chars)
                                    end
                                    (:struct gtk:css-location))
            (setf gtk::bytes 10
                  gtk::chars 10
                  gtk::lines 5
                  gtk::line-bytes 0
                  gtk::line-chars 0))

          (is (typep (setf section (gtk:css-section-new filename start end))
                     'gtk:css-section))
          (is (string= "css-accordion.css:1:1-6:1"
                       (gtk:css-section-to-string section))))))))

;;;     gtk_css_section_new_with_bytes                      Since 4.16

;;;     gtk_css_section_ref                                 not implemented
;;;     gtk_css_section_unref                               not implemented
;;;     gtk_css_section_print                               not implemented
;;;     gtk_css_section_to_string
;;;     gtk_css_section_get_bytes                           Since 4.16
;;;     gtk_css_section_get_file
;;;     gtk_css_section_get_parent
;;;     gtk_css_section_get_start_location
;;;     gtk_css_section_get_end_location

;;; --- Types and Values -------------------------------------------------------

;;;     GtkCssParserError                                   not implemented
;;;     GtkCssParserWarning                                 not implemented
;;;     GTK_CSS_PARSER_ERROR                                not implemented

;;;     GtkCssProvider

(test gtk-css-provider-class
  ;; Check type
  (is (g:type-is-object "GtkCssProvider"))
  ;; Check registered name
  (is (eq 'gtk:css-provider
          (glib:symbol-for-gtype "GtkCssProvider")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkCssProvider")
          (g:gtype (cffi:foreign-funcall "gtk_css_provider_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkCssProvider")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkCssProvider")))
  ;; Check interfaces
  (is (equal '("GtkStyleProvider")
             (glib-test:list-interfaces "GtkCssProvider")))
  ;; Check properties
  (is (equal '()
             (glib-test:list-properties "GtkCssProvider")))
  ;; Check signals
  (is (equal '("parsing-error")
             (glib-test:list-signals "GtkCssProvider")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkCssProvider" GTK:CSS-PROVIDER
                      (:SUPERCLASS GOBJECT:OBJECT
                       :EXPORT T
                       :INTERFACES ("GtkStyleProvider")
                       :TYPE-INITIALIZER "gtk_css_provider_get_type")
                      NIL)
             (gobject:get-gtype-definition "GtkCssProvider"))))

;;; --- Signals ----------------------------------------------------------------

;;;     parsing-error

;;; --- Functions --------------------------------------------------------------

;; Taken from the CSS Accordion example
(defparameter +css-button+
".accordion button {
  background-color: rgb(187,187,187);
  border-bottom-color: rgb(51,51,51);
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-left-color: rgb(51,51,51);
  border-left-style: solid;
  border-left-width: 2px;
  border-right-color: rgb(51,51,51);
  border-right-style: solid;
  border-right-width: 0;
  border-top-color: rgb(51,51,51);
  border-top-style: solid;
  border-top-width: 2px;
  color: rgb(0,0,0);
  padding-bottom: 12px;
  padding-left: 4px;
  padding-right: 4px;
  padding-top: 12px;
}

.accordion button:first-child {
  border-bottom-left-radius: 5px;
  border-bottom-right-radius: 0;
  border-top-left-radius: 5px;
  border-top-right-radius: 0;
}

.accordion button:last-child {
  border-bottom-left-radius: 0;
  border-bottom-right-radius: 5px;
  border-bottom-width: 2px;
  border-left-width: 2px;
  border-right-width: 2px;
  border-top-left-radius: 0;
  border-top-right-radius: 5px;
  border-top-width: 2px;
}

.accordion button:hover {
  background-color: rgb(72,112,188);
  padding-bottom: 12px;
  padding-left: 48px;
  padding-right: 48px;
  padding-top: 12px;
}

.accordion button *:hover {
  color: rgb(255,255,255);
}

.accordion button:active {
  background-color: rgb(153,52,1);
}

.accordion button:hover:active {
  background-color: rgb(153,52,1);
}
")

;;;     gtk_css_provider_new

(test gtk-css-provider-new
  (glib-test:with-check-memory (provider)
    (is (typep (setf provider (gtk:css-provider-new)) 'gtk:css-provider))))

;;;     gtk_css_provider_load_named

(test gtk-css-provider-load-named.1
  (glib-test:with-check-memory (provider)
    (setf provider (gtk:css-provider-new))
    (is-false (gtk:css-provider-load-named provider "Yaru"))
    (is (= 415016 (length (gtk:css-provider-to-string provider))))))

;; FIXME: The name "dark" causes a warning:
;;   Gtk-WARNING: Theme parser error: <data>:9:31-32: Expected a valid selector
;; Find a working example!?

(test gtk-css-provider-load-named.2
  (glib-test:with-check-memory (provider)
    (setf provider (gtk:css-provider-new))
    (is-false (gtk:css-provider-load-named provider "Yaru" "dark"))
    (is (= 410139 (length (gtk:css-provider-to-string provider))))))

;;;     gtk_css_provider_load_from_data

(test gtk-css-provider-load-from-data
  (glib-test:with-check-memory (provider)
    (let ((*gtk-warn-deprecated* nil))
        (setf provider (gtk:css-provider-new))
        (is-false (gtk:css-provider-load-from-data provider +css-button+))
        (is (= 1314 (length (gtk:css-provider-to-string provider)))))))

;;;     gtk_css_provider_load_from_file

(test gtk-css-provider-load-from-file
  (glib-test:with-check-memory (provider)
    (let* ((path (glib-sys:sys-path "test/resource/css-accordion.css"))
           (file (g:file-new-for-path path)))
      (setf provider (gtk:css-provider-new))
      (is-false (gtk:css-provider-load-from-file provider file))
      (is (= 2716 (length (gtk:css-provider-to-string provider)))))))

;;;     gtk_css_provider_load_from_path

(test gtk-css-provider-load-from-path
  (glib-test:with-check-memory (provider)
    (let ((path (glib-sys:sys-path "test/resource/css-accordion.css")))
      (setf provider (gtk:css-provider-new))
      (is-false (gtk:css-provider-load-from-path provider path))
      (is (= 2716 (length (gtk:css-provider-to-string provider)))))))

;;;     gtk_css_provider_load_from_resource

#-windows
(test gtk-css-provider-load-from-resource
  (glib-test:with-check-memory (provider)
    (gio:with-resource (resource (glib-sys:sys-path
                                     "test/resource/rtest-resource.gresource"))
      (let ((path "/com/crategus/test/css-accordion.css"))
        (setf provider (gtk:css-provider-new))
        (is-false (gtk:css-provider-load-from-resource provider path))
        (is (= 2716 (length (gtk:css-provider-to-string provider))))))))

;;;     gtk_css_provider_load_from_bytes

;; FIXME: We get a warning for this example. But loading seems to work.
;; Gtk-WARNING **: Theme parser error: <data>:33:1-2: Expected a valid selector
;; What is the problem in this example?

#+nil
(test gtk-css-provider-load-from-bytes
  (glib-test:with-check-memory (provider)
    (multiple-value-bind (data len)
        (cffi:foreign-string-alloc +css-button+)
      (let ((bytes (g:bytes-new data len)))
        (setf provider (gtk:css-provider-new))
        (is-false (gtk:css-provider-load-from-bytes provider bytes))
        (is (= 1314 (length (gtk:css-provider-to-string provider))))))))

;;;     gtk_css_provider_load_from_string

(test gtk-css-provider-load-from-string
  (let ((provider (gtk:css-provider-new)))
    (is-false (gtk:css-provider-load-from-string provider +css-button+))
    (is (= 1314 (length (gtk:css-provider-to-string provider))))))

;;;     gtk_css_provider_to_string

(test gtk-css-provider-to-string
  (let ((provider (gtk:css-provider-new)))
    (is (string= "" (gtk:css-provider-to-string provider)))
    (is-false (gtk:css-provider-load-from-string provider +css-button+))
    (is (string=
".accordion button {
  background-color: rgb(187,187,187);
  border-bottom-color: rgb(51,51,51);
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-left-color: rgb(51,51,51);
  border-left-style: solid;
  border-left-width: 2px;
  border-right-color: rgb(51,51,51);
  border-right-style: solid;
  border-right-width: 0;
  border-top-color: rgb(51,51,51);
  border-top-style: solid;
  border-top-width: 2px;
  color: rgb(0,0,0);
  padding-bottom: 12px;
  padding-left: 4px;
  padding-right: 4px;
  padding-top: 12px;
}

.accordion button:first-child {
  border-bottom-left-radius: 5px;
  border-bottom-right-radius: 0;
  border-top-left-radius: 5px;
  border-top-right-radius: 0;
}

.accordion button:last-child {
  border-bottom-left-radius: 0;
  border-bottom-right-radius: 5px;
  border-bottom-width: 2px;
  border-left-width: 2px;
  border-right-width: 2px;
  border-top-left-radius: 0;
  border-top-right-radius: 5px;
  border-top-width: 2px;
}

.accordion button:hover {
  background-color: rgb(72,112,188);
  padding-bottom: 12px;
  padding-left: 48px;
  padding-right: 48px;
  padding-top: 12px;
}

.accordion button *:hover {
  color: rgb(255,255,255);
}

.accordion button:active {
  background-color: rgb(153,52,1);
}

.accordion button:hover:active {
  background-color: rgb(153,52,1);
}
"
                 (gtk:css-provider-to-string provider)))))

;;; 2025-1-12

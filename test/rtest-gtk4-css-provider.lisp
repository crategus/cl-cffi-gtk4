(in-package :gtk-test)

(def-suite gtk-css-provider :in gtk-suite)
(in-suite gtk-css-provider)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkCssParserError                                  not implemented
;;;     GtkCssParserWarning                                not implemented
;;;     GTK_CSS_PARSER_ERROR                               not implemented

;;;     GtkCssLocation
;;;     GtkCssSection

;;;     GtkCssProvider

(test gtk-css-provider-class
  ;; Type check
  (is (g:type-is-object "GtkCssProvider"))
  ;; Check the registered name
  (is (eq 'gtk:css-provider
          (glib:symbol-for-gtype "GtkCssProvider")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkCssProvider")
          (g:gtype (cffi:foreign-funcall "gtk_css_provider_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkCssProvider")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkCssProvider")))
  ;; Check the interfaces
  (is (equal '("GtkStyleProvider")
             (list-interfaces "GtkCssProvider")))
  ;; Check the properties
  (is (equal '()
             (list-properties "GtkCssProvider")))
  ;; Check the signals
  (is (equal '("parsing-error")
             (list-signals "GtkCssProvider")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkCssProvider" GTK-CSS-PROVIDER
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES
                        ("GtkStyleProvider") :TYPE-INITIALIZER
                        "gtk_css_provider_get_type")
                       NIL)
             (gobject:get-g-type-definition "GtkCssProvider"))))

;;; --- Signals ----------------------------------------------------------------

;;;     parsing-error

;;; --- Functions --------------------------------------------------------------

;; Taken from the CSS Accordion example
(defparameter +css-button+
".accordion button {
    color: black;
    background-color: #bbb;
    border-style: solid;
    border-width: 2px 0 2px 2px;
    border-color: #333;

    padding: 12px 4px;
}

.accordion button:first-child {
    border-radius: 5px 0 0 5px;
}

.accordion button:last-child {
    border-radius: 0 5px 5px 0;
    border-width: 2px;
}

.accordion button:hover {
    padding: 12px 48px;
    background-color: #4870bc;
}

.accordion button *:hover {
    color: white;
}

.accordion button:hover:active,
.accordion button:active {
    background-color: #993401;
}
")

;;;     gtk_css_provider_new

(test gtk-css-provider-new
  (is (typep (gtk:css-provider-new) 'gtk:css-provider)))

;;;     gtk_css_provider_load_named

#+nil
(test gtk-css-provider-load-named.1
  (let ((provider (gtk:css-provider-new)))
    (is-false (gtk:css-provider-load-named provider "Yaru"))
    (is (= 414375 (length (gtk:css-provider-to-string provider))))))

;; FIXME: The name "dark" causes a warning:
;;   Gtk-WARNING: Theme parser error: <data>:9:31-32: Expected a valid selector
;; Find a working example!?
#+nil
(test gtk-css-provider-load-named.2
  (let ((provider (gtk:css-provider-new)))
    (is-false (gtk:css-provider-load-named provider "Yaru" "dark"))
    (is (= 409387 (length (gtk:css-provider-to-string provider))))))

;;;     gtk_css_provider_load_from_data

(test gtk-css-provider-load-from-data
  (let ((*gtk-warn-deprecated* nil))
    (let ((provider (gtk:css-provider-new)))
      (is-false (gtk:css-provider-load-from-data provider +css-button+))
      (is (= 1314 (length (gtk:css-provider-to-string provider)))))))

;;;     gtk_css_provider_load_from_file

(test gtk-css-provider-load-from-file
  (let* ((provider (gtk:css-provider-new))
         (path (sys-path "resource/css-accordion.css"))
         (file (g:file-new-for-path path)))
    (is-false (gtk:css-provider-load-from-file provider file))
    (is (= 2716 (length (gtk:css-provider-to-string provider))))))

;;;     gtk_css_provider_load_from_path

(test gtk-css-provider-load-from-path
  (let ((path (sys-path "resource/css-accordion.css"))
        (provider (gtk:css-provider-new)))
    (is-false (gtk:css-provider-load-from-path provider path))
    (is (= 2716 (length (gtk:css-provider-to-string provider))))))

;;;     gtk_css_provider_load_from_resource

#-windows
(test gtk-css-provider-load-from-resource
  (gio:with-g-resources (resource (sys-path "resource/rtest-resource.gresource"))
    (let ((provider (gtk:css-provider-new))
          (path "/com/crategus/test/css-accordion.css"))
      (is-false (gtk:css-provider-load-from-resource provider path))
      (is (= 2716 (length (gtk:css-provider-to-string provider)))))))

;;;     gtk_css_provider_load_from_bytes

;; FIXME: We get a warning for this example. But loading seems to work.
;; Gtk-WARNING **: Theme parser error: <data>:33:1-2: Expected a valid selector
;; What is the problem in this example?

#+nil
(test gtk-css-provider-load-from-bytes
  (multiple-value-bind (data len)
      (cffi:foreign-string-alloc +css-button+)
    (let ((bytes (g:bytes-new data len))
          (provider (gtk:css-provider-new)))
      (is-false (gtk:css-provider-load-from-bytes provider bytes))
      (is (= 1314 (length (gtk:css-provider-to-string provider)))))))

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

;;;     gtk_css_section_new
;;;     gtk_css_section_ref                                not implemented
;;;     gtk_css_section_unref                              not implemented
;;;     gtk_css_section_print                              not implemented
;;;     gtk_css_section_to_string
;;;     gtk_css_section_get_file
;;;     gtk_css_section_get_parent
;;;     gtk_css_section_get_start_location
;;;     gtk_css_section_get_end_location

;;; 2023-12-16

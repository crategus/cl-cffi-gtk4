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

(test css-provider-class
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

(defparameter +css-button+
"button {
   padding: 3px; }
 button > label {
   color: black;
   background-color: yellow; }
 button:first-child > label {
   background-color: red; }
 button:last-child > label {
   background-color : green; }")

;;;     gtk_css_provider_load_named

#+crategus
(test gtk-css-provider-load-named.1
  (let ((provider (gtk:css-provider-new)))
    (is-false (gtk:css-provider-load-named provider "Yaru"))
    (is (= 405137 (length (gtk:css-provider-to-string provider))))))

#+crategus
(test gtk-css-provider-load-named.2
  (let ((provider (gtk:css-provider-new)))
    (is-false (gtk:css-provider-load-named provider "Yaru" "dark"))
    (is (= 400248 (length (gtk:css-provider-to-string provider))))))

;;;     gtk_css_provider_load_from_data

(test css-provider-load-from-data
  (let ((provider (gtk:css-provider-new)))
    (is-false (gtk:css-provider-load-from-data provider +css-button+))
    (is (= 305 (length (gtk:css-provider-to-string provider))))))

;;;     gtk_css_provider_load_from_file

(test css-provider-load-from-file
  (let* ((provider (gtk:css-provider-new))
         (path (sys-path "resource/css-accordion.css"))
         (file (g:file-new-for-path path)))
    (is-false (gtk:css-provider-load-from-file provider file))
    (is (= 2716 (length (gtk:css-provider-to-string provider))))))

#+nil
(test css-provider-load-from-file
  (let* ((provider (gtk:css-provider-new))
         (path (sys-path "resource/css-accordion.css"))
         (file path))
    (is-false (gtk:css-provider-load-from-file provider file))
    (is (= 2716 (length (gtk:css-provider-to-string provider))))))


;;;     gtk_css_provider_load_from_path

(test css-provider-load-from-path
  (let ((path (sys-path "resource/css-accordion.css"))
        (provider (gtk:css-provider-new)))
    (is-false (gtk:css-provider-load-from-path provider path))
    (is (= 2716 (length (gtk:css-provider-to-string provider))))))

;;;     gtk_css_provider_load_from_resource

#-windows
(test css-provider-load-from-resource
  (gio:with-g-resources (resource (sys-path "resource/rtest-resource.gresource"))
    (let ((provider (gtk:css-provider-new))
          (path "/com/crategus/test/css-accordion.css"))
      (is-false (gtk:css-provider-load-from-resource provider path))
      (is (= 2716 (length (gtk:css-provider-to-string provider)))))))

;;;     gtk_css_provider_new

(test css-provider-new
  (is (typep (gtk:css-provider-new) 'gtk:css-provider)))

;;;     gtk_css_provider_to_string

(test gtk:css-provider-to-string
  (let ((provider (gtk:css-provider-new)))
    (is (string= "" (gtk:css-provider-to-string provider)))
    (is-false (gtk:css-provider-load-from-data provider +css-button+))
    (is (string=
"button {
  padding-bottom: 3px;
  padding-left: 3px;
  padding-right: 3px;
  padding-top: 3px;
}

button > label {
  background-color: rgb(255,255,0);
  color: rgb(0,0,0);
}

button:first-child > label {
  background-color: rgb(255,0,0);
}

button:last-child > label {
  background-color: rgb(0,128,0);
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

;;; --- 2023-5-29 --------------------------------------------------------------

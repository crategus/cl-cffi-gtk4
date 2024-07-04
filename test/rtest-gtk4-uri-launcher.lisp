(in-package :gtk-test)

(def-suite gtk-uri-launcher :in gtk-suite)
(in-suite gtk-uri-launcher)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkUriLauncher

(test gtk-uri-launcher-class
  ;; Check type
  (is (g:type-is-object "GtkUriLauncher"))
  ;; Check registered name
  (is (eq 'gtk:uri-launcher
          (glib:symbol-for-gtype "GtkUriLauncher")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkUriLauncher")
          (g:gtype (cffi:foreign-funcall "gtk_uri_launcher_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkUriLauncher")))
  ;; Check children
  (is (equal '()
             (gtk-test:list-children "GtkUriLauncher")))
  ;; Check interfaces
  (is (equal '()
             (gtk-test:list-interfaces "GtkUriLauncher")))
  ;; Check properties
  (is (equal '("uri")
             (gtk-test:list-properties "GtkUriLauncher")))
  ;; Check signals
  (is (equal '()
             (gtk-test:list-signals "GtkUriLauncher")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkUriLauncher" GTK-URI-LAUNCHER
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                                :TYPE-INITIALIZER "gtk_uri_launcher_get_type")
                               ((URI GTK-URI-LAUNCHER-URI "uri" "gchararray" T
                                 T)))
             (gobject:get-g-type-definition "GtkUriLauncher"))))

;;; --- Properties -------------------------------------------------------------

;;;     uri

;;; --- Functions --------------------------------------------------------------

;;;     gtk_uri_launcher_new
;;;     gtk_uri_launcher_launch
;;;     gtk_uri_launcher_launch_finish

;;; 2024-7-4

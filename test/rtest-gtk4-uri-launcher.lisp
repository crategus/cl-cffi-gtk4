(in-package :gtk-test)

(def-suite gtk-uri-launcher :in gtk-selector-widgets)
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
             (glib-test:list-children "GtkUriLauncher")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkUriLauncher")))
  ;; Check properties
  (is (equal '("uri")
             (glib-test:list-properties "GtkUriLauncher")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkUriLauncher")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkUriLauncher" GTK:URI-LAUNCHER
                      (:SUPERCLASS G:OBJECT
                       :EXPORT T
                       :INTERFACES NIL
                       :TYPE-INITIALIZER "gtk_uri_launcher_get_type")
                      ((URI URI-LAUNCHER-URI "uri" "gchararray" T T)))
             (gobject:get-gtype-definition "GtkUriLauncher"))))

;;; --- Properties -------------------------------------------------------------

;;;     uri

;;; --- Functions --------------------------------------------------------------

;;;     gtk_uri_launcher_new
;;;     gtk_uri_launcher_launch
;;;     gtk_uri_launcher_launch_finish

;;; 2024-9-20

(in-package :gtk-test)

(def-suite gtk-uri-launcher :in gtk-suite)
(in-suite gtk-uri-launcher)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkUriLauncher

(test gtk-uri-launcher-class
  ;; Type check
  (is (g:type-is-object "GtkUriLauncher"))
  ;; Check the registered name
  (is (eq 'gtk:uri-launcher
          (glib:symbol-for-gtype "GtkUriLauncher")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkUriLauncher")
          (g:gtype (cffi:foreign-funcall "gtk_uri_launcher_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkUriLauncher")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkUriLauncher")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkUriLauncher")))
  ;; Check the properties
  (is (equal '("uri")
             (list-properties "GtkUriLauncher")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkUriLauncher")))
  ;; Check the class definition
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

;;; --- 2023-9-2 ---------------------------------------------------------------

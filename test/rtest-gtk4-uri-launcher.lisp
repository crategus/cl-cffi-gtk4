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

(test gtk-uri-launcher-properties
  (glib-test:with-check-memory (launcher)
    (setf launcher (gtk:uri-launcher-new "https://docs.gtk.org//gtk4/"))
    (is (string= "https://docs.gtk.org//gtk4/"
                 (gtk:uri-launcher-uri launcher)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_uri_launcher_new

(test gtk-uri-launcher-new.1
  (glib-test:with-check-memory (launcher)
    (is (typep (setf launcher
                     (gtk:uri-launcher-new "https://docs.gtk.org//gtk4/"))
               'gtk:uri-launcher))
    (is (string= "https://docs.gtk.org//gtk4/"
                 (gtk:uri-launcher-uri launcher)))))

(test gtk-uri-launcher-new.2
  (glib-test:with-check-memory (launcher)
    (is (typep (setf launcher
                     (gtk:uri-launcher-new nil))
               'gtk:uri-launcher))
    (is-false (gtk:uri-launcher-uri launcher))))

;;;     gtk_uri_launcher_can_launch

(test gtk-uri-launcher-can-launch.1
  (glib-test:with-check-memory (launcher)
    (is (typep (setf launcher
                     (gtk:uri-launcher-new "https://docs.gtk.org//gtk4/"))
               'gtk:uri-launcher))
    (is-true (gtk:uri-launcher-can-launch launcher nil))))

(test gtk-uri-launcher-can-launch.2
  (glib-test:with-check-memory (launcher)
    (is (typep (setf launcher
                     (gtk:uri-launcher-new nil))
               'gtk:uri-launcher))
    (is-false (gtk:uri-launcher-can-launch launcher nil))))

;;;     gtk_uri_launcher_launch
;;;     gtk_uri_launcher_launch_finish

;;; 2026-01-03

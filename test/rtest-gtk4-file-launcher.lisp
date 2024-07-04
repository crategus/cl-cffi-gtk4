(in-package :gtk-test)

(def-suite gtk-file-launcher :in gtk-suite)
(in-suite gtk-file-launcher)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFileLauncher

(test gtk-file-launcher-class
  ;; Check type
  (is (g:type-is-object "GtkFileLauncher"))
  ;; Check registered name
  (is (eq 'gtk:file-launcher
          (glib:symbol-for-gtype "GtkFileLauncher")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkFileLauncher")
          (g:gtype (cffi:foreign-funcall "gtk_file_launcher_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkFileLauncher")))
  ;; Check children
  (is (equal '()
             (gtk-test:list-children "GtkFileLauncher")))
  ;; Check interfaces
  (is (equal '()
             (gtk-test:list-interfaces "GtkFileLauncher")))
  ;; Check properties
  (is (equal '("always-ask" "file" "writable")
             (gtk-test:list-properties "GtkFileLauncher")))
  ;; Check signals
  (is (equal '()
             (gtk-test:list-signals "GtkFileLauncher")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkFileLauncher" GTK-FILE-LAUNCHER
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                                :TYPE-INITIALIZER "gtk_file_launcher_get_type")
                               ((ALWAYS-ASK GTK-FILE-LAUNCHER-ALWAYS-ASK
                                 "always-ask" "gboolean" T T)
                                (FILE GTK-FILE-LAUNCHER-FILE "file" "GFile" T
                                 T)
                                (WRITABLE GTK-FILE-LAUNCHER-WRITABLE "writable"
                                 "gboolean" T T)))
             (gobject:get-g-type-definition "GtkFileLauncher"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-file-launcher-properties
  (let ((launcher (make-instance 'gtk:file-launcher)))
    (is-false (gtk:file-launcher-always-ask launcher))
    (is-false (gtk:file-launcher-file launcher))
    (is-false (gtk:file-launcher-writable launcher))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_file_launcher_new

;;;     gtk_file_launcher_launch
;;;     gtk_file_launcher_launch_finish
;;;     gtk_file_launcher_open_containing_folder
;;;     gtk_file_launcher_open_containing_folder_finish

;;; 2024-5-26

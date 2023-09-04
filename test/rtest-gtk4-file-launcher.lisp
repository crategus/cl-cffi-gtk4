(in-package :gtk-test)

(def-suite gtk-file-launcher :in gtk-suite)
(in-suite gtk-file-launcher)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFileLauncher

(test gtk-file-launcher-class
  ;; Type check
  (is (g:type-is-object "GtkFileLauncher"))
  ;; Check the registered name
  (is (eq 'gtk:file-launcher
          (glib:symbol-for-gtype "GtkFileLauncher")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkFileLauncher")
          (g:gtype (cffi:foreign-funcall "gtk_file_launcher_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkFileLauncher")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkFileLauncher")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkFileLauncher")))
  ;; Check the properties
  (is (equal '("file")
             (list-properties "GtkFileLauncher")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkFileLauncher")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkFileLauncher" GTK-FILE-LAUNCHER
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                                :TYPE-INITIALIZER "gtk_file_launcher_get_type")
                               ((FILE GTK-FILE-LAUNCHER-FILE "file" "GFile" T
                                 T)))
             (gobject:get-g-type-definition "GtkFileLauncher"))))

;;; --- Properties -------------------------------------------------------------

;;;     always-ask                                         Since 4.12
;;;     file

;;; --- Functions --------------------------------------------------------------

;;;     gtk_file_launcher_new

;;;     gtk_file_launcher_launch
;;;     gtk_file_launcher_launch_finish
;;;     gtk_file_launcher_open_containing_folder
;;;     gtk_file_launcher_open_containing_folder_finish

;;; --- 2023-9-2 ---------------------------------------------------------------

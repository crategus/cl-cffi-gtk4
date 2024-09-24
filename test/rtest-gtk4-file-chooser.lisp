(in-package :gtk-test)

(def-suite gtk-file-chooser :in gtk-suite)
(in-suite gtk-file-chooser)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFileChooserAction

(test gtk-file-chooser-action
  ;; Check type
  (is (g:type-is-enum "GtkFileChooserAction"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkFileChooserAction")
          (g:gtype (cffi:foreign-funcall "gtk_file_chooser_action_get_type"
                                         :size))))
  ;; Check registered name
  (is (eq 'gtk:file-chooser-action
          (glib:symbol-for-gtype "GtkFileChooserAction")))
  ;; Check names
  (is (equal '("GTK_FILE_CHOOSER_ACTION_OPEN" "GTK_FILE_CHOOSER_ACTION_SAVE"
               "GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER")
             (glib-test:list-enum-item-names "GtkFileChooserAction")))
  ;; Check values
  (is (equal '(0 1 2)
             (glib-test:list-enum-item-values "GtkFileChooserAction")))
  ;; Check nick names
  (is (equal '("open" "save" "select-folder")
             (glib-test:list-enum-item-nicks "GtkFileChooserAction")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkFileChooserAction"
                                    GTK:FILE-CHOOSER-ACTION
                       (:EXPORT T
                        :TYPE-INITIALIZER "gtk_file_chooser_action_get_type")
                       (:OPEN 0)
                       (:SAVE 1)
                       (:SELECT-FOLDER 2))
             (gobject:get-gtype-definition "GtkFileChooserAction"))))

;;;     GtkFileChooser

(test gtk-file-chooser-interface
  ;; Check type
  (is (g:type-is-interface "GtkFileChooser"))
  ;; Check registered name
  (is (eq 'gtk:file-chooser
          (glib:symbol-for-gtype "GtkFileChooser")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkFileChooser")
          (g:gtype (cffi:foreign-funcall "gtk_file_chooser_get_type" :size))))
  ;; Check interface prerequisites
  (is (equal '("GObject")
             (glib-test:list-interface-prerequisites "GtkFileChooser")))
  ;; Check interface properties
  (is (equal '("action" "create-folders" "filter" "filters" "select-multiple"
               "shortcut-folders")
             (glib-test:list-interface-properties "GtkFileChooser")))
  ;; Check interface signals
  (is (equal '()
             (glib-test:list-signals "GtkFileChooser")))
  ;; Get interface definition
  (is (equal '(GOBJECT:DEFINE-GINTERFACE "GtkFileChooser" GTK:FILE-CHOOSER
                       (:EXPORT T
                        :TYPE-INITIALIZER "gtk_file_chooser_get_type")
                       (ACTION FILE-CHOOSER-ACTION
                        "action" "GtkFileChooserAction" T T)
                       (CREATE-FOLDERS FILE-CHOOSER-CREATE-FOLDERS
                        "create-folders" "gboolean" T T)
                       (FILTER FILE-CHOOSER-FILTER "filter" "GtkFileFilter" T T)
                       (FILTERS FILE-CHOOSER-FILTERS
                        "filters" "GListModel" T NIL)
                       (SELECT-MULTIPLE FILE-CHOOSER-SELECT-MULTIPLE
                        "select-multiple" "gboolean" T T)
                       (SHORTCUT-FOLDERS FILE-CHOOSER-SHORTCUT-FOLDERS
                        "shortcut-folders" "GListModel" T NIL))
             (gobject:get-gtype-definition "GtkFileChooser"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-file-chooser-properties
  (let ((*gtk-warn-deprecated* nil))
    (let ((chooser (make-instance 'gtk:file-chooser-dialog)))
      (is (eq :open (gtk:file-chooser-action chooser)))
      (is-true (gtk:file-chooser-create-folders chooser))
      (is-false (gtk:file-chooser-filter chooser))
      (is (typep (gtk:file-chooser-filters chooser) 'g:list-model))
      (is-false (gtk:file-chooser-select-multiple chooser))
      (is (typep (gtk:file-chooser-shortcut-folders chooser) 'g:list-model)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_file_chooser_set_current_name
;;;     gtk_file_chooser_get_current_name

(test gtk-file-chooser-current-name
  (let ((*gtk-warn-deprecated* nil))
    (let ((chooser (make-instance 'gtk:file-chooser-dialog
                                  :action :save)))
      (is (string= "" (gtk:file-chooser-current-name chooser)))
      (is (string= "New name"
                   (setf (gtk:file-chooser-current-name chooser) "New name")))
      (is (string= "New name"
                   (gtk:file-chooser-current-name chooser))))))

;;;     gtk_file_chooser_get_file
;;;     gtk_file_chooser_set_file

(test gtk-file-chooser-file
  (let ((*gtk-warn-deprecated* nil))
    (let* ((filename (glib-sys:sys-path "test/rtest-gtk4-file-chooser.lisp"))
           (file (g:file-new-for-path filename))
           (chooser (make-instance 'gtk:file-chooser-widget
                                   :action :open)))
      (is (g:type-is-a (g:type-from-instance file) "GFile"))
      (is-false (gtk:file-chooser-file chooser))
      (is (eq file (setf (gtk:file-chooser-file chooser) file)))
      ;; TODO: FILE is not stored in the GtkFileChooserDialog object. Why?
      (is-false (gtk:file-chooser-file chooser)))))

(test gtk-file-chooser-namestring
  (let ((*gtk-warn-deprecated* nil))
    (let* ((path (glib-sys:sys-path "test/rtest-gtk4-file-chooser.lisp"))
           (chooser (make-instance 'gtk:file-chooser-widget
                                   :action :open)))
      (is-false (gtk:file-chooser-namestring chooser))
      (is (eq path
              (setf (gtk:file-chooser-namestring chooser) path)))
      ;; TODO: PATH is not stored in the GtkFileChooserDialog object. Why?
      (is-false (gtk:file-chooser-namestring chooser)))))

;;;     gtk_file_chooser_get_files

(test gtk-file-chooser-files
  (let ((*gtk-warn-deprecated* nil))
    (let ((chooser (make-instance 'gtk:file-chooser-widget)))
      (is (typep (gtk:file-chooser-files chooser) 'g:list-model)))))

;;;     gtk_file_chooser_set_current_folder
;;;     gtk_file_chooser_get_current_folder

(test gtk-file-chooser-current-folder
  (let ((*gtk-warn-deprecated* nil))
    (let ((path (glib-sys:sys-path "test/"))
          (chooser (make-instance 'gtk:file-chooser-widget)))
      (is-false (gtk:file-chooser-current-folder chooser))
      (is (eq path (setf (gtk:file-chooser-current-folder chooser) path)))
      (is-false (gtk:file-chooser-current-folder chooser)))))

;;;     gtk_file_chooser_add_filter
;;;     gtk_file_chooser_remove_filter
;;;     gtk_file_chooser_add_shortcut_folder
;;;     gtk_file_chooser_remove_shortcut_folder
;;;     gtk_file_chooser_add_choice
;;;     gtk_file_chooser_remove_choice
;;;     gtk_file_chooser_set_choice
;;;     gtk_file_chooser_get_choice

;;; 2024-9-20

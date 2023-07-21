(in-package :gtk-test)

(def-suite gtk-file-chooser :in gtk-suite)
(in-suite gtk-file-chooser)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFileChooserAction

(test file-chooser-action
  ;; Check the type
  (is (g:type-is-enum "GtkFileChooserAction"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkFileChooserAction")
          (g:gtype (cffi:foreign-funcall "gtk_file_chooser_action_get_type"
                                         :size))))
  ;; Check the registered name
  (is (eq 'gtk:file-chooser-action
          (glib:symbol-for-gtype "GtkFileChooserAction")))
  ;; Check the names
  (is (equal '("GTK_FILE_CHOOSER_ACTION_OPEN" "GTK_FILE_CHOOSER_ACTION_SAVE"
               "GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER")
             (list-enum-item-name "GtkFileChooserAction")))
  ;; Check the values
  (is (equal '(0 1 2)
             (list-enum-item-value "GtkFileChooserAction")))
  ;; Check the nick names
  (is (equal '("open" "save" "select-folder")
             (list-enum-item-nick "GtkFileChooserAction")))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkFileChooserAction"
                             GTK-FILE-CHOOSER-ACTION
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_file_chooser_action_get_type")
                             (:OPEN 0)
                             (:SAVE 1)
                             (:SELECT-FOLDER 2))
             (gobject:get-g-type-definition "GtkFileChooserAction"))))

;;;     GTK_FILE_CHOOSER_ERROR
;;;     GtkFileChooserError

;;;     GtkFileChooser

(test file-chooser-interface
  ;; Type check
  (is (g:type-is-interface "GtkFileChooser"))
  ;; Check the registered name
  (is (eq 'gtk:file-chooser
          (glib:symbol-for-gtype "GtkFileChooser")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkFileChooser")
          (g:gtype (cffi:foreign-funcall "gtk_file_chooser_get_type" :size))))
  ;; Get the names of the interface properties.
  (is (equal '("action" "create-folders" "filter" "filters" "select-multiple"
               "shortcut-folders")
             (list-interface-properties "GtkFileChooser")))
  ;; Get the interface definition
  (is (equal '(GOBJECT:DEFINE-G-INTERFACE "GtkFileChooser" GTK-FILE-CHOOSER
                                  (:EXPORT T
                                   :TYPE-INITIALIZER "gtk_file_chooser_get_type")
                                  (ACTION GTK-FILE-CHOOSER-ACTION
                                   "action" "GtkFileChooserAction" T T)
                                  (CREATE-FOLDERS GTK-FILE-CHOOSER-CREATE-FOLDERS
                                   "create-folders" "gboolean" T T)
                                  (FILTER GTK-FILE-CHOOSER-FILTER
                                   "filter" "GtkFileFilter" T T)
                                  (FILTERS GTK-FILE-CHOOSER-FILTERS
                                   "filters" "GListModel" T NIL)
                                  (SELECT-MULTIPLE
                                   GTK-FILE-CHOOSER-SELECT-MULTIPLE
                                   "select-multiple" "gboolean" T T)
                                  (SHORTCUT-FOLDERS
                                   GTK-FILE-CHOOSER-SHORTCUT-FOLDERS
                                   "shortcut-folders" "GListModel" T NIL))
             (gobject:get-g-type-definition "GtkFileChooser"))))

;;; --- Properties -------------------------------------------------------------

;;;     action
;;;     create-folders
;;;     filter
;;;     filters
;;;     select-multiple
;;;     shortcut-folders

(test file-chooser-properties

)

;;; Accessors
;;;
;;;     gtk_file_chooser_set_action
;;;     gtk_file_chooser_get_action
;;;     gtk_file_chooser_set_create_folders
;;;     gtk_file_chooser_get_create_folders
;;;     gtk_file_chooser_get_filter
;;;     gtk_file_chooser_get_filters
;;;     gtk_file_chooser_set_filter
;;;     gtk_file_chooser_set_select_multiple
;;;     gtk_file_chooser_get_select_multiple
;;;     gtk_file_chooser_get_shortcut_folders
;;;
;;; Functions
;;;
;;;     gtk_file_chooser_set_current_name
;;;     gtk_file_chooser_get_current_name
;;;     gtk_file_chooser_get_file
;;;     gtk_file_chooser_set_file
;;;     gtk_file_chooser_get_files
;;;     gtk_file_chooser_set_current_folder
;;;     gtk_file_chooser_get_current_folder
;;;     gtk_file_chooser_add_filter
;;;     gtk_file_chooser_remove_filter
;;;     gtk_file_chooser_add_shortcut_folder
;;;     gtk_file_chooser_remove_shortcut_folder
;;;     gtk_file_chooser_add_choice
;;;     gtk_file_chooser_remove_choice
;;;     gtk_file_chooser_set_choice
;;;     gtk_file_chooser_get_choice

;;; --- 2023-5-29 --------------------------------------------------------------

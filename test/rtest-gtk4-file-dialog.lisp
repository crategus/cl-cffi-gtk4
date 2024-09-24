(in-package :gtk-test)

(def-suite gtk-file-dialog :in gtk-suite)
(in-suite gtk-file-dialog)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFileDialog

(test gtk-file-dialog-class
  ;; Check type
  (is (g:type-is-object "GtkFileDialog"))
  ;; Check registered name
  (is (eq 'gtk:file-dialog
          (glib:symbol-for-gtype "GtkFileDialog")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkFileDialog")
          (g:gtype (cffi:foreign-funcall "gtk_file_dialog_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkFileDialog")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkFileDialog")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkFileDialog")))
  ;; Check properties
  (is (equal '("accept-label" "default-filter" "filters" "initial-file"
               "initial-folder" "initial-name" "modal" "title")
             (glib-test:list-properties "GtkFileDialog")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkFileDialog")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkFileDialog" GTK:FILE-DIALOG
                       (:SUPERCLASS GOBJECT:OBJECT
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_file_dialog_get_type")
                       ((ACCEPT-LABEL FILE-DIALOG-ACCEPT-LABEL
                         "accept-label" "gchararray" T T)
                        (DEFAULT-FILTER FILE-DIALOG-DEFAULT-FILTER
                         "default-filter" "GtkFileFilter" T T)
                        (FILTERS FILE-DIALOG-FILTERS "filters" "GListModel" T T)
                        (INITIAL-FILE FILE-DIALOG-INITIAL-FILE
                         "initial-file" "GFile" T T)
                        (INITIAL-FOLDER FILE-DIALOG-INITIAL-FOLDER
                         "initial-folder" "GFile" T T)
                        (INITIAL-NAME FILE-DIALOG-INITIAL-NAME
                         "initial-name" "gchararray" T T)
                        (MODAL FILE-DIALOG-MODAL "modal" "gboolean" T T)
                        (TITLE FILE-DIALOG-TITLE "title" "gchararray" T T)))
             (gobject:get-gtype-definition "GtkFileDialog"))))

;;; --- Properties -------------------------------------------------------------

;;;     accept-label
;;;     default-filter
;;;     filters
;;;     initial-file
;;;     initial-folder
;;;     initial-name
;;;     modal
;;;     title

(test gtk-file-dialog-properties.1
  (let ((dialog (make-instance 'gtk:file-dialog)))
    (is-false (gtk:file-dialog-accept-label dialog))
    (is-false (gtk:file-dialog-default-filter dialog))
    (is-false (gtk:file-dialog-filters dialog))
    (is-false (gtk:file-dialog-initial-file dialog))
    (is-false (gtk:file-dialog-initial-folder dialog))
    (is-false (gtk:file-dialog-initial-name dialog))
    (is-true (gtk:file-dialog-modal dialog))
    (is-false (gtk:file-dialog-title dialog))))

(test gtk-file-dialog-properties.2
  (let ((dialog (make-instance 'gtk:file-dialog))
        (filters (g:list-store-new "GtkFileFilter"))
        (filename (glib-sys:sys-path "test/rtest-gtk4-file-dialog.lisp")))
    ;; Append two file filter to the list store
    (g:list-store-append filters
                         (make-instance 'gtk:file-filter :name "filter1"))
    (g:list-store-append filters
                         (make-instance 'gtk:file-filter :name "filter2"))
    ;; accept-label
    (is (string= "label" (setf (gtk:file-dialog-accept-label dialog) "label")))
    (is (string= "label" (gtk:file-dialog-accept-label dialog)))
    ;; default-filter
    (is (typep (setf (gtk:file-dialog-default-filter dialog)
                      (make-instance 'gtk:file-filter)) 'gtk:file-filter))
    (is (typep (gtk:file-dialog-default-filter dialog) 'gtk:file-filter))
    ;; filters
    (is (typep (setf (gtk:file-dialog-filters dialog) filters) 'g:list-store))
    (is (typep (gtk:file-dialog-filters dialog) 'g:list-store))
    ;; initial-file
    (is (typep (setf (gtk:file-dialog-initial-file dialog)
                     (g:file-new-for-path filename)) 'g:object))
    ;; FIXME: Should return the g:file object. What is wrong?!
    (is-false (gtk:file-dialog-initial-file dialog))
    (is (string= "rtest-gtk4-file-dialog.lisp"
                 (gtk:file-dialog-initial-name dialog)))
    ;; initial-folder
    (is (typep (setf (gtk:file-dialog-initial-folder dialog)
                     (g:file-new-for-path (glib-sys:sys-path "test/")))
               'g:object))
    (is (typep (gtk:file-dialog-initial-folder dialog) 'g:object))
    ;; initial-name
    (is (string= "file" (setf (gtk:file-dialog-initial-name dialog) "file")))
    (is (string= "file" (gtk:file-dialog-initial-name dialog)))
    ;; modal
    (is-false (setf (gtk:file-dialog-modal dialog) nil))
    (is-false (gtk:file-dialog-modal dialog))
    ;; title
    (is (string= "title" (setf (gtk:file-dialog-title dialog) "title")))
    (is (string= "title" (gtk:file-dialog-title dialog)))))

;;; --- Functions --------------------------------------------------------------

;;;    gtk_file_dialog_new

(test gtk-file-dialog-new
  (is (typep (gtk:file-dialog-new) 'gtk:file-dialog)))

;;;     gtk_file_dialog_open
;;;     gtk_file_dialog_open_finish
;;;     gtk_file_dialog_open_multiple
;;;     gtk_file_dialog_open_multiple_finish
;;;     gtk_file_dialog_save
;;;     gtk_file_dialog_save_finish
;;;     gtk_file_dialog_select_folder
;;;     gtk_file_dialog_select_folder_finish
;;;     gtk_file_dialog_select_multiple_folders
;;;     gtk_file_dialog_select_multiple_folders_finish

;;; 2024-9-20

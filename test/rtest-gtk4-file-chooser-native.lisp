(in-package :gtk-test)

(def-suite gtk-file-chooser-native :in gtk-suite)
(in-suite gtk-file-chooser-native)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFileChooserNative

(test gtk-file-chooser-native-class
  ;; Check type
  (is (g:type-is-object "GtkFileChooserNative"))
  ;; Check registered name
  (is (eq 'gtk:file-chooser-native
          (glib:symbol-for-gtype "GtkFileChooserNative")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkFileChooserNative")
          (g:gtype (cffi:foreign-funcall "gtk_file_chooser_native_get_type"
                                         :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkNativeDialog")
          (g:type-parent "GtkFileChooserNative")))
  ;; Check children
  (is (equal '()
             (gtk-test:list-children "GtkFileChooserNative")))
  ;; Check interfaces
  (is (equal '("GtkFileChooser")
             (gtk-test:list-interfaces "GtkFileChooserNative")))
  ;; Check properties
  (is (equal '("accept-label" "action" "cancel-label" "create-folders" "filter"
               "filters" "select-multiple" "shortcut-folders")
             (gtk-test:list-properties "GtkFileChooserNative")))
  ;; Check signals
  (is (equal '()
             (gtk-test:list-signals "GtkFileChooserNative")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkFileChooserNative" GTK-FILE-CHOOSER-NATIVE
                       (:SUPERCLASS GTK-NATIVE-DIALOG :EXPORT T :INTERFACES
                        ("GtkFileChooser") :TYPE-INITIALIZER
                        "gtk_file_chooser_native_get_type")
                       ((ACCEPT-LABEL GTK-FILE-CHOOSER-NATIVE-ACCEPT-LABEL
                         "accept-label" "gchararray" T T)
                        (CANCEL-LABEL GTK-FILE-CHOOSER-NATIVE-CANCEL-LABEL
                         "cancel-label" "gchararray" T T)))
             (gobject:get-g-type-definition "GtkFileChooserNative"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-file-chooser-native-properties
  (let ((*gtk-warn-deprecated* nil))
    (let ((chooser (make-instance 'gtk:file-chooser-native)))
      (is-false (gtk:file-chooser-native-accept-label chooser))
      (is-false (gtk:file-chooser-native-cancel-label chooser)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_file_chooser_native_new

(test gtk-file-chooser-native-new
  (let ((*gtk-warn-deprecated* nil))
    (let* ((window (gtk:window-new))
           (dialog (gtk:file-chooser-native-new "title"
                                                window
                                                :open
                                                "Accept Label"
                                                "Cancel Label")))
    (is (typep dialog 'gtk:file-chooser-native))
    (is (string= "Accept Label" (gtk:file-chooser-native-accept-label dialog)))
    (is (string= "Cancel Label" (gtk:file-chooser-native-cancel-label dialog))))))

;;; 2024-4-26

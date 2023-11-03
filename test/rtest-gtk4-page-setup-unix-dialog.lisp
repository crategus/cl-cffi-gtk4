(in-package :gtk-test)

(def-suite gtk-page-setup-unix-dialog :in gtk-suite)
(in-suite gtk-page-setup-unix-dialog)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkPageSetupUnixDialog

(test gtk-page-setup-unix-dialog-class
 (let ((*gtk-warn-deprecated*))
    ;; Type check
    (is (g:type-is-object "GtkPageSetupUnixDialog"))
    ;; Check the registered name
    (is (eq 'gtk:page-setup-unix-dialog
            (glib:symbol-for-gtype "GtkPageSetupUnixDialog")))
    ;; Check the type initializer
    (is (eq (g:gtype "GtkPageSetupUnixDialog")
            (g:gtype (cffi:foreign-funcall "gtk_page_setup_unix_dialog_get_type"
                                           :size))))
    ;; Check the parent
    (is (eq (g:gtype "GtkDialog")
            (g:type-parent "GtkPageSetupUnixDialog")))
    ;; Check the children
    (is (equal '()
               (list-children "GtkPageSetupUnixDialog")))
    ;; Check the interfaces
    (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget" "GtkNative"
                 "GtkShortcutManager" "GtkRoot")
               (list-interfaces "GtkPageSetupUnixDialog")))
    ;; Check the properties
    (is (equal '()
               (list-properties "GtkPageSetupUnixDialog")))
    ;; Check the signals
    (is (equal '()
               (list-signals "GtkPageSetupUnixDialog")))
    ;; CSS name
    (is (string= "window"
                 (gtk:widget-class-css-name "GtkPageSetupUnixDialog")))
    ;; CSS classes
    (is (equal '("background" "csd" "dialog")
               (gtk:widget-css-classes
                   (make-instance 'gtk:page-setup-unix-dialog))))
    ;; Accessible role
    (is (eq :dialog (gtk:widget-class-accessible-role "GtkPageSetupUnixDialog")))
    ;; Check the class definition
    (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkPageSetupUnixDialog"
                                 GTK-PAGE-SETUP-UNIX-DIALOG
                                 (:SUPERCLASS GTK-DIALOG :EXPORT T :INTERFACES
                                  ("GtkAccessible" "GtkBuildable"
                                   "GtkConstraintTarget" "GtkNative" "GtkRoot"
                                   "GtkShortcutManager")
                                  :TYPE-INITIALIZER
                                  "gtk_page_setup_unix_dialog_get_type")
                                 NIL)
               (gobject:get-g-type-definition "GtkPageSetupUnixDialog")))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_page_setup_unix_dialog_new
;;;     gtk_page_setup_unix_dialog_set_page_setup
;;;     gtk_page_setup_unix_dialog_get_page_setup
;;;     gtk_page_setup_unix_dialog_set_print_settings
;;;     gtk_page_setup_unix_dialog_get_print_settings

;;; --- 2023-10-16 -------------------------------------------------------------

(in-package :gtk-test)

(def-suite gtk-page-setup-unix-dialog :in gtk-suite)
(in-suite gtk-page-setup-unix-dialog)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkPageSetupUnixDialog

(test gtk-page-setup-unix-dialog-class
 (let ((*gtk-warn-deprecated*))
    ;; Check type
    (is (g:type-is-object "GtkPageSetupUnixDialog"))
    ;; Check registered name
    (is (eq 'gtk:page-setup-unix-dialog
            (glib:symbol-for-gtype "GtkPageSetupUnixDialog")))
    ;; Check type initializer
    (is (eq (g:gtype "GtkPageSetupUnixDialog")
            (g:gtype (cffi:foreign-funcall "gtk_page_setup_unix_dialog_get_type"
                                           :size))))
    ;; Check parent
    (is (eq (g:gtype "GtkDialog")
            (g:type-parent "GtkPageSetupUnixDialog")))
    ;; Check children
    (is (equal '()
               (glib-test:list-children "GtkPageSetupUnixDialog")))
    ;; Check interfaces
    (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
                 "GtkNative" "GtkShortcutManager" "GtkRoot")
               (glib-test:list-interfaces "GtkPageSetupUnixDialog")))
    ;; Check properties
    (is (equal '()
               (glib-test:list-properties "GtkPageSetupUnixDialog")))
    ;; Check signals
    (is (equal '()
               (glib-test:list-signals "GtkPageSetupUnixDialog")))
    ;; Check CSS name
    (is (string= "window"
                 (gtk:widget-class-css-name "GtkPageSetupUnixDialog")))
    ;; Check accessible role
    (is (eq :dialog
            (gtk:widget-class-accessible-role "GtkPageSetupUnixDialog")))
    ;; Check class definition
    (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkPageSetupUnixDialog"
                                        GTK:PAGE-SETUP-UNIX-DIALOG
                         (:SUPERCLASS GTK:DIALOG
                          :EXPORT T
                          :INTERFACES
                          ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
                           "GtkNative" "GtkRoot" "GtkShortcutManager")
                          :TYPE-INITIALIZER
                          "gtk_page_setup_unix_dialog_get_type")
                         NIL)
               (gobject:get-gtype-definition "GtkPageSetupUnixDialog")))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_page_setup_unix_dialog_new
;;;     gtk_page_setup_unix_dialog_set_page_setup
;;;     gtk_page_setup_unix_dialog_get_page_setup
;;;     gtk_page_setup_unix_dialog_set_print_settings
;;;     gtk_page_setup_unix_dialog_get_print_settings

;;; 2024-9-20

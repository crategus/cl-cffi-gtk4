(in-package :gtk-test)

(def-suite gtk-print-unix-dialog :in gtk-suite)
(in-suite gtk-print-unix-dialog)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkPrintCapabilities

;;;     GtkPrintUnixDialog

(test gtk-print-unix-dialog-class
  (let ((*gtk-warn-deprecated* nil))
    ;; Type check
    (is (g:type-is-object "GtkPrintUnixDialog"))
    ;; Check the registered name
    (is (eq 'gtk:print-unix-dialog
            (glib:symbol-for-gtype "GtkPrintUnixDialog")))
    ;; Check the type initializer
    (is (eq (g:gtype "GtkPrintUnixDialog")
            (g:gtype (cffi:foreign-funcall "gtk_print_unix_dialog_get_type"
                                           :size))))
    ;; Check the parent
    (is (eq (g:gtype "GtkDialog")
            (g:type-parent "GtkPrintUnixDialog")))
    ;; Check the children
    (is (equal '()
               (list-children "GtkPrintUnixDialog")))
    ;; Check the interfaces
    (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
                 "GtkNative" "GtkShortcutManager" "GtkRoot")
               (list-interfaces "GtkPrintUnixDialog")))
    ;; Check the properties
    (is (equal '("current-page" "embed-page-setup" "has-selection"
                 "manual-capabilities" "page-setup" "print-settings"
                 "selected-printer" "support-selection")
               (list-properties "GtkPrintUnixDialog")))
    ;; Check the signals
    (is (equal '()
               (list-signals "GtkPrintUnixDialog")))
    ;; CSS name
    (is (string= "window"
                 (gtk:widget-class-css-name "GtkPrintUnixDialog")))
    ;; CSS classes
    ;; FIXME: Calling make-instance for GtkPrintUnixDialog causes errors or
    ;; warnings.
    #+nil
    (is (equal '("background" "csd" "dialog" "print")
               (gtk:widget-css-classes (make-instance 'gtk:print-unix-dialog))))
    ;; Accessible role
    (is (eq :dialog (gtk:widget-class-accessible-role "GtkPrintUnixDialog")))
    ;; Check the class definition
    (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkPrintUnixDialog"
                                               GTK-PRINT-UNIX-DIALOG
                         (:SUPERCLASS GTK-DIALOG :EXPORT T :INTERFACES
                          ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
                           "GtkNative" "GtkRoot" "GtkShortcutManager")
                          :TYPE-INITIALIZER "gtk_print_unix_dialog_get_type")
                         ((CURRENT-PAGE GTK-PRINT-UNIX-DIALOG-CURRENT-PAGE
                           "current-page" "gint" T T)
                          (EMBED-PAGE-SETUP
                           GTK-PRINT-UNIX-DIALOG-EMBED-PAGE-SETUP
                           "embed-page-setup" "gboolean" T T)
                          (HAS-SELECTION GTK-PRINT-UNIX-DIALOG-HAS-SELECTION
                           "has-selection" "gboolean" T T)
                          (MANUAL-CAPABILITIES
                           GTK-PRINT-UNIX-DIALOG-MANUAL-CAPABILITIES
                           "manual-capabilities" "GtkPrintCapabilities" T T)
                          (PAGE-SETUP GTK-PRINT-UNIX-DIALOG-PAGE-SETUP
                           "page-setup" "GtkPageSetup" T T)
                          (PRINT-SETTINGS GTK-PRINT-UNIX-DIALOG-PRINT-SETTINGS
                           "print-settings" "GtkPrintSettings" T T)
                          (SELECTED-PRINTER
                           GTK-PRINT-UNIX-DIALOG-SELECTED-PRINTER
                           "selected-printer" "GtkPrinter" T NIL)
                          (SUPPORT-SELECTION
                           GTK-PRINT-UNIX-DIALOG-SUPPORT-SELECTION
                           "support-selection" "gboolean" T T)))
               (gobject:get-g-type-definition "GtkPrintUnixDialog")))))

;;; --- Properties -------------------------------------------------------------

;;;     current-page
;;;     embed-page-setup
;;;     has-selection
;;;     manual-capabilities
;;;     page-setup
;;;     print-settings
;;;     selected-printer
;;;     support-selection

;;; --- Functions --------------------------------------------------------------

;;;     gtk_print_unix_dialog_new
;;;     gtk_print_unix_dialog_set_settings
;;;     gtk_print_unix_dialog_get_settings
;;;     gtk_print_unix_dialog_add_custom_tab
;;;     gtk_print_unix_dialog_get_page_setup_set

;;; --- 2023-11-1 --------------------------------------------------------------

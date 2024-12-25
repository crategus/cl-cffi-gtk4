(in-package :gtk-test)

(def-suite gtk-print-dialog :in gtk-printing)
(in-suite gtk-print-dialog)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkPrintDialog

(test gtk-print-dialog-class
  ;; Check type
  (is (g:type-is-object "GtkPrintDialog"))
  ;; Check registered name
  (is (eq 'gtk:print-dialog
          (glib:symbol-for-gtype "GtkPrintDialog")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkPrintDialog")
          (g:gtype (cffi:foreign-funcall "gtk_print_dialog_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkPrintDialog")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkPrintDialog")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkPrintDialog")))
  ;; Check properties
  (is (equal '("accept-label" "modal" "page-setup" "print-settings" "title")
             (glib-test:list-properties "GtkPrintDialog")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkPrintDialog")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkPrintDialog" GTK:PRINT-DIALOG
                       (:SUPERCLASS GOBJECT:OBJECT
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_print_dialog_get_type")
                       ((ACCEPT-LABEL PRINT-DIALOG-ACCEPT-LABEL "accept-label"
                         "gchararray" T T)
                        (MODAL PRINT-DIALOG-MODAL "modal" "gboolean" T T)
                        (PAGE-SETUP PRINT-DIALOG-PAGE-SETUP "page-setup"
                         "GtkPageSetup" T T)
                        (PRINT-SETTINGS PRINT-DIALOG-PRINT-SETTINGS
                         "print-settings" "GtkPrintSettings" T T)
                        (TITLE PRINT-DIALOG-TITLE "title" "gchararray" T T)))
             (gobject:get-gtype-definition "GtkPrintDialog"))))

;;; --- Properties -------------------------------------------------------------

;;;     accept-label
;;;     modal
;;;     page-setup
;;;     print-settings
;;;     title

(test gtk-print-dialog-properties
  (let ((dialog (make-instance 'gtk:print-dialog)))
    (is-false (gtk:print-dialog-accept-label dialog))
    (is-true (gtk:print-dialog-modal dialog))
    (is-false (gtk:print-dialog-page-setup dialog))
    (is-false (gtk:print-dialog-print-settings dialog))
    (is-false (gtk:print-dialog-title dialog))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_print_dialog_new

(test gtk-print-dialog-new
  (is (typep (gtk:print-dialog-new) 'gtk:print-dialog)))

;;;     gtk_print_dialog_print                              not implemented
;;;     gtk_print_dialog_print_file_finish                  not implemented

;;;     gtk_print_dialog_print_file
;;;     gtk_print_dialog_print_finish

;;;     gtk_print_dialog_setup
;;;     gtk_print_dialog_setup_finish

;;; 2024-11-10

(in-package :gtk-test)

(def-suite gtk-print-settings :in gtk-suite)
(in-suite gtk-print-settings)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkPrintBackend

(test gtk-print-backend-class
  ;; Check type
  (is (g:type-is-object "GtkPrintBackend"))
  ;; Check registered name
  (is (eq 'gtk:print-backend
          (glib:symbol-for-gtype "GtkPrintBackend")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkPrintBackend")
          (g:gtype (cffi:foreign-funcall "gtk_print_backend_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkPrintBackend")))
  ;; Check children
  (if *first-run-gtk-test*
      (is (equal '()
                 (list-children "GtkPrintBackend"))))
  ;; Check interfaces
  (is (equal '()
             (list-interfaces "GtkPrintBackend")))
  ;; Check properties
  (is (equal '("status")
             (list-properties "GtkPrintBackend")))
  ;; Check signals
  (is (equal '("printer-added" "printer-list-changed" "printer-list-done"
               "printer-removed" "printer-status-changed" "request-password")
             (list-signals "GtkPrintBackend")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkPrintBackend" GTK-PRINT-BACKEND
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                                :TYPE-INITIALIZER "gtk_print_backend_get_type")
                               ((STATUS GTK-PRINT-BACKEND-STATUS "status"
                                 "gint" T T)))
             (gobject:get-g-type-definition "GtkPrintBackend"))))

;;;     GtkPrinter

(test gtk-printer-class
  ;; Check type
  (is (g:type-is-object "GtkPrinter"))
  ;; Check registered name
  (is (eq 'gtk:printer
          (glib:symbol-for-gtype "GtkPrinter")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkPrinter")
          (g:gtype (cffi:foreign-funcall "gtk_printer_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkPrinter")))
  ;; Check children
  (if *first-run-gtk-test*
      (is (equal '()
                 (list-children "GtkPrinter"))))
  ;; Check interfaces
  (is (equal '()
             (list-interfaces "GtkPrinter")))
  ;; Check properties
  (is (equal '("accepting-jobs" "accepts-pdf" "accepts-ps" "backend"
               "icon-name" "is-virtual" "job-count" "location" "name" "paused"
               "state-message")
             (list-properties "GtkPrinter")))
  ;; Check signals
  (is (equal '("details-acquired")
             (list-signals "GtkPrinter")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkPrinter" GTK-PRINTER
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                                :TYPE-INITIALIZER "gtk_printer_get_type")
                               ((ACCEPTING-JOBS GTK-PRINTER-ACCEPTING-JOBS
                                 "accepting-jobs" "gboolean" T NIL)
                                (ACCEPTS-PDF GTK-PRINTER-ACCEPTS-PDF
                                 "accepts-pdf" "gboolean" T NIL)
                                (ACCEPTS-PS GTK-PRINTER-ACCEPTS-PS "accepts-ps"
                                 "gboolean" T NIL)
                                (BACKEND GTK-PRINTER-BACKEND "backend"
                                 "GtkPrintBackend" T NIL)
                                (ICON-NAME GTK-PRINTER-ICON-NAME "icon-name"
                                 "gchararray" T NIL)
                                (IS-VIRTUAL GTK-PRINTER-IS-VIRTUAL "is-virtual"
                                 "gboolean" T NIL)
                                (JOB-COUNT GTK-PRINTER-JOB-COUNT "job-count"
                                 "gint" T NIL)
                                (LOCATION GTK-PRINTER-LOCATION "location"
                                 "gchararray" T NIL)
                                (NAME GTK-PRINTER-NAME "name" "gchararray" T
                                 NIL)
                                (PAUSED GTK-PRINTER-PAUSED "paused" "gboolean"
                                 T NIL)
                                (STATE-MESSAGE GTK-PRINTER-STATE-MESSAGE
                                 "state-message" "gchararray" T NIL)))
             (gobject:get-g-type-definition "GtkPrinter"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-printer-properties
  (let ((printer (make-instance 'gtk:printer)))
    (is-true (gtk:printer-accepting-jobs printer))
    (is-false (gtk:printer-accepts-pdf printer))
    (is-true (gtk:printer-accepts-ps printer))
    (is-false (gtk:printer-backend printer))
    (is (string= "printer" (gtk:printer-icon-name printer)))
    (is-false (gtk:printer-is-virtual printer))
    (is (= 0 (gtk:printer-job-count printer)))
    (is (string= "" (gtk:printer-location printer)))
    (is (string= "" (gtk:printer-name printer)))
    (is-false (gtk:printer-paused printer))
    (is (string= "" (gtk:printer-state-message printer)))))

;;; --- Signals ----------------------------------------------------------------

;;;     details-acquired

;;; --- Functions --------------------------------------------------------------

;;;     gtk_printer_new
;;;     gtk_printer_get_description
;;;
;;;     gtk_printer_is_active
;;;     gtk_printer_is_paused
;;;     gtk_printer_is_accepting_jobs
;;;     gtk_printer_is_default
;;;
;;;     gtk_printer_list_papers
;;;     gtk_printer_compare
;;;     gtk_printer_has_details
;;;     gtk_printer_request_details
;;;     gtk_printer_get_capabilities
;;;     gtk_printer_get_default_page_size
;;;     gtk_printer_get_hard_margins
;;;     gtk_printer_get_hard_margins_for_paper_size
;;;
;;;     GtkPrinterFunc
;;;     gtk_enumerate_printers

;; TODO: This test can crash with a memory fault. Check this more carefully!?

#+nil
(test gtk-enumerate-printers
  (let (printers)
    (gtk:enumerate-printers (lambda (printer)
                              (push printer printers)
                              nil)
                            t)
    (is (every (lambda (x) (typep x 'gtk:printer)) printers))))

;;; 2024-4-27

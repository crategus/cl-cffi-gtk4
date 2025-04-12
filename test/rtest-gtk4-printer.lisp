(in-package :gtk-test)

(def-suite gtk-printer :in gtk-printing)
(in-suite gtk-printer)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkPrintBackend

(test gtk-print-backend-class
  ;; Perform these tests only if a printer is available
  (when (get-default-printer)
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
    (is (equal '("GtkPrintBackendCpdb" "GtkPrintBackendFile")
               (glib-test:list-children "GtkPrintBackend")))
    ;; Check interfaces
    (is (equal '()
               (glib-test:list-interfaces "GtkPrintBackend")))
    ;; Check properties
    (is (equal '("status")
               (glib-test:list-properties "GtkPrintBackend")))
    ;; Check signals
    (is (equal '("printer-added" "printer-list-changed" "printer-list-done"
                 "printer-removed" "printer-status-changed" "request-password")
               (glib-test:list-signals "GtkPrintBackend")))
    ;; Check class definition
    (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkPrintBackend" GTK:PRINT-BACKEND
                        (:SUPERCLASS G:OBJECT
                         :EXPORT T
                         :INTERFACES NIL
                         :TYPE-INITIALIZER "gtk_print_backend_get_type")
                        ((STATUS PRINT-BACKEND-STATUS "status" "gint" T T)))
               (gobject:get-gtype-definition "GtkPrintBackend")))))

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
  (if (get-default-printer)
      (is (equal '("GtkPrinterCpdb")
                 (glib-test:list-children "GtkPrinter")))
      (is (equal '()
                 (glib-test:list-children "GtkPrinter"))))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkPrinter")))
  ;; Check properties
  (is (equal '("accepting-jobs" "accepts-pdf" "accepts-ps" "backend"
               "icon-name" "is-virtual" "job-count" "location" "name" "paused"
               "state-message")
             (glib-test:list-properties "GtkPrinter")))
  ;; Check signals
  (is (equal '("details-acquired")
             (glib-test:list-signals "GtkPrinter")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkPrinter" GTK:PRINTER
                      (:SUPERCLASS G:OBJECT
                       :EXPORT T
                       :INTERFACES NIL
                       :TYPE-INITIALIZER "gtk_printer_get_type")
                      ((ACCEPTING-JOBS PRINTER-ACCEPTING-JOBS
                        "accepting-jobs" "gboolean" T NIL)
                       (ACCEPTS-PDF PRINTER-ACCEPTS-PDF
                        "accepts-pdf" "gboolean" T NIL)
                       (ACCEPTS-PS PRINTER-ACCEPTS-PS
                        "accepts-ps" "gboolean" T NIL)
                       (BACKEND PRINTER-BACKEND
                        "backend" "GtkPrintBackend" T NIL)
                       (ICON-NAME PRINTER-ICON-NAME
                        "icon-name" "gchararray" T NIL)
                       (IS-VIRTUAL PRINTER-IS-VIRTUAL
                        "is-virtual" "gboolean" T NIL)
                       (JOB-COUNT PRINTER-JOB-COUNT
                        "job-count" "gint" T NIL)
                       (LOCATION PRINTER-LOCATION
                        "location" "gchararray" T NIL)
                       (NAME PRINTER-NAME
                        "name" "gchararray" T NIL)
                       (PAUSED PRINTER-PAUSED
                        "paused" "gboolean" T NIL)
                       (STATE-MESSAGE PRINTER-STATE-MESSAGE
                        "state-message" "gchararray" T NIL)))
             (gobject:get-gtype-definition "GtkPrinter"))))

;;; --- Signals ----------------------------------------------------------------

;;;     details-acquired

(test gtk-printer-details-acquired-signal
  (let* ((name "details-acquired")
         (gtype (g:gtype "GtkPrinter"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    ;; Check parameter types
    (is (equal '("gboolean")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Properties -------------------------------------------------------------

;; Adds a strong reference for GtkBackend object

(test gtk-printer-properties
  (glib-test:with-check-memory (:strong 1)
    (let (printer backend)
      (when (setf printer (get-default-printer))
        (is-true (gtk:printer-accepting-jobs printer))
        (is-true (gtk:printer-accepts-pdf printer))
        (is-true (gtk:printer-accepts-ps printer))
        (is (typep (setf backend
                         (gtk:printer-backend printer)) 'gtk:print-backend))
        (is (string= "document-save" (gtk:printer-icon-name printer)))
        (is-true (gtk:printer-is-virtual printer))
        (is (= 0 (gtk:printer-job-count printer)))
        (is (string= "" (gtk:printer-location printer)))
        (is (string= "In Datei drucken" (gtk:printer-name printer)))
        (is-false (gtk:printer-paused printer))
        (is (string= "" (gtk:printer-state-message printer)))))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_printer_new

(test gtk-printer-new
  (glib-test:with-check-memory (:strong 1)
    (let (printer backend)
      (when (setf printer (get-default-printer))
        (setf backend (gtk:printer-backend printer))
        (is (typep (gtk:printer-new "printer" backend t) 'gtk:printer))))))

;;;     gtk_printer_get_description

(test gtk-printer-description
  (glib-test:with-check-memory ()
    (let (printer)
      (when (setf printer (get-default-printer))
        (is-false (gtk:printer-description printer))))))

;;;     gtk_printer_is_active

(test gtk-printer-is-active
  (glib-test:with-check-memory ()
    (let (printer)
      (when (setf printer (get-default-printer))
        (is-true (gtk:printer-is-active printer))))))

;;;     gtk_printer_is_paused

(test gtk-printer-is-paused
  (glib-test:with-check-memory ()
    (let (printer)
      (when (setf printer (get-default-printer))
        (is-false (gtk:printer-is-paused printer))))))

;;;     gtk_printer_is_accepting_jobs

(test gtk-printer-is-accepting-jobs
  (glib-test:with-check-memory ()
    (let (printer)
      (when (setf printer (get-default-printer))
        (is-true (gtk:printer-is-accepting-jobs printer))))))

;;;     gtk_printer_is_default

(test gtk-printer-is-default
  (glib-test:with-check-memory ()
    (let (printer)
      (when (setf printer (get-default-printer))
        (is-false (gtk:printer-is-default printer))))))

;;;     gtk_printer_list_papers

(test gtk-printer-list-papers
  (glib-test:with-check-memory ()
    (let (printer)
      (when (setf printer (get-default-printer))
        (is (every (lambda (x) (typep x 'gtk:page-setup))
                   (gtk:printer-list-papers printer)))))))

;;;     gtk_printer_compare

;;;     gtk_printer_has_details
;;;     gtk_printer_request_details

(test gtk-printer-has/request-details
  (glib-test:with-check-memory ()
    (let (printer)
      (when (setf printer (get-default-printer))
        (let (has-details msg)

          (g:signal-connect printer "details-acquired"
                  (lambda (printer success)
                    (setf msg (format nil "~a ~a"
                                      (gtk:printer-name printer)
                                      success))))

          (is-true (setf has-details
                         (gtk:printer-has-details printer)))
          (when has-details
            (is-false (gtk:printer-request-details printer))
            ;; TODO: The signal handler is not called. Why?!
            (is-false msg)))))))

;;;     gtk_printer_get_capabilities

(test gtk-printer-capabilities
  (glib-test:with-check-memory ()
    (let (printer)
      (when (setf printer (get-default-printer))
        (is-false (gtk:printer-capabilities printer))))))

;;;     gtk_printer_get_default_page_size

(test gtk-printer-default-page-size
  (glib-test:with-check-memory ()
    (let (printer)
      (when (setf printer (get-default-printer))
        (is-false (gtk:printer-default-page-size printer))))))

;;;     gtk_printer_get_hard_margins

(test gtk-printer-hard-margins
  (glib-test:with-check-memory ()
    (let (printer)
      (when (setf printer (get-default-printer))
        (is-false (gtk:printer-hard-margins printer))))))

;;;     gtk_printer_get_hard_margins_for_paper_size

(test gtk-printer-hard-margins-for-paper-size
  (glib-test:with-check-memory ()
    (let (printer)
      (when (setf printer (get-default-printer))
        (let ((size (gtk:paper-size-new (gtk:paper-size-default))))
          (is-false (gtk:printer-hard-margins-for-paper-size printer size)))))))

;;; 2025-4-12

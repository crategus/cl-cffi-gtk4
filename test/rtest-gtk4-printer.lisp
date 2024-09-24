(in-package :gtk-test)

(def-suite gtk-printer :in gtk-suite)
(in-suite gtk-printer)

;;;     GtkPrinterFunc
;;;     gtk_enumerate_printers

(defvar *default-printer* nil)

;; Get a default printer for more tests
(test gtk-enumerate-printers
  (when *first-run-gtk-test*
    (let (printers)
      (gtk:enumerate-printers
              (lambda (printer)
                (let ((name (gtk:printer-name printer)))
                  (push printer printers)
                  (format t "~& printer : ~a~%" name)
                  (when (string= name "In Datei drucken")
                    (gtk:printer-request-details printer)
                    (setf *default-printer* printer))
                   nil))
              t)
      (is (every (lambda (x) (typep x 'gtk:printer)) printers)))))

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
      (is (equal '("GtkPrintBackendCups" "GtkPrintBackendFile")
                 (glib-test:list-children "GtkPrintBackend"))))
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
             (gobject:get-gtype-definition "GtkPrintBackend"))))

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
      (is (equal '("GtkPrinterCups")
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

;;; --- Properties -------------------------------------------------------------

(test gtk-printer-properties
  (when *default-printer*
    (let ((printer *default-printer*))
      (is-true (gtk:printer-accepting-jobs printer))
      (is-true (gtk:printer-accepts-pdf printer))
      (is-true (gtk:printer-accepts-ps printer))
      (is (typep (gtk:printer-backend printer) 'gtk:print-backend))
      (is (string= "document-save" (gtk:printer-icon-name printer)))
      (is-true (gtk:printer-is-virtual printer))
      (is (= 0 (gtk:printer-job-count printer)))
      (is (string= "" (gtk:printer-location printer)))
      (is (string= "In Datei drucken" (gtk:printer-name printer)))
      (is-false (gtk:printer-paused printer))
      (is (string= "" (gtk:printer-state-message printer))))))

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

;;; --- Functions --------------------------------------------------------------

;;;     gtk_printer_new

(test gtk-printer-new
  (when *default-printer*
    (let ((backend (gtk:printer-backend *default-printer*)))
      (is (typep (gtk:printer-new "printer" backend t) 'gtk:printer))
      (is (typep (gtk:printer-new "printer" backend nil) 'gtk:printer)))))

;;;     gtk_printer_get_description

(test gtk-printer-description
  (when *default-printer*
    (is-false (gtk:printer-description *default-printer*))))

;;;     gtk_printer_is_active

(test gtk-printer-is-active
  (when *default-printer*
    (is-true (gtk:printer-is-active *default-printer*))))

;;;     gtk_printer_is_paused

(test gtk-printer-is-paused
  (when *default-printer*
    (is-false (gtk:printer-is-paused *default-printer*))))

;;;     gtk_printer_is_accepting_jobs

(test gtk-printer-is-accepting-jobs
  (when *default-printer*
    (is-true (gtk:printer-is-accepting-jobs *default-printer*))))

;;;     gtk_printer_is_default

(test gtk-printer-is-default
  (when *default-printer*
    (is-false (gtk:printer-is-default *default-printer*))))

;;;     gtk_printer_list_papers

(test gtk-printer-list-papers
  (when *default-printer*
    (is (every (lambda (x) (typep x 'gtk:page-setup))
               (gtk:printer-list-papers *default-printer*)))))

;;;     gtk_printer_compare

;;;     gtk_printer_has_details
;;;     gtk_printer_request_details

(test gtk-printer-has/request-details
  (when *default-printer*
    (let (has-details msg)

      (g:signal-connect *default-printer* "details-acquired"
              (lambda (printer success)
                (setf msg (format nil "~a ~a"
                                  (gtk:printer-name printer)
                                  success))))

      (is-true (setf has-details
                     (gtk:printer-has-details *default-printer*)))
      (when has-details
        (is-false (gtk:printer-request-details *default-printer*))
        ;; TODO: The signal handler is not called. Why?!
        (is-false msg)))))

;;;     gtk_printer_get_capabilities

(test gtk-printer-capabilities
  (when *default-printer*
    (is-false (gtk:printer-capabilities *default-printer*))))

;;;     gtk_printer_get_default_page_size

(test gtk-printer-default-page-size
  (when *default-printer*
    (is-false (gtk:printer-default-page-size *default-printer*))))

;;;     gtk_printer_get_hard_margins

(test gtk-printer-hard-margins
  (when *default-printer*
    (is-false (gtk:printer-hard-margins *default-printer*))))

;;;     gtk_printer_get_hard_margins_for_paper_size

(test gtk-printer-hard-margins-for-paper-size
  (when *default-printer*
    (let ((size (gtk:paper-size-new (gtk:paper-size-default))))
      (is-false (gtk:printer-hard-margins-for-paper-size *default-printer*
                                                         size)))))

;;; 2024-7-6

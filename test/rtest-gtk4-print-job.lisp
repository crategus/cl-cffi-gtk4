(in-package :gtk-test)

(def-suite gtk-print-job :in gtk-suite)
(in-suite gtk-print-job)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkPrintJob

(test gtk-print-job-class
  ;; Type check
  (is (g:type-is-object "GtkPrintJob"))
  ;; Check the registered name
  (is (eq 'gtk:print-job
          (glib:symbol-for-gtype "GtkPrintJob")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkPrintJob")
          (g:gtype (cffi:foreign-funcall "gtk_print_job_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkPrintJob")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkPrintJob")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkPrintJob")))
  ;; Check the properties
  (is (equal '("page-setup" "printer" "settings" "title" "track-print-status")
             (list-properties "GtkPrintJob")))
  ;; Check the signals
  (is (equal '("status-changed")
             (list-signals "GtkPrintJob")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkPrintJob" GTK-PRINT-JOB
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                                :TYPE-INITIALIZER "gtk_print_job_get_type")
                               ((PAGE-SETUP GTK-PRINT-JOB-PAGE-SETUP
                                 "page-setup" "GtkPageSetup" T NIL)
                                (PRINTER GTK-PRINT-JOB-PRINTER "printer"
                                 "GtkPrinter" T NIL)
                                (SETTINGS GTK-PRINT-JOB-SETTINGS "settings"
                                 "GtkPrintSettings" T NIL)
                                (TITLE GTK-PRINT-JOB-TITLE "title" "gchararray"
                                 T NIL)
                                (TRACK-PRINT-STATUS
                                 GTK-PRINT-JOB-TRACK-PRINT-STATUS
                                 "track-print-status" "gboolean" T T)))
             (gobject:get-g-type-definition "GtkPrintJob"))))

;;; --- Properties -------------------------------------------------------------

;;;     page-setup
;;;     printer
;;;     settings
;;;     title
;;;     track-print-status

;;; --- Signals ----------------------------------------------------------------

;;;     status-changed

;;; --- Functions --------------------------------------------------------------

;;;     GtkPrintJobCompleteFunc

;;;     gtk_print_job_new
;;;     gtk_print_job_get_status
;;;     gtk_print_job_set_source_file
;;;     gtk_print_job_set_source_fd
;;;     gtk_print_job_get_surface
;;;     gtk_print_job_send
;;;     gtk_print_job_get_pages
;;;     gtk_print_job_set_pages
;;;     gtk_print_job_get_page_ranges
;;;     gtk_print_job_set_page_ranges
;;;     gtk_print_job_get_page_set
;;;     gtk_print_job_set_page_set
;;;     gtk_print_job_get_num_copies
;;;     gtk_print_job_set_num_copies
;;;     gtk_print_job_get_scale
;;;     gtk_print_job_set_scale
;;;     gtk_print_job_get_n_up
;;;     gtk_print_job_set_n_up
;;;     gtk_print_job_get_n_up_layout
;;;     gtk_print_job_set_n_up_layout
;;;     gtk_print_job_get_rotate
;;;     gtk_print_job_set_rotate
;;;     gtk_print_job_get_collate
;;;     gtk_print_job_set_collate
;;;     gtk_print_job_get_reverse
;;;     gtk_print_job_set_reverse

;;; --- 2023-8-28 --------------------------------------------------------------

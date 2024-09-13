(in-package :gtk-test)

(def-suite gtk-print-job :in gtk-suite)
(in-suite gtk-print-job)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkPrintJob

(test gtk-print-job-class
  ;; Check type
  (is (g:type-is-object "GtkPrintJob"))
  ;; Check registered name
  (is (eq 'gtk:print-job
          (glib:symbol-for-gtype "GtkPrintJob")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkPrintJob")
          (g:gtype (cffi:foreign-funcall "gtk_print_job_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkPrintJob")))
  ;; Check children
  (is (equal '()
             (gtk-test:list-children "GtkPrintJob")))
  ;; Check interfaces
  (is (equal '()
             (gtk-test:list-interfaces "GtkPrintJob")))
  ;; Check properties
  (is (equal '("page-setup" "printer" "settings" "title" "track-print-status")
             (gtk-test:list-properties "GtkPrintJob")))
  ;; Check signals
  (is (equal '("status-changed")
             (gtk-test:list-signals "GtkPrintJob")))
  ;; Check class definition
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

(test gtk-print-job-properties
  (when *default-printer*
    (let* ((printer *default-printer*)
           (settings (gtk:print-settings-new))
           (setup (gtk:page-setup-new))
           (job (gtk:print-job-new "printjob" printer settings setup)))
      (is (eq setup (gtk:print-job-page-setup job)))
      (is (eq printer (gtk:print-job-printer job)))
      ;; Creates a new print settings object
      (is (typep (gtk:print-job-settings job) 'gtk:print-settings))
      (is (string= "printjob" (gtk:print-job-title job)))
      (is-false (gtk:print-job-track-print-status job)))))

;;; --- Signals ----------------------------------------------------------------

;;;     status-changed

(test gtk-print-job-status-changed-signal
  (let* ((name "status-changed")
         (gtype (g:gtype "GtkPrintJob"))
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
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_print_job_new

(test gtk-print-job-new
  (when *default-printer*
    (let ((printer *default-printer*)
          (settings (gtk:print-settings-new))
          (setup (gtk:page-setup-new)))
      (is (typep (gtk:print-job-new "printjob" printer settings setup)
                 'gtk:print-job)))))

;;;     gtk_print_job_get_status

(test gtk-print-job-status
  (when *default-printer*
    (let* ((printer *default-printer*)
           (settings (gtk:print-settings-new))
           (setup (gtk:page-setup-new))
           (job (gtk:print-job-new "printjob" printer settings setup)))
      (is (eq :initial (gtk:print-job-status job))))))

;;;     gtk_print_job_set_source_file

(test gtk-print-job-set-source-file
  (when *default-printer*
    (let* ((path (glib-sys:sys-path "test/resource/application-simple.pdf"))
           (printer *default-printer*)
           (settings (gtk:print-settings-new))
           (setup (gtk:page-setup-new))
           (job (gtk:print-job-new "printjob" printer settings setup)))
      (is-true (gtk:print-job-set-source-file job path)))))

;;;     gtk_print_job_set_source_fd

;;;     gtk_print_job_get_surface

(test gtk-print-job-surface
  (when *default-printer*
    (let* ((printer *default-printer*)
           (settings (gtk:print-settings-new))
           (setup (gtk:page-setup-new))
           (job (gtk:print-job-new "printjob" printer settings setup)))
      (is (cffi:pointerp (gtk:print-job-surface job))))))

;;;     GtkPrintJobCompleteFunc
;;;     gtk_print_job_send

#+nil
(test gtk-print-job-send
  (when *default-printer*
    (let* ((path (glib-sys:sys-path "test/resource/application-simple.pdf"))
           (printer *default-printer*)
           (settings (gtk:print-settings-new))
           (setup (gtk:page-setup-new))
           (job (gtk:print-job-new "printjob" printer settings setup)))
      (is-true (gtk:print-job-set-source-file job path))
      (is-false (gtk:print-job-send job
                                    (lambda (job)
                                      (format t
                                              "~&Print job finised: ~a~%"
                                              job)))))))

;;;     gtk_print_job_get_pages
;;;     gtk_print_job_set_pages

(test gtk-print-job-pages
  (when *default-printer*
    (let* ((printer *default-printer*)
           (settings (gtk:print-settings-new))
           (setup (gtk:page-setup-new))
           (job (gtk:print-job-new "printjob" printer settings setup)))
      (is (eq :all (gtk:print-job-pages job)))
      (is (eq :current (setf (gtk:print-job-pages job) :current)))
      (is (eq :current (gtk:print-job-pages job))))))

;;;     gtk_print_job_get_page_ranges
;;;     gtk_print_job_set_page_ranges

(test gtk-print-job-page-ranges
  (when *default-printer*
    (let* ((printer *default-printer*)
           (settings (gtk:print-settings-new))
           (setup (gtk:page-setup-new))
           (job (gtk:print-job-new "printjob" printer settings setup)))
      (is-false (gtk:print-job-page-ranges job))
      (is (equal '((1) (15 20) (25))
                  (setf (gtk:print-job-page-ranges job) '((1) (15 20) (25)))))
      (is (equal '((1) (15 20) (25)) (gtk:print-job-page-ranges job))))))

;;;     gtk_print_job_get_page_set
;;;     gtk_print_job_set_page_set

(test gtk-print-job-page-set
  (when *default-printer*
    (let* ((printer *default-printer*)
           (settings (gtk:print-settings-new))
           (setup (gtk:page-setup-new))
           (job (gtk:print-job-new "printjob" printer settings setup)))
      (is (eq :all (gtk:print-job-page-set job)))
      (is (eq :even (setf (gtk:print-job-page-set job) :even)))
      (is (eq :even (gtk:print-job-page-set job))))))

;;;     gtk_print_job_get_num_copies
;;;     gtk_print_job_set_num_copies

(test gtk-print-job-num-copies
  (when *default-printer*
    (let* ((printer *default-printer*)
           (settings (gtk:print-settings-new))
           (setup (gtk:page-setup-new))
           (job (gtk:print-job-new "printjob" printer settings setup)))
      (is (= 1 (gtk:print-job-num-copies job)))
      (is (= 10 (setf (gtk:print-job-num-copies job) 10)))
      (is (= 10 (gtk:print-job-num-copies job))))))

;;;     gtk_print_job_get_scale
;;;     gtk_print_job_set_scale

(test gtk-print-job-scale
  (when *default-printer*
    (let* ((printer *default-printer*)
           (settings (gtk:print-settings-new))
           (setup (gtk:page-setup-new))
           (job (gtk:print-job-new "printjob" printer settings setup)))
      (is (= 1.0 (gtk:print-job-scale job)))
      (is (= 2.0 (setf (gtk:print-job-scale job) 2))
      (is (= 2.0 (gtk:print-job-scale job)))))))

;;;     gtk_print_job_get_n_up
;;;     gtk_print_job_set_n_up

(test gtk-print-job-n-up
  (when *default-printer*
    (let* ((printer *default-printer*)
           (settings (gtk:print-settings-new))
           (setup (gtk:page-setup-new))
           (job (gtk:print-job-new "printjob" printer settings setup)))
      (is (= 1.0 (gtk:print-job-n-up job)))
      (is (= 2.0 (setf (gtk:print-job-n-up job) 2)))
      (is (= 2.0 (gtk:print-job-n-up job))))))

;;;     gtk_print_job_get_n_up_layout
;;;     gtk_print_job_set_n_up_layout

(test gtk-print-job-n-up-layout
  (when *default-printer*
    (let* ((printer *default-printer*)
           (settings (gtk:print-settings-new))
           (setup (gtk:page-setup-new))
           (job (gtk:print-job-new "printjob" printer settings setup)))
      (is (eq :LEFT-TO-RIGHT-TOP-TO-BOTTOM (gtk:print-job-n-up-layout job)))
      (is (eq :bottom-to-top-right-to-left
              (setf (gtk:print-job-n-up-layout job)
                    :bottom-to-top-right-to-left)))
      (is (eq :bottom-to-top-right-to-left (gtk:print-job-n-up-layout job))))))

;;;     gtk_print_job_get_rotate
;;;     gtk_print_job_set_rotate

(test gtk-print-job-rotate
  (when *default-printer*
    (let* ((printer *default-printer*)
           (settings (gtk:print-settings-new))
           (setup (gtk:page-setup-new))
           (job (gtk:print-job-new "printjob" printer settings setup)))
      (is-false (gtk:print-job-rotate job))
      (is-true (setf (gtk:print-job-rotate job) t))
      (is-true (gtk:print-job-rotate job)))))

;;;     gtk_print_job_get_collate
;;;     gtk_print_job_set_collate

(test gtk-print-job-collate
  (when *default-printer*
    (let* ((printer *default-printer*)
           (settings (gtk:print-settings-new))
           (setup (gtk:page-setup-new))
           (job (gtk:print-job-new "printjob" printer settings setup)))
      (is-true (gtk:print-job-collate job))
      (is-false (setf (gtk:print-job-collate job) nil))
      (is-false (gtk:print-job-collate job)))))

;;;     gtk_print_job_get_reverse
;;;     gtk_print_job_set_reverse

(test gtk-print-job-reverse
  (when *default-printer*
    (let* ((printer *default-printer*)
           (settings (gtk:print-settings-new))
           (setup (gtk:page-setup-new))
           (job (gtk:print-job-new "printjob" printer settings setup)))
      (is-false (gtk:print-job-reverse job))
      (is-true (setf (gtk:print-job-reverse job) t))
      (is-true (gtk:print-job-reverse job)))))

;;; 2024-7-7

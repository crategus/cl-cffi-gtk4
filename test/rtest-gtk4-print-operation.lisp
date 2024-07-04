(in-package :gtk-test)

(def-suite gtk-print-operation :in gtk-suite)
(in-suite gtk-print-operation)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkPrintStatus

(test gtk-print-status
  ;; Check type
  (is (g:type-is-enum "GtkPrintStatus"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkPrintStatus")
          (g:gtype (cffi:foreign-funcall "gtk_print_status_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:print-status
          (glib:symbol-for-gtype "GtkPrintStatus")))
  ;; Check names
  (is (equal '("GTK_PRINT_STATUS_INITIAL" "GTK_PRINT_STATUS_PREPARING"
               "GTK_PRINT_STATUS_GENERATING_DATA"
               "GTK_PRINT_STATUS_SENDING_DATA" "GTK_PRINT_STATUS_PENDING"
               "GTK_PRINT_STATUS_PENDING_ISSUE" "GTK_PRINT_STATUS_PRINTING"
               "GTK_PRINT_STATUS_FINISHED" "GTK_PRINT_STATUS_FINISHED_ABORTED")
             (gtk-test:list-enum-item-name "GtkPrintStatus")))
  ;; Check values
  (is (equal '(0 1 2 3 4 5 6 7 8)
             (gtk-test:list-enum-item-value "GtkPrintStatus")))
  ;; Check nick names
  (is (equal '("initial" "preparing" "generating-data" "sending-data" "pending"
               "pending-issue" "printing" "finished" "finished-aborted")
             (gtk-test:list-enum-item-nick "GtkPrintStatus")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkPrintStatus"
                             GTK-PRINT-STATUS
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_print_status_get_type")
                             (:INITIAL 0)
                             (:PREPARING 1)
                             (:GENERATING-DATA 2)
                             (:SENDING-DATA 3)
                             (:PENDING 4)
                             (:PENDING-ISSUE 5)
                             (:PRINTING 6)
                             (:FINISHED 7)
                             (:FINISHED-ABORTED 8))
             (gobject:get-g-type-definition "GtkPrintStatus"))))

;;;     GtkPrintOperationAction

(test gtk-print-operaton-action
  ;; Check type
  (is (g:type-is-enum "GtkPrintOperationAction"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkPrintOperationAction")
          (g:gtype (cffi:foreign-funcall "gtk_print_operation_action_get_type"
                                         :size))))
  ;; Check registered name
  (is (eq 'gtk:print-operation-action
          (glib:symbol-for-gtype "GtkPrintOperationAction")))
  ;; Check names
  (is (equal '("GTK_PRINT_OPERATION_ACTION_PRINT_DIALOG"
               "GTK_PRINT_OPERATION_ACTION_PRINT"
               "GTK_PRINT_OPERATION_ACTION_PREVIEW"
               "GTK_PRINT_OPERATION_ACTION_EXPORT")
             (gtk-test:list-enum-item-name "GtkPrintOperationAction")))
  ;; Check values
  (is (equal '(0 1 2 3)
             (gtk-test:list-enum-item-value "GtkPrintOperationAction")))
  ;; Check nick names
  (is (equal '("print-dialog" "print" "preview" "export")
             (gtk-test:list-enum-item-nick "GtkPrintOperationAction")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkPrintOperationAction"
                             GTK-PRINT-OPERATION-ACTION
                             (:EXPORT T
                              :TYPE-INITIALIZER
                              "gtk_print_operation_action_get_type")
                             (:PRINT-DIALOG 0)
                             (:PRINT 1)
                             (:PREVIEW 2)
                             (:EXPORT 3))
             (gobject:get-g-type-definition "GtkPrintOperationAction"))))

;;;     GtkPrintOperationResult

(test gtk-print-operation-result
  ;; Check type
  (is (g:type-is-enum "GtkPrintOperationResult"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkPrintOperationResult")
          (g:gtype (cffi:foreign-funcall "gtk_print_operation_result_get_type"
                                         :size))))
  ;; Check registered name
  (is (eq 'gtk:print-operation-result
          (glib:symbol-for-gtype "GtkPrintOperationResult")))
  ;; Check names
  (is (equal '("GTK_PRINT_OPERATION_RESULT_ERROR"
               "GTK_PRINT_OPERATION_RESULT_APPLY"
               "GTK_PRINT_OPERATION_RESULT_CANCEL"
               "GTK_PRINT_OPERATION_RESULT_IN_PROGRESS")
             (gtk-test:list-enum-item-name "GtkPrintOperationResult")))
  ;; Check values
  (is (equal '(0 1 2 3)
             (gtk-test:list-enum-item-value "GtkPrintOperationResult")))
  ;; Check nick names
  (is (equal '("error" "apply" "cancel" "in-progress")
             (gtk-test:list-enum-item-nick "GtkPrintOperationResult")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkPrintOperationResult"
                             GTK-PRINT-OPERATION-RESULT
                             (:EXPORT T
                              :TYPE-INITIALIZER
                              "gtk_print_operation_result_get_type")
                             (:ERROR 0)
                             (:APPLY 1)
                             (:CANCEL 2)
                             (:IN-PROGRESS 3))
             (gobject:get-g-type-definition "GtkPrintOperationResult"))))

;;;     GtkPrintOperation

(test gtk-print-operation-class
  ;; Check type
  (is (g:type-is-object "GtkPrintOperation"))
  ;; Check registered name
  (is (eq 'gtk:print-operation
          (glib:symbol-for-gtype "GtkPrintOperation")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkPrintOperation")
          (g:gtype (cffi:foreign-funcall "gtk_print_operation_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkPrintOperation")))
  ;; Check children
  (is (equal '()
             (gtk-test:list-children "GtkPrintOperation")))
  ;; Check interfaces
  (is (equal '("GtkPrintOperationPreview")
             (gtk-test:list-interfaces "GtkPrintOperation")))
  ;; Check class properties
  (is (equal '("allow-async" "current-page" "custom-tab-label"
               "default-page-setup" "embed-page-setup" "export-filename"
               "has-selection" "job-name" "n-pages" "n-pages-to-print"
               "print-settings" "show-progress" "status" "status-string"
               "support-selection" "track-print-status" "unit" "use-full-page")
             (gtk-test:list-properties "GtkPrintOperation")))
  ;; Check signals
  (is (equal '("begin-print" "create-custom-widget" "custom-widget-apply" "done"
               "draw-page" "end-print" "paginate" "preview" "request-page-setup"
               "status-changed" "update-custom-widget")
             (gtk-test:list-signals "GtkPrintOperation")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkPrintOperation"
                                             GTK-PRINT-OPERATION
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES
                        ("GtkPrintOperationPreview") :TYPE-INITIALIZER
                        "gtk_print_operation_get_type")
                       ((ALLOW-ASYNC GTK-PRINT-OPERATION-ALLOW-ASYNC
                         "allow-async" "gboolean" T T)
                        (CURRENT-PAGE GTK-PRINT-OPERATION-CURRENT-PAGE
                         "current-page" "gint" T T)
                        (CUSTOM-TAB-LABEL GTK-PRINT-OPERATION-CUSTOM-TAB-LABEL
                         "custom-tab-label" "gchararray" T T)
                        (DEFAULT-PAGE-SETUP
                         GTK-PRINT-OPERATION-DEFAULT-PAGE-SETUP
                         "default-page-setup" "GtkPageSetup" T T)
                        (EMBED-PAGE-SETUP GTK-PRINT-OPERATION-EMBED-PAGE-SETUP
                         "embed-page-setup" "gboolean" T T)
                        (EXPORT-FILENAME GTK-PRINT-OPERATION-EXPORT-FILENAME
                         "export-filename" "gchararray" T T)
                        (HAS-SELECTION GTK-PRINT-OPERATION-HAS-SELECTION
                         "has-selection" "gboolean" T T)
                        (JOB-NAME GTK-PRINT-OPERATION-JOB-NAME "job-name"
                         "gchararray" T T)
                        (N-PAGES GTK-PRINT-OPERATION-N-PAGES "n-pages" "gint" T
                         T)
                        (N-PAGES-TO-PRINT GTK-PRINT-OPERATION-N-PAGES-TO-PRINT
                         "n-pages-to-print" "gint" T NIL)
                        (PRINT-SETTINGS GTK-PRINT-OPERATION-PRINT-SETTINGS
                         "print-settings" "GtkPrintSettings" T T)
                        (SHOW-PROGRESS GTK-PRINT-OPERATION-SHOW-PROGRESS
                         "show-progress" "gboolean" T T)
                        (STATUS GTK-PRINT-OPERATION-STATUS "status"
                         "GtkPrintStatus" T NIL)
                        (STATUS-STRING GTK-PRINT-OPERATION-STATUS-STRING
                         "status-string" "gchararray" T NIL)
                        (SUPPORT-SELECTION
                         GTK-PRINT-OPERATION-SUPPORT-SELECTION
                         "support-selection" "gboolean" T T)
                        (TRACK-PRINT-STATUS
                         GTK-PRINT-OPERATION-TRACK-PRINT-STATUS
                         "track-print-status" "gboolean" T T)
                        (UNIT GTK-PRINT-OPERATION-UNIT "unit" "GtkUnit" T T)
                        (USE-FULL-PAGE GTK-PRINT-OPERATION-USE-FULL-PAGE
                         "use-full-page" "gboolean" T T)))
             (gobject:get-g-type-definition "GtkPrintOperation"))))

;;; --- Properties -------------------------------------------------------------

;;;     allow-async
;;;     current-page
;;;     custom-tab-label
;;;     default-page-setup
;;;     embed-page-setup
;;;     export-filename
;;;     has-selection
;;;     job-name
;;;     n-pages
;;;     n-pages-to-print
;;;     print-settings
;;;     show-progress
;;;     status
;;;     status-string
;;;     support-selection
;;;     track-print-status
;;;     unit
;;;     use-full-page

(test gtk-print-operation-properties
  (let ((operation (make-instance 'gtk:print-operation)))
    (is-false (gtk:print-operation-allow-async operation))
    (is (= -1 (gtk:print-operation-current-page operation)))
    (is-false (gtk:print-operation-custom-tab-label operation))
    (is-false (gtk:print-operation-default-page-setup operation))
    (is-false (gtk:print-operation-embed-page-setup operation))
    (is-false (gtk:print-operation-export-filename operation))
    (is-false (gtk:print-operation-has-selection operation))
    (is (stringp (gtk:print-operation-job-name operation)))

    ;; n-pages
    (is (= -1 (gtk:print-operation-n-pages operation)))
    (is (= 10 (setf (gtk:print-operation-n-pages operation) 10)))
    (is (= 10 (gtk:print-operation-n-pages operation)))
    ;; n-pages-to-print
    (is (= -1 (gtk:print-operation-n-pages-to-print operation)))

    (is-false (gtk:print-operation-print-settings operation))
    (is-false (gtk:print-operation-show-progress operation))
    (is (eq :initial (gtk:print-operation-status operation)))
    (is (string= "" (gtk:print-operation-status-string operation)))
    (is-false (gtk:print-operation-support-selection operation))
    (is-false (gtk:print-operation-track-print-status operation))
    (is (eq :pixel (gtk:print-operation-unit operation)))
    (is-false (gtk:print-operation-use-full-page operation))))

;;; --- Signals ----------------------------------------------------------------

;;;     begin-print
;;;     create-custom-widget
;;;     custom-widget-apply
;;;     done
;;;     draw-page
;;;     end-print
;;;     paginate
;;;     preview
;;;     request-page-setup
;;;     status-changed
;;;     update-custom-widget

;;; --- Functions --------------------------------------------------------------

;;;     gtk_print_operation_new

(test gtk-print-operation-new
  (is (typep (gtk:print-operation-new) 'gtk:print-operation)))

;;;     gtk_print_operation_get_error
;;;     gtk_print_operation_run
;;;     gtk_print_operation_cancel
;;;     gtk_print_operation_draw_page_finish
;;;     gtk_print_operation_set_defer_drawing
;;;     gtk_print_operation_is_finished
;;;     gtk_print_run_page_setup_dialog

;;;     GtkPageSetupDoneFunc

;;;     gtk_print_run_page_setup_dialog_async

;;; 2024-2-16

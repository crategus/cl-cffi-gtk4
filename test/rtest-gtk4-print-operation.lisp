(in-package :gtk-test)

(def-suite gtk-print-operation :in gtk-suite)
(in-suite gtk-print-operation)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkPrintOperationPreview

(test print-operation-preview
  ;; Type check
  (is (g:type-is-interface "GtkPrintOperationPreview"))
  ;; Check the registered name
  (is (eq 'gtk:print-operation-preview
          (glib:symbol-for-gtype "GtkPrintOperationPreview")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkPrintOperationPreview")
          (g:gtype (cffi:foreign-funcall "gtk_print_operation_preview_get_type"
                                         :size))))
  ;; Get the names of the interface properties.
  (is (equal '()
             (list-interface-properties "GtkPrintOperationPreview")))
  ;; Get the interface definition
  (is (equal '(DEFINE-G-INTERFACE "GtkPrintOperationPreview"
                                  GTK-PRINT-OPERATION-PREVIEW
                                  (:EXPORT T
                                   :TYPE-INITIALIZER
                                   "gtk_print_operation_preview_get_type"))
             (gobject:get-g-type-definition "GtkPrintOperationPreview"))))

;;; --- Signals ----------------------------------------------------------------

;;;     got-page-size
;;;     ready

;;; --- Functions --------------------------------------------------------------

;;;     gtk_print_operation_preview_end_preview
;;;     gtk_print_operation_preview_is_selected
;;;     gtk_print_operation_preview_render_page

;;; --- Types and Values -------------------------------------------------------

;;;     GtkPrintStatus

(test print-status
  ;; Check the type
  (is (g:type-is-enum "GtkPrintStatus"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkPrintStatus")
          (g:gtype (cffi:foreign-funcall "gtk_print_status_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gtk:print-status
          (glib:symbol-for-gtype "GtkPrintStatus")))
  ;; Check the names
  (is (equal '("GTK_PRINT_STATUS_INITIAL" "GTK_PRINT_STATUS_PREPARING"
               "GTK_PRINT_STATUS_GENERATING_DATA"
               "GTK_PRINT_STATUS_SENDING_DATA" "GTK_PRINT_STATUS_PENDING"
               "GTK_PRINT_STATUS_PENDING_ISSUE" "GTK_PRINT_STATUS_PRINTING"
               "GTK_PRINT_STATUS_FINISHED" "GTK_PRINT_STATUS_FINISHED_ABORTED")
             (list-enum-item-name "GtkPrintStatus")))
  ;; Check the values
  (is (equal '(0 1 2 3 4 5 6 7 8)
             (list-enum-item-value "GtkPrintStatus")))
  ;; Check the nick names
  (is (equal '("initial" "preparing" "generating-data" "sending-data" "pending"
               "pending-issue" "printing" "finished" "finished-aborted")
             (list-enum-item-nick "GtkPrintStatus")))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GtkPrintStatus"
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

(test print-operaton-action
  ;; Check the type
  (is (g:type-is-enum "GtkPrintOperationAction"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkPrintOperationAction")
          (g:gtype (cffi:foreign-funcall "gtk_print_operation_action_get_type"
                                         :size))))
  ;; Check the registered name
  (is (eq 'gtk:print-operation-action
          (glib:symbol-for-gtype "GtkPrintOperationAction")))
  ;; Check the names
  (is (equal '("GTK_PRINT_OPERATION_ACTION_PRINT_DIALOG"
               "GTK_PRINT_OPERATION_ACTION_PRINT"
               "GTK_PRINT_OPERATION_ACTION_PREVIEW"
               "GTK_PRINT_OPERATION_ACTION_EXPORT")
             (list-enum-item-name "GtkPrintOperationAction")))
  ;; Check the values
  (is (equal '(0 1 2 3)
             (list-enum-item-value "GtkPrintOperationAction")))
  ;; Check the nick names
  (is (equal '("print-dialog" "print" "preview" "export")
             (list-enum-item-nick "GtkPrintOperationAction")))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GtkPrintOperationAction"
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

(test print-operation-result
  ;; Check the type
  (is (g:type-is-enum "GtkPrintOperationResult"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkPrintOperationResult")
          (g:gtype (cffi:foreign-funcall "gtk_print_operation_result_get_type"
                                         :size))))
  ;; Check the registered name
  (is (eq 'gtk:print-operation-result
          (glib:symbol-for-gtype "GtkPrintOperationResult")))
  ;; Check the names
  (is (equal '("GTK_PRINT_OPERATION_RESULT_ERROR"
               "GTK_PRINT_OPERATION_RESULT_APPLY"
               "GTK_PRINT_OPERATION_RESULT_CANCEL"
               "GTK_PRINT_OPERATION_RESULT_IN_PROGRESS")
             (list-enum-item-name "GtkPrintOperationResult")))
  ;; Check the values
  (is (equal '(0 1 2 3)
             (list-enum-item-value "GtkPrintOperationResult")))
  ;; Check the nick names
  (is (equal '("error" "apply" "cancel" "in-progress")
             (list-enum-item-nick "GtkPrintOperationResult")))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GtkPrintOperationResult"
                             GTK-PRINT-OPERATION-RESULT
                             (:EXPORT T
                              :TYPE-INITIALIZER
                              "gtk_print_operation_result_get_type")
                             (:ERROR 0)
                             (:APPLY 1)
                             (:CANCEL 2)
                             (:IN-PROGRESS 3))
             (gobject:get-g-type-definition "GtkPrintOperationResult"))))

;;;     GtkPrintError
;;;     GTK_PRINT_ERROR

;;;     GtkPrintOperation

(test print-operation-class
  ;; Type check
  (is (g:type-is-object "GtkPrintOperation"))
  ;; Check the registered name
  (is (eq 'gtk:print-operation
          (glib:symbol-for-gtype "GtkPrintOperation")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkPrintOperation")
          (g:gtype (cffi:foreign-funcall "gtk_print_operation_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkPrintOperation")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkPrintOperation")))
  ;; Check the interfaces
  (is (equal '("GtkPrintOperationPreview")
             (list-interfaces "GtkPrintOperation")))
  ;; Check the class properties
  (is (equal '("allow-async" "current-page" "custom-tab-label"
               "default-page-setup" "embed-page-setup" "export-filename"
               "has-selection" "job-name" "n-pages" "n-pages-to-print"
               "print-settings" "show-progress" "status" "status-string"
               "support-selection" "track-print-status" "unit" "use-full-page")
             (list-properties "GtkPrintOperation")))
  ;; Check the list of signals
  (is (equal '("begin-print" "create-custom-widget" "custom-widget-apply" "done"
               "draw-page" "end-print" "paginate" "preview" "request-page-setup"
               "status-changed" "update-custom-widget")
             (list-signals "GtkPrintOperation")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkPrintOperation" GTK-PRINT-OPERATION
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
;;;     gtk_print_operation_get_error
;;;     gtk_print_operation_run
;;;     gtk_print_operation_cancel
;;;     gtk_print_operation_draw_page_finish
;;;     gtk_print_operation_set_defer_drawing
;;;     gtk_print_operation_is_finished
;;;     gtk_print_run_page_setup_dialog

;;;     GtkPageSetupDoneFunc

;;;     gtk_print_run_page_setup_dialog_async

;;; --- 2023-5-29 --------------------------------------------------------------

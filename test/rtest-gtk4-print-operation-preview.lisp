(in-package :gtk-test)

(def-suite gtk-print-operation-preview :in gtk-suite)
(in-suite gtk-print-operation-preview)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkPrintOperationPreview

(test gtk-print-operation-preview-interface
  ;; Type check
  (is (g:type-is-interface "GtkPrintOperationPreview"))
  ;; Check the registered name
  (is (eq 'gtk:print-operation-preview
          (glib:symbol-for-gtype "GtkPrintOperationPreview")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkPrintOperationPreview")
          (g:gtype (cffi:foreign-funcall "gtk_print_operation_preview_get_type"
                                         :size))))
  ;; Check the interface prerequisites
  (is (equal '("GObject")
             (list-interface-prerequisites "GtkPrintOperationPreview")))
  ;; Get the names of the interface properties.
  (is (equal '()
             (list-interface-properties "GtkPrintOperationPreview")))
  ;; Check the interface signals
  (is (equal '("got-page-size" "ready")
             (list-signals "GtkPrintOperationPreview")))
  ;; Get the interface definition
  (is (equal '(GOBJECT:DEFINE-G-INTERFACE "GtkPrintOperationPreview"
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

;;; --- 2023-8-28 --------------------------------------------------------------

(in-package :gtk-test)

(def-suite gtk-dialog :in gtk-deprecated)
(in-suite gtk-dialog)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkDialogFlags

(test gtk-dialog-flags
  ;; Check type
  (is (g:type-is-flags "GtkDialogFlags"))
  ;; Check registered name
  (is (eq 'gtk:dialog-flags
          (glib:symbol-for-gtype "GtkDialogFlags")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkDialogFlags")
          (g:gtype (cffi:foreign-funcall "gtk_dialog_flags_get_type" :size))))
  ;; Check names
  (is (equal '("GTK_DIALOG_MODAL" "GTK_DIALOG_DESTROY_WITH_PARENT"
               "GTK_DIALOG_USE_HEADER_BAR")
             (glib-test:list-flags-item-names "GtkDialogFlags")))
  ;; Check values
  (is (equal '(1 2 4)
             (glib-test:list-flags-item-values "GtkDialogFlags")))
  ;; Check nick names
  (is (equal '("modal" "destroy-with-parent" "use-header-bar")
             (glib-test:list-flags-item-nicks "GtkDialogFlags")))
  ;; Check flags definition
  (is (equal '(GOBJECT:DEFINE-GFLAGS "GtkDialogFlags" GTK:DIALOG-FLAGS
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "gtk_dialog_flags_get_type")
                                     (:MODAL 1)
                                     (:DESTROY-WITH-PARENT 2)
                                     (:USE-HEADER-BAR 4))
             (gobject:get-gtype-definition "GtkDialogFlags"))))

;;;     GtkDialog

(test gtk-dialog-class
  (let ((gtk-init:*warn-deprecated* nil))
    ;; Check type
    (is (g:type-is-object "GtkDialog"))
    ;; Check registered name
    (is (eq 'gtk:dialog
            (glib:symbol-for-gtype "GtkDialog")))
    ;; Check type initializer
    (is (eq (g:gtype "GtkDialog")
            (g:gtype (cffi:foreign-funcall "gtk_dialog_get_type" :size))))
    ;; Check parent
    (is (eq (g:gtype "GtkWindow")
            (g:type-parent "GtkDialog")))
    ;; Check children
    #-windows
    (is (equal '("GtkAppChooserDialog" "GtkColorChooserDialog"
                 "GtkFileChooserDialog" "GtkFontChooserDialog"
                 "GtkMessageDialog" "GtkPageSetupUnixDialog"
                 "GtkPrintUnixDialog")
               (glib-test:list-children "GtkDialog")))
    ;; Check interfaces
    (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
                 "GtkNative" "GtkShortcutManager" "GtkRoot")
               (glib-test:list-interfaces "GtkDialog")))
    ;; Check class properties
    (is (equal '("use-header-bar")
               (glib-test:list-properties "GtkDialog")))
    ;; Check signals
    (is (equal '("close" "response")
               (glib-test:list-signals "GtkDialog")))
    ;; Check CSS name
    (is (string= "window"
                 (gtk:widget-class-css-name "GtkDialog")))
    ;; Check accessible role
    (is (eq :dialog (gtk:widget-class-accessible-role "GtkDialog")))
    ;; Check class definition
    (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkDialog" GTK:DIALOG
                        (:SUPERCLASS GTK:WINDOW
                         :EXPORT T
                         :INTERFACES
                         ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
                          "GtkNative" "GtkRoot" "GtkShortcutManager")
                         :TYPE-INITIALIZER "gtk_dialog_get_type")
                        ((USE-HEADER-BAR DIALOG-USE-HEADER-BAR
                          "use-header-bar" "gint" T NIL)))
               (gobject:get-gtype-definition "GtkDialog")))))

;;; --- Signals ----------------------------------------------------------------

;;;     close

(test gtk-dialog-close-signal
  (let* ((name "close")
         (gtype (g:gtype "GtkDialog"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    (is (string= "close" (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    (is (equal '(:ACTION :RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     response

(test gtk-dialog-response-signal
  (let* ((name "response")
         (gtype (g:gtype "GtkDialog"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    (is (string= "response" (g:signal-query-signal-name query)))
    (is (eq (g:gtype "GtkDialog") (g:signal-query-owner-type query)))
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    (is (equal '("gint")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Properties -------------------------------------------------------------

;;;     use-header-bar

(test gtk-dialog-properties
  (let ((gtk-init:*warn-deprecated* nil))
    (glib-test:with-check-memory (dialog)
      (setf dialog (make-instance 'gtk:dialog))
      ;; The default value is not -1.
      (is (= 0 (gtk:dialog-use-header-bar dialog)))
      ;; Property USE-HEADER-BAR ist not writable
      (signals (error) (setf (gtk:dialog-use-header-bar dialog) 1))
      (is-false (gtk:window-destroy dialog)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_dialog_new

(test gtk-dialog-new
  (let ((gtk-init:*warn-deprecated* nil))
    (glib-test:with-check-memory (dialog)
      (is (typep (setf dialog (gtk:dialog-new)) 'gtk:dialog))
      (is-false (gtk:window-destroy dialog)))))

;;;     gtk_dialog_new_with_buttons

;; Adds two references for the buttons

(test gtk-dialog-new-with-buttons
  (let* ((gtk-init:*warn-deprecated* nil))
    (glib-test:with-check-memory (parent dialog :strong 2)
      (is (typep (setf parent (gtk:window-new)) 'gtk:window))
      (is (typep (setf dialog
                       (gtk:dialog-new-with-buttons "My dialog"
                                                    parent
                                                    '(:modal
                                                      :destroy-with-parent)
                                                    "_OK"
                                                    :accept
                                                    "_Cancel"
                                                    :reject))
                 'gtk:dialog))
    (is (string= "My dialog" (gtk:window-title dialog)))
    (is-false (gtk:window-destroy dialog))
    (is-false (gtk:window-destroy parent)))))

;;;     gtk_dialog_response

(test gtk-dialog-response
  (let ((gtk-init:*warn-deprecated* nil))
    (glib-test:with-check-memory (dialog)
      (let (msg)
        (setf dialog (gtk:dialog-new))
        (g:signal-connect dialog "response"
                          (lambda (widget response)
                            (declare (ignore widget))
                            (push response msg)
                            (when (= -5 response)
                              (gtk:window-destroy dialog))))
        (is-false (gtk:dialog-response dialog :cancel))
        (is-false (gtk:dialog-response dialog :ok))
        (is (equal '(-5 -6) msg))))))

;;;     gtk_dialog_add_button
;;;     gtk_dialog_add_buttons
;;;     gtk_dialog_get_response_for_widget
;;;     gtk_dialog_get_widget_for_response
;;;     gtk_dialog_add_action_widget

;; Adds 3 strong references for the buttons

(test gtk-dialog-add-buttons
  (let ((gtk-init:*warn-deprecated* nil))
    (glib-test:with-check-memory (dialog (button1 2) (button2 2) (button3 2) :strong 3)
      (setf dialog (gtk:dialog-new))
      (setf button3 (gtk:button-new))
      (is-false (gtk:dialog-add-buttons dialog "_OK" :accept "_Cancel" :reject))
      (is-false (gtk:dialog-add-action-widget dialog button3 :ok))
      (is (typep (setf button1
                       (gtk:dialog-widget-for-response dialog :accept)) 'gtk:button))
      (is (typep (setf button2
                       (gtk:dialog-widget-for-response dialog :reject)) 'gtk:button))
      (is (eq button3 (gtk:dialog-widget-for-response dialog :ok)))
      (is-false (gtk:dialog-widget-for-response dialog :cancel))
      (is (= -3 (gtk:dialog-response-for-widget dialog button1)))
      (is (= -2 (gtk:dialog-response-for-widget dialog button2)))
      (is (= -5 (gtk:dialog-response-for-widget dialog button3)))
      ;; Returns :NONE for a widget which is not in the action area
      (is (= -1 (gtk:dialog-response-for-widget dialog (gtk:button-new))))
      (gtk:window-destroy dialog))))

;;;     gtk_dialog_set_default_response
;;;     gtk_dialog_set_response_sensitive

;; Adds 2 strong references or the buttons

(test gtk-dialog-set-default-response/response-sensitive
  (let ((gtk-init:*warn-deprecated* nil))
    (glib-test:with-check-memory (dialog :strong 2)
      (setf dialog (gtk:dialog-new))
      (is-false (gtk:dialog-add-buttons dialog "_OK" :accept "_Cancel" :reject))
      (is-false (gtk:dialog-set-default-response dialog  :accept))
      (is-false (gtk:dialog-set-default-response dialog :ok))
      (is-false (gtk:dialog-set-response-sensitive dialog :accept t))
      (is-false (gtk:dialog-set-response-sensitive dialog :reject nil))
      (is-false (gtk:window-destroy dialog)))))

;;;     gtk_dialog_get_content_area
;;;     gtk_dialog_get_header_bar

;; Adds two strong references for the header bar and the content area

(test gtk-dialog-content-area/header-bar
  (let ((gtk-init:*warn-deprecated* nil))
    (glib-test:with-check-memory (dialog :strong 2)
      (setf dialog (make-instance 'gtk:dialog
                                  :use-header-bar 1))
      (is (typep (gtk:dialog-content-area dialog) 'gtk:box))
      (is (= 1 (gtk:dialog-use-header-bar dialog)))
      (is (typep (gtk:dialog-header-bar dialog) 'gtk:header-bar))
      (gtk:window-destroy dialog))))

;;; 2026-08-20

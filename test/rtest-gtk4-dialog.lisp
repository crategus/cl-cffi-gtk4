(in-package :gtk-test)

(def-suite gtk-dialog :in gtk-suite)
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
             (list-flags-item-name "GtkDialogFlags")))
  ;; Check values
  (is (equal '(1 2 4)
             (list-flags-item-value "GtkDialogFlags")))
  ;; Check nick names
  (is (equal '("modal" "destroy-with-parent" "use-header-bar")
             (list-flags-item-nick "GtkDialogFlags")))
  ;; Check flags definition
  (is (equal '(GOBJECT:DEFINE-G-FLAGS "GtkDialogFlags"
                              GTK-DIALOG-FLAGS
                              (:EXPORT T
                               :TYPE-INITIALIZER "gtk_dialog_flags_get_type")
                              (:MODAL 1)
                              (:DESTROY-WITH-PARENT 2)
                              (:USE-HEADER-BAR 4))
             (gobject:get-g-type-definition "GtkDialogFlags"))))

;;;     GtkDialog

(test gtk-dialog-class
  (let ((*gtk-warn-deprecated* nil))
    ;; Type check
    (is (g:type-is-object "GtkDialog"))
    ;; Check the registered name
    (is (eq 'gtk:dialog
            (glib:symbol-for-gtype "GtkDialog")))
    ;; Check the type initializer
    (is (eq (g:gtype "GtkDialog")
            (g:gtype (cffi:foreign-funcall "gtk_dialog_get_type" :size))))
    ;; Check the parent
    (is (eq (g:gtype "GtkWindow")
            (g:type-parent "GtkDialog")))
    ;; Check the children
    #-windows
    (is (equal '("GtkAppChooserDialog" "GtkColorChooserDialog"
                 "GtkFileChooserDialog" "GtkFontChooserDialog"
                 "GtkMessageDialog" "GtkPageSetupUnixDialog"
                 "GtkPrintUnixDialog")
               (list-children "GtkDialog")))
    ;; Check the interfaces
    (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
                 "GtkNative" "GtkShortcutManager" "GtkRoot")
               (list-interfaces "GtkDialog")))
    ;; Check the class properties
    (is (equal '("use-header-bar")
               (list-properties "GtkDialog")))
    ;; Check the list of signals
    (is (equal '("close" "response")
               (list-signals "GtkDialog")))
    ;; CSS name
    (is (string= "window"
                 (gtk:widget-class-css-name "GtkDialog")))
    ;; CSS classes
    (is (equal '("background" "dialog")
               (gtk:widget-css-classes (make-instance 'gtk:dialog))))
    ;; Accessible role
    (is (eq :dialog (gtk:widget-class-accessible-role "GtkDialog")))
    ;; Check the class definition
    (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkDialog" GTK-DIALOG
                         (:SUPERCLASS GTK-WINDOW :EXPORT T :INTERFACES
                          ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
                           "GtkNative" "GtkRoot" "GtkShortcutManager")
                          :TYPE-INITIALIZER "gtk_dialog_get_type")
                         ((USE-HEADER-BAR GTK-DIALOG-USE-HEADER-BAR
                           "use-header-bar" "gint" T NIL)))
               (gobject:get-g-type-definition "GtkDialog")))))

;;; --- Properties -------------------------------------------------------------

;;;     use-header-bar

(test gtk-dialog-properties
  (let ((*gtk-warn-deprecated* nil))
    (let ((dialog (make-instance 'gtk:dialog)))
      ;; The default value is not -1.
      (is (= 0 (gtk:dialog-use-header-bar dialog))))))

;;; --- Signals ----------------------------------------------------------------

;;;     close

(test gtk-dialog-close-signal
  (let ((query (g:signal-query (g:signal-lookup "close" "GtkDialog"))))
    (is (string= "close" (g:signal-query-signal-name query)))
    (is (string= "GtkDialog" (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:ACTION :RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

;;;     response

(test gtk-dialog-response-signal
  (let ((query (g:signal-query (g:signal-lookup "response" "GtkDialog"))))
    (is (string= "response" (g:signal-query-signal-name query)))
    (is (string= "GtkDialog" (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '("gint")
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_dialog_new

(test gtk-dialog-new
  (let ((*gtk-warn-deprecated* nil))
    (is (typep (gtk:dialog-new) 'gtk:dialog))))

;;;     gtk_dialog_new_with_buttons

(test gtk-dialog-new-with-buttons
  (let ((*gtk-warn-deprecated* nil))
    (let* ((parent (gtk:window-new))
           (dialog (gtk:dialog-new-with-buttons "My dialog"
                                                parent
                                                '(:modal :destroy-with-parent)
                                                "_OK"
                                                :accept
                                                "_Cancel"
                                                :reject)))
      (is (typep dialog 'gtk:dialog))
      (is (string= "My dialog" (gtk:window-title dialog))))))

;;;     gtk_dialog_response
;;;     gtk_dialog_add_button
;;;     gtk_dialog_add_buttons
;;;     gtk_dialog_add_action_widget
;;;     gtk_dialog_set_default_response
;;;     gtk_dialog_set_response_sensitive
;;;     gtk_dialog_get_response_for_widget
;;;     gtk_dialog_get_widget_for_response
;;;     gtk_dialog_get_content_area
;;;     gtk_dialog_get_header_bar

;;; --- 2023-11-1 --------------------------------------------------------------

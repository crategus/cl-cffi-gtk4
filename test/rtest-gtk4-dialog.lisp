(in-package :gtk-test)

(def-suite gtk-dialog :in gtk-suite)
(in-suite gtk-dialog)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkDialogFlags

(test dialog-flags
  ;; Check the type
  (is (g:type-is-flags "GtkDialogFlags"))
  ;; Check the registered name
  (is (eq 'gtk:dialog-flags
          (glib:symbol-for-gtype "GtkDialogFlags")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkDialogFlags")
          (g:gtype (cffi:foreign-funcall "gtk_dialog_flags_get_type" :size))))
  ;; Check the names
  (is (equal '("GTK_DIALOG_MODAL" "GTK_DIALOG_DESTROY_WITH_PARENT"
               "GTK_DIALOG_USE_HEADER_BAR")
             (list-flags-item-name "GtkDialogFlags")))
  ;; Check the values
  (is (equal '(1 2 4)
             (list-flags-item-value "GtkDialogFlags")))
  ;; Check the nick names
  (is (equal '("modal" "destroy-with-parent" "use-header-bar")
             (list-flags-item-nick "GtkDialogFlags")))
  ;; Check the flags definition
  (is (equal '(GOBJECT:DEFINE-G-FLAGS "GtkDialogFlags"
                              GTK-DIALOG-FLAGS
                              (:EXPORT T
                               :TYPE-INITIALIZER "gtk_dialog_flags_get_type")
                              (:MODAL 1)
                              (:DESTROY-WITH-PARENT 2)
                              (:USE-HEADER-BAR 4))
             (gobject:get-g-type-definition "GtkDialogFlags"))))

;;;     GtkResponseType

(test response-type
  ;; Check the type
  (is (g:type-is-enum "GtkResponseType"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkResponseType")
          (g:gtype (cffi:foreign-funcall "gtk_response_type_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gtk:response-type
          (glib:symbol-for-gtype "GtkResponseType")))
  ;; Check the names
  (is (equal '("GTK_RESPONSE_NONE" "GTK_RESPONSE_REJECT" "GTK_RESPONSE_ACCEPT"
               "GTK_RESPONSE_DELETE_EVENT" "GTK_RESPONSE_OK"
               "GTK_RESPONSE_CANCEL" "GTK_RESPONSE_CLOSE" "GTK_RESPONSE_YES"
               "GTK_RESPONSE_NO" "GTK_RESPONSE_APPLY" "GTK_RESPONSE_HELP")
             (list-enum-item-name "GtkResponseType")))
  ;; Check the values
  (is (equal '(-1 -2 -3 -4 -5 -6 -7 -8 -9 -10 -11)
             (list-enum-item-value "GtkResponseType")))
  ;; Check the nick names
  (is (equal '("none" "reject" "accept" "delete-event" "ok" "cancel" "close"
               "yes" "no" "apply" "help")
             (list-enum-item-nick "GtkResponseType")))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkResponseType"
                             GTK-RESPONSE-TYPE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_response_type_get_type")
                             (:NONE -1)
                             (:REJECT -2)
                             (:ACCEPT -3)
                             (:DELETE-EVENT -4)
                             (:OK -5)
                             (:CANCEL -6)
                             (:CLOSE -7)
                             (:YES -8)
                             (:NO -9)
                             (:APPLY -10)
                             (:HELP -11))
             (gobject:get-g-type-definition "GtkResponseType"))))

;;;     GtkDialog

(test dialog-class
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
               "GtkFileChooserDialog" "GtkFontChooserDialog" "GtkMessageDialog"
               "GtkPageSetupUnixDialog" "GtkPrintUnixDialog")
             (list-children "GtkDialog")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget" "GtkNative"
               "GtkShortcutManager" "GtkRoot")
             (list-interfaces "GtkDialog")))
  ;; Check the class properties
  (is (equal '("use-header-bar")
             (list-properties "GtkDialog")))
  ;; Check the list of signals
  (is (equal '("close" "response")
             (list-signals "GtkDialog")))
  ;; CSS information
  (is (string= "window"
               (gtk:widget-class-css-name "GtkDialog")))
  (is (string=
"[window.background.dialog:dir(ltr)]
"
               (gtk:style-context-to-string
                   (gtk:widget-style-context (make-instance 'gtk:dialog))
                   :none)))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkDialog" GTK-DIALOG
                       (:SUPERCLASS GTK-WINDOW :EXPORT T :INTERFACES
                        ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
                         "GtkNative" "GtkRoot" "GtkShortcutManager")
                        :TYPE-INITIALIZER "gtk_dialog_get_type")
                       ((USE-HEADER-BAR GTK-DIALOG-USE-HEADER-BAR
                         "use-header-bar" "gint" T NIL)))
             (gobject:get-g-type-definition "GtkDialog"))))

;;; --- Properties -------------------------------------------------------------

;;;     use-header-bar

(test dialog-properties
  (let ((dialog (make-instance 'gtk:dialog)))
    ;; The default value is not -1.
    (is (= 0 (gtk:dialog-use-header-bar dialog)))))

;;; --- Signals ----------------------------------------------------------------

;;;     close

(test dialog-close-signal
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

(test dialog-response-signal
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

(test dialog-new
  (is (typep (gtk:dialog-new) 'gtk:dialog)))

;;;     gtk_dialog_new_with_buttons

(test dialog-new-with-buttons
  (let* ((parent (gtk:window-new))
         (dialog (gtk:dialog-new-with-buttons "My dialog"
                                              parent
                                              '(:modal :destroy-with-parent)
                                              "_OK"
                                              :accept
                                              "_Cancel"
                                              :reject)))
    (is (typep dialog 'gtk:dialog))
    (is (string= "My dialog" (gtk:window-title dialog)))))

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

;;; --- 2023-5-29 --------------------------------------------------------------

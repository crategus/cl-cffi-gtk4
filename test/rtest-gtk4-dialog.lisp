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
                        :TYPE-INITIALIZER "gtk_dialog_flags_get_type")
                       (:MODAL 1)
                       (:DESTROY-WITH-PARENT 2)
                       (:USE-HEADER-BAR 4))
             (gobject:get-gtype-definition "GtkDialogFlags"))))

;;;     GtkDialog

(test gtk-dialog-class
  (let ((*gtk-warn-deprecated* nil))
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
  (let ((*gtk-warn-deprecated* nil))
    (glib-test:with-check-memory (dialog)
      (setf dialog (make-instance 'gtk:dialog))
      ;; The default value is not -1.
      (is (= 0 (gtk:dialog-use-header-bar dialog)))
      (is-false (gtk:window-destroy dialog)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_dialog_new

(test gtk-dialog-new
  (let ((*gtk-warn-deprecated* nil))
    (glib-test:with-check-memory (dialog)
      (is (typep (setf dialog (gtk:dialog-new)) 'gtk:dialog))
      (is-false (gtk:window-destroy dialog)))))

;;;     gtk_dialog_new_with_buttons

;; FIXME: Creates the following error messages after several runs and when
;; destroying objects. This seems to be a general problem. We have this
;; problem also for an GtkShortcutsWindow widget.

;; GLib-GObject-CRITICAL **:
;; g_signal_handlers_disconnect_matched:
;; assertion 'G_TYPE_CHECK_INSTANCE (instance)' failed

#+nil
(test gtk-dialog-new-with-buttons
  (when *first-run-testsuite*
    (let* ((*gtk-warn-deprecated* nil))
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
      (is-false (gtk:window-destroy parent))
      (is-false (gtk:window-destroy dialog))))))

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

;;; 2024-12-23

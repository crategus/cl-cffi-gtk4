(in-package :gtk-test)

(def-suite gtk-message-dialog :in gtk-deprecated)
(in-suite gtk-message-dialog)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkButtonsType

(test gtk-buttons-type
  ;; Check type
  (is (g:type-is-enum "GtkButtonsType"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkButtonsType")
          (g:gtype (cffi:foreign-funcall "gtk_buttons_type_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:buttons-type
          (glib:symbol-for-gtype "GtkButtonsType")))
  ;; Check names
  (is (equal '("GTK_BUTTONS_NONE" "GTK_BUTTONS_OK" "GTK_BUTTONS_CLOSE"
               "GTK_BUTTONS_CANCEL" "GTK_BUTTONS_YES_NO"
               "GTK_BUTTONS_OK_CANCEL")
             (glib-test:list-enum-item-names "GtkButtonsType")))
  ;; Check values
  (is (equal '(0 1 2 3 4 5)
             (glib-test:list-enum-item-values "GtkButtonsType")))
  ;; Check nick names
  (is (equal '("none" "ok" "close" "cancel" "yes-no" "ok-cancel")
             (glib-test:list-enum-item-nicks "GtkButtonsType")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkButtonsType" GTK:BUTTONS-TYPE
                      (:EXPORT T
                       :TYPE-INITIALIZER "gtk_buttons_type_get_type")
                      (:NONE 0)
                      (:OK 1)
                      (:CLOSE 2)
                      (:CANCEL 3)
                      (:YES-NO 4)
                      (:OK-CANCEL 5))
             (gobject:get-gtype-definition "GtkButtonsType"))))

;;;     GtkMessageDialog

(test gtk-message-dialog-class
  (let ((gtk-init:*warn-deprecated* nil))
    ;; Check type
    (is (g:type-is-object "GtkMessageDialog"))
    ;; Check registered name
    (is (eq 'gtk:message-dialog
            (glib:symbol-for-gtype "GtkMessageDialog")))
    ;; Check type initializer
    (is (eq (g:gtype "GtkMessageDialog")
            (g:gtype (cffi:foreign-funcall "gtk_message_dialog_get_type"
                                           :size))))
    ;; Check parent
    (is (eq (g:gtype "GtkDialog")
            (g:type-parent "GtkMessageDialog")))
    ;; Check children
    (is (equal '()
               (glib-test:list-children "GtkMessageDialog")))
    ;; Check interfaces
    (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
                 "GtkNative" "GtkShortcutManager" "GtkRoot")
               (glib-test:list-interfaces "GtkMessageDialog")))
    ;; Check class properties
    (is (equal '("buttons" "message-area" "message-type" "secondary-text"
                 "secondary-use-markup" "text" "use-markup")
               (glib-test:list-properties "GtkMessageDialog")))
    ;; Check signals
    (is (equal '()
               (glib-test:list-signals "GtkMessageDialog")))
    ;; Check CSS name
    (is (string= "window"
                 (gtk:widget-class-css-name "GtkMessageDialog")))
    ;; Check accessible role
    (is (eq :dialog (gtk:widget-class-accessible-role "GtkMessageDialog")))
    ;; Check class definition
    (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkMessageDialog" GTK:MESSAGE-DIALOG
                        (:SUPERCLASS GTK:DIALOG
                         :EXPORT T
                         :INTERFACES
                         ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
                          "GtkNative" "GtkRoot" "GtkShortcutManager")
                         :TYPE-INITIALIZER "gtk_message_dialog_get_type")
                        ((BUTTONS MESSAGE-DIALOG-BUTTONS
                          "buttons" "GtkButtonsType" NIL NIL)
                         (MESSAGE-AREA MESSAGE-DIALOG-MESSAGE-AREA
                          "message-area" "GtkWidget" T NIL)
                         (MESSAGE-TYPE MESSAGE-DIALOG-MESSAGE-TYPE
                          "message-type" "GtkMessageType" T T)
                         (SECONDARY-TEXT MESSAGE-DIALOG-SECONDARY-TEXT
                          "secondary-text" "gchararray" T T)
                         (SECONDARY-USE-MARKUP
                          MESSAGE-DIALOG-SECONDARY-USE-MARKUP
                          "secondary-use-markup" "gboolean" T T)
                         (TEXT MESSAGE-DIALOG-TEXT "text" "gchararray" T T)
                         (USE-MARKUP MESSAGE-DIALOG-USE-MARKUP
                          "use-markup" "gboolean" T T)))
               (gobject:get-gtype-definition "GtkMessageDialog")))))

;;; --- Properties -------------------------------------------------------------

(test gtk-message-dialog-properties
  (let ((gtk-init:*warn-deprecated* nil))
    (glib-test:with-check-memory (dialog :strong 1)
      (setf dialog (make-instance 'gtk:message-dialog))
      (is (typep  (gtk:message-dialog-message-area dialog) 'gtk:box))
      (is (eq :info (gtk:message-dialog-message-type dialog)))
      (is-false (gtk:message-dialog-secondary-text dialog))
      (is-false (gtk:message-dialog-secondary-use-markup dialog))
      (is (string= "" (gtk:message-dialog-text dialog)))
      (is-false (gtk:message-dialog-use-markup dialog))
      (is-false (gtk:window-destroy dialog)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_message_dialog_new

(test gtk-message-dialog-new
  (let ((gtk-init:*warn-deprecated* nil))
    (glib-test:with-check-memory (dialog parent)
      (setf parent (gtk:window-new))
      (setf dialog
            (gtk:message-dialog-new parent '(:modal) :info :ok "message"))
      (is (string= "message" (gtk:message-dialog-text dialog)))
      (is-false (gtk:message-dialog-use-markup dialog))
      (is-false (gtk:message-dialog-secondary-text dialog))
      (is-false (gtk:window-destroy dialog))
      (is-false (gtk:window-destroy parent)))))

;;;     gtk_message_dialog_new_with_markup

(test gtk-message-dialog-new-with-markup
  (let ((gtk-init:*warn-deprecated* nil))
    (glib-test:with-check-memory (dialog parent)
      (setf parent (gtk:window-new))
      (setf dialog
            (gtk:message-dialog-new-with-markup parent
                                                '(:modal) :info :ok
                                                "<b>message</b>"))
      (is (string= "<b>message</b>" (gtk:message-dialog-text dialog)))
      (is-true (gtk:message-dialog-use-markup dialog))
      (is-false (gtk:message-dialog-secondary-text dialog))
      (is-false (gtk:window-destroy dialog))
      (is-false (gtk:window-destroy parent)))))

;;;     gtk_message_dialog_set_markup
;;;     gtk_message_dialog_format_secondary_text
;;;     gtk_message_dialog_format_secondary_markup

(test gtk-message-dialog-format-secondary-text/markup
  (let ((gtk-init:*warn-deprecated* nil))
    (glib-test:with-check-memory (dialog parent)
      (setf parent (gtk:window-new))
      (setf dialog
            (gtk:message-dialog-new parent '(:modal) :info :ok ""))
      (is (string= "<b>message</b>"
                   (gtk:message-dialog-set-markup dialog "<b>message</b>")))
      (is (string= "<b>message</b>" (gtk:message-dialog-text dialog)))
      (is-true "<b>message</b>" (gtk:message-dialog-use-markup dialog))
      (is (string= "message"
                   (gtk:message-dialog-format-secondary-text dialog "message")))
      (is (string= "message" (gtk:message-dialog-secondary-text dialog)))
      (is-false (gtk:message-dialog-secondary-use-markup dialog))
      (is (string= "message"
                   (gtk:message-dialog-format-secondary-markup dialog "message")))
      (is (string= "message" (gtk:message-dialog-secondary-text dialog)))
      (is-true (gtk:message-dialog-secondary-use-markup dialog))
      (is-false (gtk:window-destroy dialog))
      (is-false (gtk:window-destroy parent)))))

;;; 2026-08-20

(in-package :gtk-test)

(def-suite gtk-message-dialog :in gtk-suite)
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
             (list-enum-item-name "GtkButtonsType")))
  ;; Check values
  (is (equal '(0 1 2 3 4 5)
             (list-enum-item-value "GtkButtonsType")))
  ;; Check nick names
  (is (equal '("none" "ok" "close" "cancel" "yes-no" "ok-cancel")
             (list-enum-item-nick "GtkButtonsType")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkButtonsType"
                             GTK-BUTTONS-TYPE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_buttons_type_get_type")
                          (:NONE 0)
                          (:OK 1)
                          (:CLOSE 2)
                          (:CANCEL 3)
                          (:YES-NO 4)
                          (:OK-CANCEL 5))
             (gobject:get-g-type-definition "GtkButtonsType"))))

;;;     GtkMessageDialog

(test gtk-message-dialog-class
  (let ((*gtk-warn-deprecated* nil))
    ;; Type check
    (is (g:type-is-object "GtkMessageDialog"))
    ;; Check registered name
    (is (eq 'gtk:message-dialog
            (glib:symbol-for-gtype "GtkMessageDialog")))
    ;; Check the type initializer
    (is (eq (g:gtype "GtkMessageDialog")
            (g:gtype (cffi:foreign-funcall "gtk_message_dialog_get_type"
                                           :size))))
    ;; Check parent
    (is (eq (g:gtype "GtkDialog")
            (g:type-parent "GtkMessageDialog")))
    ;; Check children
    (is (equal '()
               (list-children "GtkMessageDialog")))
    ;; Check interfaces
    (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
                 "GtkNative" "GtkShortcutManager" "GtkRoot")
               (list-interfaces "GtkMessageDialog")))
    ;; Check class properties
    (is (equal '("buttons" "message-area" "message-type" "secondary-text"
                 "secondary-use-markup" "text" "use-markup")
               (list-properties "GtkMessageDialog")))
    ;; Check list of signals
    (is (equal '()
               (list-signals "GtkMessageDialog")))
    ;; Check CSS name
    (is (string= "window"
                 (gtk:widget-class-css-name "GtkMessageDialog")))
    ;; Check CSS classes
    #-windows
    (is (equal '("background" "csd" "dialog" "message")
               (gtk:widget-css-classes (make-instance 'gtk:message-dialog))))
    #+windows
    (is (equal '("background" "dialog" "message")
               (gtk:widget-css-classes (make-instance 'gtk:message-dialog))))
    ;; Check accessible role
    (is (eq :dialog (gtk:widget-class-accessible-role "GtkMessageDialog")))
    ;; Check class definition
    (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkMessageDialog"
                                               GTK-MESSAGE-DIALOG
                         (:SUPERCLASS GTK-DIALOG :EXPORT T :INTERFACES
                          ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
                           "GtkNative" "GtkRoot" "GtkShortcutManager")
                          :TYPE-INITIALIZER "gtk_message_dialog_get_type")
                         ((BUTTONS GTK-MESSAGE-DIALOG-BUTTONS "buttons"
                           "GtkButtonsType" NIL NIL)
                          (MESSAGE-AREA GTK-MESSAGE-DIALOG-MESSAGE-AREA
                           "message-area" "GtkWidget" T NIL)
                          (MESSAGE-TYPE GTK-MESSAGE-DIALOG-MESSAGE-TYPE
                           "message-type" "GtkMessageType" T T)
                          (SECONDARY-TEXT GTK-MESSAGE-DIALOG-SECONDARY-TEXT
                           "secondary-text" "gchararray" T T)
                          (SECONDARY-USE-MARKUP
                           GTK-MESSAGE-DIALOG-SECONDARY-USE-MARKUP
                           "secondary-use-markup" "gboolean" T T)
                          (TEXT GTK-MESSAGE-DIALOG-TEXT "text" "gchararray" T T)
                          (USE-MARKUP GTK-MESSAGE-DIALOG-USE-MARKUP "use-markup"
                           "gboolean" T T)))
               (gobject:get-g-type-definition "GtkMessageDialog")))))

;;; --- Properties -------------------------------------------------------------

(test gtk-message-dialog-properties
  (let ((*gtk-warn-deprecated* nil))
    (let ((dialog (make-instance 'gtk:message-dialog)))
      (is (typep  (gtk:message-dialog-message-area dialog) 'gtk:box))
      (is (eq :info (gtk:message-dialog-message-type dialog)))
      (is-false (gtk:message-dialog-secondary-text dialog))
      (is-false (gtk:message-dialog-secondary-use-markup dialog))
      (is (string= "" (gtk:message-dialog-text dialog)))
      (is-false (gtk:message-dialog-use-markup dialog)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_message_dialog_new
;;;     gtk_message_dialog_new_with_markup
;;;     gtk_message_dialog_set_markup
;;;     gtk_message_dialog_format_secondary_text
;;;     gtk_message_dialog_format_secondary_markup

;;; 2024-5-1

(in-package :gtk-test)

(def-suite gtk-alert-dialog :in gtk-suite)
(in-suite gtk-alert-dialog)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkAlertDialog

(test gtk-alert-dialog-class
  ;; Type check
  (is (g:type-is-object "GtkAlertDialog"))
  ;; Check the registered name
  (is (eq 'gtk:alert-dialog
          (glib:symbol-for-gtype "GtkAlertDialog")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkAlertDialog")
          (g:gtype (cffi:foreign-funcall "gtk_alert_dialog_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkAlertDialog")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkAlertDialog")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkAlertDialog")))
  ;; Check the properties
  (is (equal '("buttons" "cancel-button" "default-button" "detail" "message"
               "modal")
             (list-properties "GtkAlertDialog")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkAlertDialog")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkAlertDialog" GTK-ALERT-DIALOG
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                                :TYPE-INITIALIZER "gtk_alert_dialog_get_type")
                               ((BUTTONS GTK-ALERT-DIALOG-BUTTONS "buttons"
                                 "GStrv" T T)
                                (CANCEL-BUTTON GTK-ALERT-DIALOG-CANCEL-BUTTON
                                 "cancel-button" "gint" T T)
                                (DEFAULT-BUTTON GTK-ALERT-DIALOG-DEFAULT-BUTTON
                                 "default-button" "gint" T T)
                                (DETAIL GTK-ALERT-DIALOG-DETAIL "detail"
                                 "gchararray" T T)
                                (MESSAGE GTK-ALERT-DIALOG-MESSAGE "message"
                                 "gchararray" T T)
                                (MODAL GTK-ALERT-DIALOG-MODAL "modal"
                                 "gboolean" T T)))
             (gobject:get-g-type-definition "GtkAlertDialog"))))

;;; --- Properties -------------------------------------------------------------

;;;     buttons
;;;     cancel-button
;;;     default-button
;;;     detail
;;;     message

(test gtk-alert-dialog-properties.1
  (let ((dialog (make-instance 'gtk:alert-dialog)))
    (is-false (gtk:alert-dialog-buttons dialog))
    (is (eq -1 (gtk:alert-dialog-cancel-button dialog)))
    (is (eq -1 (gtk:alert-dialog-default-button dialog)))
    (is-false (gtk:alert-dialog-detail dialog))
    (is-false (gtk:alert-dialog-message dialog))))

(test gtk-alert-dialog-properties.2
  (let ((dialog (make-instance 'gtk:alert-dialog)))
    (is (equal '("close" "ok")
               (setf (gtk:alert-dialog-buttons dialog) '("close" "ok"))))
    (is (equal '("close" "ok")
               (gtk:alert-dialog-buttons dialog)))
    (is (= 0 (setf (gtk:alert-dialog-cancel-button dialog) 0)))
    (is (= 0 (gtk:alert-dialog-cancel-button dialog)))
    (is (= 1 (setf (gtk:alert-dialog-cancel-button dialog) 1)))
    (is (= 1 (gtk:alert-dialog-cancel-button dialog)))
    (is (string= "detail"
                 (setf (gtk:alert-dialog-detail dialog) "detail")))
    (is (string= "detail" (gtk:alert-dialog-detail dialog)))
    (is (string= "message"
                 (setf (gtk:alert-dialog-message dialog) "message")))
    (is (string= "message" (gtk:alert-dialog-message dialog)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_alert_dialog_new

(test gtk-alert-dialog-new
  (is (typep (gtk:alert-dialog-new nil) 'gtk:alert-dialog))
  (is (typep (gtk:alert-dialog-new "msg") 'gtk:alert-dialog))
  (is (typep (gtk:alert-dialog-new "msg ~a~%" 199) 'gtk:alert-dialog)))

;;;     gtk_alert_dialog_choose
;;;     gtk_alert_dialog_choose_finish
;;;     gtk_alert_dialog_show

;;; --- 2023-9-20 --------------------------------------------------------------

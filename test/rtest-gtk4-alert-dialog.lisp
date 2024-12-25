(in-package :gtk-test)

(def-suite gtk-alert-dialog :in gtk-windows)
(in-suite gtk-alert-dialog)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkAlertDialog

(test gtk-alert-dialog-class
  ;; Check type
  (is (g:type-is-object "GtkAlertDialog"))
  ;; Check registered name
  (is (eq 'gtk:alert-dialog
          (glib:symbol-for-gtype "GtkAlertDialog")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkAlertDialog")
          (g:gtype (cffi:foreign-funcall "gtk_alert_dialog_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkAlertDialog")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkAlertDialog")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkAlertDialog")))
  ;; Check properties
  (is (equal '("buttons" "cancel-button" "default-button" "detail" "message"
               "modal")
             (glib-test:list-properties "GtkAlertDialog")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkAlertDialog")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkAlertDialog" GTK:ALERT-DIALOG
                      (:SUPERCLASS GOBJECT:OBJECT
                       :EXPORT T
                       :INTERFACES NIL
                       :TYPE-INITIALIZER "gtk_alert_dialog_get_type")
                      ((BUTTONS ALERT-DIALOG-BUTTONS "buttons" "GStrv" T T)
                       (CANCEL-BUTTON ALERT-DIALOG-CANCEL-BUTTON
                        "cancel-button" "gint" T T)
                       (DEFAULT-BUTTON ALERT-DIALOG-DEFAULT-BUTTON
                        "default-button" "gint" T T)
                       (DETAIL ALERT-DIALOG-DETAIL "detail" "gchararray" T T)
                       (MESSAGE ALERT-DIALOG-MESSAGE "message" "gchararray" T T)
                       (MODAL ALERT-DIALOG-MODAL "modal" "gboolean" T T)))
             (gobject:get-gtype-definition "GtkAlertDialog"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-alert-dialog-properties.1
  (glib-test:with-check-memory (dialog)
    (setf dialog (make-instance 'gtk:alert-dialog))
    (is-false (gtk:alert-dialog-buttons dialog))
    (is (eq -1 (gtk:alert-dialog-cancel-button dialog)))
    (is (eq -1 (gtk:alert-dialog-default-button dialog)))
    (is-false (gtk:alert-dialog-detail dialog))
    (is-false (gtk:alert-dialog-message dialog))))

(test gtk-alert-dialog-properties.2
  (glib-test:with-check-memory (dialog)
    (setf dialog (make-instance 'gtk:alert-dialog))
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
  (glib-test:with-check-memory ()
    (is (typep (gtk:alert-dialog-new nil) 'gtk:alert-dialog))
    (is (typep (gtk:alert-dialog-new "msg") 'gtk:alert-dialog))
    (is (typep (gtk:alert-dialog-new "msg ~a~%" 199) 'gtk:alert-dialog))))

;;;     gtk_alert_dialog_choose
;;;     gtk_alert_dialog_choose_finish
;;;     gtk_alert_dialog_show

;;; 2024-12-23

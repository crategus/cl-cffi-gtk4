(in-package :gtk-test)

(def-suite gtk-app-chooser :in gtk-deprecated)
(in-suite gtk-app-chooser)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkAppChooser

(test gtk-app-chooser-interface
  ;; Check type
  (is (g:type-is-interface "GtkAppChooser"))
  ;; Check registered name
  (is (eq 'gtk:app-chooser
          (glib:symbol-for-gtype "GtkAppChooser")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkAppChooser")
          (g:gtype (cffi:foreign-funcall "gtk_app_chooser_get_type" :size))))
  ;; Check interface prerequisites
  (is (equal '("GtkWidget")
             (glib-test:list-interface-prerequisites "GtkAppChooser")))
  ;; Check interface properties
  (is (equal '("content-type")
             (glib-test:list-interface-properties "GtkAppChooser")))
  ;; Check interface signals
  (is (equal '()
             (glib-test:list-signals "GtkAppChooser")))
  ;; Check interface definition
  (is (equal '(GOBJECT:DEFINE-GINTERFACE "GtkAppChooser" GTK:APP-CHOOSER
                      (:EXPORT T
                       :TYPE-INITIALIZER "gtk_app_chooser_get_type")
                      (CONTENT-TYPE APP-CHOOSER-CONTENT-TYPE
                       "content-type" "gchararray" T NIL))
             (gobject:get-gtype-definition "GtkAppChooser"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-app-chooser-properties
  (let ((gtk-init:*warn-deprecated* nil))
    (glib-test:with-check-memory (button)
      (setf button (make-instance 'gtk:app-chooser-button))
      (is-false (gtk:app-chooser-content-type button)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_app_chooser_get_app_info

#-windows
(test gtk-app-chooser-app-info
  (when *first-run-testsuite*
    (let ((gtk-init:*warn-deprecated* nil))
      (glib-test:with-check-memory (button :strong 1)
        (setf button (gtk:app-chooser-button-new "text/plain"))
        (is (typep (gtk:app-chooser-app-info button) 'g:object))))))

;;;     gtk_app_chooser_refresh

#-windows
(test gtk-app-chooser-refresh
  (when *first-run-testsuite*
    (let ((gtk-init:*warn-deprecated* nil))
      (glib-test:with-check-memory (button :strong 1)
        (setf button (gtk:app-chooser-button-new "text/plain"))
        (is-false (gtk:app-chooser-refresh button))
        (is (typep (gtk:app-chooser-app-info button) 'g:object))))))

;;; 2024-12-25

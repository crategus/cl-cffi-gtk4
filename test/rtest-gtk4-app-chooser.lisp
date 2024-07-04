(in-package :gtk-test)

(def-suite gtk-app-chooser :in gtk-suite)
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
             (gtk-test:list-interface-prerequisites "GtkAppChooser")))
  ;; Check interface properties
  (is (equal '("content-type")
             (gtk-test:list-interface-properties "GtkAppChooser")))
  ;; Check interface signals
  (is (equal '()
             (gtk-test:list-signals "GtkAppChooser")))
  ;; Check interface definition
  (is (equal '(GOBJECT:DEFINE-G-INTERFACE "GtkAppChooser" GTK-APP-CHOOSER
                            (:EXPORT T :TYPE-INITIALIZER
                             "gtk_app_chooser_get_type")
                            (CONTENT-TYPE GTK-APP-CHOOSER-CONTENT-TYPE
                             "content-type" "gchararray" T NIL))
             (gobject:get-g-type-definition "GtkAppChooser"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-app-chooser-properties
  (let ((gtk-init:*gtk-warn-deprecated* nil))
    (let ((button (make-instance 'gtk:app-chooser-button)))
      (is-false (gtk:app-chooser-content-type button)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_app_chooser_get_app_info

#-windows
(test gtk-app-chooser-app-info
  (let ((gtk-init:*gtk-warn-deprecated* nil))
    (let ((button (gtk:app-chooser-button-new "text/plain")))
      (is (typep (gtk:app-chooser-app-info button) 'g:object)))))

;;;     gtk_app_chooser_refresh

#-windows
(test gtk-app-chooser-refresh
  (let ((gtk-init:*gtk-warn-deprecated* nil))
    (let ((button (gtk:app-chooser-button-new "text/plain")))
      (is-false (gtk:app-chooser-refresh button))
      (is (typep (gtk:app-chooser-app-info button) 'g:object)))))

;;; 2024-4-26

(in-package :gtk-test)

(def-suite gtk-app-chooser :in gtk-suite)
(in-suite gtk-app-chooser)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkAppChooser

(test gtk-app-chooser-interface
  ;; Type check
  (is (g:type-is-interface "GtkAppChooser"))
  ;; Check the registered name
  (is (eq 'gtk:app-chooser
          (glib:symbol-for-gtype "GtkAppChooser")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkAppChooser")
          (g:gtype (cffi:foreign-funcall "gtk_app_chooser_get_type" :size))))
  ;; Check the interface prerequisites
  (is (equal '("GtkWidget")
             (list-interface-prerequisites "GtkAppChooser")))
  ;; Check the interface properties
  (is (equal '("content-type")
             (list-interface-properties "GtkAppChooser")))
  ;; Check the interface signals
  (is (equal '()
             (list-signals "GtkAppChooser")))
  ;; Get the interface definition
  (is (equal '(GOBJECT:DEFINE-G-INTERFACE "GtkAppChooser" GTK-APP-CHOOSER
                            (:EXPORT T :TYPE-INITIALIZER
                             "gtk_app_chooser_get_type")
                            (CONTENT-TYPE GTK-APP-CHOOSER-CONTENT-TYPE
                             "content-type" "gchararray" T NIL))
             (gobject:get-g-type-definition "GtkAppChooser"))))

;;; --- Properties -------------------------------------------------------------

;;;     content-type

;;; --- Functions --------------------------------------------------------------

;;;     gtk_app_chooser_get_app_info
;;;     gtk_app_chooser_refresh

;;; --- 2023-8-29 --------------------------------------------------------------

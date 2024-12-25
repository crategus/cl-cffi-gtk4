(in-package :gtk-test)

(def-suite gtk-spinner :in gtk-display-widgets)
(in-suite gtk-spinner)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkSpinner

(test gtk-spinner-class
  ;; Check type
  (is (g:type-is-object "GtkSpinner"))
  ;; Check registered name
  (is (eq 'gtk:spinner
          (glib:symbol-for-gtype "GtkSpinner")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkSpinner")
          (g:gtype (cffi:foreign-funcall "gtk_spinner_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget") (g:type-parent "GtkSpinner")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkSpinner")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (glib-test:list-interfaces "GtkSpinner")))
  ;; Check properties
  (is (equal '("spinning")
             (glib-test:list-properties "GtkSpinner")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkSpinner")))
  ;; Checkk CSS name
  (is (string= "spinner"
               (gtk:widget-class-css-name "GtkSpinner")))
  ;; Check accessible role
  (is (eq :widget (gtk:widget-class-accessible-role "GtkSpinner")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkSpinner" GTK:SPINNER
                       (:SUPERCLASS GTK:WIDGET
                        :EXPORT T
                        :INTERFACES
                        ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
                        :TYPE-INITIALIZER "gtk_spinner_get_type")
                       ((SPINNING SPINNER-SPINNING "spinning" "gboolean" T T)))
             (gobject:get-gtype-definition "GtkSpinner"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-spinner-properties
  (let ((spinner (make-instance 'gtk:spinner)))
    (is-false (gtk:spinner-spinning spinner))
    (is-true  (setf (gtk:spinner-spinning spinner) t))
    (is-true (gtk:spinner-spinning spinner))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_spinner_new

(test gtk-spinner-new
  (is (typep (gtk:spinner-new) 'gtk:spinner)))

;;;     gtk_spinner_start
;;;     gtk_spinner_stop

(test gtk-spinner-start/stop
  (let ((spinner (make-instance 'gtk:spinner)))
    (is-false (gtk:spinner-spinning spinner))
    (is-false (gtk:spinner-start spinner))
    (is-true (gtk:spinner-spinning spinner))
    (is-false (gtk:spinner-stop spinner))
    (is-false (gtk:spinner-spinning spinner))))

;;; 2024-9-20

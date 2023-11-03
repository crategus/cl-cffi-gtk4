(in-package :gtk-test)

(def-suite gtk-spinner :in gtk-suite)
(in-suite gtk-spinner)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkSpinner

(test gtk-spinner-class
  ;; Type check
  (is (g:type-is-object "GtkSpinner"))
  ;; Check the registered name
  (is (eq 'gtk:spinner
          (glib:symbol-for-gtype "GtkSpinner")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkSpinner")
          (g:gtype (cffi:foreign-funcall "gtk_spinner_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget") (g:type-parent "GtkSpinner")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkSpinner")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (list-interfaces "GtkSpinner")))
  ;; Check the properties
  (is (equal '("spinning")
             (list-properties "GtkSpinner")))
  ;; Check the list of signals
  (is (equal '()
             (list-signals "GtkSpinner")))
  ;; CSS name
  (is (string= "spinner"
               (gtk:widget-class-css-name "GtkSpinner")))
  ;; CSS classes
  (is (equal '()
             (gtk:widget-css-classes (make-instance 'gtk:spinner))))
  ;; Accessible role
  (is (eq :widget (gtk:widget-class-accessible-role "GtkSpinner")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkSpinner" GTK-SPINNER
                       (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                        ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
                        :TYPE-INITIALIZER "gtk_spinner_get_type")
                       ((SPINNING GTK-SPINNER-SPINNING "spinning" "gboolean" T
                         T)))
             (gobject:get-g-type-definition "GtkSpinner"))))

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

;;; --- 2023-8-24 --------------------------------------------------------------

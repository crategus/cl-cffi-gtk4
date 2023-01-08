(def-suite gtk-spinner :in gtk-suite)
(in-suite gtk-spinner)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkSpinner

(test gtk-spinner-class
  ;; Type check
  (is (g:type-is-object "GtkSpinner"))
  ;; Check the registered name
  (is (eq 'gtk-spinner
          (gobject:symbol-for-gtype "GtkSpinner")))
  ;; Check the type initializer
  (is (eq (gtype "GtkSpinner")
          (gtype (foreign-funcall "gtk_spinner_get_type" g-size))))
  ;; Check the parent
  (is (eq (gtype "GtkWidget") (g-type-parent "GtkSpinner")))
  ;; Check the children
  (is (equal '()
             (mapcar #'g-type-name (g-type-children "GtkSpinner"))))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (mapcar #'g-type-name (g-type-interfaces "GtkSpinner"))))
  ;; Check the class properties
  (is (equal '("spinning")
             (list-class-property-names "GtkSpinner")))
  ;; Check the list of signals
  (is (equal '()
             (sort (mapcar #'g-signal-name
                           (g-signal-list-ids "GtkSpinner"))
                   #'string<)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkSpinner" GTK-SPINNER
                       (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                        ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
                        :TYPE-INITIALIZER "gtk_spinner_get_type")
                       ((SPINNING GTK-SPINNER-SPINNING "spinning" "gboolean" T
                         T)))
             (get-g-type-definition "GtkSpinner"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-spinner-properties
  (let ((spinner (make-instance 'gtk-spinner)))
    (is-false (gtk-spinner-spinning spinner))
    (is-true  (setf (gtk-spinner-spinning spinner) t))
    (is-true (gtk-spinner-spinning spinner))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_spinner_new

(test gtk-spinner-new
  (is (typep (gtk-spinner-new) 'gtk-spinner)))

;;;     gtk_spinner_start
;;;     gtk_spinner_stop

(test gtk-spinner-start/stop
  (let ((spinner (make-instance 'gtk-spinner)))
    (is-false (gtk-spinner-spinning spinner))
    (is-false (gtk-spinner-start spinner))
    (is-true (gtk-spinner-spinning spinner))
    (is-false (gtk-spinner-stop spinner))
    (is-false (gtk-spinner-spinning spinner))))

;;; 2022-5-27

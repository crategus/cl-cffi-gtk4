(in-package :gtk-test)

(def-suite gtk-separator :in gtk-suite)
(in-suite gtk-separator)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkSeparator

(test gtk-separator-class
  ;; Type check
  (is (g:type-is-object "GtkSeparator"))
  ;; Check the registered name
  (is (eq 'gtk:separator
          (glib:symbol-for-gtype "GtkSeparator")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkSeparator")
          (g:gtype (cffi:foreign-funcall "gtk_separator_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkSeparator")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkSeparator")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkOrientable")
             (list-interfaces "GtkSeparator")))
  ;; Check the properties
  (is (equal '("orientation")
             (list-properties "GtkSeparator")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkSeparator")))
  ;; CSS name
  (is (string= "separator"
               (gtk:widget-class-css-name "GtkSeparator")))
  ;; CSS classes
  (is (equal '("horizontal")
             (gtk:widget-css-classes (make-instance 'gtk:separator))))
  ;; Accessible role
  (is (eq :separator (gtk:widget-class-accessible-role "GtkSeparator")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkSeparator" GTK-SEPARATOR
                       (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                        ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
                         "GtkOrientable")
                        :TYPE-INITIALIZER "gtk_separator_get_type")
                       NIL)
             (gobject:get-g-type-definition "GtkSeparator"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_separator_new

;;; --- 2023-5-29 --------------------------------------------------------------

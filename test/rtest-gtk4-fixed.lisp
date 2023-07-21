(in-package :gtk-test)

(def-suite gtk-fixed :in gtk-suite)
(in-suite gtk-fixed)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFixed

(test fixed-class
  ;; Type check
  (is (g:type-is-object "GtkFixed"))
  ;; Check the registered name
  (is (eq 'gtk:fixed
          (glib:symbol-for-gtype "GtkFixed")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkFixed")
          (g:gtype (cffi:foreign-funcall "gtk_fixed_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkFixed")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkFixed")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (list-interfaces "GtkFixed")))
  ;; Check the properties
  (is (equal '()
             (list-properties "GtkFixed")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkFixed")))
  ;; CSS information
  (is (string= "widget"
               (gtk:widget-class-css-name "GtkFixed")))
  (is (string=
"widget:dir(ltr)
"
               (gtk:style-context-to-string
                   (gtk:widget-style-context (make-instance 'gtk:fixed))
                   :none)))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkFixed" GTK-FIXED
                       (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                        ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
                        :TYPE-INITIALIZER "gtk_fixed_get_type")
                       NIL)
             (gobject:get-g-type-definition "GtkFixed"))))


;;; --- Functions --------------------------------------------------------------

;;;     gtk_fixed_new
;;;     gtk_fixed_put
;;;     gtk_fixed_remove
;;;     gtk_fixed_move
;;;     gtk_fixed_get_child_position
;;;     gtk_fixed_get_child_transform
;;;     gtk_fixed_set_child_transform

;;; --- 2023-5-29 --------------------------------------------------------------

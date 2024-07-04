(in-package :gtk-test)

(def-suite gtk-statusbar :in gtk-suite)
(in-suite gtk-statusbar)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkStatusbar

(test gtk-statusbar-class
  ;; Check type
  (is (g:type-is-object "GtkStatusbar"))
  ;; Check registered name
  (is (eq 'gtk:statusbar
          (glib:symbol-for-gtype "GtkStatusbar")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkStatusbar")
          (g:gtype (cffi:foreign-funcall "gtk_statusbar_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkStatusbar")))
  ;; Check children
  (is (equal '()
             (gtk-test:list-children "GtkStatusbar")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (gtk-test:list-interfaces "GtkStatusbar")))
  ;; Check properties
  (is (equal '()
             (gtk-test:list-properties "GtkStatusbar")))
  ;; Check signals
  (is (equal '("text-popped" "text-pushed")
             (gtk-test:list-signals "GtkStatusbar")))
  ;; Check CSS name
  (is (string= "statusbar"
               (gtk:widget-class-css-name "GtkStatusbar")))
  ;; Check accessible role
  (is (eq :widget (gtk:widget-class-accessible-role "GtkStatusbar")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkStatusbar" GTK-STATUSBAR
                               (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                                ("GtkAccessible" "GtkBuildable"
                                 "GtkConstraintTarget")
                                :TYPE-INITIALIZER "gtk_statusbar_get_type")
                               NIL)
             (gobject:get-g-type-definition "GtkStatusbar"))))

;;; --- Signals ----------------------------------------------------------------

;;;     text-popped
;;;     text-pushed

;;; --- Functions --------------------------------------------------------------

;;;     gtk_statusbar_new

(test gtk-statusbar-new
  (let ((gtk-init:*gtk-warn-deprecated* nil))
    (is (typep (gtk:statusbar-new) 'gtk:statusbar))))

;;;     gtk_statusbar_get_context_id
;;;     gtk_statusbar_push
;;;     gtk_statusbar_pop
;;;     gtk_statusbar_remove
;;;     gtk_statusbar_remove_all

;;; 2024-5-18

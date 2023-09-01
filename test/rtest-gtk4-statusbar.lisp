(in-package :gtk-test)

(def-suite gtk-statusbar :in gtk-suite)
(in-suite gtk-statusbar)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkStatusbar

(test gtk-statusbar-class
  ;; Type check
  (is (g:type-is-object "GtkStatusbar"))
  ;; Check the registered name
  (is (eq 'gtk:statusbar
          (glib:symbol-for-gtype "GtkStatusbar")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkStatusbar")
          (g:gtype (cffi:foreign-funcall "gtk_statusbar_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkStatusbar")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkStatusbar")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (list-interfaces "GtkStatusbar")))
  ;; Check the properties
  (is (equal '()
             (list-properties "GtkStatusbar")))
  ;; Check the signals
  (is (equal '("text-popped" "text-pushed")
             (list-signals "GtkStatusbar")))
  ;; CSS name
  (is (string= "statusbar"
               (gtk:widget-class-css-name "GtkStatusbar")))
  ;; CSS style context
  (is (string=
"statusbar:dir(ltr)
  box.horizontal:dir(ltr)
    label:dir(ltr)
"
               (print-style-context "GtkStatusbar")))
  ;; Check the class definition
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
  (is (typep (gtk:statusbar-new) 'gtk:statusbar)))

;;;     gtk_statusbar_get_context_id
;;;     gtk_statusbar_push
;;;     gtk_statusbar_pop
;;;     gtk_statusbar_remove
;;;     gtk_statusbar_remove_all

;;; --- 2023-8-24 --------------------------------------------------------------

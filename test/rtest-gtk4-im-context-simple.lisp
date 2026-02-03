(in-package :gtk-test)

(def-suite gtk-im-context-simple :in gtk-input-methods)
(in-suite gtk-im-context-simple)

;;; --- Types and Values -------------------------------------------------------

;;;     GTK_MAX_COMPOSE_LEN

;;;     GtkIMContextSimple

(test gtk-im-context-simple-class
  ;; Check type
  (is (g:type-is-object "GtkIMContextSimple"))
  ;; Check registered name
  (is (eq 'gtk:im-context-simple
          (glib:symbol-for-gtype "GtkIMContextSimple")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkIMContextSimple")
          (g:gtype (cffi:foreign-funcall "gtk_im_context_simple_get_type"
                                         :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkIMContext")
          (g:type-parent "GtkIMContextSimple")))
  ;; Check children
  #+crategus
  (is (equal '("GtkIMContextBroadway")
             (glib-test:list-children "GtkIMContextSimple")))
  #+windows
  (is (equal '()
             (glib-test:list-children "GtkIMContextSimple")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkIMContextSimple")))
  ;; Check class properties
  (is (equal '()
             (glib-test:list-properties "GtkIMContextSimple")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkIMContextSimple")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkIMContextSimple" GTK:IM-CONTEXT-SIMPLE
                       (:SUPERCLASS GTK:IM-CONTEXT
                        :EXPORT T
                        :INTERFACES NIL)
                       NIL)
             (gobject:get-gtype-definition "GtkIMContextSimple"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_im_context_simple_new

(test gtk-im-context-simple-new
  (let ((method (gtk:im-context-simple-new)))
    (is (typep method 'gtk:im-context-simple))
    (is (equal '() (gtk:im-context-input-hints method)))
    (is (eq :free-form (gtk:im-context-input-purpose method)))))

;;;     gtk_im_context_simple_add_table                     Deprecated 4.4

;;;     gtk_im_context_simple_add_compose_file

(test gtk-im-context-simple-add-compose-file
  (glib-test:with-check-memory (context)
    (let ((file "/usr/share/X11/locale/am_ET.UTF-8/Compose"))
      (is (typep (setf context
                       (gtk:im-context-simple-new)) 'gtk:im-context-simple))
      (is-false (gtk:im-context-simple-add-compose-file context file)))))

;;; 2026-01-31

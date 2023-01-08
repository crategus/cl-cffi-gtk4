(def-suite gtk-im-context-simple :in gtk-suite)
(in-suite gtk-im-context-simple)

;;; --- Types and Values -------------------------------------------------------

;;;     GTK_MAX_COMPOSE_LEN

;;;     GtkIMContextSimple

(test gtk-im-context-simple-class
  ;; Type check
  (is (g:type-is-object "GtkIMContextSimple"))
  ;; Check the registered name
  (is (eq 'gtk-im-context-simple
          (gobject:symbol-for-gtype "GtkIMContextSimple")))
  ;; Check the type initializer
  (is (eq (gtype "GtkIMContextSimple")
          (gtype (foreign-funcall "gtk_im_context_simple_get_type" g-size))))
  ;; Check the parent
  (is (eq (gtype "GtkIMContext")
          (g-type-parent "GtkIMContextSimple")))
  ;; Check the children
  (is (equal '("GtkIMContextWayland")
             (mapcar #'g-type-name (g-type-children "GtkIMContextSimple"))))
  ;; Check the interfaces
  (is (equal '()
             (mapcar #'g-type-name (g-type-interfaces "GtkIMContextSimple"))))
  ;; Check the class properties
  (is (equal '()
             (list-class-property-names "GtkIMContextSimple")))
  ;; Check the list of signals
  (is (equal '()
             (sort (mapcar #'g-signal-name
                           (g-signal-list-ids "GtkIMContextSimple"))
                   #'string<)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkIMContextSimple" GTK-I-M-CONTEXT-SIMPLE
                       (:SUPERCLASS GTK-I-M-CONTEXT :EXPORT T :INTERFACES NIL)
                       NIL)
             (get-g-type-definition "GtkIMContextSimple"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_im_context_simple_new

(test gtk-im-context-simple-new
  (let ((method (gtk-im-context-simple-new)))
    (is (typep method 'gtk-im-context-simple))
    (is (equal '() (gtk-im-context-input-hints method)))
    (is (eq :free-form (gtk-im-context-input-purpose method)))))

;;;     gtk_im_context_simple_add_table
;;;     gtk_im_context_simple_add_compose_file

;;; 2022-7-10

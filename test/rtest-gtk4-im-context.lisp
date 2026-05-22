(in-package :gtk-test)

(def-suite gtk-im-context :in gtk-input-methods)
(in-suite gtk-im-context)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkIMContext

(test gtk-im-context-class
  ;; Check type
  (is (g:type-is-object "GtkIMContext"))
  ;; Check registered name
  (is (eq 'gtk:im-context
          (glib:symbol-for-gtype "GtkIMContext")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkIMContext")
          (g:gtype (cffi:foreign-funcall "gtk_im_context_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkIMContext")))
  ;; Check children
  #-windows
  (is (equal '("GtkIMContextSimple" "GtkIMContextWayland" "GtkIMMulticontext"
               "IBusIMContext")
             (glib-test:list-children "GtkIMContext")))
  #+windows
  (is (equal '("GtkIMContextIME" "GtkIMContextSimple" "GtkIMMulticontext")
             (glib-test:list-children "GtkIMContext")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkIMContext")))
  ;; Check class properties
  (is (equal '("input-hints" "input-purpose")
             (glib-test:list-properties "GtkIMContext")))
  ;; Check signals
  (is (equal '("commit" "delete-surrounding" "invalid-composition"
               "preedit-changed" "preedit-end" "preedit-start"
               "retrieve-surrounding")
             (glib-test:list-signals "GtkIMContext")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkIMContext" GTK:IM-CONTEXT
                       (:SUPERCLASS G:OBJECT
                        :EXPORT T
                        :INTERFACES NIL)
                       ((INPUT-HINTS IM-CONTEXT-INPUT-HINTS
                         "input-hints" "GtkInputHints" T T)
                        (INPUT-PURPOSE IM-CONTEXT-INPUT-PURPOSE
                         "input-purpose" "GtkInputPurpose" T T)))
             (gobject:get-gtype-definition "GtkIMContext"))))

;;; --- Signals ----------------------------------------------------------------

;;;     commit

(test gtk-im-context-commit-signal
  (let* ((name "commit")
         (gtype (g:gtype "GtkIMContext"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("gchararray")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     delete-surrounding

(test gtk-im-context-delete-surrounding-signal
  (let* ((name "delete-surrounding")
         (gtype (g:gtype "GtkIMContext"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "gboolean") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("gint" "gint")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     invalid-composition

(test gtk-im-context-invalid-composition-signal
  (let* ((name "invalid-composition")
         (gtype (g:gtype "GtkIMContext"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "gboolean") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("gchararray")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     preedit-changed

(test gtk-im-context-preedit-changed-signal
  (let* ((name "preedit-changed")
         (gtype (g:gtype "GtkIMContext"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     preedit-end

(test gtk-im-context-preedit-end-signal
  (let* ((name "preedit-end")
         (gtype (g:gtype "GtkIMContext"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     preedit-start

(test gtk-im-context-preedit-start-signal
  (let* ((name "preedit-start")
         (gtype (g:gtype "GtkIMContext"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     retrieve-surrounding

(test gtk-im-context-retrieve-surrounding-signal
  (let* ((name "retrieve-surrounding")
         (gtype (g:gtype "GtkIMContext"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "gboolean") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Properties -------------------------------------------------------------

;; GTK-IM-CONTEXT is an abstract class, we use GTK-IM-CONTEXT-SIMPLE

(test gtk-im-context-properties
  (glib-test:with-check-memory (context)
    (is (typep (setf context (gtk:im-context-simple-new)) 'gtk:im-context-simple))
    ;; Property INPUT-HINTS
    (is (equal '(:READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkIMContext" "input-hints")))
    (is (equal '() (gtk:im-context-input-hints context)))
    (is (equal '(:spellcheck :lowercase)
               (setf (gtk:im-context-input-hints context) '(:spellcheck :lowercase))))
    ;; Property INPUT-PURPOSE
    (is (equal '(:READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkIMContext" "input-purpose")))
    (is (eq :free-form (gtk:im-context-input-purpose context)))
    (is (eq :alpha (setf (gtk:im-context-input-purpose context) :alpha)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_im_context_get_preedit_string
;;;     gtk_im_context_filter_keypress
;;;     gtk_im_context_filter_key
;;;     gtk_im_context_focus_in
;;;     gtk_im_context_focus_out
;;;     gtk_im_context_reset
;;;     gtk_im_context_set_client_widget
;;;     gtk_im_context_set_cursor_location
;;;     gtk_im_context_set_use_preedit
;;;     gtk_im_context_set_surrounding                      Deprecated 4.2
;;;     gtk_im_context_get_surrounding                      Deprecated 4.2
;;;     gtk_im_context_delete_surrounding
;;;     gtk_im_context_get_surrounding_with_selection       Since 4.2
;;;     gtk_im_context_set_surrounding_with_selection       Since 4.2

;;; 2026-05-15

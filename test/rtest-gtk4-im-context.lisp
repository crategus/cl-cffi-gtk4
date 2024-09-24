(in-package :gtk-test)

(def-suite gtk-im-context :in gtk-suite)
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
  (is (equal '("GtkIMContextSimple" "GtkIMMulticontext" "IBusIMContext")
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
  (is (equal '("commit" "delete-surrounding" "preedit-changed" "preedit-end"
               "preedit-start" "retrieve-surrounding")
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

;;; --- Properties -------------------------------------------------------------

;; GTK-IM-CONTEXT is an abstract class, we use GTK-IM-CONTEXT-SIMPLE

;;;     input-hints
;;;     input-purpose

(test gtk-im-context-properties
  (let ((method (make-instance 'gtk:im-context-simple)))
    (is (typep method 'gtk:im-context))
    ;; Property INPUT-HINTS
    (is (equal '() (gtk:im-context-input-hints method)))
    (is (equal '(:spellcheck :lowercase)
               (setf (gtk:im-context-input-hints method)
                     '(:spellcheck :lowercase))))
    (is (equal '(:spellcheck :lowercase)
               (gtk:im-context-input-hints method)))
    ;; Property INPUT-PURPOSE
    (is (eq :free-form (gtk:im-context-input-purpose method)))
    (is (eq :alpha (setf (gtk:im-context-input-purpose method) :alpha)))
    (is (eq :alpha (gtk:im-context-input-purpose method)))))

;;; --- Signals ----------------------------------------------------------------

;;;     commit
;;;     delete-surrounding
;;;     preedit-changed
;;;     preedit-end
;;;     preedit-start
;;;     retrieve-surrounding

(test gtk-im-context-signals
  ;; Query info for "commit" signal
  (let ((query (g:signal-query (g:signal-lookup "commit" "GtkIMContext"))))
    (is (string= "commit" (g:signal-query-signal-name query)))
    (is (string= "GtkIMContext"
                 (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '("gchararray")
               (sort (mapcar #'g:type-name (g:signal-query-param-types query))
                     #'string<)))
    (is-false (g:signal-query-signal-detail query)))
  ;; Query info for "delete-surrounding" signal
  (let ((query (g:signal-query (g:signal-lookup "delete-surrounding"
                                                "GtkIMContext"))))
    (is (string= "delete-surrounding" (g:signal-query-signal-name query)))
    (is (string= "GtkIMContext"
                 (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "gboolean" (g:type-name (g:signal-query-return-type query))))
    (is (equal '("gint" "gint")
               (sort (mapcar #'g:type-name (g:signal-query-param-types query))
                     #'string<)))
    (is-false (g:signal-query-signal-detail query)))
  ;; Query info for "preedit-changed" signal
  (let ((query (g:signal-query (g:signal-lookup "preedit-changed"
                                                "GtkIMContext"))))
    (is (string= "preedit-changed" (g:signal-query-signal-name query)))
    (is (string= "GtkIMContext"
                 (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '()
               (sort (mapcar #'g:type-name (g:signal-query-param-types query))
                     #'string<)))
    (is-false (g:signal-query-signal-detail query)))
  ;; Query info for "preedit-end" signal
  (let ((query (g:signal-query (g:signal-lookup "preedit-end" "GtkIMContext"))))
    (is (string= "preedit-end" (g:signal-query-signal-name query)))
    (is (string= "GtkIMContext"
                 (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '()
               (sort (mapcar #'g:type-name (g:signal-query-param-types query))
                     #'string<)))
    (is-false (g:signal-query-signal-detail query)))
  ;; Query info for "preedit-start" signal
  (let ((query (g:signal-query (g:signal-lookup "preedit-start"
                                                "GtkIMContext"))))
    (is (string= "preedit-start" (g:signal-query-signal-name query)))
    (is (string= "GtkIMContext"
                 (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '()
               (sort (mapcar #'g:type-name (g:signal-query-param-types query))
                     #'string<)))
    (is-false (g:signal-query-signal-detail query)))
  ;; Query info for "retrieve-surrounding" signal
  (let ((query (g:signal-query (g:signal-lookup "retrieve-surrounding"
                                                "GtkIMContext"))))
    (is (string= "retrieve-surrounding" (g:signal-query-signal-name query)))
    (is (string= "GtkIMContext"
                 (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "gboolean" (g:type-name (g:signal-query-return-type query))))
    (is (equal '()
               (sort (mapcar #'g:type-name (g:signal-query-param-types query))
                     #'string<)))
    (is-false (g:signal-query-signal-detail query))))

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
;;;     gtk_im_context_set_surrounding                     Deprecated 4.2
;;;     gtk_im_context_get_surrounding                     Deprecated 4.2
;;;     gtk_im_context_delete_surrounding
;;;     gtk_im_context_get_surrounding_with_selection      Since 4.2
;;;     gtk_im_context_set_surrounding_with_selection      Since 4.2

;;; 2024-9-20

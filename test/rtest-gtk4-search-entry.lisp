(in-package :gtk-test)

(def-suite gtk-search-entry :in gtk-data-entry)
(in-suite gtk-search-entry)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkSearchEntry

(test gtk-search-entry-class
  ;; Check type
  (is (g:type-is-object "GtkSearchEntry"))
  ;; Check registered name
  (is (eq 'gtk:search-entry
          (glib:symbol-for-gtype "GtkSearchEntry")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkSearchEntry")
          (g:gtype (cffi:foreign-funcall "gtk_search_entry_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkSearchEntry")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkSearchEntry")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkEditable")
             (glib-test:list-interfaces "GtkSearchEntry")))
  ;; Check properties
  (is (equal '("activates-default" "cursor-position" "editable" "enable-undo"
               "input-hints" "input-purpose" "key-capture-widget"
               "max-width-chars" "placeholder-text" "search-delay"
               "selection-bound" "text" "width-chars" "xalign")
             (glib-test:list-properties "GtkSearchEntry")))
  ;; Check signals
  (is (equal '("activate" "next-match" "previous-match" "search-changed"
               "search-started" "stop-search")
             (glib-test:list-signals "GtkSearchEntry")))
  ;; Check CSS name
  (is (string= "entry"
               (gtk:widget-class-css-name "GtkSearchEntry")))
  ;; Check accessible role
  (is (eq :search-box (gtk:widget-class-accessible-role "GtkSearchEntry")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkSearchEntry" GTK:SEARCH-ENTRY
                      (:SUPERCLASS GTK:WIDGET
                       :EXPORT T
                       :INTERFACES ("GtkAccessible" "GtkBuildable"
                                    "GtkConstraintTarget" "GtkEditable")
                       :TYPE-INITIALIZER "gtk_search_entry_get_type")
                      ((ACTIVATES-DEFAULT SEARCH-ENTRY-ACTIVATES-DEFAULT
                        "activates-default" "gboolean" T T)
                       (INPUT-HINTS SEARCH-ENTRY-INPUT-HINTS "input-hints"
                        "GtkInputHints" T T)
                       (INPUT-PURPOSE SEARCH-ENTRY-INPUT-PURPOSE
                        "input-purpose" "GtkInputPurpose" T T)
                       (KEY-CAPTURE-WIDGET SEARCH-ENTRY-KEY-CAPTURE-WIDGET
                        "key-capture-widget" "GtkWidget" T T)
                       (PLACEHOLDER-TEXT SEARCH-ENTRY-PLACEHOLDER-TEXT
                        "placeholder-text" "gchararray" T T)
                       (SEARCH-DELAY SEARCH-ENTRY-SEARCH-DELAY "search-delay"
                        "guint" T T)))
             (gobject:get-gtype-definition "GtkSearchEntry"))))

;;; --- Signals ----------------------------------------------------------------

;;;     activate

(test gtk-search-entry-activate-signal
  (let* ((name "activate")
         (gtype (g:gtype "GtkSearchEntry"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:ACTION :RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     next-match

(test gtk-search-entry-next-match-signal
  (let* ((name "next-match")
         (gtype (g:gtype "GtkSearchEntry"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:ACTION :RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     previous-match

(test gtk-search-entry-previous-match-signal
  (let* ((name "previous-match")
         (gtype (g:gtype "GtkSearchEntry"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:ACTION :RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     search-changed

(test gtk-search-entry-search-changed-signal
  (let* ((name "search-changed")
         (gtype (g:gtype "GtkSearchEntry"))
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

;;;     search-started

(test gtk-search-entry-search-started-signal
  (let* ((name "search-started")
         (gtype (g:gtype "GtkSearchEntry"))
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

;;;     stop-search

(test gtk-search-entry-stop-search-signal
  (let* ((name "stop-search")
         (gtype (g:gtype "GtkSearchEntry"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:ACTION :RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Properties -------------------------------------------------------------

;;;     activates-default
;;;     input-hints                                         Since 4.14
;;;     input-purpose                                       Since 4.14
;;;     key-capture-widget                                  Since 4.22
;;;     placeholder-text
;;;     search-delay                                        Since 4.8

(test gtk-search-entry-properties
  (glib-test:with-check-memory (entry)
    (is (typep (setf entry (make-instance 'gtk:search-entry)) 'gtk:search-entry))
    ;; Property ACTIVATES-DEFAULT
    (is (equal '(:READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkSearchEntry" "activates-default")))
    (is-false (gtk:search-entry-activates-default entry))
    (is-true (setf (gtk:search-entry-activates-default entry) t))
    ;; Property INPUT-HINTS since 4.13
    (is (equal '(:READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkSearchEntry" "input-hints")))
    (is-false (gtk:search-entry-input-hints entry))
    (is (eq :spellcheck (setf (gtk:search-entry-input-hints entry) :spellcheck)))
    ;; Property INPUT-PURPOSE sinde 4.14
    (is (equal '(:READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkSearchEntry" "input-purpose")))
    (is (eq :free-form (gtk:search-entry-input-purpose entry)))
    (is (eq :alpha (setf (gtk:search-entry-input-purpose entry) :alpha)))
    ;; Property KEY-CAPTURE-WIDGET since 4.22
    (is (equal '(:CONSTRUCT :READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkSearchEntry" "key-capture-widget")))
    (is-false (gtk:search-entry-key-capture-widget entry))
    (is (typep (setf (gtk:search-entry-key-capture-widget entry)
                     (gtk:button-new)) 'gtk:button))
    ;; Property PLACEHOLDER-TEXT
    (is (equal '(:READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkSearchEntry" "placeholder-text")))
    (is-false (gtk:search-entry-placeholder-text entry))
    (is (string= "text" (setf (gtk:search-entry-placeholder-text entry) "text")))
    ;; Property SEARCH-DELAY since 4.8
    (is (equal '(:READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkSearchEntry" "search-delay")))
    (is (= 150 (gtk:search-entry-search-delay entry)))
    (is (= 200 (setf (gtk:search-entry-search-delay entry) 200)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_search_entry_new

(test gtk-search-entry-new
  (glib-test:with-check-memory (entry)
    (is (typep (setf entry (gtk:search-entry-new)) 'gtk:search-entry))))

;;;     gtk_search_entry_set_key_capture_widget
;;;     gtk_search_entry_get_key_capture_widget

;;; 2026-05-17

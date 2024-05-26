(in-package :gtk-test)

(def-suite gtk-search-entry :in gtk-suite)
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
             (list-children "GtkSearchEntry")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkEditable")
             (list-interfaces "GtkSearchEntry")))
  ;; Check properties
  (is (equal '("activates-default" "cursor-position" "editable" "enable-undo"
               "input-hints" "input-purpose" "max-width-chars"
               "placeholder-text" "search-delay" "selection-bound" "text"
               "width-chars" "xalign")
             (list-properties "GtkSearchEntry")))
  ;; Check signals
  (is (equal '("activate" "next-match" "previous-match" "search-changed"
               "search-started" "stop-search")
             (list-signals "GtkSearchEntry")))
  ;; Check CSS name
  (is (string= "entry"
               (gtk:widget-class-css-name "GtkSearchEntry")))
  ;; Check CSS classes
  (is (equal '("search")
             (gtk:widget-css-classes (make-instance 'gtk:search-entry))))
  ;; Check accessible role
  (is (eq :search-box (gtk:widget-class-accessible-role "GtkSearchEntry")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkSearchEntry" GTK-SEARCH-ENTRY
                               (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                                ("GtkAccessible" "GtkBuildable"
                                 "GtkConstraintTarget" "GtkEditable")
                                :TYPE-INITIALIZER "gtk_search_entry_get_type")
                               ((ACTIVATES-DEFAULT
                                 GTK-SEARCH-ENTRY-ACTIVATES-DEFAULT
                                 "activates-default" "gboolean" T T)
                                (INPUT-HINTS GTK-SEARCH-ENTRY-INPUT-HINTS
                                 "input-hints" "GtkInputHints" T T)
                                (INPUT-PURPOSE GTK-SEARCH-ENTRY-INPUT-PURPOSE
                                 "input-purpose" "GtkInputPurpose" T T)
                                (PLACEHOLDER-TEXT
                                 GTK-SEARCH-ENTRY-PLACEHOLDER-TEXT
                                 "placeholder-text" "gchararray" T T)
                                (SEARCH-DELAY GTK-SEARCH-ENTRY-SEARCH-DELAY
                                 "search-delay" "guint" T T)))
             (gobject:get-g-type-definition "GtkSearchEntry"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-search-entry-properties
  (let ((entry (make-instance 'gtk:search-entry)))
    (is-false (gtk:search-entry-activates-default entry))
    (is-false (gtk:search-entry-input-hints entry))
    (is (eq :free-form (gtk:search-entry-input-purpose entry)))
    (is-false (gtk:search-entry-placeholder-text entry))
    (is (= 150 (gtk:search-entry-search-delay entry)))))

;;; --- Signals ----------------------------------------------------------------

;;;     activate
;;;     next-match
;;;     previous-match
;;;     search-changed
;;;     search-started
;;;     stop-search

;;; --- Functions --------------------------------------------------------------

;;;     gtk_search_entry_new

(test gtk-search-entry-new
  (is (typep (gtk:search-entry-new) 'gtk:search-entry)))

;;;     gtk_search_entry_set_key_capture_widget
;;;     gtk_search_entry_get_key_capture_widget

;;; 2024-5-26

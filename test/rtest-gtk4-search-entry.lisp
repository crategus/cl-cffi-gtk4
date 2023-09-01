(in-package :gtk-test)

(def-suite gtk-search-entry :in gtk-suite)
(in-suite gtk-search-entry)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkSearchEntry

(test gtk-search-entry-class
  ;; Type check
  (is (g:type-is-object "GtkSearchEntry"))
  ;; Check the registered name
  (is (eq 'gtk:search-entry
          (glib:symbol-for-gtype "GtkSearchEntry")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkSearchEntry")
          (g:gtype (cffi:foreign-funcall "gtk_search_entry_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkSearchEntry")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkSearchEntry")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkEditable")
             (list-interfaces "GtkSearchEntry")))
  ;; Check the properties
  (is (equal '("activates-default" "cursor-position" "editable" "enable-undo"
               "max-width-chars" "placeholder-text" "search-delay"
               "selection-bound" "text" "width-chars" "xalign")
             (list-properties "GtkSearchEntry")))
  ;; Check the signals
  (is (equal '("activate" "next-match" "previous-match" "search-changed"
               "search-started" "stop-search")
             (list-signals "GtkSearchEntry")))
  ;; CSS name
  (is (string= "entry"
               (gtk:widget-class-css-name "GtkSearchEntry")))
  ;; CSS style context
  (is (string=
"entry.search:dir(ltr)
  image:dir(ltr)
  text:dir(ltr)
    undershoot.left:dir(ltr)
    undershoot.right:dir(ltr)
  image:dir(ltr)
"
               (print-style-context "GtkSearchEntry")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkSearchEntry" GTK-SEARCH-ENTRY
                               (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                                ("GtkAccessible" "GtkBuildable"
                                 "GtkConstraintTarget" "GtkEditable")
                                :TYPE-INITIALIZER "gtk_search_entry_get_type")
                               ((ACTIVATES-DEFAULT
                                 GTK-SEARCH-ENTRY-ACTIVATES-DEFAULT
                                 "activates-default" "gboolean" T T)
                                (PLACEHOLDER-TEXT
                                 GTK-SEARCH-ENTRY-PLACEHOLDER-TEXT
                                 "placeholder-text" "gchararray" T T)
                                (SEARCH-DELAY GTK-SEARCH-ENTRY-SEARCH-DELAY
                                 "search-delay" "guint" T T)))
             (gobject:get-g-type-definition "GtkSearchEntry"))))

;;; --- Properties -------------------------------------------------------------

;;;     activates-default
;;;     placeholder-text
;;;     search-delay                                       Since 4.8

(test gtk-search-entry-properties
  (let ((entry (make-instance 'gtk:search-entry)))
    (is-false (gtk:search-entry-activates-default entry))
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
;;;     gtk_search_entry_set_key_capture_widget
;;;     gtk_search_entry_get_key_capture_widget

;;; --- 2023-8-26 --------------------------------------------------------------

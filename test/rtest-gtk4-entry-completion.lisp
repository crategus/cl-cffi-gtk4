(in-package :gtk-test)

(def-suite gtk-entry-completion :in gtk-suite)
(in-suite gtk-entry-completion)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkEntryCompletion

(test entry-completion-class
  ;; Type check
  (is (g:type-is-object "GtkEntryCompletion"))
  ;; Check the registered name
  (is (eq 'gtk:entry-completion
          (glib:symbol-for-gtype "GtkEntryCompletion")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkEntryCompletion")
          (g:gtype (cffi:foreign-funcall "gtk_entry_completion_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkEntryCompletion")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkEntryCompletion")))
  ;; Check the interfaces
  (is (equal '("GtkCellLayout" "GtkBuildable")
             (list-interfaces "GtkEntryCompletion")))
  ;; Check the properties
  (is (equal '("cell-area" "inline-completion" "inline-selection"
               "minimum-key-length" "model" "popup-completion" "popup-set-width"
               "popup-single-match" "text-column")
             (list-properties "GtkEntryCompletion")))
  ;; Check the signals
  (is (equal '("cursor-on-match" "insert-prefix" "match-selected" "no-matches")
             (list-signals "GtkEntryCompletion")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkEntryCompletion" GTK-ENTRY-COMPLETION
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES
                        ("GtkBuildable" "GtkCellLayout") :TYPE-INITIALIZER
                        "gtk_entry_completion_get_type")
                       ((CELL-AREA GTK-ENTRY-COMPLETION-CELL-AREA "cell-area"
                         "GtkCellArea" T NIL)
                        (INLINE-COMPLETION
                         GTK-ENTRY-COMPLETION-INLINE-COMPLETION
                         "inline-completion" "gboolean" T T)
                        (INLINE-SELECTION GTK-ENTRY-COMPLETION-INLINE-SELECTION
                         "inline-selection" "gboolean" T T)
                        (MINIMUM-KEY-LENGTH
                         GTK-ENTRY-COMPLETION-MINIMUM-KEY-LENGTH
                         "minimum-key-length" "gint" T T)
                        (MODEL GTK-ENTRY-COMPLETION-MODEL "model"
                         "GtkTreeModel" T T)
                        (POPUP-COMPLETION GTK-ENTRY-COMPLETION-POPUP-COMPLETION
                         "popup-completion" "gboolean" T T)
                        (POPUP-SET-WIDTH GTK-ENTRY-COMPLETION-POPUP-SET-WIDTH
                         "popup-set-width" "gboolean" T T)
                        (POPUP-SINGLE-MATCH
                         GTK-ENTRY-COMPLETION-POPUP-SINGLE-MATCH
                         "popup-single-match" "gboolean" T T)
                        (TEXT-COLUMN GTK-ENTRY-COMPLETION-TEXT-COLUMN
                         "text-column" "gint" T T)))
             (gobject:get-g-type-definition "GtkEntryCompletion"))))

;;; --- Properties -------------------------------------------------------------

;;;     cell-area
;;;     inline-completion
;;;     inline-selection
;;;     minimum-key-length
;;;     model
;;;     popup-completion
;;;     popup-set-width
;;;     popup-single-match
;;;     text-column

;;; --- Signals ----------------------------------------------------------------

;;;     cursor-on-match
;;;     insert-prefix
;;;     match-selected
;;;     no-matches

;;; --- Functions --------------------------------------------------------------

;;;     GtkEntryCompletionMatchFunc
;;;     gtk_entry_completion_new
;;;     gtk_entry_completion_new_with_area
;;;     gtk_entry_completion_get_entry
;;;     gtk_entry_completion_set_match_func
;;;     gtk_entry_completion_compute_prefix
;;;     gtk_entry_completion_complete
;;;     gtk_entry_completion_get_completion_prefix
;;;     gtk_entry_completion_insert_prefix

;;; --- 2023-5-29 --------------------------------------------------------------

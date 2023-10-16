(in-package :gtk-test)

(def-suite gtk-text-buffer :in gtk-suite)
(in-suite gtk-text-buffer)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkTextBuffer

(test gtk-text-buffer-class
  ;; Type check
  (is (g:type-is-object "GtkTextBuffer"))
  ;; Check the registered name
  (is (eq 'gtk:text-buffer
          (glib:symbol-for-gtype "GtkTextBuffer")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkTextBuffer")
          (g:gtype (cffi:foreign-funcall "gtk_text_buffer_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkTextBuffer")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkTextBuffer")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkTextBuffer")))
  ;; Check the properties
  (is (equal '("can-redo" "can-undo" "cursor-position" "enable-undo"
               "has-selection" "tag-table" "text")
             (list-properties "GtkTextBuffer")))
  ;; Check the signals
  (is (equal '("apply-tag" "begin-user-action" "changed" "delete-range"
               "end-user-action" "insert-child-anchor" "insert-paintable"
               "insert-text" "mark-deleted" "mark-set" "modified-changed"
               "paste-done" "redo" "remove-tag" "undo")
             (list-signals "GtkTextBuffer")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkTextBuffer" GTK-TEXT-BUFFER
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                                :TYPE-INITIALIZER "gtk_text_buffer_get_type")
                               ((CAN-REDO GTK-TEXT-BUFFER-CAN-REDO "can-redo"
                                 "gboolean" T NIL)
                                (CAN-UNDO GTK-TEXT-BUFFER-CAN-UNDO "can-undo"
                                 "gboolean" T NIL)
                                (CURSOR-POSITION
                                 GTK-TEXT-BUFFER-CURSOR-POSITION
                                 "cursor-position" "gint" T NIL)
                                (ENABLE-UNDO GTK-TEXT-BUFFER-ENABLE-UNDO
                                 "enable-undo" "gboolean" T T)
                                (HAS-SELECTION GTK-TEXT-BUFFER-HAS-SELECTION
                                 "has-selection" "gboolean" T NIL)
                                (TAG-TABLE GTK-TEXT-BUFFER-TAG-TABLE
                                 "tag-table" "GtkTextTagTable" T NIL)
                                (TEXT GTK-TEXT-BUFFER-TEXT "text" "gchararray"
                                 T T)))
             (gobject:get-g-type-definition "GtkTextBuffer"))))

;;; --- Properties -------------------------------------------------------------

;;;     can-redo
;;;     can-undo
;;;     cursor-position
;;;     enable-undo
;;;     has-selection
;;;     tag-table
;;;     text

(test gtk-text-buffer-properties
  (let ((buffer (make-instance 'gtk:text-buffer)))
    (is-false (gtk:text-buffer-can-redo buffer))
    (is-false (gtk:text-buffer-can-undo buffer))
    (is (= 0 (gtk:text-buffer-cursor-position buffer)))
    (is-true (gtk:text-buffer-enable-undo buffer))
    (is-false (gtk:text-buffer-has-selection buffer))
    (is (typep (gtk:text-buffer-tag-table buffer) 'gtk:text-tag-table))
    (is (string= "" (gtk:text-buffer-text buffer)))))

;;; --- Signals ----------------------------------------------------------------

;;;     apply-tag
;;;     begin-user-action
;;;     changed
;;;     delete-range
;;;     end-user-action
;;;     insert-child-anchor
;;;     insert-paintable
;;;     insert-text
;;;     mark-deleted
;;;     mark-set
;;;     modified-changed
;;;     paste-done
;;;     redo
;;;     remove-tag
;;;     undo

;;; --- Functions --------------------------------------------------------------

;;;     gtk_text_buffer_new

(test gtk-text-buffer-new
  (is (typep (gtk:text-buffer-new) 'gtk:text-buffer))
  (is (typep (gtk:text-buffer-new (gtk:text-tag-table-new)) 'gtk:text-buffer)))

;;;     gtk_text_buffer_get_line_count
;;;     gtk_text_buffer_get_char_count

(test gtk-text-buffer-line/char-count
  (let ((buffer (gtk:text-buffer-new)))
    (setf (gtk:text-buffer-text buffer) *lorem-ipsum-long*)
    (is (= 5 (gtk:text-buffer-line-count buffer)))
    (is (= 1794 (gtk:text-buffer-char-count buffer)))))

;;;     gtk_text_buffer_insert

(test gtk-text-buffer-insert
  (let ((buffer (gtk:text-buffer-new)))
    (is-true (gtk:text-buffer-insert buffer "First"))
    (is (string= "First" (gtk:text-buffer-text buffer)))
    (is-true (gtk:text-buffer-insert buffer " Second"))
    (is (string= "First Second" (gtk:text-buffer-text buffer)))))

;;;     gtk_text_buffer_insert_range
;;;     gtk_text_buffer_insert_with_tags
;;;     gtk_text_buffer_insert_markup
;;;     gtk_text_buffer_insert_paintable
;;;     gtk_text_buffer_delete
;;;     gtk_text_buffer_delete_interactive
;;;     gtk_text_buffer_backspace
;;;     gtk_text_buffer_get_slice
;;;     gtk_text_buffer_insert_child_anchor
;;;     gtk_text_buffer_create_child_anchor
;;;     gtk_text_buffer_create_mark
;;;     gtk_text_buffer_move_mark
;;;     gtk_text_buffer_move_mark_by_name
;;;     gtk_text_buffer_add_mark
;;;     gtk_text_buffer_delete_mark
;;;     gtk_text_buffer_delete_mark_by_name
;;;     gtk_text_buffer_get_mark
;;;     gtk_text_buffer_get_insert
;;;     gtk_text_buffer_get_selection_bound
;;;     gtk_text_buffer_place_cursor
;;;     gtk_text_buffer_select_range
;;;     gtk_text_buffer_apply_tag
;;;     gtk_text_buffer_remove_tag
;;;     gtk_text_buffer_apply_tag_by_name
;;;     gtk_text_buffer_remove_tag_by_name
;;;     gtk_text_buffer_remove_all_tags

;;;     gtk_text_buffer_create_tag

(test gtk-text-buffer-create-tag
  (let ((buffer (gtk:text-buffer-new)))
    ;; Named tag
    (is (typep (gtk:text-buffer-create-tag buffer "font-italic"
                                           :font "fixed" :style :italic)
               'gtk:text-tag))
    ;; Anonymous tag
    (is (typep (gtk:text-buffer-create-tag buffer nil
                                           :font "fixed" :style :italic)
               'gtk:text-tag))))

;;;     gtk_text_buffer_get_iter_at_line_offset
;;;     gtk_text_buffer_get_iter_at_offset
;;;     gtk_text_buffer_get_iter_at_line
;;;     gtk_text_buffer_get_iter_at_line_index
;;;     gtk_text_buffer_get_iter_at_mark
;;;     gtk_text_buffer_get_iter_at_child_anchor
;;;     gtk_text_buffer_get_start_iter
;;;     gtk_text_buffer_get_end_iter
;;;     gtk_text_buffer_get_bounds
;;;     gtk_text_buffer_get_modified
;;;     gtk_text_buffer_set_modified
;;;     gtk_text_buffer_delete_selection
;;;     gtk_text_buffer_paste_clipboard
;;;     gtk_text_buffer_copy_clipboard
;;;     gtk_text_buffer_cut_clipboard
;;;     gtk_text_buffer_get_selection_bounds
;;;     gtk_text_buffer_get_selection_content
;;;     gtk_text_buffer_begin_user_action
;;;     gtk_text_buffer_end_user_action
;;;     gtk_text_buffer_add_selection_clipboard
;;;     gtk_text_buffer_remove_selection_clipboard
;;;     gtk_text_buffer_get_max_undo_levels
;;;     gtk_text_buffer_set_max_undo_levels
;;;     gtk_text_buffer_undo
;;;     gtk_text_buffer_redo
;;;     gtk_text_buffer_begin_irreversible_action
;;;     gtk_text_buffer_end_irreversible_action

;;; --- 2023-10-3 --------------------------------------------------------------

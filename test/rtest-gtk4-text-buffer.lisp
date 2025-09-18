(in-package :gtk-test)

(def-suite gtk-text-buffer :in gtk-multiline-editor)
(in-suite gtk-text-buffer)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkTextBufferNotifyFlags

(test gtk-text-buffer-notify-flags
  ;; Check type
  (is (g:type-is-flags "GtkTextBufferNotifyFlags"))
  ;; Check registered name
  (is (eq 'gtk:text-buffer-notify-flags
          (glib:symbol-for-gtype "GtkTextBufferNotifyFlags")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkTextBufferNotifyFlags")
          (g:gtype (cffi:foreign-funcall "gtk_text_buffer_notify_flags_get_type"
                                         :size))))
  ;; Check names
  (is (equal '("GTK_TEXT_BUFFER_NOTIFY_BEFORE_INSERT"
               "GTK_TEXT_BUFFER_NOTIFY_AFTER_INSERT"
               "GTK_TEXT_BUFFER_NOTIFY_BEFORE_DELETE"
               "GTK_TEXT_BUFFER_NOTIFY_AFTER_DELETE")
             (glib-test:list-flags-item-names "GtkTextBufferNotifyFlags")))
  ;; Check values
  (is (equal '(1 2 4 8)
             (glib-test:list-flags-item-values "GtkTextBufferNotifyFlags")))
  ;; Check nick names
  (is (equal '("before-insert" "after-insert" "before-delete" "after-delete")
             (glib-test:list-flags-item-nicks "GtkTextBufferNotifyFlags")))
  ;; Check flags definition
  (is (equal '(GOBJECT:DEFINE-GFLAGS "GtkTextBufferNotifyFlags"
                                     GTK:TEXT-BUFFER-NOTIFY-FLAGS
                       (:EXPORT T
                        :TYPE-INITIALIZER "gtk_text_buffer_notify_flags_get_type")
                       (:BEFORE-INSERT 1)
                       (:AFTER-INSERT 2)
                       (:BEFORE-DELETE 4)
                       (:AFTER-DELETE 8))
             (gobject:get-gtype-definition "GtkTextBufferNotifyFlags"))))

;;;     GtkTextBuffer

(test gtk-text-buffer-class
  ;; Check type
  (is (g:type-is-object "GtkTextBuffer"))
  ;; Check registered name
  (is (eq 'gtk:text-buffer
          (glib:symbol-for-gtype "GtkTextBuffer")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkTextBuffer")
          (g:gtype (cffi:foreign-funcall "gtk_text_buffer_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkTextBuffer")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkTextBuffer")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkTextBuffer")))
  ;; Check properties
  (is (equal '("can-redo" "can-undo" "cursor-position" "enable-undo"
               "has-selection" "tag-table" "text")
             (glib-test:list-properties "GtkTextBuffer")))
  ;; Check signals
  (is (equal '("apply-tag" "begin-user-action" "changed" "delete-range"
               "end-user-action" "insert-child-anchor" "insert-paintable"
               "insert-text" "mark-deleted" "mark-set" "modified-changed"
               "paste-done" "redo" "remove-tag" "undo")
             (glib-test:list-signals "GtkTextBuffer")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkTextBuffer" GTK:TEXT-BUFFER
                      (:SUPERCLASS G:OBJECT
                       :EXPORT T
                       :INTERFACES NIL
                       :TYPE-INITIALIZER "gtk_text_buffer_get_type")
                      ((CAN-REDO TEXT-BUFFER-CAN-REDO
                        "can-redo" "gboolean" T NIL)
                       (CAN-UNDO TEXT-BUFFER-CAN-UNDO
                        "can-undo" "gboolean" T NIL)
                       (CURSOR-POSITION TEXT-BUFFER-CURSOR-POSITION
                        "cursor-position" "gint" T NIL)
                       (ENABLE-UNDO TEXT-BUFFER-ENABLE-UNDO
                        "enable-undo" "gboolean" T T)
                       (HAS-SELECTION TEXT-BUFFER-HAS-SELECTION
                        "has-selection" "gboolean" T NIL)
                       (TAG-TABLE TEXT-BUFFER-TAG-TABLE
                        "tag-table" "GtkTextTagTable" T NIL)
                       (TEXT TEXT-BUFFER-TEXT "text" "gchararray" T T)))
             (gobject:get-gtype-definition "GtkTextBuffer"))))

;;; --- Properties -------------------------------------------------------------

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

(test gtk-text-buffer-insert.1
  (let ((buffer (gtk:text-buffer-new)))
    (is-true (gtk:text-buffer-insert buffer :cursor "First"))
    (is (string= "First" (gtk:text-buffer-text buffer)))
    (is-true (gtk:text-buffer-insert buffer :cursor " Second"))
    (is (string= "First Second" (gtk:text-buffer-text buffer)))))

(test gtk-text-buffer-insert.2
  (let ((buffer (gtk:text-buffer-new)))
    (is-true (gtk:text-buffer-insert buffer :cursor "First Third"))
    (is (string= "First Third" (gtk:text-buffer-text buffer)))
    (is-false (gtk:text-buffer-insert buffer
                                      (gtk:text-buffer-iter-at-offset buffer 6)
                                       "Second "
                                       :editable nil
                                       :interactive t))
    (is-true (gtk:text-buffer-insert buffer
                                     (gtk:text-buffer-iter-at-offset buffer 6)
                                      "Second "
                                      :editable t
                                      :interactive t))
    (is (string= "First Second Third" (gtk:text-buffer-text buffer)))))

;;;     gtk_text_buffer_insert_range

(test gtk-text-buffer-insert-range
  (let ((buffer (gtk:text-buffer-new))
        iter iter1 iter2)
    (is (string= "First second third"
                 (setf (gtk:text-buffer-text buffer)
                       "First second third")))
    ;; Take the word "second" and copy it to the start of the buffer
    (is (typep (setf iter (gtk:text-buffer-start-iter buffer)) 'gtk:text-iter))
    (is (typep (setf iter1 (gtk:text-buffer-iter-at-offset buffer 6))
               'gtk:text-iter))
    (is (typep (setf iter2 (gtk:text-buffer-iter-at-offset buffer 13))
               'gtk:text-iter))
    (is-true (gtk:text-buffer-insert-range buffer iter iter1 iter2))
    (is (string= "second First second third" (gtk:text-buffer-text buffer)))))

;;;     gtk_text_buffer_insert_with_tags

;;;     gtk_text_buffer_insert_markup

(test gtk-text-buffer-insert-markup
  (let ((buffer (gtk:text-buffer-new))
        iter)
    (is (string= "First second third"
                 (setf (gtk:text-buffer-text buffer)
                       "First second third")))
    (is (typep (setf iter (gtk:text-buffer-iter-at-offset buffer 6))
               'gtk:text-iter))
    (is-false (gtk:text-buffer-insert-markup buffer
                                             iter
                                             "<b>bold</b> "))
    (is (string= "First bold second third" (gtk:text-buffer-text buffer)))))

;;;     gtk_text_buffer_insert_paintable

;;;     gtk_text_buffer_delete
;;;     gtk_text_buffer_delete_interactive

(test gtk-text-buffer-delete
  (let* ((buffer (gtk:text-buffer-new (gtk:text-tag-table-new)))
         (tag (gtk:text-tag-new "editable" :editable nil))
         (table (gtk:text-buffer-tag-table buffer)))
    (is-true (gtk:text-tag-table-add table tag))
    (is-true (setf (gtk:text-buffer-text buffer) "First second third"))
    ;; Make the text not editable
    (is-false (gtk:text-buffer-apply-tag buffer
                                         "editable"
                                         (gtk:text-buffer-start-iter buffer)
                                         (gtk:text-buffer-end-iter buffer)))
    ;; Case :interactive nil :editable nil
    (is-true (gtk:text-buffer-delete buffer
                                     (gtk:text-buffer-iter-at-offset buffer 6)
                                     (gtk:text-buffer-iter-at-offset buffer 12)))
    (is (string= "First  third" (gtk:text-buffer-text buffer)))
    ;; Case :interactive t :editable nil
    (is-true (setf (gtk:text-buffer-text buffer) "First second third"))
    (is-false (gtk:text-buffer-apply-tag buffer
                                         "editable"
                                         (gtk:text-buffer-start-iter buffer)
                                         (gtk:text-buffer-end-iter buffer)))
    (is-false (gtk:text-buffer-delete buffer
                                      (gtk:text-buffer-iter-at-offset buffer 6)
                                      (gtk:text-buffer-iter-at-offset buffer 12)
                                      :interactive t))
    (is (string= "First second third" (gtk:text-buffer-text buffer)))
    ;; Case :interactive t :editable t
    ;; TODO: The text is not deleted as expected ?!
    (is-true (setf (gtk:text-buffer-text buffer) "First second third"))
    (is-false (gtk:text-buffer-apply-tag buffer
                                         tag
                                         (gtk:text-buffer-start-iter buffer)
                                         (gtk:text-buffer-end-iter buffer)))
    (is-false (gtk:text-buffer-delete buffer
                                      (gtk:text-buffer-iter-at-offset buffer 6)
                                      (gtk:text-buffer-iter-at-offset buffer 12)
                                      :interactive t :editable t))
    (is (string= "First second third" (gtk:text-buffer-text buffer)))
    ;; Check memory management
    (is-false (gtk:text-tag-table-remove table tag))
    (is (= 3 (g:object-ref-count table)))   ; TODO: Why 3 references?!
    (is (= 1 (g:object-ref-count tag)))
    (is (= 1 (g:object-ref-count buffer)))))

;;;     gtk_text_buffer_backspace

(test gtk-text-buffer-backspace
  (let ((buffer (gtk:text-buffer-new))
        iter)
    (is (string= "First second third"
                 (setf (gtk:text-buffer-text buffer)
                       "First second third")))
    (is (typep (setf iter (gtk:text-buffer-iter-at-offset buffer 12))
               'gtk:text-iter))
    (is-true (gtk:text-buffer-backspace buffer iter :editable t))
    (is-true (gtk:text-buffer-backspace buffer iter :editable t))
    (is-true (gtk:text-buffer-backspace buffer iter :editable t))
    (is (string= "First sec third" (gtk:text-buffer-text buffer)))))

;;;     gtk_text_buffer_get_slice

;; TODO: The INCLUDE argument does not make a difference!?

(test gtk-text-buffer-get-slice
  (let* ((path (glib-sys:sys-path "test/resource/ducky.png"))
         (paintable (gdk:texture-new-from-filename path))
         (buffer (gtk:text-buffer-new))
         iter)
    (is (string= "First second third"
                 (setf (gtk:text-buffer-text buffer)
                       "First second third")))
    (is (typep (setf iter (gtk:text-buffer-iter-at-offset buffer 12))
               'gtk:text-iter))
    (is-false (gtk:text-buffer-insert-paintable buffer iter paintable))
    (is (string= "First second￼ third"
                 (gtk:text-buffer-get-slice buffer
                                            (gtk:text-buffer-start-iter buffer)
                                            (gtk:text-buffer-end-iter buffer)
                                            t)))
    (is (string= "First second￼ third"
                 (gtk:text-buffer-get-slice buffer
                                            (gtk:text-buffer-start-iter buffer)
                                            (gtk:text-buffer-end-iter buffer)
                                            nil)))
    ;; Check memory management
    (is (string= "" (setf (gtk:text-buffer-text buffer) "")))
    (is (= 1 (g:object-ref-count paintable)))
    (is (= 1 (g:object-ref-count buffer)))))

;;;     gtk_text_buffer_create_child_anchor
;;;     gtk_text_buffer_get_iter_at_child_anchor

(test gtk-text-buffer-create-child-anchor
  (let* ((buffer (make-instance 'gtk:text-buffer
                                :text "Some text for the text buffer."))
         (iter (gtk:text-buffer-iter-at-offset buffer 5))
         anchor iter1)

    (setf anchor (gtk:text-buffer-create-child-anchor buffer iter))
    (setf iter1 (gtk:text-buffer-iter-at-child-anchor buffer anchor))

    (is (eq #\t (gtk:text-iter-char iter)))
    #+sbcl
    (is (eq #\OBJECT_REPLACEMENT_CHARACTER (gtk:text-iter-char iter1)))
    #+ccl ; FIXME: What is the object replacement character for ccl?
    (is (characterp (gtk:text-iter-char iter1)))
    ;; Check memory management
    (is (string= "" (setf (gtk:text-buffer-text buffer) "")))
    (is (= 1 (g:object-ref-count anchor)))
    (is (= 1 (g:object-ref-count buffer)))))

;;;     gtk_text_buffer_insert_child_anchor

(test gtk-text-buffer-insert-child-anchor
  (let* ((buffer (make-instance 'gtk:text-buffer
                                :text "Some text for the text buffer."))
         (iter (gtk:text-buffer-iter-at-offset buffer 5))
         anchor iter1)

    (is (typep (setf anchor (gtk:text-child-anchor-new)) 'gtk:text-child-anchor))
    (is (typep (gtk:text-buffer-insert-child-anchor buffer
                                                    iter
                                                    anchor)
               'gtk:text-child-anchor))
    (setf iter1 (gtk:text-buffer-iter-at-child-anchor buffer anchor))

    (is (eq #\t (gtk:text-iter-char iter)))
    #+sbcl
    (is (eq #\OBJECT_REPLACEMENT_CHARACTER (gtk:text-iter-char iter1)))
    #+ccl ; FIXME: What is the object replacement character for ccl?
    (is (characterp (gtk:text-iter-char iter1)))
    ;; Check memory management
    (is (string= "" (setf (gtk:text-buffer-text buffer) "")))
    (is (= 1 (g:object-ref-count anchor)))
    (is (= 1 (g:object-ref-count buffer)))))

;;;     gtk_text_buffer_create_mark
;;;     gtk_text_buffer_move_mark
;;;     gtk_text_buffer_move_mark_by_name

(test gtk-text-buffer-create/move-mark
  (let* ((buffer (make-instance 'gtk:text-buffer
                               :text "Some text for the text buffer."))
         (iter (gtk:text-buffer-iter-at-offset buffer 5))
         mark)

    (is (typep (setf mark
                     (gtk:text-buffer-create-mark buffer "Mark" iter))
               'gtk:text-mark))
    (is (eq #\t (gtk:text-iter-char iter)))
    (is (eq #\t (gtk:text-iter-char (gtk:text-buffer-iter-at-mark buffer "Mark"))))

    (is-true (gtk:text-iter-move iter))
    (is-false (gtk:text-buffer-move-mark buffer "Mark" iter))
    (is (eq #\e (gtk:text-iter-char iter)))
    (is (eq #\e (gtk:text-iter-char (gtk:text-buffer-iter-at-mark buffer "Mark"))))

    (is-true (gtk:text-iter-move iter))
    (is-false (gtk:text-buffer-move-mark buffer mark iter))
    (is (eq #\x (gtk:text-iter-char iter)))
    (is (eq #\x (gtk:text-iter-char (gtk:text-buffer-iter-at-mark buffer "Mark"))))
    ;; Check memory management
    (is-false (gtk:text-buffer-delete-mark buffer mark))
    (is (= 1 (g:object-ref-count mark)))
    (is (= 1 (g:object-ref-count buffer)))))

;;;     gtk_text_buffer_add_mark
;;;     gtk_text_buffer_delete_mark
;;;     gtk_text_buffer_delete_mark_by_name
;;;     gtk_text_buffer_get_mark

(test gtk-text-buffer-add/delete-mark
  (let* ((buffer (make-instance 'gtk:text-buffer
                               :text "Some text for the text buffer."))
         (iter (gtk:text-buffer-iter-at-offset buffer 5))
         (mark (gtk:text-mark-new "Mark" nil)))

    (is-false (gtk:text-buffer-add-mark buffer mark iter))
    (is (eq mark (gtk:text-buffer-mark buffer "Mark")))
    (is-false (gtk:text-buffer-delete-mark buffer mark))
    (is-false (gtk:text-buffer-mark buffer "Mark"))

    (is-false (gtk:text-buffer-add-mark buffer mark iter))
    (is (eq mark (gtk:text-buffer-mark buffer "Mark")))
    (is-false (gtk:text-buffer-delete-mark buffer "Mark"))
    (is-false (gtk:text-buffer-mark buffer "Mark"))))

;;;     gtk_text_buffer_get_insert
;;;     gtk_text_buffer_get_selection_bound
;;;     gtk_text_buffer_place_cursor

(test gtk-text-buffer-insert/selection-bound
  (let* ((buffer (make-instance 'gtk:text-buffer
                                :text "Some text for the text buffer."))
         (iter (gtk:text-buffer-iter-at-offset buffer 5))
         (insert (gtk:text-buffer-mark buffer "insert"))
         (selection (gtk:text-buffer-mark buffer "selection_bound")))

    (is (typep (gtk:text-buffer-get-insert buffer) 'gtk:text-mark))
    (is (typep (gtk:text-buffer-selection-bound buffer) 'gtk:text-mark))

    (is (eq insert (gtk:text-buffer-get-insert buffer)))
    (is (eq selection (gtk:text-buffer-selection-bound buffer)))

    (is-false (gtk:text-buffer-place-cursor buffer iter))
    (is (eq #\t (gtk:text-iter-char (gtk:text-buffer-iter-at-mark buffer insert))))
    (is (eq #\t (gtk:text-iter-char (gtk:text-buffer-iter-at-mark buffer selection))))
    ;; Check memory management
    (is (string= "" (setf (gtk:text-buffer-text buffer) "")))
    (is (= 3 (g:object-ref-count insert)))
    (is (= 3 (g:object-ref-count selection)))
    (is (= 1 (g:object-ref-count buffer)))))

;;;     gtk_text_buffer_select_range

(test gtk-text-buffer-select-range
  (let* ((buffer (make-instance 'gtk:text-buffer
                                :text "Some text for the buffer."))
         (start (gtk:text-buffer-iter-at-offset buffer 5))
         (end (gtk:text-buffer-iter-at-offset buffer 10)))

    (is (= 1 (g:object-ref-count buffer)))

    (is-false (gtk:text-buffer-select-range buffer start end))
    ;; The last call adds a reference to BUFFER
    (is (= 2 (g:object-ref-count buffer)))

    (is-true (gtk:text-buffer-delete-selection buffer))
    (is (string= "Some for the buffer." (gtk:text-buffer-text buffer)))
    ;; Check memory management
    (is (string= "" (setf (gtk:text-buffer-text buffer) "")))
    (is (= 2 (g:object-ref-count buffer)))))

;;;     gtk_text_buffer_apply_tag
;;;     gtk_text_buffer_apply_tag_by_name
;;;     gtk_text_buffer_remove_tag
;;;     gtk_text_buffer_remove_tag_by_name

(test gtk-text-buffer-apply/remove-tag
  (let* ((buffer (make-instance 'gtk:text-buffer
                                :text "Some text for the buffer."))
         (start (gtk:text-buffer-iter-at-offset buffer 5))
         (end (gtk:text-buffer-iter-at-offset buffer 9))
         (tag1 (gtk:text-tag-new "tag1" :weight 100))
         (tag2 (gtk:text-tag-new "tag2" :weight 200)))

    (is-true (gtk:text-tag-table-add (gtk:text-buffer-tag-table buffer) tag1))
    (is-true (gtk:text-tag-table-add (gtk:text-buffer-tag-table buffer) tag2))
    (is (= 2 (gtk:text-tag-table-size (gtk:text-buffer-tag-table buffer))))

    (is-false (gtk:text-buffer-apply-tag buffer tag1 start end))
    (is-false (gtk:text-buffer-apply-tag buffer "tag2" start end))

    (is (member tag1 (gtk:text-iter-tags start) :test #'eq))
    (is (member tag2 (gtk:text-iter-tags start) :test #'eq))

    (is-false (gtk:text-buffer-remove-tag buffer "tag1" start end))
    (is-false (member tag1 (gtk:text-iter-tags start) :test #'eq))
    (is-false (gtk:text-buffer-remove-tag buffer tag2 start end))
    (is-false (member tag2 (gtk:text-iter-tags start) :test #'eq))
    ;; Check memory management
    (is (= 3 (g:object-ref-count tag1))) ; TODO: Can we reduce the references?
    (is (= 3 (g:object-ref-count tag2)))
    (is (= 1 (g:object-ref-count buffer)))))

;;;     gtk_text_buffer_remove_all_tags

(test gtk-text-buffer-remove-all-tags
  (let* ((buffer (make-instance 'gtk:text-buffer
                                :text "Some text for the buffer."))
         (start (gtk:text-buffer-iter-at-offset buffer 5))
         (end (gtk:text-buffer-iter-at-offset buffer 9))
         (tag1 (gtk:text-tag-new "tag1" :weight 100))
         (tag2 (gtk:text-tag-new "tag2" :weight 200)))

    (is-true (gtk:text-tag-table-add (gtk:text-buffer-tag-table buffer) tag1))
    (is-true (gtk:text-tag-table-add (gtk:text-buffer-tag-table buffer) tag2))
    (is (= 2 (gtk:text-tag-table-size (gtk:text-buffer-tag-table buffer))))

    (is-false (gtk:text-buffer-apply-tag buffer tag1 start end))
    (is-false (gtk:text-buffer-apply-tag buffer "tag2" start end))

    (is (member tag1 (gtk:text-iter-tags start) :test #'eq))
    (is (member tag2 (gtk:text-iter-tags start) :test #'eq))

    (is-false (gtk:text-buffer-remove-all-tags buffer start end))

    (is-false (member tag1 (gtk:text-iter-tags start) :test #'eq))
    (is-false (member tag2 (gtk:text-iter-tags start) :test #'eq))))

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

(test gtk-text-buffer-iter-at-line-offset
  (let ((buffer (gtk:text-buffer-new))
        iter1 iter2)
    (is-true (setf (gtk:text-buffer-text buffer) *some-lines*))
    (is (= 8 (gtk:text-buffer-line-count buffer)))
    (is (typep (setf iter1
                     (gtk:text-buffer-iter-at-line-offset buffer 0 11))
               'gtk:text-iter))
    (is (typep (setf iter2
                     (gtk:text-buffer-iter-at-line-offset buffer 0 20))
               'gtk:text-iter))
    (is (string= "important"
                 (gtk:text-buffer-get-text buffer iter1 iter2)))
    (is (typep (setf iter1
                     (gtk:text-buffer-iter-at-line-offset buffer 1 25))
               'gtk:text-iter))
    (is (typep (setf iter2
                     (gtk:text-buffer-iter-at-line-offset buffer 1 30))
               'gtk:text-iter))
    (is (string= "UTF-8"
                 (gtk:text-buffer-get-text buffer iter1 iter2)))))

;;;     gtk_text_buffer_get_iter_at_offset
;;;     gtk_text_buffer_get_iter_at_line

(test gtk-text-buffer-iter-at-offset/line
  (let ((buffer (make-instance 'gtk:text-buffer
                               :text *some-lines*))
        iter)

    (setf iter (gtk:text-buffer-iter-at-offset buffer 54))
    (is (eq #\G (gtk:text-iter-char iter)))

    (setf iter (gtk:text-buffer-iter-at-line buffer 1))
    (is (eq #\G (gtk:text-iter-char iter)))))

;;;     gtk_text_buffer_get_iter_at_line_index

(test gtk-text-buffer-iter-at-line-index
  (let ((buffer (make-instance 'gtk:text-buffer
                               :text *some-lines*))
        iter)
    (is (typep (setf iter (gtk:text-buffer-iter-at-line-index buffer 3 5))
               'gtk:text-iter))
    (is (eq #\s (gtk:text-iter-char iter)))))

;;;     gtk_text_buffer_get_iter_at_mark

(test gtk-text-buffer-get-iter-at-mark
  (let ((buffer (make-instance 'gtk:text-buffer
                               :text *some-lines*)))
    (is (typep (gtk:text-buffer-iter-at-mark buffer "insert") 'gtk:text-iter))
    (is (typep (gtk:text-buffer-iter-at-mark buffer "selection_bound")
               'gtk:text-iter))))

;;;     gtk_text_buffer_get_start_iter
;;;     gtk_text_buffer_get_end_iter

(test gtk-text-buffer-start/end-iter
  (let ((buffer (make-instance 'gtk:text-buffer
                               :text *some-lines*))
        start end)
    (is (typep (setf start (gtk:text-buffer-start-iter buffer)) 'gtk:text-iter))
    (is-true (gtk:text-iter-is-start start))
    (is (typep (setf end (gtk:text-buffer-end-iter buffer)) 'gtk:text-iter))
    (is-true (gtk:text-iter-is-end end))))

;;;     gtk_text_buffer_get_bounds

(test gtk-text-buffer-bounds
  (let ((buffer (make-instance 'gtk:text-buffer
                               :text *some-lines*)))
    (multiple-value-bind (start end)
        (gtk:text-buffer-bounds buffer)
      (is-true (gtk:text-iter-is-start start))
      (is-true (gtk:text-iter-is-end end)))))

;;;     gtk_text_buffer_get_modified
;;;     gtk_text_buffer_set_modified

(test gtk-text-buffer-bounds
  (let ((buffer (make-instance 'gtk:text-buffer
                               :text *some-lines*)))
    (is-true (gtk:text-buffer-modified buffer))
    (is-false (setf (gtk:text-buffer-modified buffer) nil))
    (is-false (gtk:text-buffer-modified buffer))))

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

;;;     GtkTextBufferCommitNotify                           Since 4.16
;;;     gtk_text_buffer_add_commit_notify                   Since 4.16
;;;     gtk_text_buffer_remove_commit_notify                Since 4.16

;;; 2025-09-18

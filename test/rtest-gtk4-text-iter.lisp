(in-package :gtk-test)

(def-suite gtk-text-iter :in gtk-multiline-editor)
(in-suite gtk-text-iter)

(defparameter gtk-text-iter
              '(gtk:text-iter-buffer
                gtk:text-iter-new
                gtk:text-iter-copy
                gtk:text-iter-assign
                gtk:text-iter-offset
                gtk:text-iter-line
                gtk:text-iter-line-offset
                gtk:text-iter-line-index
                gtk:text-iter-char
                gtk:text-iter-slice
                gtk:text-iter-text
                gtk:text-iter-paintable
                gtk:text-iter-marks
                gtk:text-iter-toggled-tags
                gtk:text-iter-child-anchor

                gtk:text-iter-starts-tag
                gtk:text-iter-ends-tag

                gtk:text-iter-toggles-tag
                gtk:text-iter-has-tag
                gtk:text-iter-tags
                gtk:text-iter-editable
                gtk:text-iter-can-insert

                gtk:text-iter-starts-word
                gtk:text-iter-ends-word
                gtk:text-iter-inside-word

                gtk:text-iter-starts-line
                gtk:text-iter-ends-line

                gtk:text-iter-starts-sentence
                gtk:text-iter-ends-sentence
                gtk:text-iter-inside-sentence

                gtk:text-iter-is-cursor-position
                gtk:text-iter-chars-in-line
                gtk:text-iter-bytes-in-line
                gtk:text-iter-language
                gtk:text-iter-is-end
                gtk:text-iter-is-start

                gtk:text-iter-move

                gtk:text-iter-forward-to-end
                gtk:text-iter-forward-to-line-end
                gtk:text-iter-forward-to-tag-toggle
                gtk:text-iter-backward-to-tag-toggle

                gtk:text-iter-find-char
                gtk:text-iter-search

                gtk:text-iter-equal
                gtk:text-iter-compare
                gtk:text-iter-in-range
                gtk:text-iter-order))

(export 'gtk-text-iter)

(defvar *gtk-text-iter-sample-text*
  "Weit hinten, hinter den Wortbergen, fern der Länder Vokalien und Konsonantien
leben die Blindtexte. Abgeschieden wohnen Sie in Buchstabenhausen an der Küste
des Semantik, eines großen Sprachozeans. Ein kleines Bächlein namens Duden
fließt durch ihren Ort und versorgt sie mit den nötigen Regelialien. Es ist ein
paradiesmatisches Land, in dem einem gebratene Satzteile in den Mund fliegen.
Nicht einmal von der allmächtigen Interpunktion werden die Blindtexte beherrscht
– ein geradezu unorthographisches Leben.

Eines Tages aber beschloss eine kleine Zeile Blindtext, ihr Name war Lorem
Ipsum, hinaus zu gehen in die weite Grammatik. Der große Oxmox riet ihr davon
ab, da es dort wimmele von bösen Kommata, wilden Fragezeichen und hinterhältigen
Semikola, doch das Blindtextchen ließ sich nicht beirren. Es packte seine sieben
Versalien, schob sich sein Initial in den Gürtel und machte sich auf den Weg.

Als es die ersten Hügel des Kursivgebirges erklommen hatte, warf es einen
letzten Blick zurück auf die Skyline seiner Heimatstadt Buchstabenhausen, die
Headline von Alphabetdorf und die Subline seiner eigenen Straße, der
Zeilengasse. Wehmütig lief ihm eine rhetorische Frage über die Wange, dann
setzte es seinen Weg fort.

Unterwegs traf es eine Copy. Die Copy warnte das Blindtextchen, da, wo sie
herkäme, wäre sie zigmal umgeschrieben worden und alles, was von ihrem Ursprung
noch übrig wäre, sei das Wort »und« und das Blindtextchen solle umkehren und
wieder in sein eigenes, sicheres Land zurückkehren.

Doch alles Gutzureden konnte es nicht überzeugen und so dauerte es nicht lange,
bis ihm ein paar heimtückische Werbetexter auflauerten, es mit Longe und Parole
betrunken machten und es dann in ihre Agentur schleppten, wo sie es für ihre
Projekte wieder und wieder missbrauchten. Und wenn es nicht umgeschrieben wurde,
dann benutzen Sie es immer noch.")

;;;     GtkTextSearchFlags

(test gtk-text-search-flags
  ;; Check type
  (is (g:type-is-flags "GtkTextSearchFlags"))
  ;; Check registered name
  (is (eq 'gtk:text-search-flags
          (glib:symbol-for-gtype "GtkTextSearchFlags")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkTextSearchFlags")
          (g:gtype (cffi:foreign-funcall "gtk_text_search_flags_get_type"
                                         :size))))
  ;; Check names
  (is (equal '("GTK_TEXT_SEARCH_VISIBLE_ONLY" "GTK_TEXT_SEARCH_TEXT_ONLY"
               "GTK_TEXT_SEARCH_CASE_INSENSITIVE")
             (glib-test:list-flags-item-names "GtkTextSearchFlags")))
  ;; Check values
  (is (equal '(1 2 4)
             (glib-test:list-flags-item-values "GtkTextSearchFlags")))
  ;; Check nick names
  (is (equal '("visible-only" "text-only" "case-insensitive")
             (glib-test:list-flags-item-nicks "GtkTextSearchFlags")))
  ;; Check flags definition
  (is (equal '(GOBJECT:DEFINE-GFLAGS "GtkTextSearchFlags"
                                     GTK:TEXT-SEARCH-FLAGS
                       (:EXPORT T
                        :TYPE-INITIALIZER "gtk_text_search_flags_get_type")
                       (:VISIBLE-ONLY 1)
                       (:TEXT-ONLY 2)
                       (:CASE-INSENSITIVE 4))
             (gobject:get-gtype-definition "GtkTextSearchFlags"))))

;;;   GtkTextIter

(test gtk-text-iter-boxed
  ;; Check type
  (is (g:type-is-boxed "GtkTextIter"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkTextIter")
          (g:gtype (cffi:foreign-funcall "gtk_text_iter_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:text-iter
          (glib:symbol-for-gtype "GtkTextIter"))))

;;;     gtk_text_iter_get_buffer

;;;     gtk_text_iter_get_buffer

(test gtk-text-iter-buffer
  (let* ((buffer (make-instance 'gtk:text-buffer
                                :text "Some sample text for the text buffer."))
         (iter (gtk:text-buffer-start-iter buffer)))
    (is (typep iter 'gtk:text-iter))
    (is (cffi:pointerp (glib:pointer iter)))
    (is (eq buffer (gtk:text-iter-buffer iter)))))

;;;     gtk-text-iter-new
;;;     gtk_text_iter_copy
;;;     gtk_text_iter_assign

(test gtk-text-iter-new/copy/assign
  (let* ((buffer (make-instance 'gtk:text-buffer
                                :text "Some sample text for the text buffer."))
         (iter1 (gtk:text-buffer-start-iter buffer))
         (iter2 (gtk:text-iter-copy iter1))
         (iter3 (gtk:text-iter-new)))
    (is-false (gtk:text-iter-assign iter3 iter1))
    (is (typep iter1 'gtk:text-iter))
    (is (cffi:pointerp (glib:pointer iter1)))
    (is (eq buffer (gtk:text-iter-buffer iter1)))
    (is (typep iter2 'gtk:text-iter))
    (is (cffi:pointerp (glib:pointer iter2)))
    (is (eq buffer (gtk:text-iter-buffer iter2)))
    (is (typep iter3 'gtk:text-iter))
    (is (cffi:pointerp (glib:pointer iter3)))
    (is (eq buffer (gtk:text-iter-buffer iter3)))))

;;;     gtk_text_iter_get_offset
;;;     gtk_text_iter_set_offset

(test gtk-text-iter-offset
  (let* ((buffer (make-instance 'gtk:text-buffer
                                :text
                                "Some sample text for the text buffer."))
         (iter (gtk:text-buffer-iter-at-offset buffer 12)))
    (is (= 12 (gtk:text-iter-offset iter)))
    (is (eq #\t (gtk:text-iter-char iter)))
    (is (= 17 (setf (gtk:text-iter-offset iter) 17)))
    (is (eq #\f (gtk:text-iter-char iter)))))

;;;     gtk_text_iter_get_line
;;;     gtk_text_iter_set_line

(test gtk-text-iter-line
  (let* ((buffer (make-instance 'gtk:text-buffer
                                :text *gtk-text-iter-sample-text*))
         (iter (gtk:text-buffer-iter-at-offset buffer 100)))
    (is (= 1 (gtk:text-iter-line iter)))
    (is (eq #\A (gtk:text-iter-char iter)))
    (is (= 5 (setf (gtk:text-iter-line iter) 5)))
    (is (eq #\N (gtk:text-iter-char iter)))))

;;;     gtk_text_iter_get_line_offset
;;;     gtk_text_iter_set_line_offset
;;;     gtk_text_iter_get_visible_line_offset
;;;     gtk_text_iter_set_visible_line_offset

(test gtk-text-iter-line-offset.1
  (let* ((buffer (make-instance 'gtk:text-buffer
                                :text
                                "Some sample text for the text buffer."))
         (iter (gtk:text-buffer-iter-at-offset buffer 12)))
    (is (= 12 (gtk:text-iter-line-offset iter)))
    (is (eq #\t (gtk:text-iter-char iter)))
    (is (= 17 (setf (gtk:text-iter-line-offset iter) 17)))
    (is (= 17 (gtk:text-iter-line-offset iter)))
    (is (eq #\f (gtk:text-iter-char iter)))))

(test gtk-text-iter-line-offset.2
  (let* ((buffer (make-instance 'gtk:text-buffer
                                :text
                                "Some sample text for the text buffer."))
         (iter (gtk:text-buffer-iter-at-offset buffer 12)))
    (is (= 12 (gtk:text-iter-line-offset iter :visible t)))
    (is (eq #\t (gtk:text-iter-char iter)))
    (is (= 17 (setf (gtk:text-iter-line-offset iter :visible t) 17)))
    (is (= 17 (gtk:text-iter-line-offset iter :visible t)))
    (is (eq #\f (gtk:text-iter-char iter)))))

;;;     gtk_text_iter_get_line_index
;;;     gtk_text_iter_set_line_index
;;;     gtk_text_iter_get_visible_line_index
;;;     gtk_text_iter_set_visible_line_index

(test gtk-text-iter-line-index.1
  (let* ((buffer (make-instance 'gtk:text-buffer
                                :text
                                "Some sample text for the text buffer."))
         (iter (gtk:text-buffer-iter-at-offset buffer 12)))
    (is (= 12 (gtk:text-iter-line-index iter)))
    (is (eq #\t (gtk:text-iter-char iter)))
    (is (= 17 (setf (gtk:text-iter-line-index iter) 17)))
    (is (= 17 (gtk:text-iter-line-index iter)))
    (is (eq #\f (gtk:text-iter-char iter)))))

(test gtk-text-iter-line-index.2
  (let* ((buffer (make-instance 'gtk:text-buffer
                                :text
                                "Some sample text for the text buffer."))
         (iter (gtk:text-buffer-iter-at-offset buffer 12)))
    (is (= 12 (gtk:text-iter-line-index iter :visible t)))
    (is (eq #\t (gtk:text-iter-char iter)))
    (is (= 17 (setf (gtk:text-iter-line-index iter :visible t) 17)))
    (is (= 17 (gtk:text-iter-line-index iter :visible t)))
    (is (eq #\f (gtk:text-iter-char iter)))))

;;;   gtk_text_iter_get_char

(test gtk-text-iter-char
  (let* ((buffer (make-instance 'gtk:text-buffer
                                :text
                                "Some sample text for the text buffer."))
         (iter (gtk:text-buffer-iter-at-offset buffer 12)))
    (is (eql #\t (gtk:text-iter-char iter)))
    ;; Move to the end of the text buffer
    (is-false (gtk:text-iter-forward-to-end iter))
    (is (eql #\Nul (gtk:text-iter-char iter)))))

;;;   gtk_text_iter_get_slice
;;;   gtk_text_iter_get_visible_slice

(test gtk-text-iter-slice.1
  (let* ((buffer (make-instance 'gtk:text-buffer
                                :text
                                "Some sample text for the text buffer."))
         (start (gtk:text-buffer-iter-at-offset buffer 12))
         (end (gtk:text-buffer-iter-at-offset buffer 16)))
    (is (string= "text" (gtk:text-iter-slice start end)))))

(test gtk-text-iter-slice.2
  (let* ((buffer (make-instance 'gtk:text-buffer
                                :text
                                "Some sample text for the text buffer."))
         (start (gtk:text-buffer-iter-at-offset buffer 12))
         (end (gtk:text-buffer-iter-at-offset buffer 16)))
    (is (string= "text" (gtk:text-iter-slice start end :visible t)))))

;;;     gtk_text_iter_get_text
;;;     gtk_text_iter_get_visible_text

(test gtk-text-iter-text.1
  (let* ((buffer (make-instance 'gtk:text-buffer
                                :text
                                "Some sample text for the text buffer."))
         (start (gtk:text-buffer-iter-at-offset buffer 12))
         (end (gtk:text-buffer-iter-at-offset buffer 16)))
    (is (string= "text" (gtk:text-iter-text start end)))))

(test gtk-text-iter-text.2
  (let* ((buffer (make-instance 'gtk:text-buffer
                                :text
                                "Some sample text for the text buffer."))
         (start (gtk:text-buffer-iter-at-offset buffer 12))
         (end (gtk:text-buffer-iter-at-offset buffer 16)))
    (is (string= "text" (gtk:text-iter-text start end :visible t)))))

;;;   gtk_text_iter_get_paintable

(test gtk-text-iter-paintable
  (let* ((buffer (make-instance 'gtk:text-buffer
                                :text
                                "Some sample text for the text buffer."))
         (iter (gtk:text-buffer-iter-at-offset buffer 12))
         (pixbuf (make-instance 'gdk:pixbuf))
         (paintable (gdk:texture-new-for-pixbuf pixbuf)))
    ;; Insert a paintable
    (gtk:text-buffer-insert-paintable buffer iter paintable)
    (let ((iter (gtk:text-buffer-iter-at-offset buffer 12)))
      (is (typep (gtk:text-iter-paintable iter) 'gdk:texture))
      (is (eq #\OBJECT_REPLACEMENT_CHARACTER (gtk:text-iter-char iter)))
      (is (eq #\Nul (gtk:text-iter-char (gtk:text-buffer-end-iter buffer)))))
    ;; Check memory management
    (is (string= "" (setf (gtk:text-buffer-text buffer) "")))
    (is (= 2 (g:object-ref-count pixbuf)))
    (is (= 1 (g:object-ref-count paintable)))
    (is (= 1 (g:object-ref-count buffer)))))

;;;   gtk_text_iter_get_marks

(test gtk-text-iter-marks
  (let* ((buffer (make-instance 'gtk:text-buffer
                                :text
                                "Some sample text for the text buffer."))
         (mark (gtk:text-mark-new nil))
         (iter (gtk:text-buffer-iter-at-offset buffer 12)))
    (gtk:text-buffer-add-mark buffer mark iter)
    (is (eq 'gtk:text-mark
            (type-of (first (gtk:text-iter-marks iter)))))
    ;; Check memory management
    (is-false (gtk:text-buffer-delete-mark buffer mark))
    (is (= 1 (g:object-ref-count mark)))
    (is (= 1 (g:object-ref-count buffer)))))

;;;   gtk_text_iter_get_toggled_tags

(test gtk-text-iter-toggled-tags
  (let* ((buffer (make-instance 'gtk:text-buffer
                                :text
                                "Some sample text for the text buffer."))
         (start (gtk:text-buffer-iter-at-offset buffer 12))
         (end (gtk:text-buffer-iter-at-offset buffer 16))
         (table (gtk:text-buffer-tag-table buffer))
         (tag nil))
    (gtk:text-tag-table-add table
                            (setf tag
                                  (make-instance 'gtk:text-tag
                                                 :name "bold"
                                                 :weight 700)))
    (gtk:text-buffer-apply-tag buffer "bold" start end)
    (is (eq 'gtk:text-tag
            (type-of (first (gtk:text-iter-toggled-tags start t)))))
    ;; Remove tag from text tag table and text tag table from buffer
    (is-false (gtk:text-tag-table-remove table tag))
    ;; Check memory management
    (is (= 3 (g:object-ref-count table)))     ; TODO: Why 3 references?
    (is (= 1 (g:object-ref-count buffer)))))

;;;   gtk_text_iter_get_child_anchor

(test gtk-text-iter-child-anchor
  (let* ((buffer (make-instance 'gtk:text-buffer
                                :text
                                "Some sample text for the text buffer."))
         (iter (gtk:text-buffer-iter-at-offset buffer 12))
         (anchor nil))
    (is (typep (setf anchor
                     (gtk:text-buffer-create-child-anchor buffer iter))
               'gtk:text-child-anchor))
    ;; Move iter one char backwards
    (is-true (gtk:text-iter-move iter :count -1))
    (is (typep (gtk:text-iter-child-anchor iter) 'gtk:text-child-anchor))
    ;; Check memory management
    (is (string= "" (setf (gtk:text-buffer-text buffer) "")))
    (is (= 1 (g:object-ref-count anchor)))
    (is (= 1 (g:object-ref-count buffer)))))

;;;     gtk_text_iter_starts_tag
;;;     gtk_text_iter_ends_tag

(test gtk-text-iter-starts/ends-tag
  (let* ((buffer (make-instance 'gtk:text-buffer
                                :text
                                "Some sample text for the text buffer."))
         (start (gtk:text-buffer-iter-at-offset buffer 12))
         (end (gtk:text-buffer-iter-at-offset buffer 16))
         (table (gtk:text-buffer-tag-table buffer))
         (tag (make-instance 'gtk:text-tag
                             :name "bold"
                             :weight 700)))
    (is-true (gtk:text-tag-table-add table tag))
    (is-false (gtk:text-buffer-apply-tag buffer "bold" start end))

    (is-true (gtk:text-iter-starts-tag start tag))
    (is-false (gtk:text-iter-ends-tag start tag))

    (is-false (gtk:text-iter-starts-tag end tag))
    (is-true (gtk:text-iter-ends-tag end tag))
    ;; Remove tag from text tag table
    (is-false (gtk:text-tag-table-remove table tag))
    ;; Check memory management
    (is (= 3 (g:object-ref-count table)))  ; TODO: Why 3 references?!
    (is (= 1 (g:object-ref-count tag)))
    (is (= 1 (g:object-ref-count buffer)))))

;;;     gtk_text_iter_toggles_tag

(test gtk-text-iter-toggles-tag
  (let* ((buffer (make-instance 'gtk:text-buffer
                                :text
                                "Some sample text for the text buffer."))
         (start (gtk:text-buffer-iter-at-offset buffer 12))
         (end (gtk:text-buffer-iter-at-offset buffer 16))
         (table (gtk:text-buffer-tag-table buffer))
         (tag (make-instance 'gtk:text-tag
                             :name "bold"
                             :weight 700)))
    (is-true (gtk:text-tag-table-add table tag))
    (is-false (gtk:text-buffer-apply-tag buffer "bold" start end))

    (is-true (gtk:text-iter-toggles-tag start tag))
    (is-true (gtk:text-iter-toggles-tag end tag))
    ;; Remove tag from text tag table
    (is-false (gtk:text-tag-table-remove table tag))
    ;; Check memory management
    (is (= 3 (g:object-ref-count table)))   ; TODO: Why 3 references?!
    (is (= 1 (g:object-ref-count tag)))
    (is (= 1 (g:object-ref-count buffer)))))

;;;     gtk_text_iter_has_tag

(test gtk-text-iter-has-tag
  (let* ((buffer (make-instance 'gtk:text-buffer
                                :text
                                "Some sample text for the text buffer."))
         (start (gtk:text-buffer-iter-at-offset buffer 12))
         (end (gtk:text-buffer-iter-at-offset buffer 16))
         (table (gtk:text-buffer-tag-table buffer))
         (tag (make-instance 'gtk:text-tag
                             :name "bold"
                             :weight 700)))
    (is-true (gtk:text-tag-table-add table tag))
    (is-false (gtk:text-buffer-apply-tag buffer "bold" start end))

    (is-true (gtk:text-iter-has-tag start tag))
    (is-false (gtk:text-iter-has-tag end tag))
    (is-true (gtk:text-iter-move end :count -1))
    (is-true (gtk:text-iter-has-tag end tag))
    ;; Remove tag from text tag table
    (is-false (gtk:text-tag-table-remove table tag))
    ;; Check memory management
    (is (= 3 (g:object-ref-count table)))   ; TODO: Why 3 references?!
    (is (= 1 (g:object-ref-count tag)))
    (is (= 1 (g:object-ref-count buffer)))))

;;;     gtk_text_iter_get_tags

(test gtk-text-iter-tags
  (let* ((buffer (make-instance 'gtk:text-buffer
                                :text
                                "Some sample text for the text buffer."))
         (start (gtk:text-buffer-iter-at-offset buffer 12))
         (end (gtk:text-buffer-iter-at-offset buffer 16))
         (table (gtk:text-buffer-tag-table buffer))
         (tag (make-instance 'gtk:text-tag
                             :name "bold"
                             :weight 700)))
    (is-true (gtk:text-tag-table-add table tag))
    (is-false (gtk:text-buffer-apply-tag buffer "bold" start end))
    (is (member tag (gtk:text-iter-tags start) :test #'eq))
    (is-false (gtk:text-iter-tags end))
    ;; Remove tag from text tag table
    (is-false (gtk:text-tag-table-remove table tag))
    ;; Check memory management
    (is (= 3 (g:object-ref-count table)))   ; TODO: Why 3 references?!
    (is (= 1 (g:object-ref-count tag)))
    (is (= 1 (g:object-ref-count buffer)))))

;;;     gtk_text_iter_editable

(test gtk-text-iter-editable
  (let* ((buffer (make-instance 'gtk:text-buffer
                                :text
                                "Some sample text for the text buffer."))
         (start (gtk:text-buffer-iter-at-offset buffer 12))
         (end (gtk:text-buffer-iter-at-offset buffer 16))
         (table (gtk:text-buffer-tag-table buffer))
         (tag (make-instance 'gtk:text-tag
                             :name "editable"
                             :editable nil)))
    (is-true (gtk:text-tag-table-add table tag))
    (is-false (gtk:text-buffer-apply-tag buffer "editable" start end))

    (is-false (gtk:text-iter-editable start nil))
    (is-false (gtk:text-iter-editable start t))

    (is-false (gtk:text-iter-editable end nil))
    (is-true (gtk:text-iter-editable end t))
    ;; Remove tag from text tag table
    (is-false (gtk:text-tag-table-remove table tag))
    ;; Check memory management
    (is (= 3 (g:object-ref-count table)))   ; TODO: Why 3 references?!
    (is (= 1 (g:object-ref-count tag)))
    (is (= 1 (g:object-ref-count buffer)))))

;;;     gtk_text_iter_can_insert

(test gtk-text-iter-can-insert
  (let* ((buffer (make-instance 'gtk:text-buffer
                                :text
                                "Some sample text for the text buffer."))
         (start (gtk:text-buffer-iter-at-offset buffer 12))
         (end (gtk:text-buffer-iter-at-offset buffer 16))
         (table (gtk:text-buffer-tag-table buffer))
         (tag (make-instance 'gtk:text-tag
                             :name "editable"
                             :editable nil)))
    (is-true (gtk:text-tag-table-add table tag))
    (is-false (gtk:text-buffer-apply-tag buffer "editable" start end))

    (is-false (gtk:text-iter-can-insert start nil))
    (is-true (gtk:text-iter-can-insert start t))

    (is-false (gtk:text-iter-can-insert end nil))
    (is-true (gtk:text-iter-can-insert end t))
    ;; Remove tag from text tag table
    (is-false (gtk:text-tag-table-remove table tag))
    ;; Check memory management
    (is (= 3 (g:object-ref-count table)))   ; TODO: Why 3 references?!
    (is (= 1 (g:object-ref-count tag)))
    (is (= 1 (g:object-ref-count buffer)))))

;;;     gtk_text_iter_starts_word
;;;     gtk_text_iter_ends_word
;;;     gtk_text_iter_inside_word

(test gtk-text-iter-starts/ends-word
  (let* ((buffer (make-instance 'gtk:text-buffer
                                :text
                                "Some sample text for the text buffer."))
         (start (gtk:text-buffer-iter-at-offset buffer 12))
         (end (gtk:text-buffer-iter-at-offset buffer 16)))
    (is-true (gtk:text-iter-starts-word start))
    (is-true (gtk:text-iter-ends-word end))
    (is-true (gtk:text-iter-inside-word start))
    (is-false (gtk:text-iter-inside-word end))))

;;;     gtk_text_iter_starts_line
;;;     gtk_text_iter_ends_line

(test gtk-text-iter-starts/ends-line
  (let* ((buffer (make-instance 'gtk:text-buffer
                                :text
                                "Some sample text for the text buffer."))
         (start (gtk:text-buffer-start-iter buffer))
         (end (gtk:text-buffer-end-iter buffer)))

    (is-true (gtk:text-iter-starts-line start))
    (is-true (gtk:text-iter-ends-line end))))

;;;     gtk_text_iter_starts_sentence
;;;     gtk_text_iter_ends_sentence
;;;     gtk_text_iter_inside_sentence

(test gtk-text-iter-starts/ends-sentence
  (let* ((buffer (make-instance 'gtk:text-buffer
                                :text
                                "Some sample text for the text buffer."))
         (start (gtk:text-buffer-start-iter buffer))
         (end (gtk:text-buffer-end-iter buffer)))
    (is-true (gtk:text-iter-starts-sentence start))
    (is-true (gtk:text-iter-ends-sentence end))
    (is-true (gtk:text-iter-inside-sentence start))
    (is-false (gtk:text-iter-inside-sentence end))))

;;;     gtk_text_iter_is_cursor_position

(test gtk-text-iter-is-cursor-position
  (let* ((buffer (make-instance 'gtk:text-buffer
                                :text
                                "Some sample text for the text buffer."))
         (start (gtk:text-buffer-iter-at-offset buffer 12))
         (end (gtk:text-buffer-iter-at-offset buffer 16)))
    (is-true (gtk:text-iter-is-cursor-position start))
    (is-true (gtk:text-iter-is-cursor-position end))))

;;;     gtk_text_iter_get_chars_in_line

(test gtk-text-iter-chars-in-line
  (let* ((buffer (make-instance 'gtk:text-buffer
                                :text
                                "Zwölf Ägypter gehen auf der Straße."))
         (start (gtk:text-buffer-iter-at-offset buffer 12))
         (end (gtk:text-buffer-iter-at-offset buffer 16)))
    (is (= 35 (gtk:text-iter-chars-in-line start)))
    (is (= 35 (gtk:text-iter-chars-in-line end)))))

;;;     gtk_text_iter_get_bytes_in_line

(test gtk-text-iter-bytes-in-line
  (let* ((buffer (make-instance 'gtk:text-buffer
                                :text
                                "Zwölf Ägypter gehen auf der Straße."))
         (start (gtk:text-buffer-iter-at-offset buffer 12))
         (end (gtk:text-buffer-iter-at-offset buffer 16)))
    (is (= 38 (gtk:text-iter-bytes-in-line start)))
    (is (= 38 (gtk:text-iter-bytes-in-line end)))))

;;;     gtk_text_iter_get_language

(test gtk-text-iter-language
  (let* ((buffer (make-instance 'gtk:text-buffer
                                :text "Some sample text for the text buffer."))
         (iter (gtk:text-buffer-start-iter buffer)))
    (is (typep (gtk:text-iter-language iter) 'pango:language))
    (is (string= (pango:language-to-string (gtk:default-language))
                 (pango:language-to-string (gtk:text-iter-language iter))))))

;;;     gtk_text_iter_is_end
;;;     gtk_text_iter_is_start

(test gtk-text-iter-is-end/start
  (let* ((buffer (make-instance 'gtk:text-buffer
                                :text
                                "Some sample text for the text buffer."))
         (start (gtk:text-buffer-start-iter buffer))
         (end (gtk:text-buffer-end-iter buffer)))
    (is-true (gtk:text-iter-is-start start))
    (is-false (gtk:text-iter-is-start end))
    (is-false (gtk:text-iter-is-end start))
    (is-true (gtk:text-iter-is-end end))))

;;;     gtk:text-iter-move

;;;     gtk_text_iter_forward_char
;;;     gtk_text_iter_backward_char
;;;     gtk_text_iter_forward_chars
;;;     gtk_text_iter_backward_chars

(test gtk-text-iter-move-char
  (let* ((buffer (make-instance 'gtk:text-buffer
                                :text
                                "Zwölf Ägypter gehen auf der Straße."))
         (start (gtk:text-buffer-start-iter buffer)))
    (is (eq #\Z (gtk:text-iter-char start)))
    (is-true (gtk:text-iter-move start))
    (is (eq #\w (gtk:text-iter-char start)))
    (is-true (gtk:text-iter-move start :direction :backward))
    (is (eq #\Z (gtk:text-iter-char start)))
    (is-true (gtk:text-iter-move start :direction :forward))
    (is (eq #\w (gtk:text-iter-char start)))
    (is-true (gtk:text-iter-move start :count 5 :direction :forward :by :char))
    (is (eq #\Ä (gtk:text-iter-char start)))))

;;;     gtk_text_iter_forward_line
;;;     gtk_text_iter_backward_line
;;;     gtk_text_iter_forward_lines
;;;     gtk_text_iter_backward_lines
;;;     gtk_text_iter_forward_word_ends
;;;     gtk_text_iter_backward_word_starts
;;;     gtk_text_iter_forward_word_end
;;;     gtk_text_iter_backward_word_start
;;;     gtk_text_iter_forward_cursor_position
;;;     gtk_text_iter_backward_cursor_position
;;;     gtk_text_iter_forward_cursor_positions
;;;     gtk_text_iter_backward_cursor_positions
;;;     gtk_text_iter_backward_sentence_start
;;;     gtk_text_iter_backward_sentence_starts
;;;     gtk_text_iter_forward_sentence_end
;;;     gtk_text_iter_forward_sentence_ends
;;;     gtk_text_iter_forward_visible_word_ends
;;;     gtk_text_iter_backward_visible_word_starts
;;;     gtk_text_iter_forward_visible_word_end
;;;     gtk_text_iter_backward_visible_word_start
;;;     gtk_text_iter_forward_visible_cursor_position
;;;     gtk_text_iter_backward_visible_cursor_position
;;;     gtk_text_iter_forward_visible_cursor_positions
;;;     gtk_text_iter_backward_visible_cursor_positions
;;;     gtk_text_iter_forward_visible_line
;;;     gtk_text_iter_backward_visible_line
;;;     gtk_text_iter_forward_visible_lines
;;;     gtk_text_iter_backward_visible_lines

;;;     gtk_text_iter_forward_to_end

(test gtk-text-iter-forward-to-end
  (let* ((buffer (make-instance 'gtk:text-buffer
                                :text
                                "Some sample text for the text buffer."))
         (iter (gtk:text-buffer-start-iter buffer)))
    (is (eq #\S (gtk:text-iter-char iter)))
    ;; Move to the end of the text buffer
    (is-false (gtk:text-iter-forward-to-end iter))
    (is (eq #\Nul (gtk:text-iter-char iter)))))

;;;     gtk_text_iter_forward_to_line_end

(test gtk-text-iter-forward-to-line-end
  (let* ((buffer (make-instance 'gtk:text-buffer
                                :text
                                "Some sample text for the text buffer."))
         (iter (gtk:text-buffer-start-iter buffer)))
    (is (eq #\S (gtk:text-iter-char iter)))
    ;; Move to the end of the text buffer
    (is-false (gtk:text-iter-forward-to-line-end iter))
    (is (eq #\Nul (gtk:text-iter-char iter)))))

;;;     gtk_text_iter_forward_to_tag_toggle
;;;     gtk_text_iter_backward_to_tag_toggle

(test gtk-text-iter-forward/backward-to-tag-toggle
  (let* ((buffer (make-instance 'gtk:text-buffer
                                :text
                                "Some sample text for the text buffer."))
         (start (gtk:text-buffer-iter-at-offset buffer 12))
         (end (gtk:text-buffer-iter-at-offset buffer 16))
         (table (gtk:text-buffer-tag-table buffer))
         (tag (make-instance 'gtk:text-tag
                             :name "bold"
                             :weight 700)))
    (is-true (gtk:text-tag-table-add table tag))
    (is-false (gtk:text-buffer-apply-tag buffer "bold" start end))

    (is (typep (setf start (gtk:text-buffer-start-iter buffer)) 'gtk:text-iter))
    (is (typep (setf end (gtk:text-buffer-end-iter buffer)) 'gtk:text-iter))

    (is-true (gtk:text-iter-forward-to-tag-toggle start tag))
    (is (= 12 (gtk:text-iter-offset start)))
    (is-true (gtk:text-iter-backward-to-tag-toggle end tag))
    (is (= 16 (gtk:text-iter-offset end)))
    ;; Remove tag from text tag table
    (is-false (gtk:text-tag-table-remove table tag))
    ;; Check memory management
    (is (= 3 (g:object-ref-count table)))   ; TODO: Why 3 references?!
    (is (= 1 (g:object-ref-count tag)))
    (is (= 1 (g:object-ref-count buffer)))))

;;;     gtk_text_iter_forward_find_char
;;;     gtk_text_iter_backward_find_char

(test gtk-text-iter-find-char
  (let* ((buffer (make-instance 'gtk:text-buffer
                                :text *gtk-text-iter-sample-text*))
         (start (gtk:text-buffer-start-iter buffer))
         (end (gtk:text-buffer-end-iter buffer)))

    (is-true (gtk:text-iter-find-char start
                                      (lambda (x) (or (eq x #\ü) (eq x #\Ü)))))
    (is (= 152 (gtk:text-iter-offset start)))

    (is-true (gtk:text-iter-find-char end
                                      (lambda (x) (or (eq x #\ü) (eq x #\Ü)))
                                      :direction :backward
                                      :limit start))
    (is (= 1745 (gtk:text-iter-offset end)))))

;;;     gtk_text_iter_forward_search
;;;     gtk_text_iter_backward_search

(test gtk-text-iter-search
  (let* ((buffer (make-instance 'gtk:text-buffer
                                :text *gtk-text-iter-sample-text*))
         (iter1 (gtk:text-buffer-start-iter buffer))
         (iter2 (gtk:text-buffer-end-iter buffer)))

    (multiple-value-bind (start end)
        (gtk:text-iter-search iter1 "Gürtel")
      (is (= 870 (gtk:text-iter-offset start)))
      (is (= 876 (gtk:text-iter-offset end))))

    (multiple-value-bind (start end)
        (gtk:text-iter-search iter2 "Gürtel" :direction :backward)
      (is (= 870 (gtk:text-iter-offset start)))
      (is (= 876 (gtk:text-iter-offset end))))

    (is-false (gtk:text-iter-search iter1 "XXXX" :direction :forward))))

;;;     gtk_text_iter_equal

(test gtk-text-iter-equal
  (let* ((buffer (make-instance 'gtk:text-buffer
                                :text
                                "Some sample text for the text buffer."))
         (start1 (gtk:text-buffer-iter-at-offset buffer 12))
         (start2 (gtk:text-buffer-iter-at-offset buffer 12))
         (end (gtk:text-buffer-iter-at-offset buffer 16)))

    (is-true (gtk:text-iter-equal start1 start2))
    (is-false (gtk:text-iter-equal start1 end))
    (is-false (gtk:text-iter-equal start2 end))))

;;;     gtk_text_iter_compare

(test gtk-text-iter-compare
  (let* ((buffer (make-instance 'gtk:text-buffer
                                :text
                                "Some sample text for the text buffer."))
         (start1 (gtk:text-buffer-iter-at-offset buffer 12))
         (start2 (gtk:text-buffer-iter-at-offset buffer 12))
         (end (gtk:text-buffer-iter-at-offset buffer 16)))

    (is (=  0 (gtk:text-iter-compare start1 start2)))
    (is (= -1 (gtk:text-iter-compare start1 end)))
    (is (=  1 (gtk:text-iter-compare end start2)))))

;;;     gtk_text_iter_in_range

(test gtk-text-iter-in-range
  (let* ((buffer (make-instance 'gtk:text-buffer
                                :text
                                "Some sample text for the text buffer."))
         (start (gtk:text-buffer-iter-at-offset buffer 12))
         (center (gtk:text-buffer-iter-at-offset buffer 14))
         (end (gtk:text-buffer-iter-at-offset buffer 16)))

    (is-true (gtk:text-iter-in-range center start end))
    (is-false (gtk:text-iter-in-range end start center))
    (is-false (gtk:text-iter-in-range start center end))))

;;;     gtk_text_iter_order

(test gtk-text-iter-order
  (let* ((buffer (make-instance 'gtk:text-buffer
                                :text
                                "Some sample text for the text buffer."))
         (start (gtk:text-buffer-iter-at-offset buffer 12))
         (center (gtk:text-buffer-iter-at-offset buffer 14))
         (end (gtk:text-buffer-iter-at-offset buffer 16)))

    (is-false (gtk:text-iter-order end start))

    (is-true (gtk:text-iter-in-range center end start))
    (is-false (gtk:text-iter-in-range start end center))
    (is-false (gtk:text-iter-in-range end center start))))

;;; 2024-10-26

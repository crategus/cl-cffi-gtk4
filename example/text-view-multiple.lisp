;;;; Multiple Views
;;;;
;;;; The <tt>GtkTextView</tt> widget displays a <tt>GtkTextBuffer</tt> object.
;;;; One <tt>GtkTextBuffer</tt> object can be displayed by multiple
;;;; <tt>GtkTextView</tt> widgets. This demo has two views displaying a single
;;;; buffer, and shows off the widget's text formatting features.
;;;;
;;;; 2024-4-6

(in-package :gtk4-example)

;; Create a bunch of tags. Note that it is also possible to create tags with
;; the gtk:text-tag-new function then add them to the tag table for the buffer,
;; the gtk:text-buffer-create-tag function is just a convenience function. Also
;; note that you do not have to give tags a name; pass nil for the name to
;; create an anonymous tag.
;;
;; In any real app, another useful optimization would be to create a
;; GtkTextTagTable in advance, and reuse the same tag table for all the buffers
;; with the same tag set, instead of creating new copies of the same tags for
;; every buffer.
;;
;; Tags are assigned default priorities in order of addition to the tag table.
;; That is, tags created later that affect the same text property affected by
;; an earlier tag will override the earlier tag.  You can modify tag priorities
;; with the gtk:text-tag-priority function.
(defun create-tags (buffer)
  (gtk:text-buffer-create-tag buffer "heading"
                                     :weight 700 ; for :bold
                                     :size (* 15 pango:+scale+))
  (gtk:text-buffer-create-tag buffer "italic"
                                     :style :italic)
  (gtk:text-buffer-create-tag buffer "bold"
                                     :weight 700) ; for :bold
  (gtk:text-buffer-create-tag buffer "big"
                                     :size (* 20 pango:+scale+))
  (gtk:text-buffer-create-tag buffer "xx-small"
                                     :scale pango:+scale-xx-small+)
  (gtk:text-buffer-create-tag buffer "x-large"
                                     :scale pango:+scale-x-large+)
  (gtk:text-buffer-create-tag buffer "monospace"
                                     :family "monospace")
  (gtk:text-buffer-create-tag buffer "blue-foreground"
                                     :foreground "blue")
  (gtk:text-buffer-create-tag buffer "red-background"
                                     :background "red")
  (gtk:text-buffer-create-tag buffer "big-gap-before-line"
                                     :pixels-above-lines 30)
  (gtk:text-buffer-create-tag buffer "big-gap-after-line"
                                     :pixels-below-lines 30)
  (gtk:text-buffer-create-tag buffer "double-spaced-line"
                                     :pixels-inside-wrap 10)
  (gtk:text-buffer-create-tag buffer "not-editable"
                                     :editable nil)
  (gtk:text-buffer-create-tag buffer "word-wrap"
                                     :wrap-mode :word)
  (gtk:text-buffer-create-tag buffer "char-wrap"
                                     :wrap-mode :char)
  (gtk:text-buffer-create-tag buffer "no-wrap"
                                     :wrap-mode :none)
  (gtk:text-buffer-create-tag buffer "center"
                                     :justification :center)
  (gtk:text-buffer-create-tag buffer "right-justify"
                                     :justification :right)
  (gtk:text-buffer-create-tag buffer "wide-margins"
                                     :left-margin 50
                                     :right-margin 50)
  (gtk:text-buffer-create-tag buffer "strikethrough"
                                     :strikethrough t)
  (gtk:text-buffer-create-tag buffer "underline"
                                     :underline :single)
  (gtk:text-buffer-create-tag buffer "double-underline"
                                     :underline :double)
  (gtk:text-buffer-create-tag buffer "superscript"
                                     :rise (* 10 pango:+scale+)
                                     :size (* 8 pango:+scale+))
  (gtk:text-buffer-create-tag buffer "subscript"
                                     :rise (* -10 pango:+scale+)
                                     :size (* 8 pango:+scale+))
  (gtk:text-buffer-create-tag buffer "rtl-quote"
                                     :wrap-mode :word
                                     :direction :rtl
                                     :indent 30
                                     :left-margin 20
                                     :right-margin 20))

(defun insert-text (view)
  (let* ((display (gtk:widget-display view))
         (theme (gtk:icon-theme-for-display display))
         (icon (gtk:icon-theme-lookup-icon theme
                                           "drive-hardisk"
                                           nil
                                           32 1
                                           (gtk:widget-direction view)
                                           :none))
         (buffer (gtk:text-view-buffer view))
         ;; Get start of buffer. Each insertion will revalidate the iterator
         ;; to point to just after the inserted text.
         (iter (gtk:text-buffer-iter-at-offset buffer 0)))

;; TODO: Implement GtkNuclearAnimation
;;  nuclear = gtk_nuclear_animation_new (TRUE);

    (gtk:text-buffer-begin-irreversible-action buffer)
    (gtk:text-buffer-insert buffer iter
                            (format nil
        "The text widget can display text with all kinds of nifty attributes. ~
         It also supports multiple views of the same buffer; this demo is ~
         showing the same buffer in two places.~%~%"))
    ;; Insert Font Styles section
    (gtk:text-buffer-insert-with-tags buffer iter "Font Styles. " "heading")
    (gtk:text-buffer-insert buffer iter "For example, you can have ")
    (gtk:text-buffer-insert-with-tags buffer iter "italic" "italic")
    (gtk:text-buffer-insert buffer iter ", ")
    (gtk:text-buffer-insert-with-tags buffer iter "bold" "bold")
    (gtk:text-buffer-insert buffer iter ",  or ")
    (gtk:text-buffer-insert-with-tags buffer iter
                                      "monospace (typewriter)"
                                      "monospace")
    (gtk:text-buffer-insert buffer iter ",  or ")
    (gtk:text-buffer-insert-with-tags buffer iter "big" "big")
    (gtk:text-buffer-insert buffer iter " text.")
    (gtk:text-buffer-insert buffer iter
                            (format nil
        "It's best not to hardcode specific text sizes; you can use relative ~
         sizes as with CSS, such as "))
    (gtk:text-buffer-insert-with-tags buffer iter "xx-small" "xx-small")
    (gtk:text-buffer-insert buffer iter " or ")
    (gtk:text-buffer-insert-with-tags buffer iter "x-large" "x-large")
    (gtk:text-buffer-insert buffer iter
                            (format nil
        " to ensure that your program properly adapts if the user changes the ~
         default font size.~%~%"))
    ;; Insert Colors section
    (gtk:text-buffer-insert-with-tags buffer iter "Colors. " "heading")
    (gtk:text-buffer-insert buffer iter "Colors such as ")
    (gtk:text-buffer-insert-with-tags buffer iter
                                      "a blue foreground"
                                      "blue-foreground")
    (gtk:text-buffer-insert buffer iter " or ")
    (gtk:text-buffer-insert-with-tags buffer iter
                                      "a red background"
                                      "red-background")
    (gtk:text-buffer-insert buffer iter " or even ")
    (gtk:text-buffer-insert-with-tags buffer iter
                                      "a blue foreground on red background"
                                      "blue-foreground"
                                      "red-background")
    (gtk:text-buffer-insert buffer iter
                            (format nil
                             " (select that to read it) can be used.~%~%"))
    ;; Insert Underline, Strikethrough, and Rise section
    (gtk:text-buffer-insert-with-tags buffer iter
                                      "Underline, strikethrough, and rise. "
                                      "heading")
    (gtk:text-buffer-insert-with-tags buffer iter
                                      "Strikethrough"
                                      "strikethrough")
    (gtk:text-buffer-insert buffer iter ", ")
    (gtk:text-buffer-insert-with-tags buffer iter
                                      "underline"
                                      "underline")
    (gtk:text-buffer-insert buffer iter ", ")
    (gtk:text-buffer-insert-with-tags buffer iter
                                      "double underline"
                                      "double-underline")
    (gtk:text-buffer-insert buffer iter ", ")
    (gtk:text-buffer-insert-with-tags buffer iter
                                      "superscript"
                                      "superscript")
    (gtk:text-buffer-insert buffer iter ", and ")
    (gtk:text-buffer-insert-with-tags buffer iter
                                      "subscript"
                                      "subscript")
    (gtk:text-buffer-insert buffer iter
                            (format nil " are all supported.~%~%"))
    ;; Insert Images section
    (gtk:text-buffer-insert-with-tags buffer iter "Images. " "heading")
    (gtk:text-buffer-insert buffer iter "The buffer can have images in it: ")
    (gtk:text-buffer-insert-paintable buffer iter icon)

;; TODO: Implement GtkNuclearAnimation
;;  gtk_text_buffer_insert_paintable (buffer, &iter, nuclear);

    (gtk:text-buffer-insert buffer iter (format nil " for example.~%~%"))
    ;; Insert Spacing section
    (gtk:text-buffer-insert-with-tags buffer iter
                                      "Spacing. "
                                      "heading")
    (gtk:text-buffer-insert buffer iter
                            (format nil
        "You can adjust the amount of space before each line.~%"))
    (gtk:text-buffer-insert-with-tags buffer iter
                                      (format nil
        "This line has a whole lot of space before it.~%")
                                      "big-gap-before-line"
                                      "wide-margins")
    (gtk:text-buffer-insert-with-tags buffer iter
                                      (format nil
        "You can also adjust the amount of space after each line; ~
         this line has a whole lot of space after it.~%")
                                      "big-gap-after-line"
                                      "wide-margins")
    (gtk:text-buffer-insert-with-tags buffer iter
                                      (format nil
        "You can also adjust the amount of space between wrapped lines ~
         this line has extra space between each wrapped line in the same ~
         paragraph. To show off wrapping, some filler text: the quick ~
         brown fox jumped over the lazy dog. Blah blah blah blah blah ~
         blah blah blah blah.~%")
                                      "double-spaced-line"
                                      "wide-margins")
    (gtk:text-buffer-insert buffer iter
                            (format nil
        "Also note that those lines have extra-wide margins.~%~%"))
    ;; Insert Editable section
    (gtk:text-buffer-insert-with-tags buffer iter "Editability. " "heading")
    (gtk:text-buffer-insert-with-tags buffer iter
                                      (format nil
        "This line is 'locked down' and can't be edited by the user - just ~
         try it! You can't delete this line.~%~%")
                                      "not-editable")
    ;; Insert Wrapping section
    (gtk:text-buffer-insert-with-tags buffer iter "Wrapping. " "heading")
    (gtk:text-buffer-insert buffer iter
                            (format nil
        "This line (and most of the others in this buffer) is word-wrapped, ~
         using the proper Unicode algorithm. Word wrap should work in all ~
         scripts and languages that GTK supports. Let's make this a long ~
         paragraph to demonstrate: blah blah blah blah blah blah blah blah ~
         blah blah blah blah blah blah blah blah blah blah blah~%~%"))
    (gtk:text-buffer-insert-with-tags buffer iter
                                      (format nil
        "This line has character-based wrapping, and can wrap between any two ~
         character glyphs. Let's make this a long paragraph to demonstrate: ~
         blah blah blah blah blah blah blah blah blah blah blah blah blah blah ~
         blah blah blah blah blah~%~%")
         "char-wrap")
    (gtk:text-buffer-insert-with-tags buffer iter
                                      (format nil
        "This line has all wrapping turned off, so it makes the horizontal ~
         scrollbar appear.~%~%")
                                      "no-wrap")
    ;; Insert Justification section
    (gtk:text-buffer-insert-with-tags buffer iter
                                      (format nil "Justification.~%")
                                      "heading")
    (gtk:text-buffer-insert-with-tags buffer iter
                                      (format nil
        "This line has center justification.~%")
                                      "center")
    (gtk:text-buffer-insert-with-tags buffer iter
                                      (format nil
        "This line has right justification.~%")
                                      "right-justify")
    (gtk:text-buffer-insert-with-tags buffer iter
                                      (format nil
        "~%This line has big wide margins. Text text text text text text text ~
         text text text text text text text text text text text text text text ~
         text text text text text text text text text text text text text text ~
         text.~%")
                                      "wide-margins")
    ;; Insert Internationalization section
    (gtk:text-buffer-insert-with-tags buffer iter
                                      (format nil "Internationalization.~%")
                                      "heading")

;; TODO: Implement the missing section about Internationalization

;;  gtk_text_buffer_insert (buffer, &iter,
;;      "You can put all sorts of Unicode text in the buffer.\n\nGerman "
;;      "(Deutsch S\303\274d) Gr\303\274\303\237 Gott\nGreek "
;;      "(\316\225\316\273\316\273\316\267\316\275\316\271\316\272\316\254) "
;;      "\316\223\316\265\316\271\316\254 \317\203\316\261\317\202\nHebrew     "
;;      "\327\251\327\234\327\225\327\235\nJapanese "
;;      "(\346\227\245\346\234\254\350\252\236)\n\nThe widget properly handles "
;;      "bidirectional text, word wrapping, DOS/UNIX/Unicode paragraph "
;;      "separators, grapheme boundaries, and so on using the Pango "
;;      " internationalization framework.\n", -1);

;;  gtk_text_buffer_insert (buffer, &iter,
;;      "Here's a word-wrapped quote in a right-to-left language:\n", -1);
;;  gtk_text_buffer_insert_with_tags_by_name (buffer, &iter,
;;      "\331\210\331\202\330\257 \330\250\330\257\330\243 "
;;      "\330\253\331\204\330\247\330\253 \331\205\331\206 "
;;      "\330\243\331\203\330\253\330\261"
;;      "\330\247\331\204\331\205\330\244\330\263\330\263\330\247\330\252 "
;;      "\330\252\331\202\330\257\331\205\330\247 \331\201\331\212 "
;;      "\330\264\330\250\331\203\330\251"
;;      "\330\247\331\203\330\263\331\212\331\210\331\206 "
;;      "\330\250\330\261\330\247\331\205\330\254\331\207\330\247 "
;;      "\331\203\331\205\331\206\330\270\331\205\330\247\330\252 "
;;      "\331\204\330\247 \330\252\330\263\330\271\331\211"
;;      "\331\204\331\204\330\261\330\250\330\255\330\214 "
;;      "\330\253\331\205 \330\252\330\255\331\210\331\204\330\252 "
;;      "\331\201\331\212"
;;      "\330\247\331\204\330\263\331\206\331\210\330\247\330\252 "
;;      "\330\247\331\204\330\256\331\205\330\263"
;;      "\330\247\331\204\331\205\330\247\330\266\331\212\330\251 "
;;      "\330\245\331\204\331\211"
;;      "\331\205\330\244\330\263\330\263\330\247\330\252 "
;;      "\331\205\330\247\331\204\331\212\330\251"
;;      " \331\205\331\206\330\270\331\205\330\251\330\214 "
;;      "\331\210\330\250\330\247\330\252\330\252"
;;;     "\330\254\330\262\330\241\330\247 "
;;      "\331\205\331\206 \330\247\331\204\331\206\330\270\330\247\331\205 "
;;      "\330\247\331\204\331\205\330\247\331\204\331\212 \331\201\331\212 "
;;      "\330\250\331\204\330\257\330\247\331\206\331\207\330\247\330\214 "
;;      "\331\210\331\204\331\203\331\206\331\207\330\247"
;;      " \330\252\330\252\330\256\330\265\330\265 "
;;      "\331\201\331\212 \330\256\330\257\331\205\330\251"
;;      "\331\202\330\267\330\247\330\271 "
;;      "\330\247\331\204\331\205\330\264\330\261\331\210\330\271\330\247\330"
;;      "\252 "
;;      "\330\247\331\204\330\265\330\272\331\212\330\261\330\251. "
;;      "\331\210\330\243\330\255\330\257 "
;;      "\330\243\331\203\330\253\330\261 \331\207\330\260\331\207 "
;;      "\330\247\331\204\331\205\330\244\330\263\330\263\330\247\330\252 "
;;      "\331\206\330\254\330\247\330\255\330\247 \331\207\331\210 "
;;      "\302\273\330\250\330\247\331\206\331\203\331\210\330\263\331"
;;      "\210\331\204\302\253 "
;;      "\331\201\331\212"
;;      "\330\250\331\210\331\204\331\212\331\201\331\212\330\247.\n\n",
;;      -1, "rtl_quote", NULL);

    ;; Insert Widgets section
    (gtk:text-buffer-insert-with-tags buffer iter
                                      (format nil "Widgets.~%")
                                      "heading")
    (gtk:text-buffer-insert buffer iter
                            (format nil
        "You can put widgets in the buffer: Here's a button: "))
    ;; Insert the anchors and the text for the widgets
    (gtk:text-buffer-create-child-anchor buffer iter)
    (gtk:text-buffer-insert buffer iter " and a menu: ")
    (gtk:text-buffer-create-child-anchor buffer iter)
    (gtk:text-buffer-insert buffer iter " and a scale: ")
    (gtk:text-buffer-create-child-anchor buffer iter)
    (gtk:text-buffer-insert buffer iter " finally a text entry: ")
    (gtk:text-buffer-create-child-anchor buffer iter)
    (gtk:text-buffer-insert buffer iter (format nil ".~%"))
    ;; Last section
    (gtk:text-buffer-insert buffer iter
                            (format nil
        "~%~%This demo doesn't demonstrate all the GtkTextBuffer features; ~
         it leaves out, for example: invisible/hidden text, tab stops, ~
         application-drawn areas on the sides of the widget for displaying ~
         breakpoints and such..."))
    ;; Apply word_wrap tag to whole buffer
    (multiple-value-bind (start end)
        (gtk:text-buffer-bounds buffer)
      (gtk:text-buffer-apply-tag buffer "word-wrap" start end))
    (gtk:text-buffer-end-irreversible-action buffer)))

(defun attach-widgets (view)
  (flet ((find-anchor (iter)
           (iter (while (gtk:text-iter-move iter))
                 (let ((anchor (gtk:text-iter-child-anchor iter)))
                   (when anchor (return anchor))))))
    (let* ((buffer (gtk:text-view-buffer view))
           (iter (gtk:text-buffer-start-iter buffer))
           (anchor nil) (widget nil))
      (iter (for count from 0)
            (while (setf anchor (find-anchor iter)))
            (cond ((= 0 count)
                   (setf widget
                         (gtk:button-new-with-label "Click me"))
                   (g:signal-connect widget "clicked"
                                     (lambda (button)
                                       (easter-egg-cb button))))
                  ((= 1 count)
                   (let ((strings '("Option 1" "Option 2" "Option 3")))
                     (setf widget
                           (gtk:drop-down-new-from-strings strings))))
                  ((= 2 count)
                   (setf widget
                         (make-instance 'gtk:scale
                                        :orientation :horizontal
                                        :width-request 100
                                        :height-request -1))
                   (gtk:range-set-range widget 0 100))
                  ((= 3 count)
                   (setf widget
                         (make-instance 'gtk:entry
                                        :width-chars 10))))
            (gtk:text-view-add-child-at-anchor view widget anchor)))))

(defun recursive-attach-view (depth view anchor)
  (unless (> depth 4)
    (let* ((buffer (gtk:text-view-buffer view))
           (child (make-instance 'gtk:text-view
                                 :buffer buffer
                                 :top-margin 3
                                 :bottom-margin 3
                                 :left-margin 3
                                 :right-margin 3
                                 :indent 20
                                 :width-request (- 400 (* 20 depth))))
           (frame (make-instance 'gtk:frame
                                 :child child
                                 :label
                                 (format nil "Nested Text View (~a)" depth))))
    (gtk:text-view-add-child-at-anchor view frame anchor)
    (recursive-attach-view (+ depth 1) child anchor))))

(defun easter-egg-cb (button)
  (let* ((buffer (gtk:text-buffer-new))
         (iter (gtk:text-buffer-start-iter buffer))
         (view (make-instance 'gtk:text-view
                              :buffer buffer
                              :top-margin 3
                              :bottom-margin 3
                              :left-margin 6
                              :right-margin 6))
         (scrolled (make-instance 'gtk:scrolled-window
                                  :child view
                                  :vscrollbar-policy :automatic
                                  :hscrollbar-policy :automatic))
         (window (make-instance 'gtk:window
                                :title "Nested Text View"
                                :child scrolled
                                :modal t
                                :transient-for (gtk:widget-root button)
                                :default-width 400
                                :default-height 400))
         (anchor nil))
    (gtk:text-buffer-insert buffer iter
                            (format nil
        "This buffer is shared by a set of nested text views.~%~%"))
    (setf anchor (gtk:text-buffer-create-child-anchor buffer iter))
    (gtk:text-buffer-insert buffer iter
                            (format nil
        "~%Don't do this in real applications, please.~%"))
    (recursive-attach-view 1 view anchor)
    (gtk:window-present window)))

(defun do-text-view-multiple (&optional application)
  (let* ((view1 (make-instance 'gtk:text-view
                               :top-margin 6
                               :bottom-margin 6
                               :left-margin 6
                               :right-margin 6))
         ;; For convenience, we just use the autocreated buffer from the first
         ;; text view; you could also create the buffer by itself with the
         ;;; gtk:text-buffer-new function, then later create a view widget.
         (buffer (gtk:text-view-buffer view1))
         (view2 (make-instance 'gtk:text-view
                               :buffer buffer
                               :top-margin 6
                               :bottom-margin 6
                               :left-margin 6
                               :right-margin 6))
         (vpaned (make-instance 'gtk:paned
                                :orientation :vertical))
         (window (make-instance 'gtk:window
                                :title "Text View Multiple Views"
                                :child vpaned
                                :application application
                                :default-with 450
                                :default-height 450)))
    (let ((scrolled (make-instance 'gtk:scrolled-window
                                   :hscrollbar-policy :automatic
                                   :vscrollbar-policy :automatic)))
      (setf (gtk:paned-start-child vpaned) scrolled)
      (setf (gtk:paned-resize-start-child vpaned) nil)
      (setf (gtk:paned-shrink-start-child vpaned) t)
      (setf (gtk:scrolled-window-child scrolled) view1))
    (let ((scrolled (make-instance 'gtk:scrolled-window
                                   :hscrollbar-policy :automatic
                                   :vscrollbar-policy :automatic)))
      (setf (gtk:paned-end-child vpaned) scrolled)
      (setf (gtk:paned-resize-end-child vpaned) t)
      (setf (gtk:paned-shrink-end-child vpaned) t)
      (setf (gtk:scrolled-window-child scrolled) view2))
    (create-tags buffer)
    (insert-text view1)
    (attach-widgets view1)
    (attach-widgets view2)
    (gtk:window-present window)))

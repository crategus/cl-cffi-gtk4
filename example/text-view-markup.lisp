;;;; Markup
;;;;
;;;; <tt>GtkTextBuffer</tt> lets you define your own tags that can influence
;;;; text formatting in a variety of ways. In this example, we show that
;;;; <tt>GtkTextBuffer</tt> can load Pango markup and automatically generate
;;;; suitable tags.
;;;;
;;;; 2024-4-6

(in-package :gtk4-example)

(defvar str
        (format nil
"Text sizes: <span size='xx-small'>tiny </span><span size='x-small'>very small ~
</span><span size='small'>small </span><span size='medium'>normal </span>~
<span size='large'>large </span><span size='x-large'>very large </span>~
<span size='xx-large'>huge</span>

Text styles: <span style='normal'>Normal</span> <span style='italic'>Italic~
</span> <span style='oblique'>Olique</span>

Text weights: <span weight='thin'>thin</span> <span weight='light'>light</span>~
<span weight='normal'>normal</span> <span weight='bold'>bold</span> ~
<span weight='ultraheavy'>ultraheavy</span>

Text <span color='gray'>c<span color='green'>o</span>l<span color='tomato'>o~
</span>rs</span> and <span background='pink'>backgrounds</span>

Colorful <span underline='low' underline-color='blue'><span underline='double' ~
underline-color='red'>under</span>lines</span> and <span background='pink'>~
<span underline='error'>mo</span><span underline='error' ~
underline-color='green'>re</span></span>

Colorful <span strikethrough='true' strikethrough-color=~
'magenta'>strikethroughs</span> and <span overline='single' overline_color=~
'green'>overlines</span>

Superscripts and subscripts: 𝜀<span rise='-6000' size='x-small' font_desc=~
'italic'>0</span> = 𝜔<span rise='8000' size='smaller'>𝜔<span rise='14000' ~
size='smaller'>𝜔<span rise='20000'>.<span rise='23000'>.<span rise='26000'>.~
</span></span></span></span></span>

<span letter_spacing='3000'>Letterspacing</span>

OpenType font features: <span font_desc='sans regular' font_features=~
'dlig=0'>feast</span> versus <span font_desc='sans regular' font_features=~
'dlig=1'>feast</span>

Shortcuts: <tt>Monospace</tt> – <b>Bold</b> – <i>Italic</i> – <big>Big</big> – ~
<small>Small</small> – <u>Underlined</u> – <s>Strikethrough</s> – ~
Super<sup>script</sup> – Sub<sub>script</sub>

hy­phen­ation al­go­rithm is a <span allow_breaks='false' style='italic'>set of ~
rules</span>, espe­ci­ally one co­di­fied for im­ple­men­tation in a com­pu­ter ~
pro­gram, that de­ci­des at which points a word can be bro­ken over two lines with ~
a hy­phen. For ex­am­ple, a hy­phen­ation al­go­rithm might de­cide that im­peach­ment ~
can be broken as impeach‧ment or im‧peachment but not impe‧achment.

<span insert_hyphens='false'>one/two three/four five/six seven/eight nine/ten~
</span>

<span line_height='1.33'>Line height: This is an example of widely spaced ~
text. It was achieved by setting the line-height factor to 1.33. You can set ~
the line-height factor to any value between 0 and 10. ~
Note that the line height affects the spacing between paragraphs as well as ~
between the wrapped lines inside a paragraph.</span>

Transforms: <span text_transform='uppercase'>straße</span> <span ~
text_transform='capitalize'>up, up and away</span>"))

(defun do-text-view-markup (&optional application)
  (let* ((bytes nil) (markup nil)
         (view1 (make-instance 'gtk:text-view
                               :editable nil
                               :wrap-mode :char
                               :left-margin 10
                               :right-margin 10))
         (scrolled1 (make-instance 'gtk:scrolled-window
                                   :child view1
                                   :hscrollbar-policy :automatic
                                   :vscrollbar-policy :automatic))
         (view2 (make-instance 'gtk:text-view
                               :editable nil
                               :wrap-mode :char
                               :left-margin 10
                               :right-margin 10))
         (scrolled2 (make-instance 'gtk:scrolled-window
                                   :child view2
                                   :hscrollbar-policy :automatic
                                   :vscrollbar-policy :automatic))
         (header (make-instance 'gtk:header-bar))
         (stack (make-instance 'gtk:stack))
         (window (make-instance 'gtk:window
                                :title "Markup"
                                :application application
                                :child stack
                                :titlebar header
                                :default-width 600
                                :default-height 680))
         (button (make-instance 'gtk:check-button
                                :label "Source"
                                :valign :center
                                :active nil)))
    (gtk:header-bar-pack-start header button)
    (gtk:stack-add-named stack scrolled1 "formatted")
    (gtk:stack-add-named stack scrolled2 "source")
    (g:signal-connect button "toggled"
        (lambda (button)
          (if (gtk:check-button-active button)
              (setf (gtk:stack-visible-child-name stack) "source")
              (let ((buffer (gtk:text-view-buffer view2)))
                 (format t "Change view~%")
                 (multiple-value-bind (start end)
                     (gtk:text-buffer-bounds buffer)
                   (setf markup
                         (gtk:text-buffer-get-text buffer start end)))
                 (setf buffer
                       (gtk:text-view-buffer view1))
                 (multiple-value-bind (start end)
                     (gtk:text-buffer-bounds buffer)
                   (gtk:text-buffer-begin-irreversible-action buffer)
                   (gtk:text-buffer-delete buffer start end)
                   (gtk:text-buffer-insert-markup buffer start markup)
                   (gtk:text-buffer-end-irreversible-action buffer))
                 (setf (gtk:stack-visible-child-name stack) "formatted")))))
    (multiple-value-bind (data len)
        (cffi:foreign-string-alloc str)
      (setf bytes (g:bytes-new data len)))
    (setf markup (g:bytes-data bytes))
    (let* ((buffer (gtk:text-view-buffer view1))
           (start (gtk:text-buffer-start-iter buffer)))
      (gtk:text-buffer-begin-irreversible-action buffer)
      (gtk:text-buffer-insert-markup buffer start markup)
      (gtk:text-buffer-end-irreversible-action buffer))
    (let* ((buffer (gtk:text-view-buffer view2))
           (start (gtk:text-buffer-start-iter buffer)))
      (gtk:text-buffer-begin-irreversible-action buffer)
      (gtk:text-buffer-insert buffer start markup)
      (gtk:text-buffer-end-irreversible-action buffer))
    (gtk:window-present window)))

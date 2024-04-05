;;;; Hypertext
;;;;
;;;; Usually, tags modify the appearance of text in the view, e.g. making it
;;;; bold or colored or underlined. But tags are not restricted to appearance.
;;;; They can also affect the behavior of mouse and key presses, as this demo
;;;; shows. We also demonstrate adding other things to a text view, such as
;;;; clickable icons and widgets which can also replace a character (try copying
;;;; the ghost text).

(in-package :gtk4-example)

;; Quick-and-dirty text-to-speech for a single word. If you do not hear
;; anything, you are missing espeak-ng on your system.
(defun say-word (word)
  (cffi:foreign-funcall "g_spawn_async"
                        :pointer (cffi:null-pointer)
                        g:strv-t (list "espeak-ng" "-ven" word)
                        :pointer (cffi:null-pointer)
                        :int 4 ; for G_SPAWN_SEARCH_PATH
                        :pointer (cffi:null-pointer)
                        :pointer (cffi:null-pointer)
                        :pointer (cffi:null-pointer)
                        :pointer (cffi:null-pointer)
                        :boolean))

;; Looks at all tags covering the position of iter in the text view, and if one
;; of them is a link, follow it by showing the page identified by the data
;; attached to it.
(defun follow-if-link (view iter)
  (let ((tags (gtk:text-iter-tags iter)))
    (dolist (tag tags)
      (let ((page (g:object-data tag "page")))
        (when page
          (show-page view page)
          (return))))))

;; Links can be activated by pressing Enter.
(defun key-pressed-cb (view keyval)
  (when (or (= keyval 65293)   ; GDK_KEY_Return
            (= keyval 65421))  ; GDK_KEY_KP_Enter
    (let* ((buffer (gtk:text-view-buffer view))
           (mark (gtk:text-buffer-get-insert buffer))
           (iter (gtk:text-buffer-iter-at-mark buffer mark)))
      (follow-if-link view iter)))
  gdk:+event-propagate+)

(defun released-cb (view x y)
  (let ((x (round x))
        (y (round y))
        (buffer (gtk:text-view-buffer view))
        (iter nil))
  (multiple-value-bind (xbuffer ybuffer)
      (gtk:text-view-window-to-buffer-coords view :widget x y)
    (when (and (not (gtk:text-buffer-selection-bounds buffer))
               (setf iter
                     (gtk:text-view-iter-at-location view xbuffer ybuffer)))
      (follow-if-link view iter)))))

;; Looks at all tags covering the position (x, y) in the text view, and if one
;; of them is a link, change the cursor to the "hands" cursor typically used by
;; web browsers.
(let ((hovering-over-link nil))
  (defun motion-cb (view x y)
    (let ((x (round x))
          (y (round y))
          (hovering nil))
      (multiple-value-bind (xbuffer ybuffer)
          (gtk:text-view-window-to-buffer-coords view :widget x y)
        (let* ((iter (gtk:text-view-iter-at-location view xbuffer ybuffer))
               (tags (gtk:text-iter-tags iter)))
          (dolist (tag tags)
            (let ((page (g:object-data tag "page")))
              (when page
                (setf hovering t)
                (return))))
          (unless (eq hovering hovering-over-link)
            (setf hovering-over-link hovering)
            (if hovering-over-link
                (gtk:widget-set-cursor-from-name view "pointer")
                (gtk:widget-set-cursor-from-name view "text"))))))))

;; Inserts a piece of text into the buffer, giving it the usual appearance of a
;; hyperlink in a web browser: blue and underlined. Additionally, attaches some
;; data on the tag, to make it recognizable as a link.
(defun insert-link (buffer iter text page)
  (let ((tag (gtk:text-buffer-create-tag buffer nil
                                         :foreground "blue"
                                         :underline :single)))
    (setf (g:object-data tag "page") page)
    (gtk:text-buffer-insert-with-tags buffer iter text tag)))

;; Fills the buffer with text and interspersed links. In any real hypertext
;; application, this method would parse a file to identify the links.
(defun show-page (view page)
  (let* ((buffer (gtk:text-view-buffer view))
         (bold (gtk:text-buffer-create-tag buffer nil
                                           :weight 700 ; for :bold
                                           :scale pango:+scale-x-large+))
         (mono (gtk:text-buffer-create-tag buffer nil
                                           :familiy "monospace"))
         (nobreaks (gtk:text-buffer-create-tag buffer nil
                                               :allow-breaks nil))
         (iter nil))
    (setf (gtk:text-buffer-text buffer) "")
    (setf iter (gtk:text-buffer-iter-at-offset buffer 0))
    (gtk:text-buffer-begin-irreversible-action buffer)
    (cond ((= page 1)
           (gtk:text-buffer-insert buffer iter
                                   "Some text to show that simple ")
           (insert-link buffer iter "hypertext" 3)
           (gtk:text-buffer-insert buffer iter
                                   " can easily be realized with ")
           (insert-link buffer iter "tags" 2)
           (gtk:text-buffer-insert buffer iter
                                   (format nil ".~c" #\linefeed))
           (gtk:text-buffer-insert buffer iter
                                   "Of course you can also embed Emoji 😋, ")
           (gtk:text-buffer-insert buffer iter "icons ")
           (let* ((display (gtk:widget-display view))
                  (theme (gtk:icon-theme-for-display display))
                  (icon (gtk:icon-theme-lookup-icon theme
                                                    "view-conceal-symbolic"
                                                    nil
                                                    16
                                                    1
                                                    :ltr
                                                    :none)))
             (gtk:text-buffer-insert-paintable buffer iter icon))
           (gtk:text-buffer-insert buffer iter
                                   ", or even widgets ")
           (let ((anchor (gtk:text-buffer-create-child-anchor buffer iter))
                 (child (gtk:level-bar-new-for-interval 0 100)))
             (setf (gtk:level-bar-value child) 50)
             (setf (gtk:widget-size-request child) '(100 -1))
             (gtk:text-view-add-child-at-anchor view child anchor))
           (gtk:text-buffer-insert buffer iter "and labels with ")
           (let ((child (gtk:label-new "ghost"))
                 (anchor (gtk:text-child-anchor-new-with-replacement "👻")))
             (gtk:text-buffer-insert-child-anchor buffer iter anchor)
             (gtk:text-view-add-child-at-anchor view child anchor))
           (gtk:text-buffer-insert buffer iter " text."))
          ((= page 2)
           (let ((mark (gtk:text-buffer-create-mark buffer "mark" iter t))
                 (start nil))
             (gtk:text-buffer-insert-with-tags buffer iter "tag" bold)
             (gtk:text-buffer-insert buffer iter " /")
             (setf start (gtk:text-buffer-iter-at-mark buffer mark))
             (gtk:text-buffer-apply-tag buffer nobreaks start iter)
             (gtk:text-buffer-insert buffer iter " ")
             (gtk:text-buffer-move-mark buffer mark iter)
             (gtk:text-buffer-insert-with-tags buffer iter "tag" mono)
             (gtk:text-buffer-insert buffer iter " /")
             (setf start (gtk:text-buffer-iter-at-mark buffer mark))
             (gtk:text-buffer-apply-tag buffer nobreaks start iter)
             (gtk:text-buffer-insert buffer iter " ")
             (let* ((anchor (gtk:text-buffer-create-child-anchor buffer iter))
                    (iconname "audio-volume-high-symbolic")
                    (child (gtk:image-new-from-icon-name iconname))
                    (controller (gtk:gesture-click-new)))
               (gtk:widget-set-cursor-from-name child "pointer")
               (g:signal-connect controller "pressed"
                                 (lambda (gesture n x y)
                                   (declare (ignore gesture n x y))
                                   (say-word "tag")))
               (gtk:widget-add-controller child controller)
               (gtk:text-view-add-child-at-anchor view child anchor))
             (gtk:text-buffer-insert buffer iter
                                     (format nil
                                     "~%~%~
                                      An attribute that can be applied to some ~
                                      range of text. For example, a tag might ~
                                      be called “bold” and make the text ~
                                      inside the tag bold. ~
                                      ~%~%~
                                      However, the tag concept is more general ~
                                      than that;  tags don't have to affect ~
                                      appearance. They can instead affect the ~
                                      behavior of mouse and key presses, ~
                                      “lock” a range of text so the  user ~
                                      can't edit it, or countless other things.~
                                      ~%~%"))
             (insert-link buffer iter "Go back" 1)
             (gtk:text-buffer-delete-mark buffer mark)))
          ((= page 3)
           (let ((mark (gtk:text-buffer-create-mark buffer "mark" iter t))
                 (start nil))
             (gtk:text-buffer-insert-with-tags buffer iter "hypertext" bold)
             (gtk:text-buffer-insert buffer iter " /")
             (setf start (gtk:text-buffer-iter-at-mark buffer mark))
             (gtk:text-buffer-apply-tag buffer nobreaks start iter)
             (gtk:text-buffer-insert buffer iter " ")
             (gtk:text-buffer-move-mark buffer mark iter)
             (gtk:text-buffer-insert-with-tags buffer iter
                                               "ˈhaɪ pərˌtɛkst" mono)
             (gtk:text-buffer-insert buffer iter " /")
             (setf start (gtk:text-buffer-iter-at-mark buffer mark))
             (gtk:text-buffer-apply-tag buffer nobreaks start iter)
             (gtk:text-buffer-insert buffer iter " ")
             (let* ((anchor (gtk:text-buffer-create-child-anchor buffer iter))
                    (iconname "audio-volume-high-symbolic")
                    (child (gtk:image-new-from-icon-name iconname))
                    (controller (gtk:gesture-click-new)))
               (gtk:widget-set-cursor-from-name child "pointer")
               (g:signal-connect controller "pressed"
                                 (lambda (gesture n x y)
                                   (declare (ignore gesture n x y))
                                   (say-word "hypertext")))
               (gtk:widget-add-controller child controller)
               (gtk:text-view-add-child-at-anchor view child anchor))
             (gtk:text-buffer-insert buffer iter
                                     (format nil
                                     "~%~%~
                                      Machine-readable text that is not ~
                                      sequential but is organized so that ~
                                      related items of information are ~
                                      connected. ~
                                      ~%~%"))
             (insert-link buffer iter "Go back" 1)
             (gtk:text-buffer-delete-mark buffer mark)))
          (t
           (error "Page number out of range.")))
    (gtk:text-buffer-end-irreversible-action buffer)))

(defun do-text-view-hypertext (&optional application)
  (let* ((view (make-instance 'gtk:text-view
                              :wrap-mode :word
                              :top-margin 20
                              :bottom-margin 20
                              :left-margin 20
                              :right-margin 20
                              :pixels-below-line 10))
         (scrolled (make-instance 'gtk:scrolled-window
                                  :child view
                                  :hscrollbar-policy :never
                                  :vscrollbar-policy :automatic))
         (window (make-instance 'gtk:window
                                :title "Hypertext"
                                :application application
                                :child scrolled
                                :default-width 330
                                :default-height 330
                                :resizable nil)))
    ;; Allow undo and redo actions on the text buffer of the text view
    (setf (gtk:text-buffer-enable-undo (gtk:text-view-buffer view)) t)
    (let (controller)
      (setf controller (gtk:event-controller-key-new))
      (g:signal-connect controller "key-pressed"
                        (lambda (controller keyval keycode state)
                          (declare (ignore controller keycode state))
                          (key-pressed-cb view keyval)))
      (gtk:widget-add-controller view controller)
      (setf controller (gtk:gesture-click-new))
      (g:signal-connect controller "released"
                        (lambda (gesture n x y)
                          (declare (ignore n))
                          (when (= 1 (gtk:gesture-single-button gesture))
                            (released-cb view x y))))
      (gtk:widget-add-controller view controller)
      (setf controller (gtk:event-controller-motion-new))
      (g:signal-connect controller "motion"
                        (lambda (controller x y)
                          (declare (ignore controller))
                          (motion-cb view x y)))
      (gtk:widget-add-controller view controller))
    (show-page view 1)
    (gtk:window-present window)))

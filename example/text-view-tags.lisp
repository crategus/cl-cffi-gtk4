;;;; Text View Tags
;;;;
;;;; 2024-4-6

(in-package :gtk4-example)

(defun do-text-view-tags (&optional application)
  (flet ((on-toggle-button-clicked (button buffer tag)
           (when (gtk:text-buffer-has-selection buffer)
             (multiple-value-bind (start end)
                 (gtk:text-buffer-selection-bounds buffer)
               (if (gtk:toggle-button-active button)
                   (gtk:text-buffer-apply-tag buffer tag start end)
                   (gtk:text-buffer-remove-tag buffer tag start end))))))
    (let* ((vbox (make-instance 'gtk:box
                                :orientation :vertical))
           (window (make-instance 'gtk:window
                                  :application application
                                  :child vbox
                                  :title "Text View Tags"
                                  :default-width 350
                                  :default-height 250))
           (toolbar (make-instance 'gtk:box
                                   :orientation :horizontal
                                   :spacing 3))
           (textview (make-instance 'gtk:text-view
                                    :wrap-mode :word
                                    :hexpand t
                                    :vexpand t
                                    :top-margin 6
                                    :left-margin 6
                                    :right-margin 6))
           (buffer (gtk:text-view-buffer textview))
           (buttons nil))
      ;; Signal handler for cursor movements in the text buffer
      (g:signal-connect buffer "notify::cursor-position"
          (lambda (object pspec)
            (declare (ignore pspec))
            (let* ((cursor (gtk:text-buffer-cursor-position object))
                   (iter (gtk:text-buffer-iter-at-offset buffer cursor))
                   (tags (mapcar #'gtk:text-tag-name
                                 (gtk:text-iter-tags iter))))
              ;; Iterate over the toggle buttons
              (dolist (button buttons)
                (let ((label (gtk:widget-name button)))
                  ;; Activate/Deactivate the buttons
                  (if (member label tags :test #'string=)
                      (setf (gtk:toggle-button-active button) t)
                      (setf (gtk:toggle-button-active button) nil)))))))
      ;; Create toggle button for Bold
      (let ((button (make-instance 'gtk:toggle-button
                                   :icon-name "format-text-bold"
                                   :name "Bold")))
        (push button buttons)
        (g:signal-connect button "clicked"
           (lambda (widget)
             (on-toggle-button-clicked widget buffer "Bold")))
        (gtk:box-append toolbar button))
      ;; Create toogle button for Italic
      (let ((button (make-instance 'gtk:toggle-button
                                   :icon-name "format-text-italic"
                                   :name "Italic")))
        (push button buttons)
        (g:signal-connect button "clicked"
           (lambda (widget)
             (on-toggle-button-clicked widget buffer "Italic")))
        (gtk:box-append toolbar button))
      ;; Create toggle button for Underline
      (let ((button (make-instance 'gtk:toggle-button
                                   :icon-name "format-text-underline"
                                   :name "Underline")))
        (push button buttons)
        (g:signal-connect button "clicked"
           (lambda (widget)
             (on-toggle-button-clicked widget buffer "Underline")))
        (gtk:box-append toolbar button))
      ;; Create toggle button for Strikethrough
      (let ((button (make-instance 'gtk:toggle-button
                                   :icon-name "format-text-strikethrough"
                                   :name "Strikethrough")))
        (push button buttons)
        (g:signal-connect button "clicked"
           (lambda (widget)
             (on-toggle-button-clicked widget buffer "Strikethrough")))
        (gtk:box-append toolbar button))
      ;; Create tags associated with the text buffer
      (gtk:text-tag-table-add (gtk:text-buffer-tag-table buffer)
                              (make-instance 'gtk:text-tag
                                             :name "Bold"
                                             :weight 700))
      (gtk:text-tag-table-add (gtk:text-buffer-tag-table buffer)
                              (make-instance 'gtk:text-tag
                                             :name "Italic"
                                             :style :italic))
      (gtk:text-tag-table-add (gtk:text-buffer-tag-table buffer)
                              (make-instance 'gtk:text-tag
                                             :name "Underline"
                                             :underline :single))
      (gtk:text-tag-table-add (gtk:text-buffer-tag-table buffer)
                              (make-instance 'gtk:text-tag
                                             :name "Strikethrough"
                                             :strikethrough t))
      ;; Set some text
      (setf (gtk:text-buffer-text buffer) *lorem-ipsum-short*)
      ;; Pack the widgets and present the window
      (gtk:box-append vbox toolbar)
      (gtk:box-append vbox textview)
      (gtk:window-present window))))

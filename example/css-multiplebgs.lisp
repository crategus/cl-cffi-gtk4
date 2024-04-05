;;;; CSS Multiple Backgrounds
;;;;
;;;; GTK themes are written using CSS. Every widget is build of multiple items
;;;; that you can style very similarly to a regular website.
;;;;
;;;; 2024-4-2

(in-package :gtk4-example)

;; TODO: What is the correct way to remove the provider?!

#+nil
(defun apply-css (provider widget)
  (let ((display (gtk:widget-display widget)))
    (gtk:style-context-add-provider-for-display display provider)
    (g:object-set-data-full widget "provider"
        (lambda ()
          (gtk:style-context-remove-provider-for-display display provider)))
))

(defun create-radio-toolbar (area)
  (let ((group nil)
        (toolbar (make-instance 'gtk:box
                                :orientation :horizontal))
        (button (make-instance 'gtk:toggle-button
                               :label "Default")))
    ;; Set the CSS class for the toolbar
    (gtk:widget-add-css-class toolbar "toolbar")
    ;; Add "Default" toggle button
    (setf group button)
    (gtk:box-append toolbar button)
    (g:signal-connect button "toggled"
        (lambda (button)
          (when (gtk:toggle-button-active button)
            (setf (gtk:widget-name area) "canvas-default"))))
    ;; Add "Bricks" toggle button
    (setf button (gtk:toggle-button-new-with-label "Bricks"))
    (setf (gtk:toggle-button-group button) group)
    (setf group button)
    (gtk:box-append toolbar button)
    (setf (gtk:toggle-button-active button) t)
    (g:signal-connect button "toggled"
        (lambda (button)
          (when (gtk:toggle-button-active button)
            (setf (gtk:widget-name area) "canvas-bricks"))))
    ;; Add "Tartan" radio tool button
    (setf button (gtk:toggle-button-new-with-label "Tartan"))
    (setf (gtk:toggle-button-group button) group)
    (setf group button)
    (gtk:box-append toolbar button)
    (g:signal-connect button "toggled"
        (lambda (button)
          (when (gtk:toggle-button-active button)
            (setf (gtk:widget-name area) "canvas-tartan"))))
    ;; Add "Stripes" radio tool button
    (setf button (gtk:toggle-button-new-with-label "Stripes"))
    (setf (gtk:toggle-button-group button) group)
    (setf group button)
    (gtk:box-append toolbar button)
    (g:signal-connect button "toggled"
        (lambda (button)
          (when (gtk:toggle-button-active button)
            (setf (gtk:widget-name area) "canvas-stripes"))))
    ;; Add "Paper" radio tool button
    (setf button (gtk:toggle-button-new-with-label "Paper"))
    (setf (gtk:toggle-button-group button) group)
    (setf group button)
    (gtk:box-append toolbar button)
    (g:signal-connect button "toggled"
        (lambda (button)
          (when (gtk:toggle-button-active button)
            (setf (gtk:widget-name area) "canvas-paper"))))
    ;; Return the toolbar with the added radio tool buttons
    toolbar))

(defun do-css-multiplebgs (&optional application)
  (let* ((text (make-instance 'gtk:text-buffer))
         (view (make-instance 'gtk:text-view
                              :buffer text
                              :top-margin 12
                              :monospace t))
         (scrolled (make-instance 'gtk:scrolled-window
                                  :child view))
         (box (make-instance 'gtk:box
                             :orientation :vertical
                             :height-request 150))
         (paned (make-instance 'gtk:paned
                               :orientation :vertical
                               :position 150
                               :wide-handle t
                               :start-child box
                               :end-child scrolled))
         (area (make-instance 'gtk:drawing-area
                               :name "canvas-bricks"
                               :height-request 150))
         (overlay (make-instance 'gtk:overlay
                                 :child area
                                 :vexpand t))
         (vbox (make-instance 'gtk:box
                              :orientation :vertical))
         (window (make-instance 'gtk:window
                                :application application
                                :title "CSS Multiple Backgrounds"
                                :child vbox
                                :default-height 420
                                :default-width 600))
         (toolbar (create-radio-toolbar area))
         (button (make-instance 'gtk:button
                                :name "bricks-button"
                                :margin 12
                                :halign :center
                                :valign :center
                                :width-request 270
                                :height-request 96))
         (display (gtk:widget-display window))
         (provider (make-instance 'gtk:css-provider)))
    (g:signal-connect window "destroy"
        (lambda (widget)
          (let ((display (gtk:widget-display widget)))
            (gtk:style-context-remove-provider-for-display display provider))))
    (g:signal-connect text "changed"
        (lambda (buffer)
          (let ((start (gtk:text-buffer-start-iter buffer))
                (end (gtk:text-buffer-end-iter buffer)))
            (gtk:text-buffer-remove-all-tags buffer start end)
            (gtk:css-provider-load-from-string
                              provider
                              (gtk:text-buffer-get-text buffer start end nil))
            (gtk:style-context-add-provider-for-display display provider))))

    (g:signal-connect provider "parsing-error"
        (lambda (provider section err)
          (declare (ignore provider err))
          (let* ((startloc (gtk:css-section-start-location section))
                 (start (gtk:text-buffer-iter-at-line-index
                            text
                            (gtk:css-location-lines startloc)
                            (gtk:css-location-line-bytes startloc)))
                 (endloc (gtk:css-section-end-location section))
                 (end (gtk:text-buffer-iter-at-line-index
                          text
                          (gtk:css-location-lines endloc)
                          (gtk:css-location-line-bytes endloc))))
            (gtk:text-buffer-apply-tag text "error" start end)
            gdk:+event-stop+)))
    (gtk:text-tag-table-add (gtk:text-buffer-tag-table text)
                            (make-instance 'gtk:text-tag
                                           :name "error"
                                           :underline :error))
    (setf (gtk:text-buffer-text text)
          (read-file (sys-path "resource/css-multiplebgs.css")))
    ;; Add the widgets to the window
    (gtk:overlay-add-overlay overlay button)
    (gtk:overlay-add-overlay overlay paned)
    (gtk:box-append vbox toolbar)
    (gtk:box-append vbox overlay)
    ;; Apply the provider to the window
    (gtk:css-provider-load-from-path provider
                                     (sys-path "resource/css-multiplebgs.css"))
    (gtk:style-context-add-provider-for-display display provider)
    ;; Show the window
    (gtk:window-present window)))

;;;; CSS Shadows
;;;;
;;;; This demo shows how to use CSS shadows.
;;;;
;;;; 2024-4-4

(in-package :gtk4-example)

(defun create-toolbar ()
  (let ((toolbar (make-instance 'gtk:box
                                :orientation :horizontal
                                :css-name "toolbar")))
    (gtk:box-append toolbar
                    (make-instance 'gtk:button
                                   :icon-name "go-next"))
    (gtk:box-append toolbar
                    (make-instance 'gtk:button
                                   :icon-name "go-previous"))
    (gtk:box-append toolbar
                    (make-instance 'gtk:button
                                   :label "Hello World"))
    toolbar))

(defun do-css-shadows (&optional application)
  (let* ((toolbar (create-toolbar))
         (text (make-instance 'gtk:text-buffer))
         (view (make-instance 'gtk:text-view
                              :monospace t
                              :top-margin 12
                              :buffer text))
         (scrolled (make-instance 'gtk:scrolled-window
                                  :child view))
         (paned (make-instance 'gtk:paned
                               :start-child toolbar
                               :end-child scrolled
                               :orientation :vertical
                               :position 48
                               :shrink-start-child nil
                               :wide-handle t))
         (window (make-instance 'gtk:window
                                :application application
                                :child paned
                                :title "CSS Shadows"
                                :default-height 420
                                :default-width 600))
         (provider (make-instance 'gtk:css-provider)))
    (gtk:widget-add-css-class window "shadows")
    (g:signal-connect text "changed"
        (lambda (buffer)
          (let ((start (gtk:text-buffer-start-iter buffer))
                (end (gtk:text-buffer-end-iter buffer)))
            (gtk:text-buffer-remove-all-tags buffer start end)
            (gtk:css-provider-load-from-string
                              provider
                              (gtk:text-buffer-get-text buffer start end nil)))))
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
          (read-file (sys-path "resource/css-shadows.css")))
    ;; Apply the provider to the window
    (gtk:widget-add-provider window provider)
    ;; Show the window
    (gtk:window-present window)))

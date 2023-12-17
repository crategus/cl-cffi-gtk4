;;;; Theming/CSS Basics
;;;;
;;;; Gtk themes are written using CSS. Every widget is build of multiple items
;;;; that you can style very similarly to a regular website.
;;;;
;;;; 2023-12-16

(in-package :gtk4-example)

(defun do-css-basics (&optional application)
  (let* ((text (make-instance 'gtk:text-buffer))
         (view (make-instance 'gtk:text-view
                              :buffer text))
         (scrolled (make-instance 'gtk:scrolled-window
                                  :child view))
         (window (make-instance 'gtk:window
                                :application application
                                :title "Example CSS Basics"
                                :child scrolled
                                :default-height 420
                                :default-width 600))
         (provider (make-instance 'gtk:css-provider)))
    (gtk:widget-add-css-class scrolled "demo")
    (gtk:style-context-add-provider-for-display (gdk:display-default)
                                                provider
                                                gtk:+gtk-priority-user+)
    (g:signal-connect window "destroy"
        (lambda (widget)
          (declare (ignore widget))
          (gtk:style-context-remove-provider-for-display (gdk:display-default)
                                                         provider)))
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
            +gdk-event-stop+)))
    (gtk:text-tag-table-add (gtk:text-buffer-tag-table text)
                            (make-instance 'gtk:text-tag
                                           :name "error"
                                           :underline :error))
    (setf (gtk:text-buffer-text text)
          (read-file (sys-path "resource/css-basics.css")))
    ;; Present the window
    (gtk:window-present window)))

;;;; Assistant
;;;;
;;;; Demonstrates a sample multi-step assistant. Assistants are used to divide
;;;; an operation into several simpler sequential steps, and to guide the user
;;;; through these steps.
;;;;
;;;; 2024-4-19

(in-package :gtk4-example)

(defun create-assistant-page1 (assistant)
  (let ((box (make-instance 'gtk:box
                            :orientation :horizontal
                            :spacing 12
                            :margin-start 12
                            :margin-end 12))
        (label (make-instance 'gtk:label
                              :valign :center
                              :use-markup t
                              :label "<b>Fill out the entry to continue</b>"))
        (entry (make-instance 'gtk:entry
                              :valign :center
                              :activate-defaults t)))
    ;; Mark the page complete if we have text in the entry
    (g:signal-connect entry "changed"
        (lambda (widget)
          (let* ((num (gtk:assistant-current-page assistant))
                 (page (gtk:assistant-nth-page assistant num)))
            (if (> (length (gtk:editable-text widget)) 0)
                (setf (gtk:assistant-page-complete assistant page) t)
                (setf (gtk:assistant-page-complete assistant page) nil)))))
   ;; Pack the widgets
    (gtk:box-append box label)
    (gtk:box-append box entry)
    ;; Append the first page to the assistant
    (gtk:assistant-append-page assistant box)
    (setf (gtk:assistant-page-title assistant box) "Page 1")
    (setf (gtk:assistant-page-type assistant box) :intro)))

(defun create-assistant-page2 (assistant)
  (let* ((box (make-instance 'gtk:box
                             :orientation :vertical
                             :valign :center
                             :halign :center
                             :spacing 12))
         (label (make-instance 'gtk:label
                               :use-markup t
                               :label
                               "<b>Optional data, you may continue.</b>"))
         (check (make-instance 'gtk:check-button
                               :child label)))
    ;; Pack the widgets
    (gtk:box-append box check)
    ;; Append the second page to the assistant
    (gtk:assistant-append-page assistant box)
    (setf (gtk:assistant-page-complete assistant box) t)
    (setf (gtk:assistant-page-title assistant box) "Page 2")))

(defun create-assistant-page3 (assistant)
  (let ((label (make-instance 'gtk:label
                              :use-markup t
                              :label
                               "<b>Confirmation page, press Apply</b>")))
    ;; Append the third page to the assistant
    (gtk:assistant-append-page assistant label)
    (setf (gtk:assistant-page-title assistant label) "Confirmation")
    (setf (gtk:assistant-page-type assistant label) :confirm)
    (setf (gtk:assistant-page-complete assistant label) t)))

(defun create-assistant-page4 (assistant pbar)
  ;; Append the fourth page to the assistant
  (gtk:assistant-append-page assistant pbar)
  (setf (gtk:assistant-page-type assistant pbar) :progress)
  (setf (gtk:assistant-page-title assistant pbar) "Applying changes")
  ;; Prevent the assistant from being closed while applying changes
  (setf (gtk:assistant-page-complete assistant pbar) nil))

(defun do-assistant (&optional application)
  (let* ((assistant (make-instance 'gtk:assistant
                                   :title "Assistant"
                                   :use-header-bar 1
                                   :application application
                                   :default-height 240))
         (pbar (make-instance 'gtk:progress-bar
                              :show-text t
                              :halign :center
                              :valign :center))
         (provider (gtk:css-provider-new))
         ;; Make the progressbar larger
         (css ".pbar progressbar > trough,
               .pbar progressbar > trough > progress {
                   min-height : 36px; }"))
    ;; Change the appearance of the progress bar
    (gtk:css-provider-load-from-string provider css)
    (gtk:widget-add-css-class assistant "pbar")
    (gtk:widget-add-provider assistant provider)
    ;; Signal handlers for the assistant
    (g:signal-connect assistant "prepare"
      (lambda (assistant page)
        (declare (ignore page))
        ;; The fourth page (counting from zero) is the progress page. The user
        ;; clicked Apply to get here so we tell the assistant to commit, which
        ;; means the changes up to this point are permanent and cannot be
        ;; cancelled or revisited.
        (when (= 3 (gtk:assistant-current-page assistant))
          (gtk:assistant-commit assistant))))
    (g:signal-connect assistant "apply"
      (lambda (assistant)
        ;; Start a timer to simulate changes taking a few seconds to apply
        (g:timeout-add 100
                       (lambda ()
                         (let ((fraction (+ 0.025d0
                                            (gtk:progress-bar-fraction pbar))))
                           (cond ((< fraction 1.0d0)
                                  (setf (gtk:progress-bar-fraction pbar)
                                        fraction)
                                  g:+source-continue+)
                                 (t
                                  (gtk:window-destroy assistant)
                                  g:+source-remove+)))))))
    ;; Create and add the pages of the assistant
    (create-assistant-page1 assistant)
    (create-assistant-page2 assistant)
    (create-assistant-page3 assistant)
    (create-assistant-page4 assistant pbar)
    ;; Show the assistant
    (gtk:window-present assistant)))

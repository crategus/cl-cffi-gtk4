;;;;  Example Progress Bar- 2022-7-23

(in-package :gtk4-example)

(defstruct pbar
  widget
  timer
  mode)

(defun progress-bar-timeout (pdata)
  (if (pbar-mode pdata)
      (gtk:progress-bar-pulse (pbar-widget pdata))
      (let ((val (+ (gtk:progress-bar-fraction (pbar-widget pdata))
                    0.01)))
        (when (> val 1.0) (setq val 0.0))
        (setf (gtk:progress-bar-fraction (pbar-widget pdata)) val)))
  +g-source-continue+)

(defun do-progress-bar (&optional application)
  (let* ((vbox (make-instance 'gtk:box
                              :orientation :vertical
                              :margin-top 12
                              :margin-bottom 12
                              :margin-start 12
                              :margin-end 12
                              :spacing 6))
         (window (make-instance 'gtk:window
                                :title "Example Progress Bar"
                                :child vbox
                                :application application
                                :default-width 320))
         (pdata (make-pbar :widget (make-instance 'gtk:progress-bar))))

    (g:signal-connect window "destroy"
                      (lambda (widget)
                        (declare (ignore widget))
                        (g:source-remove (pbar-timer pdata))
                        (setf (pbar-timer pdata) 0)))

    (let ((provider (gtk:css-provider-new))
          (context (gtk:widget-style-context (pbar-widget pdata)))
          ;; TODO: This does not work.
          (css-data "progressbar > trough,
                     progressbar > trough > progress {
                         min-height : 24px; }"))
       (gtk:css-provider-load-from-data provider css-data)
       (gtk:style-context-add-provider context
                                       provider
                                       +gtk-priority-application+))
    (setf (pbar-timer pdata)
          (g:timeout-add 100
                         (lambda ()
                           (progress-bar-timeout pdata))))
    (gtk:box-append vbox
                    (make-instance 'gtk:label
                                   :use-markup t
                                   :xalign 0.0
                                   :label "<b>Progress bar</b>"))
    (gtk:box-append vbox (pbar-widget pdata))
    (gtk:box-append vbox
                    (make-instance 'gtk:label
                                   :use-markup t
                                   :xalign 0.0
                                   :margin-top 12
                                   :label "<b>Change properties</b>"))

    (let ((check (gtk:check-button-new-with-label "Show text")))
      (g:signal-connect check "toggled"
         (lambda (widget)
           (declare (ignore widget))
           (if (gtk:check-button-active check)
               (setf (gtk:progress-bar-text (pbar-widget pdata))
                     (if (pbar-mode pdata)
                         "Progress bar is in activity mode"
                         "Progress bar is in normal mode"))
               (setf (gtk:progress-bar-text (pbar-widget pdata)) ""))
           (setf (gtk:progress-bar-show-text (pbar-widget pdata))
                 (gtk:check-button-active check))))
      (gtk:box-append vbox check))

    (let ((check (gtk:check-button-new-with-label "Activity mode")))
      (g:signal-connect check "toggled"
         (lambda (widget)
           (declare (ignore widget))
           (setf (pbar-mode pdata)
                 (not (pbar-mode pdata)))
           (if (pbar-mode pdata)
               (progn
                 (gtk:progress-bar-pulse (pbar-widget pdata))
                 (setf (gtk:progress-bar-text (pbar-widget pdata))
                       "Progress bar is in activity mode"))
               (progn
                 (setf (gtk:progress-bar-text (pbar-widget pdata))
                       "Progress bar is in normal mode")
                 (setf (gtk:progress-bar-fraction (pbar-widget pdata))
                       0.0)))))
      (gtk:box-append vbox check))

    (let ((check (gtk:check-button-new-with-label "Inverted")))
      (g:signal-connect check "toggled"
         (lambda (widget)
           (declare (ignore widget))
           (setf (gtk:progress-bar-inverted (pbar-widget pdata))
                 (gtk:check-button-active check))))
      (gtk:box-append vbox check))

    (gtk:widget-show window)))

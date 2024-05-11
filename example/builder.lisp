;;;; Builder
;;;;
;;;; Demonstrates a traditional interface, loaded from a XML description, and
;;;; shows how to connect actions to the menu items and toolbar buttons.
;;;;
;;;; Last version: 2024-5-10

(in-package :gtk4-example)

(defun status-message (status text)
  (let ((id (g:object-data status "timeout")))
    (when id
      (g:source-remove id))
    (setf (gtk:label-text status) text)
    (setf id
          (g:timeout-add 5000
                         (lambda ()
                           (setf (gtk:label-text status) "")
                           (setf (g:object-data status "timeout") nil)
                           glib:+source-remove+)))))

(defun not-implemented (status action parameter)
  (declare (ignore parameter))
  (let ((text (format nil "Action ~:@(~a~) not implemented"
                          (g:action-name action))))
    (status-message status text)))

(defun help-activate (status action parameter)
  (declare (ignore action parameter))
  (status-message status "Help not available"))

(defun quit-activate (window action parameter)
  (declare (ignore action parameter))
  (gtk:window-destroy window))

(defun about-activate (about action parameter)
  (declare (ignore action parameter))
  (gtk:window-present about))

(defun do-builder (&optional application)
  (let* ((path (sys-path "resource/builder.ui"))
         (builder (gtk:builder-new-from-file path))
         (window (gtk:builder-object builder "window1"))
         (about (gtk:builder-object builder "aboutdialog1"))
         (status (gtk:builder-object builder "statusbar1"))
         (entries (list (list "new"
                              (lambda (action parameter)
                                (not-implemented status action parameter)))
                        (list "open"
                              (lambda (action parameter)
                                (not-implemented status action parameter)))
                        (list "save"
                              (lambda (action parameter)
                                (not-implemented status action parameter)))
                        (list "save-as"
                              (lambda (action parameter)
                                (not-implemented status action parameter)))
                        (list "copy"
                              (lambda (action parameter)
                                (not-implemented status action parameter)))
                        (list "cut"
                              (lambda (action parameter)
                                (not-implemented status action parameter)))
                        (list "paste"
                              (lambda (action parameter)
                                (not-implemented status action parameter)))
                        (list "quit"
                              (lambda (action parameter)
                                (quit-activate window action parameter)))
                        (list "about"
                              (lambda (action parameter)
                                (about-activate about action parameter)))
                        (list "help"
                              (lambda (action parameter)
                                (help-activate status action parameter)))))
         (actions (g:simple-action-group-new))
         (controller (gtk:shortcut-controller-new)))
    ;; Set application on window
    (setf (gtk:window-application window) application)
    ;; Add action entries
    (g:action-map-add-action-entries actions entries)
    (gtk:widget-insert-action-group window "win" actions)
    ;; Set destroy notify for about dialog
    (g:object-set-data-full window "about"
                            (lambda ()
                              (gtk:window-destroy about)))
    ;; Set properties for about dialog
    (setf (gtk:window-transient-for about) window)
    (setf (gtk:window-hide-on-close about) t)
    ;; Prepare shortcut controller
    (setf (gtk:shortcut-controller-scope controller) :global)
    (gtk:widget-add-controller window controller)
    ;; Add shortcuts
    (gtk:shortcut-controller-add-shortcut
            controller
            (gtk:shortcut-new (gtk:keyval-trigger-new #\n :control-mask)
                              (gtk:named-action-new "win.new")))
    (gtk:shortcut-controller-add-shortcut
            controller
            (gtk:shortcut-new (gtk:keyval-trigger-new #\o :control-mask)
                              (gtk:named-action-new "win.open")))
    (gtk:shortcut-controller-add-shortcut
            controller
            (gtk:shortcut-new (gtk:keyval-trigger-new #\s :control-mask)
                              (gtk:named-action-new "win.save")))
    (gtk:shortcut-controller-add-shortcut
            controller
            (gtk:shortcut-new (gtk:keyval-trigger-new #\s
                                                  '(:control-mask :shift-mask))
                              (gtk:named-action-new "win.save-as")))
    (gtk:shortcut-controller-add-shortcut
            controller
            (gtk:shortcut-new (gtk:keyval-trigger-new #\q :control-mask)
                              (gtk:named-action-new "win.quit")))
    (gtk:shortcut-controller-add-shortcut
            controller
            (gtk:shortcut-new (gtk:keyval-trigger-new #\c :control-mask)
                              (gtk:named-action-new "win.copy")))
    (gtk:shortcut-controller-add-shortcut
            controller
            (gtk:shortcut-new (gtk:keyval-trigger-new #\x :control-mask)
                              (gtk:named-action-new "win.cut")))
    (gtk:shortcut-controller-add-shortcut
            controller
            (gtk:shortcut-new (gtk:keyval-trigger-new #\v :control-mask)
                              (gtk:named-action-new "win.paste")))
    (gtk:shortcut-controller-add-shortcut
            controller
            (gtk:shortcut-new (gtk:keyval-trigger-new 65470 :none)
                              (gtk:named-action-new "win.help")))
    (gtk:shortcut-controller-add-shortcut
            controller
            (gtk:shortcut-new (gtk:keyval-trigger-new 65476 :none)
                              (gtk:named-action-new "win.about")))
    ;; Present window
    (gtk:window-present window)))

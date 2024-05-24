;;;; Application with Menubar
;;;;
;;;; Last version: 2024-5-24

(in-package :gtk4-application)

(defun status-message (status text)
  (let ((id (g:object-data status "timeout")))
    (when id
      (g:source-remove id))
    (setf (gtk:label-text status) text)
    (setf id
          (g:timeout-add 10000
                         (lambda ()
                           (setf (gtk:label-text status) "")
                           (setf (g:object-data status "timeout") nil)
                           glib:+source-remove+)))))

(defun change-state (statusbar action parameter)
  (let ((msg (format nil "Action ~:@(~a~) called with parameter ~:@(~a~)"
                         (g:action-name action)
                         (g:variant-boolean parameter))))
    (status-message statusbar msg)
    (setf (g:action-state action) parameter)))

(defun change-radio-state (statusbar action parameter)
  (let ((msg (format nil "Action ~:@(~a~) called with parameter ~:@(~a~)"
                         (g:action-name action)
                         (g:variant-string parameter))))
    (status-message statusbar msg)
    (setf (g:action-state action) parameter)))

(defun application-menubar (&rest argv)
  (let (;; Create an application
        (app (make-instance 'gtk:application
                            :application-id "com.crategus.application-menubar"
                            :flags :default-flags)))
    ;; Connect signal "startup" to the application
    (g:signal-connect app "startup"
        (lambda (application)
          ;; Intitialize the application menu and the menubar
          (let ((path (glib-sys:sys-path "resource/menubar.ui"
                                         "gtk4-application"))
                (builder (make-instance 'gtk:builder)))
            ;; Read the menus from a file
            (gtk:builder-add-from-file builder path)
            ;; Set the menubar
            (setf (gtk:application-menubar application)
                  (gtk:builder-object builder "menubar")))))
    ;; Connect signal "activate" to the application
    (g:signal-connect app "activate"
        (lambda (application)
          ;; Create an application window
          (let* ((statusbar (make-instance 'gtk:label
                                           :label "No message"
                                           :xalign 0.0
                                           :margin-start 6
                                           :margin-top 6
                                           :margin-bottom 6))
                 ;; Define action entries for the menu items
                 (entries (list
                            (list "toolbar" nil nil "true"
                                  (lambda (action parameter)
                                    (change-state statusbar action parameter)))
                            (list "statusbar" nil nil "false"
                                  (lambda (action parameter)
                                    (change-state statusbar action parameter)))
                            (list "fullscreen" nil nil "false"
                                  (lambda (action parameter)
                                    (change-state statusbar action parameter)))
                            (list "sources"
                                  nil
                                  "s"
                                  "'vala'"
                                  (lambda (action parameter)
                                    (change-radio-state statusbar
                                                        action
                                                        parameter)))
                            (list "markup"
                                  nil
                                  "s"
                                  "'html'"
                                  (lambda (action parameter)
                                    (change-radio-state statusbar
                                                        action
                                                        parameter)))))
                 (vbox (make-instance 'gtk:box
                                      :orientation :vertical))
                 (window (make-instance 'gtk:application-window
                                        :application application
                                        :title "Application Menubar"
                                        :child vbox
                                        :show-menubar t
                                        :default-width 480
                                        :default-height 300)))
            ;; Add the action entries to the application window
            (g:action-map-add-action-entries window entries)
            ;; Add a statusbar
            (gtk:box-append vbox (make-instance 'gtk:text-view
                                                :top-margin 6
                                                :bottom-margin 6
                                                :left-margin 6
                                                :vexpand :fill))
            (gtk:box-append vbox statusbar)
            ;; Present the application window
            (gtk:window-present window))))
    ;; Run the application
    (g:application-run app argv)))

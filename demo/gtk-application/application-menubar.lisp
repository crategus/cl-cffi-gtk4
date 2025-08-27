;;;; Application with Menubar
;;;;
;;;; This example adds a menu bar to the application that appears at the top
;;;; of each window. The menus are loaded from a UI definition file and
;;;; assigned using the gtk:application-menubar function in the startup
;;;; signal handler.
;;;;
;;;; The application window is created within the ACTIVATE signal handler. It
;;;; contains a vertical GTK:BOX widget as a child widget. This box contains a
;;;; GTK:TEXT-VIEW widget and a GTK:LABEL widget that represent a status bar at
;;;; the bottom of the application window. The status bar displays information
;;;; about the activated menu items.
;;;;
;;;; Some action entries for menu items are defined and installed for the
;;;; application window with the g:action-map-add-action-entries function.
;;;; The "Sources" and "Markup" menu items are statefull action entries which
;;;; are presented as radio buttons.
;;;;
;;;; Selecting of one of the menu items with an action entry displays text in
;;;; the status bar for 10 seconds. To achieve this, a timeout source is
;;;; installed with the g:timeout-add function after setting the label. This
;;;; clears the status bar label after 10 seconds.
;;;;
;;;; Notes:
;;;;
;;;; The ID for the timeout source is stored and retrieved using the
;;;; g:object-data function on the status label. As an alternative, a
;;;; 'Let over Lambda' implementation of the STATUS-MESSAGE function can be
;;;; used. See the example in the source code.
;;;;
;;;; Last updated: 2025-08-27

(in-package :gtk4-application)

(defun status-message (status text)
  (let ((id (g:object-data status "timeout")))
    (when id (g:source-remove id))
    (setf (gtk:label-text status) text)
    (setf id
          (g:timeout-add 10000
                         (lambda ()
                           (setf (gtk:label-text status) "")
                           (setf (g:object-data status "timeout") nil)
                           glib:+source-remove+)))))

;; Second way to implement the timeout source.
(let (id)
  (defun status-message1 (status text)
    (when id (g:source-remove id))
    (setf (gtk:label-text status) text)
    (setf id
          (g:timeout-add 10000
                         (lambda ()
                           (setf (gtk:label-text status) "")
                           (setf id nil)
                           glib:+source-remove+)))))

(defun change-state (statusbar action parameter)
  (let ((msg (format nil "Action ~:@(~a~) called with parameter ~:@(~a~)"
                         (g:action-name action)
                         (g:variant-boolean parameter))))
    (status-message1 statusbar msg)
    (setf (g:action-state action) parameter)))

(defun change-radio-state (statusbar action parameter)
  (let ((msg (format nil "Action ~:@(~a~) called with parameter ~:@(~a~)"
                         (g:action-name action)
                         (g:variant-string parameter))))
    (status-message1 statusbar msg)
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
            ;; Read menus from a UI definition file
            (gtk:builder-add-from-file builder path)
            ;; Set the menubar for the application
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
                 (entries `(("toolbar" nil nil "true"
                             ,(lambda (action param)
                                (change-state statusbar action param)))
                            ("statusbar" nil nil "false"
                             ,(lambda (action param)
                                (change-state statusbar action param)))
                            ("fullscreen" nil nil "false"
                             ,(lambda (action param)
                                (change-state statusbar action param)))
                            ("sources" nil "s" "'vala'"
                             ,(lambda (action param)
                                (change-radio-state statusbar action param)))
                            ("markup" nil "s" "'html'"
                              ,(lambda (action param)
                                 (change-radio-state statusbar action param)))))
                 (vbox (make-instance 'gtk:box
                                      :orientation :vertical))
                 (window (make-instance 'gtk:application-window
                                        :application application
                                        :title "Application Menubar"
                                        :child vbox
                                        :show-menubar t
                                        :default-width 480
                                        :default-height 300)))
            ;; Add action entries to the application window
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

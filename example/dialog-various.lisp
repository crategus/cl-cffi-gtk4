;;;; Various Dialog Windows
;;;;
;;;; In this example, various GtkDialog, GtkMessageDialog and GtkAboutDialog
;;;; widgets are shown.
;;;;
;;;; 2024-4-4

(in-package :gtk4-example)

(defun dialog-license-text ()
  (format nil
          "Permission is hereby granted, free of charge, to any person ~
           obtaining a copy of this software and associated documentation ~
           files (the 'Software'), to deal in the Software without ~
           restriction, including without limitation the rights to use, copy, ~
           modify, merge, publish, distribute, sublicense, and/or sell copies ~
           of the Software, and to permit persons to whom the Software is ~
           furnished to do so, subject to the following conditions: ~%~% ~
           The above copyright notice and this permission notice shall be ~
           included in all copies or substantial portions of the ~
           Software. ~% ~% ~
           THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND, ~
           EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF ~
           MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND ~
           NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS ~
           BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ~
           ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN ~
           CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE ~
           SOFTWARE."))

(defun create-dialog (parent &optional (headerbar-p -1))
  (let ((dialog (make-instance 'gtk:dialog
                               :transient-for parent
                               :modal t
                               :use-header-bar headerbar-p
                               :title "Dialog Window")))
    ;; Connect a handler to the "response" signal
    (g:signal-connect dialog "response"
                      (lambda (widget response)
                        (format t "Response is: ~s~%" response)
                        (gtk:window-destroy widget)))
    ;; Add a label widget within a box to the content area
    (let ((vbox (make-instance 'gtk:box
                               :orientation :vertical
                               :margin-top 12
                               :margin-bottom 12
                               :margin-start 12
                               :margin-end 12))
          (label (make-instance 'gtk:label
                                :wrap t
                                :label
                                (format nil
                                        "The content area is the place to ~
                                         put in the widgets."))))
      (gtk:box-append vbox label)
      (gtk:box-append (gtk:dialog-content-area dialog) vbox))
    ;; Add buttons to the action area
    (gtk:dialog-add-button dialog "Yes" :yes)
    (gtk:dialog-add-button dialog "Cancel" :cancel)
    (gtk:dialog-set-default-response dialog :cancel)
    ;; Show the dialog
    (gtk:window-present dialog)))

(defun create-dialog-from-ui (parent)
  (let ((builder (make-instance 'gtk:builder)))
    (gtk:builder-add-from-file builder (sys-path "resource/dialog.ui"))
    (let ((dialog (gtk:builder-object builder "dialog")))
      (g:signal-connect dialog "response"
                        (lambda (widget response)
                          (format t "response ID is : ~a~%" response)
                          (gtk:window-destroy widget)))
      (setf (gtk:window-transient-for dialog) parent)
      (gtk:window-present dialog))))

(defun create-message-dialog (parent &optional (mtype :info))
  (let ((dialog (make-instance 'gtk:message-dialog
                               :transient-for parent
                               :modal t
                               :message-type mtype
                               :buttons :ok
                               :text "Message Dialog"
                               :secondary-text
                               (format nil
                                       "This is a message dialog of type ~a."
                                       mtype))))
    (g:signal-connect dialog "response"
                      (lambda (widget response)
                        (declare (ignore response))
                        (gtk:window-destroy widget)))
    ;; Show the message dialog
    (gtk:window-present dialog)))

(defun create-about-dialog (parent)
  (gtk:show-about-dialog parent
                         :modal t
                         :program-name "GTK Demo"
                         :version
                         (asdf:component-version (asdf:find-system :gtk4-demo))
                         :copyright "(c) Dieter Kaiser"
                         :website "github.com/crategus/cl-cffi-gtk4"
                         :website-label "Project web site"
                         :license (dialog-license-text)
                         :authors '("Dieter Kaiser")
                         :documenters '("Dieter Kaiser")
                         :artists '("None")
                         :logo-icon-name "applications-development"
                         :wrap-license t))

(defun radio-button-toggled (button)
  (when (gtk:check-button-active button)
    (let ((mtype (cdr (assoc (gtk:check-button-label button)
                             '(("Info" . :info)
                               ("Warning" . :warning)
                               ("Question" . :question)
                               ("Error" . :error))
                             :test #'string=))))
      (format t "message type is : ~a~%" mtype)
      mtype)))

(defun do-dialog-various (&optional application)
  (let* ((mtype :info)
         (vbox (make-instance 'gtk:box
                              :orientation :vertical
                              :margin-top 12
                              :margin-bottom 12
                              :margin-start 12
                              :margin-end 12
                              :spacing 6))
         (window (make-instance 'gtk:window
                                :application application
                                :type :toplevel
                                :title "Various Dialogs"
                                :child vbox
                                :default-width 270
                                :border-width 12))
         (check (make-instance 'gtk:check-button
                               :margin-bottom 12
                               :label "Show Dialog with Header Bar")))
    ;; Show dialog
    (let ((button (make-instance 'gtk:button
                                 :label "Show Dialog")))
      (gtk:box-append vbox button)
      (g:signal-connect button "clicked"
         (lambda (widget)
           (declare (ignore widget))
           ;; Create and show the dialog
           (create-dialog window
                          (if (gtk:check-button-active check)
                              1
                              -1)))))
    ;; Show dialog from UI
    (let ((button (make-instance 'gtk:button
                                 :label "Show Dialog from UI")))
      (gtk:box-append vbox button)
      (g:signal-connect button "clicked"
         (lambda (widget)
           (declare (ignore widget))
           ;; Create and show the dialog
           (create-dialog-from-ui window))))
    ;; Add the check button to the vertical box
    (gtk:box-append vbox check)
    ;; Show about dialog
    (let ((button (make-instance 'gtk:button
                                 :margin-bottom 12
                                 :label "Show About Dialog")))
      (gtk:box-append vbox button)
      (g:signal-connect button "clicked"
         (lambda (widget)
           (declare (ignore widget))
           ;; Create and show the about dialog
           (create-about-dialog window))))
    ;; Show message dialog
    (let ((button (make-instance 'gtk:button
                                 :label "Show Message Dialog")))
      (gtk:box-append vbox button)
      (g:signal-connect button "clicked"
         (lambda (widget)
           (declare (ignore widget))
           ;; Create and show the message dialog
           (create-message-dialog window mtype))))
    ;; Add the radio buttons to select the message type
    (let* ((radio (gtk:check-button-new-with-label "Info"))
           (group radio)
           (hbox (make-instance 'gtk:box
                                :orientation :horizontal)))
      (g:signal-connect radio "toggled"
                        (lambda (widget)
                           (setf mtype (radio-button-toggled widget))))
      (setf (gtk:check-button-active radio) t)
      (gtk:box-append hbox radio)
      ;; Second radio button
      (setf radio
            (gtk:check-button-new-with-label "Warning"))
      (g:signal-connect radio "toggled"
                        (lambda (widget)
                          (setf mtype (radio-button-toggled widget))))
      (setf (gtk:check-button-group radio) group)
      (setf group radio)
      (gtk:box-append hbox radio)
      ;; Third radio button
      (setf radio
            (gtk:check-button-new-with-label "Question"))
      (g:signal-connect radio "toggled"
                        (lambda (widget)
                          (setf mtype (radio-button-toggled widget))))
      (setf (gtk:check-button-group radio) group)
      (setf group radio)
      (gtk:box-append hbox radio)
      ;; Fourth radio button
      (setf radio
            (gtk:check-button-new-with-label "Error"))
      (g:signal-connect radio "toggled"
                        (lambda (widget)
                          (setf mtype (radio-button-toggled widget))))
      (setf (gtk:check-button-group radio) group)
      (gtk:box-append hbox radio)
      (gtk:box-append vbox hbox))
    ;; Pack and show the widgets
    (gtk:window-present window)))

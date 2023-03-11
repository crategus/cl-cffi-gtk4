;;;; Example Dialog Windows - 2022-11-11

(in-package :gtk4-example)

(defun dialog-license-text ()
  (format nil
          "This program is free software: you can redistribute it and/or ~
          modify it under the terms of the GNU Lesser General Public ~
          License for Lisp as published by the Free Software Foundation, ~
          either version 3 of the License, or (at your option) any later ~
          version and with a preamble to the GNU Lesser General Public ~
          License that clarifies the terms for use with Lisp programs and ~
          is referred as the LLGPL.~%~% ~
          This program is distributed in the hope that it will be useful, ~
          but WITHOUT ANY WARRANTY; without even the implied warranty of ~
          MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the ~
          GNU Lesser General Public License for more details. ~%~% ~
          You should have received a copy of the GNU Lesser General Public ~
          License along with this program and the preamble to the Gnu ~
          Lesser General Public License.  If not, see ~
          <http://www.gnu.org/licenses/> and ~
          <http://opensource.franz.com/preamble.html>."))

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
    (gtk:widget-show dialog)))

(defun create-dialog-from-ui (parent)
  (let ((builder (make-instance 'gtk:builder)))
    (gtk:builder-add-from-file builder (sys-path "resource/dialog.ui"))
    (let ((dialog (gtk:builder-object builder "dialog")))
      (g:signal-connect dialog "response"
                        (lambda (widget response)
                          (format t "response ID is : ~a~%" response)
                          (gtk:window-destroy widget)))
      (setf (gtk:window-transient-for dialog) parent)
      (gtk:widget-show dialog))))

(defun create-message-dialog (parent &optional (mtype :info))
  (let ((dialog (make-instance 'gtk:message-dialog
                               :transient-for parent
                               :modal t
                               :message-type mtype
                               :buttons :cancel
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
    (gtk:widget-show dialog)))

(defun create-about-dialog (parent)
  (gtk:show-about-dialog parent
                         :modal t
                         :program-name "GTK Demo"
                         :version "0.00"
                         :copyright "(c) Dieter Kaiser"
                         :website "github.com/crategus/cl-cffi-gtk"
                         :website-label "Project web site"
                         :license (dialog-license-text)
                         :authors '("Kalyanov Dmitry"
                                    "Dieter Kaiser")
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
    (gtk:widget-show window)))

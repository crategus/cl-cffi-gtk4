;;;; Password Entry
;;;;
;;;; <tt>GtkPasswordEntry</tt> provides common functionality of entries that are
;;;; used to enter passwords and other secrets. It will display a warning if
;;;; <tt>CapsLock</tt> is on, and it can optionally provide a way to see the
;;;; text.
;;;;
;;;; 2024-4-6

(in-package :gtk4-example)

(flet ((update-button (button entry1 entry2)
         (let ((text1 (gtk:editable-text entry1))
               (text2 (gtk:editable-text entry2)))
           (when (and (> (length text1) 0)
                      (string= text1 text2))
             (setf (gtk:widget-sensitive button) t)))))

  (defun do-password-entry (&optional application)
    (let* ((box (make-instance 'gtk:box
                               :orientation :vertical
                               :spacing 6
                               :margin-start 18
                               :margin-end 18
                               :margin-top 18
                               :margin-bottom 18))
           (header (make-instance 'gtk:header-bar
                                  :show-title-buttons nil))
           (button (make-instance 'gtk:button
                                  :label "_Done"
                                  :use-underline t
                                  :sensitive nil))
           (cancel (make-instance 'gtk:button
                                  :label "_Cancel"
                                  :use-underline t))
           (window (make-instance 'gtk:window
                                  :application application
                                  :child box
                                  :title "Choose a Password"
                                  :titlebar header
                                  :resizable nil
                                  :deletable nil))
           (entry1 (make-instance 'gtk:password-entry
                                  :show-peek-icon t
                                  :placeholder-text "Password"
                                  :activates-default t))
           (entry2 (make-instance 'gtk:password-entry
                                  :show-peek-icon t
                                  :placeholder-text "Confirm"
                                  :activates-default t)))
      (g:signal-connect entry1 "notify::text"
                        (lambda (object pspec)
                          (declare (ignore object pspec))
                          (update-button button entry1 entry2)))
      (g:signal-connect entry2 "notify::text"
                        (lambda (object pspec)
                          (declare (ignore object pspec))
                          (update-button button entry1 entry2)))
      (gtk:widget-add-css-class button "suggested-action")
      (g:signal-connect button "clicked"
                        (lambda (button)
                          (let ((parent (gtk:widget-root button)))
                            (gtk:window-destroy parent))))
      (g:signal-connect cancel "clicked"
                        (lambda (button)
                          (let ((parent (gtk:widget-root button)))
                            (gtk:window-destroy parent))))
      (gtk:box-append box entry1)
      (gtk:box-append box entry2)
      (gtk:header-bar-pack-end header button)
      (gtk:header-bar-pack-start header cancel)
      (gtk:window-present window))))

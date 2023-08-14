;;;; Example Revealer Icon - 2023-8-8
;;;;
;;;; GtkRevealer is a container that animates showing and hiding
;;;; of its sole child with nice transitions.

(in-package :gtk4-example)

(defun do-revealer-icon (&optional (application nil))
  (let* ((count 0)
         (timeout 0)
         (path (sys-path "resource/revealer-icon.ui"))
         (builder (gtk:builder-new-from-file path))
         (window (gtk:builder-object builder "window")))
    (g:signal-connect window "close-request"
                             (lambda (window)
                               (declare (ignore window))
                               (when (not (= timeout 0))
                                 (g:source-remove timeout)
                                 (setf timeout 0))))
    (setf (gtk:window-application window) application)
    (setf timeout
          (g:timeout-add 690
              (lambda ()
                (let* ((name (format nil "revealer~d" count))
                       (revealer (gtk:builder-object builder name)))
                  (setf (gtk:revealer-reveal-child revealer) t)
                  (g:signal-connect revealer "notify::child-revealed"
                      (lambda (widget pspec)
                        (declare (ignore pspec))
                        (when (gtk:widget-mapped widget)
                          (setf (gtk:revealer-reveal-child widget)
                                (not (gtk:revealer-child-revealed widget))))))
                  (setf count (+ count 1))
                  (if (>= count 9)
                      (progn
                        (setf timeout 0)
                        nil)
                      t)))))
    (setf (gtk:widget-visible window) t)))

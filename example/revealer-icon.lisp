;;;; Example Revealer Icon - 2022-11-11
;;;;
;;;; GtkRevealer is a container that animates showing and hiding
;;;; of its sole child with nice transitions.
;;;;
;;;; TODO: This example uses the gtk-widget-mapped function, but the
;;;; documentation says: This function should only ever be called in a derived
;;;; "map" or "unmap" implementation of the widget.
;;;; What is a better implementation?

(in-package :gtk4-example)

(defun do-revealer-icon (&optional (application nil))
  (let* ((count 0)
         (timeout 0)
         (builder (gtk:builder-new-from-file (sys-path "resource/revealer-icon.ui")))
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
    (gtk:widget-show window)))

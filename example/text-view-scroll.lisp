;;;; Automatic Scrolling
;;;;
;;;; This example demonstrates how to use the gravity of GtkTextMarks to keep
;;;; a text view scrolled to the bottom while appending text.

(in-package :gtk4-example)

;; Scroll to the end of the text buffer
(let ((count 0))
  (defun scroll-to-end (view)
    (let* ((buffer (gtk:text-view-buffer view))
           ;; Get the "end" mark. It is located at the end of buffer because
           ;; of right gravity
           (mark (gtk:text-buffer-mark buffer "end"))
           (iter (gtk:text-buffer-iter-at-mark buffer mark))
           (ctrlstr (concatenate 'string
                                 (coerce (list #\~) 'string)
                                 (format nil "~d" count)
                                 (coerce (list #\: #\< #\~ #\>) 'string))))
      ;; Emulate typewriter behavior, shift to the left if we
      ;; are far enough to the right.
      (if (> count 100) (setf count 0) (incf count))
      ;; and insert some text at its position, the iter will be
      ;; revalidated after insertion to point to the end of inserted text
      (gtk:text-buffer-insert buffer iter (format nil "~%"))
      (gtk:text-buffer-insert buffer iter (format nil "~3d " count))
      (gtk:text-buffer-insert buffer iter (format nil ctrlstr))
      (gtk:text-buffer-insert buffer iter
                                     (format nil
        "Scroll to end scroll to end scroll to end scroll to end"))
      ;; Now scroll the end mark onscreen.
      (gtk:text-view-scroll-mark-onscreen view mark)
      glib:+g-source-continue+)))

;; Scroll to the bottom of the text buffer
(let ((count 0))
  (defun scroll-to-bottom (view)
    (let* ((buffer (gtk:text-view-buffer view))
           ;; Get end iterator
           (iter (gtk:text-buffer-end-iter buffer))
           (mark (gtk:text-buffer-mark buffer "scroll"))
           (ctrlstr (concatenate 'string
                                 (coerce (list #\~) 'string)
                                 (format nil "~d" count)
                                 (coerce (list #\: #\< #\~ #\>) 'string))))
      ;; Shift text back if we got enough to the right
      (if (> count 40) (setf count 0) (incf count))

      (gtk:text-buffer-insert buffer iter (format nil "~%"))
      (gtk:text-buffer-insert buffer iter (format nil "~3d " count))
      (gtk:text-buffer-insert buffer iter (format nil ctrlstr))
      (gtk:text-buffer-insert buffer iter
                                     (format nil
        "Scroll to bottom scroll to bottom scroll to bottom scroll to bottom"))
      ;; Move the iterator to the beginning of line, so we don't scroll
      ;; in horizontal direction
      (setf (gtk:text-iter-line-offset iter) 0)
      ;; and place the mark at iter. the mark will stay there after we
      ;; insert some text at the end because it has left gravity.
      (gtk:text-buffer-move-mark buffer mark iter)
      ;; Scroll the mark onscreen
      (gtk:text-view-scroll-mark-onscreen view mark)
      glib:+g-source-continue+)))

(defun setup-scroll (view to-end)
  (let* ((buffer (gtk:text-view-buffer view))
         (iter (gtk:text-buffer-end-iter buffer)))
    (if to-end
        (progn
          ;; If we want to scroll to the end, including horizontal scrolling,
          ;; then we just create a mark with right gravity at the end of the
          ;; buffer. It will stay at the end unless explicitly moved with
          ;; gtk_text_buffer_move_mark.
          (gtk:text-buffer-create-mark buffer "end" iter nil)
          ;; Add scrolling timeout.
          (g:timeout-add 50
                         (lambda ()
                           (scroll-to-end view))))
        (progn
          ;; If we want to scroll to the bottom, but not scroll horizontally,
          ;; then an end mark won't do the job. Just create a mark so we can
          ;; use it with gtk_text_view_scroll_mark_onscreen, we'll position it
          ;; explicitly when needed. Use left gravity so the mark stays where
          ;; we put it after inserting new text.
          (gtk:text-buffer-create-mark buffer "scroll" iter t)
          ;; Add scrolling timeout.
          (g:timeout-add 100
                         (lambda ()
                           (scroll-to-bottom view)))))))

(defun create-text-view (box to-end)
  (let* ((view (make-instance 'gtk:text-view))
         (scrolled (make-instance 'gtk:scrolled-window
                                  :child view))
         (timeout (setup-scroll view to-end)))
    (gtk:box-append box scrolled)
    (g:signal-connect view "destroy"
                      (lambda (view)
                        (declare (ignore view))
                        (g:source-remove timeout)))))

(defun do-text-view-scroll (&optional application)
  (let* ((hbox (make-instance 'gtk:box
                              :orientation :horizontal
                              :homogeneous t))
         (window (make-instance 'gtk:window
                                :title "Text View Automatic Scrolling"
                                :application application
                                :child hbox
                                :default-width 600
                                :default-height 600)))
    (create-text-view hbox t)
    (create-text-view hbox nil)
    (gtk:window-present window)))

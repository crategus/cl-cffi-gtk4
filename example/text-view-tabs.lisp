;;;; Text View Tabs
;;;;
;;;; <tt>GtkTextView</tt> can position text at fixed positions, using tabs. Tabs
;;;; can specify alignment, and also allow aligning numbers on the decimal
;;;; point. The example here has three tabs, with left, numeric and right
;;;; alignment.
;;;;
;;;; 2024-4-6

(in-package :gtk4-example)

(defun do-text-view-tabs (&optional application)
  (let* ((tabs (pango:tab-array-new 3 t))
         (view (make-instance 'gtk:text-view
                              :wrap-mode :word
                              :top-margin 20
                              :bottom-margin 20
                              :left-margin 20
                              :right-margin 20))
         (buffer (gtk:text-view-buffer view))
         (scrolled (make-instance 'gtk:scrolled-window
                                  :child view
                                  :vscrollbar-policy :never
                                  :hscrollbar-policy :automatic))
         (window (make-instance 'gtk:window
                                :title "Text View Tabs"
                                :application application
                                :child scrolled
                                :default-width 340
                                :default-height 140
                                :resizable nil)))

    (setf (pango:tab-array-tab tabs 0) '(:left 0))
    (setf (pango:tab-array-tab tabs 1) '(:decimal 150))
    (setf (pango:tab-array-tab tabs 2) '(:right 290))

    (setf (pango:tab-array-decimal-point tabs 1) #\,)

    (setf (gtk:text-view-tabs view) tabs)

    (format t " tabs : ~%~a~%" (pango:tab-array-to-string tabs))

    (setf (gtk:text-buffer-text buffer)
          (format nil
                  "one~c2,0~cthree~%four~c5,555~csix~%seven~c88,88~cnine"
                  #\Tab #\Tab #\Tab #\Tab #\Tab #\Tab))
    (gtk:window-present window)))

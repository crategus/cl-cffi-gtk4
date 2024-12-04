;;;; Stack Sidebar
;;;;
;;;; The <tt>GtkStackSidebar</tt> widget provides an automatic sidebar widget to
;;;; control navigation of a <tt>GtkStack</tt> widget. This widget automatically
;;;; updates it content based on what is presently available in the
;;;; <tt>GtkStack</tt> widget, and using the <tt>title</tt> child property to
;;;; set the display labels.
;;;;
;;;; 2024-4-14

(in-package :gtk4-example)

(defun do-stack-sidebar (&optional (application nil))
  (let* ((header (make-instance 'gtk:header-bar
                                :title-widget (gtk:label-new "Stack Sidebar")
                                :decoration-layout ":close"
                                :show-title-buttons t))
         (box (make-instance 'gtk:box
                             :orientation :horizontal))
         (window (make-instance 'gtk:window
                                :titlebar header
                                :type :toplevel
                                :child box
                                :application application
                                :width-request 500
                                :height-request 450))
         (stack (make-instance 'gtk:stack
                               :transition-type :slide-up-down))
         (sidebar (make-instance 'gtk:stack-sidebar
                                 :stack stack)))
      (dolist (name '("Welcome to GTK"
                      "GtkStackSidebar Widget"
                      "Automatic navigation"
                      "Consistent appearance"
                      "Scrolling"
                      "Page 6"
                      "Page 7"
                      "Page 8"
                      "Page 9"))
        (if (string= "Welcome to GTK" name)
            (let ((image (gtk:image-new-from-icon-name "help-about")))
              (setf (gtk:image-pixel-size image) 256)
              (gtk:stack-add-titled stack image name name))
            (let ((label (make-instance 'gtk:label
                                        :label name)))
              (gtk:stack-add-titled stack label name name))))
      (gtk:box-append box sidebar)
      (gtk:box-append box stack)
      (gtk:window-present window)))

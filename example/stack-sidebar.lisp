;;;; Stack Sidebar - 2023-8-9
;;;;
;;;; GtkStackSidebar provides an automatic sidebar widget to control navigation
;;;; of a GtkStack object. This widget automatically updates it content based on
;;;; what is presently available in the GtkStack object, and using the "title"
;;;; child property to set the display labels.


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
;              (setf (gtk:stack-child-title stack image) name))
            (let ((label (make-instance 'gtk:label
                                        :label name)))
              (gtk:stack-add-titled stack label name name))))
      (gtk:box-append box sidebar)
      (gtk:box-append box stack)
      (setf (gtk:widget-visible window) t)))

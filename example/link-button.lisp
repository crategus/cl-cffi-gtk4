;;;; Link button - 2022-11-18

(in-package :gtk4-example)

(defun do-link-button (&optional application)
  (let* ((vbox (make-instance 'gtk:box
                             :orientation :vertical
                             :row-spacing 6
                             :homogeneous t))
         (window (make-instance 'gtk:window
                                :title "Example Link Button"
                                :child vbox
                                :application application
                                :default-width 280
                                :border-width 18)))

    (gtk:box-append vbox
                    (make-instance 'gtk:label
                                   :use-markup t
                                   :label
                                   "<b>Link Button with url</b>"))
    (gtk:box-append vbox
                    (gtk:link-button-new "http://www.gtk.org/"))
    (gtk:box-append vbox
                    (make-instance 'gtk:label
                                   :margin-top 12
                                   :use-markup t
                                   :label
                                   "<b>Link Button with Label</b>"))
    (gtk:box-append vbox
                    (gtk:link-button-new-with-label "http://www.gtk.org/"
                                                    "Project WebSite"))
    (gtk:widget-show window)))

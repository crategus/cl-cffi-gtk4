;;;; Example Search Bar - 2023-8-26

(in-package :gtk4-example)

(defun do-search-bar (&optional (application nil))
  (let* ((box (make-instance 'gtk:box
                             :orientation :horizontal
                             :spacing 6))
         (searchbar (make-instance 'gtk:search-bar
                                   :child box
                                   :valign :start))
         (window (make-instance 'gtk:application-window
                                :title "Search Bar"
                                :application application
                                :child searchbar))
         (entry (make-instance 'gtk:search-entry
                               :hexpand t))
         (button (make-instance 'gtk:menu-button)))
    (gtk:box-append box entry)
    (gtk:box-append box button)
    (setf (gtk:search-bar-key-capture-widget searchbar) window)
    (gtk:search-bar-connect-entry searchbar entry)
    (gtk:window-present window)))

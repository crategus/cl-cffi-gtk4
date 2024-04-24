;;;; Search Bar
;;;;
;;;; The <tt>gtk:search-bar</tt> widget is a widget made to have a search entry.
;;;; It can also contain additional widgets, such as drop-down menus, or
;;;; buttons. The search bar would appear when a search is started through
;;;; typing on the keyboard, or the search mode of the application is toggled
;;;; on.
;;;;
;;;; 2024-4-20

(in-package :gtk4-example)

(defun do-search-bar (&optional application)
  (let* ((box (make-instance 'gtk:box
                             :orientation :horizontal
                             :spacing 6))
         (searchbar (make-instance 'gtk:search-bar
                                   :child box
                                   :valign :start))
         (window (make-instance 'gtk:window
                                :title "Search Bar"
                                :application application
                                :child searchbar))
         (entry (make-instance 'gtk:search-entry
                               :hexpand t))
         (button (make-instance 'gtk:menu-button)))
    ;; Connect search entry to search bar
    (gtk:search-bar-connect-entry searchbar entry)
    (setf (gtk:search-bar-key-capture-widget searchbar) window)
    ;; Pack widgets and present window
    (gtk:box-append box entry)
    (gtk:box-append box button)
    (gtk:window-present window)))

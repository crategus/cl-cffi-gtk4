;;;; Text Entry Completion
;;;;
;;;; <tt>GtkEntryCompletion</tt> provides a mechanism for adding support for
;;;; completion in <tt>GtkEntry</tt>.
;;;;
;;;; 2024-4-6

(in-package :gtk4-example)

(defun create-completion-model ()
  (let ((store (make-instance 'gtk:list-store
                              :column-types '("gchararray"))))
    (gtk:list-store-set store (gtk:list-store-append store) "GNOME")
    (gtk:list-store-set store (gtk:list-store-append store) "total")
    (gtk:list-store-set store (gtk:list-store-append store) "totally")
    store))

;; FIXME: There is a problem with a popup-completion. The popup does not show
;; the text.

(defun do-entry-completion (&optional application)
  (let* ((hbox (make-instance 'gtk:box
                              :orientation :horizontal
                              :spacing 30
                              :margin-top 12
                              :margin-bottom 12
                              :margin-start 12
                              :margin-end 12))
         (window (make-instance 'gtk:window
                                :title "Entry Completion"
                                :child hbox
                                :application application
                                :default-width 360
                                :resizable nil))
         (vbox (make-instance 'gtk:box
                              :orientation :vertical)))
    (gtk:box-append vbox
                    (make-instance 'gtk:label
                                   :label "<b>Entry with Completion</b>"
                                   :halign :start
                                   :margin-bottom 6
                                   :use-markup t))
    (let* (;; Create the completion object
           (completion (make-instance 'gtk:entry-completion
                                      :text-column 0
                                      :inline-completion t
                                      :popup-completion nil
                                      :model (create-completion-model)))
           ;; Create the entry with a completion
           (entry (make-instance 'gtk:entry
                                 :completion completion)))
      (gtk:box-append vbox entry))
    (gtk:box-append hbox vbox)
    (gtk:box-append hbox
                    (make-instance 'gtk:label
                                   :valign :start
                                   :use-markup t
                                   :wrap t
                                   :label
                                   (format nil
                                           "Try writing <b>total</b> or ~
                                            <b>GNOME</b> for example.")))
    (gtk:window-present window)))

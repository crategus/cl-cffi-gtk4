;;;; Combo Box Text - 2022-11-25

(in-package :gtk4-example)

(defun do-combo-box-text (&optional application)
  (let* ((hbox (make-instance 'gtk:box
                              :orientation :horizontal
                              :spacing 24))
         (window (make-instance 'gtk:window
                                :title "Example Combo Box Text"
                                :child hbox
                                :application application))
         (vbox1 (make-instance 'gtk:box
                               :orientation :vertical
                               :spacing 6))
         (vbox2 (make-instance 'gtk:box
                               :orientation :vertical
                               :spacing 6))
         (label (make-instance 'gtk:label :label "Label"))
         (combo (make-instance 'gtk:combo-box-text
                               :has-entry t)))
    ;; Setup the combo box
    (gtk:combo-box-text-append-text combo "First entry")
    (gtk:combo-box-text-append-text combo "Second entry")
    (gtk:combo-box-text-append-text combo "Third entry")
    ;; Combo box selection has changed
    (g:signal-connect combo "changed"
        (lambda (object)
          (let ((value (gtk:combo-box-text-active-text object)))
            (gtk:label-set-markup label
                                  (format nil "<tt>~a</tt>" value)))))
    ;; Select the first entry of the combo box
    (setf (gtk:combo-box-active combo) 0)
    ;; Setup the entry for the combo box
    (let ((entry (gtk:combo-box-child combo))) ; TODO: Is this correct?
      (setf (gtk:entry-primary-icon-name entry) "list-add")
      (setf (gtk:entry-primary-icon-tooltip-text entry) "Add to Combo Box")
      (setf (gtk:entry-secondary-icon-name entry) "list-remove")
      (setf (gtk:entry-secondary-icon-tooltip-text entry)
            "Remove from Combo Box")
;      ;; Toggle the primary and secondary icons of the entry
;      (g:signal-connect entry "focus-in-event"
;          (lambda (widget event)
;            (declare (ignore event))
;            (setf (gtk:entry-primary-icon-sensitive widget) t)
;            (setf (gtk:entry-secondary-icon-sensitive widget) nil)))
;      (g:signal-connect entry "focus-out-event"
;          (lambda (widget event)
;            (declare (ignore event))
;            (setf (gtk:entry-primary-icon-sensitive widget) nil)
;            (setf (gtk:entry-secondary-icon-sensitive widget) t)))
      ;; One of the icons of the entry has been pressed
      (g:signal-connect entry "icon-press"
          (lambda (object pos)
            (if (eq :primary pos)
                (let ((text (gtk:entry-buffer-text (gtk:entry-buffer object))))
                  (gtk:combo-box-text-append-text combo text))
                (let ((active (gtk:combo-box-active combo)))
                  (gtk:combo-box-text-remove combo active)
                  (setf (gtk:combo-box-active combo) active))))))
    ;; Pack and show widgets
    (gtk:box-append vbox1
                    (make-instance 'gtk:label
                                   :xalign 0
                                   :use-markup t
                                   :label "<b>Select item</b>"))
    (gtk:box-append vbox1 combo)
    (gtk:box-append hbox vbox1)
    (gtk:box-append vbox2
                    (make-instance 'gtk:label
                                   :xalign 0
                                   :use-markup t
                                   :label "<b>Activated item</b>"))
    (gtk:box-append vbox2 label)
    (gtk:box-append hbox vbox2)
    (gtk:widget-show window)))

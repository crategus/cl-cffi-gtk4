;;;; Combo Box Text
;;;;
;;;; The <tt>gtk:combo-box-text</tt> widget is a simple variant of the
;;;; <tt>gtk:combo-box</tt> widget that hides the model-view complexity for
;;;; simple text-only use cases.
;;;;
;;;; To create a <tt>gtk:combo-box-text</tt> widget, use the
;;;; <tt>gtk:combo-box-text-new</tt> or
;;;; <tt>gtk:combo-box-text-new-with-entry</tt> functions.
;;;;
;;;; You can add items to a <tt>gtk:combo-box-text</tt> widget with the
;;;; <tt>gtk:combo-box-text-append-text</tt>,
;;;; <tt>gtk:combo-box-text-insert-text</tt> or
;;;; <tt>gtk:combo-box-text-prepend-text</tt> functions and remove options with
;;;; the <tt>gtk:combo-box-text-remove</tt> function.
;;;;
;;;; If the <tt>gtk:combo-box-text</tt> widget contains an entry via the
;;;; has-entry property, its contents can be retrieved using the
;;;; <tt>gtk:combo-box-text-active-text</tt> function. The entry itself can be
;;;; accessed by calling the <tt>gtk:combo-bpx-child</tt> function on the combo
;;;; box.
;;;;
;;;; 2024-4-6

(in-package :gtk4-example)

(defun do-combo-box-text (&optional application)
  (let* (;; Switch off warnings for deprectated GtkComboBoxText widget
         (gtk-init:*gtk-warn-deprecated* nil)
         (hbox (make-instance 'gtk:box
                              :orientation :horizontal
                              :margin-top 24
                              :margin-bottom 24
                              :margin-start 24
                              :margin-end 24
                              :spacing 24))
         (window (make-instance 'gtk:window
                                :title "Combo Box Text"
                                :child hbox
                                :application application))
         (vbox1 (make-instance 'gtk:box
                               :orientation :vertical
                               :spacing 12))
         (vbox2 (make-instance 'gtk:box
                               :orientation :vertical
                               :spacing 12))
         (label (make-instance 'gtk:label
                               :label "Label"))
         (combo (make-instance 'gtk:combo-box-text
                               :has-entry t)))
    ;; Setup combo box
    (gtk:combo-box-text-append-text combo "First entry")
    (gtk:combo-box-text-append-text combo "Second entry")
    (gtk:combo-box-text-append-text combo "Third entry")
    ;; Combo box selection has changed
    (g:signal-connect combo "changed"
        (lambda (object)
          (let ((value (gtk:combo-box-text-active-text object)))
            (gtk:label-set-markup label
                                  (format nil "<tt>~a</tt>" value)))))
    ;; Select first entry of the combo box
    (setf (gtk:combo-box-active combo) 0)
    ;; Setup the entry for the combo box
    (let ((entry (gtk:combo-box-child combo)))
      (setf (gtk:entry-primary-icon-name entry) "list-add")
      (setf (gtk:entry-primary-icon-tooltip-text entry) "Add to Combo Box")
      (setf (gtk:entry-secondary-icon-name entry) "list-remove")
      (setf (gtk:entry-secondary-icon-tooltip-text entry)
            "Remove from Combo Box")
      ;; One of the icons of the entry has been pressed
      (g:signal-connect entry "icon-press"
          (lambda (object pos)
            (if (eq :primary pos)
                (let ((text (gtk:entry-buffer-text (gtk:entry-buffer object))))
                  (gtk:combo-box-text-append-text combo text))
                (let ((active (gtk:combo-box-active combo)))
                  (gtk:combo-box-text-remove combo active)
                  (setf (gtk:combo-box-active combo) active))))))
    ;; Pack widgets and present window
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
    (gtk:window-present window)))

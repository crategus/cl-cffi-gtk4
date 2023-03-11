;;;; Example More Buttons - 2022-11-18

(in-package :gtk4-example)

(defun do-button-more (&optional application)
  (let* ((grid (make-instance 'gtk:grid
                              :halign :center
                              :valign :center
                              :column-spacing 9
                              :row-spacing 9))
         (window (make-instance 'gtk:window
                                :title "Example More Buttons"
                                :child grid
                                :application application
                                :default-width 300
                                :default-height 180
                                :border-width 12)))

    ;; These are the standard functions to create a button
    (gtk:grid-attach grid
                     (gtk:button-new-with-label "Label")
                     0 0 1 1)
    (gtk:grid-attach grid
                     (gtk:button-new-with-mnemonic "_Mnemonic")
                     0 1 1 1)
    (gtk:grid-attach grid
                     (gtk:button-new-from-icon-name "edit-paste")
                     0 2 1 1)

    ;; Create some buttons with make-instance
    (gtk:grid-attach grid
                     (make-instance 'gtk:button
                                    :child
                                    (make-instance 'gtk:image
                                                   :icon-name "edit-clear")
                                    :label "Bearbeiten")
                     1 0 1 1)
    (gtk:grid-attach grid
                     (make-instance 'gtk:button
                                    :child
                                    (make-instance 'gtk:image
                                                   :icon-name "edit-copy")
                                    :label "Ausschneiden")
                     1 1 1 1)
    (gtk:grid-attach grid
                     (make-instance 'gtk:button
                                    :child
                                    (make-instance 'gtk:image
                                                   :icon-name "edit-cut")
                                    :label "Abbrechen")
                     1 2 1 1)

    (gtk:widget-show window)))

;;;; Example Font Button Label - 2022-11-20

(in-package :gtk4-example)

(defun font-button-filter (family face)
  (declare (ignore face))
  (member (pango:font-family-name family)
          '("Purisa" "Sans" "Serif" "Times New Roman")
          :test #'equal))

(defun do-font-button-label (&optional application)
  (let* ((box (make-instance 'gtk:box
                             :orientation :horizontal
                             :spacing 12))
         (window (make-instance 'gtk:window
                                :title "Example Font Chooser Button"
                                :application application
                                :child box
                                :default-width 300
                                :default-height 100))
         (provider (gtk:css-provider-new))
         (label (make-instance 'gtk:label
                               :label "Font Button"
                               :use-markup t))
         (button (make-instance 'gtk:font-button)))

    (g:signal-connect button "font-set"
        (lambda (chooser)
          (let* (;; Get the font description
                 (desc (gtk:font-chooser-font-desc chooser))
                 ;; Get font informations from the font description
                 (family (pango:font-description-family desc))
                 (size (pango:pixels (pango:font-description-size desc)))
                 (style (pango:font-description-style desc))
                 (weight (pango:font-description-weight desc))
                 (variant (pango:font-description-variant desc))
                 (stretch (pango:font-description-stretch desc))
                 ;; Write the CSS string
                 (css-label (format nil "label { font-family : ~a; ~
                                                 font-weight : ~a; ~
                                                 font-style : ~a; ~
                                                 font-variant : ~a; ~
                                                 font-stretch : ~a; ~
                                                 font-size : ~apx; }"
                                        family
                                        weight
                                        style
                                        variant
                                        stretch
                                        size))
                 ;; Get the style of the label
                 (context (gtk:widget-style-context label)))
           ;; Update the font of the label
           (gtk:css-provider-load-from-data provider css-label)
           (gtk:style-context-add-provider context
                                           provider
                                           gtk:+priority-user+))))
    ;; Set a filter function to select fonts for the font chooser
    (gtk:font-chooser-set-filter-func button #'font-button-filter)
    ;; Pack the widgets
    (gtk:box-append box button)
    (gtk:box-append box label)
    ;; Show the widgets
    (gtk:widget-show window)))

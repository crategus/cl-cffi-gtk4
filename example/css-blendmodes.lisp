;;;; Theming/CSS Blend Modes - 2021-11-27
;;;;
;;;; You can blend multiple backgrounds using the CSS blend modes available.

;; FIXME: This example gives a warning. What is the problem?
;;        Gtk-CRITICAL **: 16:38:45.006:
;;        gtk_root_get_focus: assertion 'GTK_IS_ROOT (self)' failed

(in-package #:gtk4-example)

(defparameter +blend-modes+ '(("Color"         "color")
                              ("Color (burn)"  "color-burn")
                              ("Color (dodge)" "color-dodge")
                              ("Darken"        "darken")
                              ("Difference"    "difference")
                              ("Exclusion"     "exclusion")
                              ("Hard Light"    "hard-light")
                              ("Hue"           "hue")
                              ("Lighten"       "lighten")
                              ("Luminosity"    "luminosity")
                              ("Multiply"      "multiply")
                              ("Normal"        "normal")
                              ("Overlay"       "overlay")
                              ("Saturate"      "saturation")
                              ("Screen"        "screen")
                              ("Soft Light"    "soft-light")))

(defun do-css-blendmodes (&optional application)
  (let* ((builder (gtk:builder-new-from-file (sys-path "resource/css-blendmodes.ui")))
         (provider (make-instance 'gtk:css-provider))
         (listbox (make-instance 'gtk:list-box))
         (window (gtk:builder-object builder "window")))
    (setf (gtk:window-application window) application)
    ;; Setup the CSS provider for window
    (gtk:style-context-add-provider-for-display (gdk:display-default)
                                                provider
                                                +gtk-priority-application+)
    ;; Signal handler for listbox
    (g:signal-connect listbox "row-activated"
        (lambda (listbox row)
          (declare (ignore listbox))
          (let* ((mode (second (elt +blend-modes+
                                    (gtk:list-box-row-index row))))
                 (str (format nil
                              (read-file (sys-path "resource/css-blendmodes.css"))
                      mode mode mode)))
            (gtk:css-provider-load-from-data provider str))))
    ;; Fill the list box
    (let ((normal nil))
      (dolist (mode +blend-modes+)
        (let ((row (make-instance 'gtk:list-box-row)))
           (setf (gtk:list-box-row-child row)
                 (make-instance 'gtk:label
                                :label (first mode)
                                :xalign 0.0))
           (gtk:list-box-insert listbox row -1)
           (when (string= "normal" (second mode))
             (setq normal row))))
      ;; Select the "normal" row
      (gtk:list-box-select-row listbox normal)
      (g:signal-emit normal "activate"))
    ;; Add listbox to scrolled window
    (setf (gtk:scrolled-window-child (gtk:builder-object builder
                                                         "scrolledwindow"))
          listbox)
    ;; Show the window
    (gtk:widget-show window)))

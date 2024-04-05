;;;; CSS Blend Modes
;;;;
;;;; You can blend multiple backgrounds using the CSS blend modes available.
;;;;
;;;; 2024-4-2

;; FIXME: This example gives a warning. What is the problem?
;;        Gtk-CRITICAL **: 16:38:45.006:
;;        gtk_root_get_focus: assertion 'GTK_IS_ROOT (self)' failed

(in-package :gtk4-example)

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
  (let* ((path1 (sys-path "resource/css-blendmodes.ui" :gtk4-example))
         (builder (gtk:builder-new-from-file path1))
         (listbox (make-instance 'gtk:list-box))
         (window (gtk:builder-object builder "window"))
         (display (gtk:widget-display window))
         (provider (make-instance 'gtk:css-provider))
         (path2 (sys-path "resource/gtk4-example.gresource" :gtk4-example))
         (resource (g:resource-load path2)))
    ;; Register the resources
    (g:resources-register resource)
    ;; Add the window to the application
    (setf (gtk:window-application window) application)
    ;; Setup the CSS provider for window
    (gtk:style-context-add-provider-for-display display provider)
    (g:signal-connect window "destroy"
        (lambda (widget)
          (let ((display (gtk:widget-display widget)))
            (gtk:style-context-remove-provider-for-display display provider))))
    ;; Signal handler for listbox
    (g:signal-connect listbox "row-activated"
        (lambda (listbox row)
          (declare (ignore listbox))
          (let* ((mode (second (elt +blend-modes+
                                    (gtk:list-box-row-index row))))
                 (path (sys-path "resource/css-blendmodes.css" :gtk4-example))
                 (str (format nil (read-file path) mode mode mode)))
            (gtk:css-provider-load-from-string provider str))))
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
    (gtk:window-present window)))

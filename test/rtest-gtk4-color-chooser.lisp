(in-package :gtk-test)

(def-suite gtk-color-chooser :in gtk-suite)
(in-suite gtk-color-chooser)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkColorChooser

(test color-chooser-interface
  ;; Type check
  (is (g:type-is-interface "GtkColorChooser"))
  ;; Check the registered name
  (is (eq 'gtk:color-chooser
          (gobject:symbol-for-gtype "GtkColorChooser")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkColorChooser")
          (g:gtype (cffi:foreign-funcall "gtk_color_chooser_get_type" :size))))
  ;; Get the names of the interface properties.
  (is (equal '("rgba" "use-alpha")
             (list-interface-properties "GtkColorChooser")))
  ;; Get the interface definition
  (is (equal '(DEFINE-G-INTERFACE "GtkColorChooser" GTK-COLOR-CHOOSER
                    (:EXPORT T :TYPE-INITIALIZER "gtk_color_chooser_get_type")
                    (RGBA GTK-COLOR-CHOOSER-RGBA "rgba" "GdkRGBA" T T)
                    (USE-ALPHA GTK-COLOR-CHOOSER-USE-ALPHA "use-alpha"
                     "gboolean" T T))
             (gobject:get-g-type-definition "GtkColorChooser"))))

;;; --- Properties -------------------------------------------------------------

;;;     rgba
;;;     use-alpha

(test color-chooser-properties
  (let ((chooser (make-instance 'gtk:color-button)))
    (is (typep (gtk:color-chooser-rgba chooser) 'gdk:rgba))
    (is (typep (setf (gtk:color-chooser-rgba chooser) (gdk:rgba-parse "Blue"))
               'gdk:rgba))
    (is-false (gtk:color-chooser-use-alpha chooser))
    (is-true (setf (gtk:color-chooser-use-alpha chooser) t))))

;;; --- Signals ----------------------------------------------------------------

;;;     color-activated

(test color-chooser-color-activated-signal
  (let ((query (g:signal-query (g:signal-lookup "color-activated"
                                                "GtkColorChooser"))))
    (is (string= "color-activated" (g:signal-query-signal-name query)))
    (is (string= "GtkColorChooser"
                 (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '("GdkRGBA")
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_color_chooser_add_palette

;;;     gtk_hsv_to_rgb

(test hsv-to-rgb
  (is (equal '(0.0 0.0 0.0) (multiple-value-list (gtk:hsv-to-rgb 1 0 0))))
  (is (equal '(0.0 0.0 0.0) (multiple-value-list (gtk:hsv-to-rgb 0.5 0 0))))
  (is (equal '(1.0 0.0 0.0) (multiple-value-list (gtk:hsv-to-rgb 1 1 1))))
  (is (equal '(0.25 0.5 0.5) (multiple-value-list (gtk:hsv-to-rgb 0.5 0.5 0.5)))))

;;;     gtk_rgb_to_hsv

(test rgb-to-hsv
  (is (equal '(0.0 1.0 1.0) (multiple-value-list (gtk:rgb-to-hsv 1 0 0))))
  (is (equal '(0.0 1.0 0.5) (multiple-value-list (gtk:rgb-to-hsv 0.5 0 0))))
  (is (equal '(0.0 0.0 1.0) (multiple-value-list (gtk:rgb-to-hsv 1 1 1))))
  (is (equal '(0.0 0.0 0.5) (multiple-value-list (gtk:rgb-to-hsv 0.5 0.5 0.5)))))

;;; --- 2023-3-18 --------------------------------------------------------------

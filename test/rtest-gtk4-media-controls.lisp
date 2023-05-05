(in-package :gtk-test)

(def-suite gtk-media-controls :in gtk-suite)
(in-suite gtk-media-controls)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkMediaControls

(test gtk-media-controls-class
  ;; Type check
  (is (g:type-is-object "GtkMediaControls"))
  ;; Check the registered name
  (is (eq 'gtk:media-controls
          (gobject:symbol-for-gtype "GtkMediaControls")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkMediaControls")
          (g:gtype (cffi:foreign-funcall "gtk_media_controls_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget") (g:type-parent "GtkMediaControls")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkMediaControls")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (list-interfaces "GtkMediaControls")))
  ;; Check the class properties
  (is (equal '("media-stream")
             (list-properties "GtkMediaControls")))
  ;; Check the list of signals
  (is (equal '()
             (list-signals "GtkMediaControls")))
  ;; CSS information
  (is (string= "controls"
               (gtk:widget-class-css-name "GtkMediaControls")))
  (is (string=
"controls:dir(ltr)
  box.horizontal:disabled:dir(ltr)
    button.flat.image-button:disabled:dir(ltr)
      image:disabled:dir(ltr)
    box.horizontal:disabled:dir(ltr)
      label:disabled:dir(ltr)
      scale.horizontal:disabled:dir(ltr)
        trough:disabled:dir(ltr)
          highlight.top:disabled:dir(ltr)
          slider:disabled:dir(ltr)
      label:disabled:dir(ltr)
    scalebutton.scale:disabled:dir(ltr)
      button.flat.image-button.toggle:disabled:dir(ltr)
        image:disabled:dir(ltr)
      [popover.background.scale-popup:disabled:dir(ltr)]
        contents:disabled:dir(ltr)
          box.vertical:disabled:dir(ltr)
            button.flat.image-button:disabled:dir(ltr)
              image:disabled:dir(ltr)
            scale.vertical:disabled:dir(ltr)
              trough:disabled:dir(ltr)
                highlight.bottom:disabled:dir(ltr)
                slider:disabled:dir(ltr)
            button.flat.image-button:disabled:dir(ltr)
              image:disabled:dir(ltr)
        arrow:dir(ltr)
"
               (print-style-context "GtkMediaControls")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkMediaControls" GTK-MEDIA-CONTROLS
                       (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                        ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
                        :TYPE-INITIALIZER "gtk_media_controls_get_type")
                       ((MEDIA-STREAM GTK-MEDIA-CONTROLS-MEDIA-STREAM
                         "media-stream" "GtkMediaStream" T T)))
             (get-g-type-definition "GtkMediaControls"))))

;;; --- Properties -------------------------------------------------------------

;;;     media-stream

(test gtk-media-controls-properties
  (let ((controls (make-instance 'gtk:media-controls)))
    (is-false (gtk:media-controls-media-stream controls))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_media_controls_new

;;; --- 2023-5-3 ---------------------------------------------------------------

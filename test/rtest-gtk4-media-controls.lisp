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
          (glib:symbol-for-gtype "GtkMediaControls")))
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
  ;; CSS name
  (is (string= "controls"
               (gtk:widget-class-css-name "GtkMediaControls")))
  ;; CSS classes
  (is (equal '()
             (gtk:widget-css-classes (make-instance 'gtk:media-controls))))
  ;; Accessible role
  (is (eq :widget (gtk:widget-class-accessible-role "GtkMediaControls")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkMediaControls" GTK-MEDIA-CONTROLS
                       (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                        ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
                        :TYPE-INITIALIZER "gtk_media_controls_get_type")
                       ((MEDIA-STREAM GTK-MEDIA-CONTROLS-MEDIA-STREAM
                         "media-stream" "GtkMediaStream" T T)))
             (gobject:get-g-type-definition "GtkMediaControls"))))

;;; --- Properties -------------------------------------------------------------

;;;     media-stream

(test gtk-media-controls-properties
  (let ((controls (make-instance 'gtk:media-controls)))
    (is-false (gtk:media-controls-media-stream controls))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_media_controls_new

;;; --- 2023-11-3 --------------------------------------------------------------

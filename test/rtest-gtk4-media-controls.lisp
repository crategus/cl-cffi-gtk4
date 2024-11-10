(in-package :gtk-test)

(def-suite gtk-media-controls :in gtk-suite)
(in-suite gtk-media-controls)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkMediaControls

(test gtk-media-controls-class
  ;; Check type
  (is (g:type-is-object "GtkMediaControls"))
  ;; Check registered name
  (is (eq 'gtk:media-controls
          (glib:symbol-for-gtype "GtkMediaControls")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkMediaControls")
          (g:gtype (cffi:foreign-funcall "gtk_media_controls_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget") (g:type-parent "GtkMediaControls")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkMediaControls")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (glib-test:list-interfaces "GtkMediaControls")))
  ;; Check class properties
  (is (equal '("media-stream")
             (glib-test:list-properties "GtkMediaControls")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkMediaControls")))
  ;; Check CSS name
  (is (string= "controls"
               (gtk:widget-class-css-name "GtkMediaControls")))
  ;; Check accessible role
  (is (eq :widget (gtk:widget-class-accessible-role "GtkMediaControls")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkMediaControls" GTK:MEDIA-CONTROLS
                       (:SUPERCLASS GTK:WIDGET
                        :EXPORT T
                        :INTERFACES
                        ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
                        :TYPE-INITIALIZER "gtk_media_controls_get_type")
                       ((MEDIA-STREAM MEDIA-CONTROLS-MEDIA-STREAM
                         "media-stream" "GtkMediaStream" T T)))
             (gobject:get-gtype-definition "GtkMediaControls"))))

;;; --- Properties -------------------------------------------------------------

;;;     media-stream

(test gtk-media-controls-properties
  (let ((controls (make-instance 'gtk:media-controls)))
    (is-false (gtk:media-controls-media-stream controls))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_media_controls_new

(test gtk-media-controls-new
  (let ((stream (gtk:media-file-new)))
    (is (typep (gtk:media-controls-new stream) 'gtk:media-controls))))

;;; 2024-10-31

(in-package :gtk-test)

(def-suite gtk-aspect-frame :in gtk-suite)
(in-suite gtk-aspect-frame)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkAspectFrame

(test gtk-aspect-frame-class
  ;; Check type
  (is (g:type-is-object "GtkAspectFrame"))
  ;; Check registered name
  (is (eq 'gtk:aspect-frame
          (glib:symbol-for-gtype "GtkAspectFrame")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkAspectFrame")
          (g:gtype (cffi:foreign-funcall "gtk_aspect_frame_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkAspectFrame")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkAspectFrame")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (glib-test:list-interfaces "GtkAspectFrame")))
  ;; Check properties
  (is (equal '("child" "obey-child" "ratio" "xalign" "yalign")
             (glib-test:list-properties "GtkAspectFrame")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkAspectFrame")))
  ;; Check CSS name
  (is (string= "aspectframe"
               (gtk:widget-class-css-name "GtkAspectFrame")))
  ;; Check accessible role
  (is (eq :generic (gtk:widget-class-accessible-role "GtkAspectFrame")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkAspectFrame" GTK:ASPECT-FRAME
                      (:SUPERCLASS GTK:WIDGET
                       :EXPORT T
                       :INTERFACES
                       ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
                       :TYPE-INITIALIZER "gtk_aspect_frame_get_type")
                      ((CHILD ASPECT-FRAME-CHILD "child" "GtkWidget" T T)
                       (OBEY-CHILD ASPECT-FRAME-OBEY-CHILD
                        "obey-child" "gboolean" T T)
                       (RATIO ASPECT-FRAME-RATIO "ratio" "gfloat" T T)
                       (XALIGN ASPECT-FRAME-XALIGN "xalign" "gfloat" T T)
                       (YALIGN ASPECT-FRAME-YALIGN "yalign" "gfloat" T T)))
             (gobject:get-gtype-definition "GtkAspectFrame"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-aspect-frame-properties
  (let ((frame (make-instance 'gtk:aspect-frame)))
    (is-false (gtk:aspect-frame-child frame))
    (is-true (gtk:aspect-frame-obey-child frame))
    (is (= 1.0 (gtk:aspect-frame-ratio frame)))
    (is (= 0.5 (gtk:aspect-frame-xalign frame)))
    (is (= 0.5 (gtk:aspect-frame-yalign frame)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_aspect_frame_new

(test gtk-aspect-frame-new
  (let ((frame (gtk:aspect-frame-new 0.25 0.50 2.5 nil)))
    (is (typep frame 'gtk:aspect-frame))
    (is-false (gtk:aspect-frame-child frame))
    (is-false (gtk:aspect-frame-obey-child frame))
    (is (= 2.5 (gtk:aspect-frame-ratio frame)))
    (is (= 0.25 (gtk:aspect-frame-xalign frame)))
    (is (= 0.50 (gtk:aspect-frame-yalign frame)))))

;;; 2024-4-21

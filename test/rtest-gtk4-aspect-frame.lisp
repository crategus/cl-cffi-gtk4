(in-package :gtk-test)

(def-suite gtk-aspect-frame :in gtk-suite)
(in-suite gtk-aspect-frame)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkAspectFrame

(test gtk-aspect-frame-class
  ;; Type check
  (is (g:type-is-object "GtkAspectFrame"))
  ;; Check the registered name
  (is (eq 'gtk:aspect-frame
          (glib:symbol-for-gtype "GtkAspectFrame")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkAspectFrame")
          (g:gtype (cffi:foreign-funcall "gtk_aspect_frame_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkAspectFrame")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkAspectFrame")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (list-interfaces "GtkAspectFrame")))
  ;; Check the properties
  (is (equal '("child" "obey-child" "ratio" "xalign" "yalign")
             (list-properties "GtkAspectFrame")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkAspectFrame")))
  ;; CSS name
  (is (string= "aspectframe"
               (gtk:widget-class-css-name "GtkAspectFrame")))
  ;; CSS classes
  (is (equal '()
             (gtk:widget-css-classes (make-instance 'gtk:aspect-frame))))
  ;; Accessible role
  (is (eq :generic (gtk:widget-class-accessible-role "GtkAspectFrame")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkAspectFrame" GTK-ASPECT-FRAME
                               (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                                ("GtkAccessible" "GtkBuildable"
                                 "GtkConstraintTarget")
                                :TYPE-INITIALIZER "gtk_aspect_frame_get_type")
                               ((CHILD GTK-ASPECT-FRAME-CHILD "child"
                                 "GtkWidget" T T)
                                (OBEY-CHILD GTK-ASPECT-FRAME-OBEY-CHILD
                                 "obey-child" "gboolean" T T)
                                (RATIO GTK-ASPECT-FRAME-RATIO "ratio" "gfloat"
                                 T T)
                                (XALIGN GTK-ASPECT-FRAME-XALIGN "xalign"
                                 "gfloat" T T)
                                (YALIGN GTK-ASPECT-FRAME-YALIGN "yalign"
                                 "gfloat" T T)))
             (gobject:get-g-type-definition "GtkAspectFrame"))))

;;; --- Properties -------------------------------------------------------------

;;;     child
;;;     obey-child
;;;     ratio
;;;     xalign
;;;     yalign

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

;;; --- 2023-11-4 --------------------------------------------------------------

(in-package :gtk-test)

(def-suite gtk-frame :in gtk-suite)
(in-suite gtk-frame)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFrame

(test gtk-frame-class
  ;; Type check
  (is (g:type-is-object "GtkFrame"))
  ;; Check the registered name
  (is (eq 'gtk:frame
          (glib:symbol-for-gtype "GtkFrame")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkFrame")
          (g:gtype (cffi:foreign-funcall "gtk_frame_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkFrame")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkFrame")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (list-interfaces "GtkFrame")))
  ;; Check the properties
  (is (equal '("child" "label" "label-widget" "label-xalign")
             (list-properties "GtkFrame")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkFrame")))
  ;; CSS name
  (is (string= "frame"
               (gtk:widget-class-css-name "GtkFrame")))
  ;; CSS classes
  (is (equal '()
             (gtk:widget-css-classes (make-instance 'gtk:frame))))
  ;; Accessible role
  (is (eq :group (gtk:widget-class-accessible-role "GtkFrame")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkFrame" GTK-FRAME
                       (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                        ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
                        :TYPE-INITIALIZER "gtk_frame_get_type")
                       ((CHILD GTK-FRAME-CHILD "child" "GtkWidget" T T)
                        (LABEL GTK-FRAME-LABEL "label" "gchararray" T T)
                        (LABEL-WIDGET GTK-FRAME-LABEL-WIDGET "label-widget"
                         "GtkWidget" T T)
                        (LABEL-XALIGN GTK-FRAME-LABEL-XALIGN "label-xalign"
                         "gfloat" T T)))
             (gobject:get-g-type-definition "GtkFrame"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-frame-properties.1
  (let ((frame (make-instance 'gtk:frame
                              :child (make-instance 'gtk:button))))
    (is (typep (gtk:frame-child frame) 'gtk:button))
    (is-false (gtk:frame-label frame))
    (is-false (gtk:frame-label-widget frame))
    (is (= 0.0 (gtk:frame-label-xalign frame)))))

(test gtk-frame-properties.2
  (let ((frame (gtk:frame-new "label")))
    (is (string= "label" (gtk:frame-label frame)))
    (is (typep (gtk:frame-label-widget frame) 'gtk:label))
    (is (string= "label" (gtk:label-label (gtk:frame-label-widget frame))))))

(test gtk-frame-properties.3
  (let ((frame (gtk:frame-new "label")))
    ;; The default value is 0.0
    (is (= 0.0 (gtk:frame-label-xalign frame)))
    ;; Set float value
    (is (= 1.0 (setf (gtk:frame-label-xalign frame) 1.0)))
    (is (= 1.0 (gtk:frame-label-xalign frame)))
    ;; Set integer value
    (is (= 1.0 (setf (gtk:frame-label-xalign frame) 1)))
    (is (= 1.0 (gtk:frame-label-xalign frame)))
    ;; Set rational value
    (is (= 0.5 (setf (gtk:frame-label-xalign frame) 1/2)))
    (is (= 0.5 (gtk:frame-label-xalign frame)))
    ;; Set double float value
    (is (= 0.3d0 (setf (gtk:frame-label-xalign frame) 0.3d0)))
    (is (= 0.3 (gtk:frame-label-xalign frame)))))

(test gtk-frame-properties.4
  ;; Initalize the label-xalign property with different types for the value
  (is (= 0.0 (gtk:frame-label-xalign (make-instance 'gtk:frame))))
  (is (= 1.0 (gtk:frame-label-xalign (make-instance 'gtk:frame
                                                    :label-xalign 1.0))))
  (is (= 1.0 (gtk:frame-label-xalign (make-instance 'gtk:frame
                                                    :label-xalign 1))))
  (is (= 1.0 (gtk:frame-label-xalign (make-instance 'gtk:frame
                                                    :label-xalign 1.0d0))))
  (is (= 0.5 (gtk:frame-label-xalign (make-instance 'gtk:frame
                                                    :label-xalign 1/2)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_frame_new

(test gtk-frame-new
  (is (typep (gtk:frame-new) 'gtk:frame))
  (is (typep (gtk:frame-new nil) 'gtk:frame))
  (is (typep (gtk:frame-new "label") 'gtk:frame)))

;;;     gtk_frame_get_align
;;;     gtk_frame_set_align

(test gtk-frame-label-align
  (let ((frame (gtk:frame-new "label")))
    (is (= 0.0 (gtk:frame-label-align frame)))
    (is (= 1.0 (setf (gtk:frame-label-align frame) 1.0)))
    (is (= 1.0 (gtk:frame-label-align frame)))
    (is (= 1.0 (setf (gtk:frame-label-align frame) 1)))
    (is (= 1.0 (gtk:frame-label-align frame)))
    (is (= 0.5 (setf (gtk:frame-label-align frame) 1/2)))
    (is (= 0.5 (gtk:frame-label-align frame)))
    (is (= 0.3d0 (setf (gtk:frame-label-align frame) 0.3d0)))
    (is (= 0.3 (gtk:frame-label-align frame)))))

;;; 2024-1-7

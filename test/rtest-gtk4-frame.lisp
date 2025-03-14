(in-package :gtk-test)

(def-suite gtk-frame :in gtk-ornaments)
(in-suite gtk-frame)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFrame

(test gtk-frame-class
  ;; Check type
  (is (g:type-is-object "GtkFrame"))
  ;; Check registered name
  (is (eq 'gtk:frame
          (glib:symbol-for-gtype "GtkFrame")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkFrame")
          (g:gtype (cffi:foreign-funcall "gtk_frame_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkFrame")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkFrame")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (glib-test:list-interfaces "GtkFrame")))
  ;; Check properties
  (is (equal '("child" "label" "label-widget" "label-xalign")
             (glib-test:list-properties "GtkFrame")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkFrame")))
  ;; Check CSS name
  (is (string= "frame"
               (gtk:widget-class-css-name "GtkFrame")))
  ;; Check accessible role
  (is (eq :group (gtk:widget-class-accessible-role "GtkFrame")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkFrame" GTK:FRAME
                       (:SUPERCLASS GTK:WIDGET
                        :EXPORT T
                        :INTERFACES
                        ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
                        :TYPE-INITIALIZER "gtk_frame_get_type")
                       ((CHILD FRAME-CHILD "child" "GtkWidget" T T)
                        (LABEL FRAME-LABEL "label" "gchararray" T T)
                        (LABEL-WIDGET FRAME-LABEL-WIDGET
                         "label-widget" "GtkWidget" T T)
                        (LABEL-XALIGN FRAME-LABEL-XALIGN
                         "label-xalign" "gfloat" T T)))
             (gobject:get-gtype-definition "GtkFrame"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-frame-properties.1
  (let* ((button (make-instance 'gtk:button))
         (frame (make-instance 'gtk:frame
                               :child button)))
    (is (typep (gtk:frame-child frame) 'gtk:button))
    (is-false (gtk:frame-label frame))
    (is-false (gtk:frame-label-widget frame))
    (is (= 0.0 (gtk:frame-label-xalign frame)))
    ;; Check memory management
    (is-false (setf (gtk:frame-child frame) nil))
    (is (= 1 (g:object-ref-count button)))
    (is (= 1 (g:object-ref-count frame)))))

(test gtk-frame-properties.2
  (let ((frame (gtk:frame-new "label")))
    (is (string= "label" (gtk:frame-label frame)))
    (is (typep (gtk:frame-label-widget frame) 'gtk:label))
    (is (string= "label" (gtk:label-label (gtk:frame-label-widget frame))))
    ;; Check memory management
    (is-false (setf (gtk:frame-label-widget frame) nil))
    (is (= 1 (g:object-ref-count frame)))))

(test gtk-frame-properties.3
  (let ((frame (gtk:frame-new "label")))
    ;; The default value is 0.0
    (is (= 0.0 (gtk:frame-label-xalign frame)))
    ;; Set float
    (is (= 1.0 (setf (gtk:frame-label-xalign frame) 1.0)))
    (is (= 1.0 (gtk:frame-label-xalign frame)))
    ;; Set integer
    (is (= 1.0 (setf (gtk:frame-label-xalign frame) 1)))
    (is (= 1.0 (gtk:frame-label-xalign frame)))
    ;; Set rational
    (is (= 0.5 (setf (gtk:frame-label-xalign frame) 1/2)))
    (is (= 0.5 (gtk:frame-label-xalign frame)))
    ;; Set double float
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

;;; 2024-10-27

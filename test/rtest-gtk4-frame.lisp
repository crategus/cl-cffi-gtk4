(in-package :gtk-test)

(def-suite gtk-frame :in gtk-suite)
(in-suite gtk-frame)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFrame

(test frame-class
  ;; Type check
  (is (g:type-is-object "GtkFrame"))
  ;; Check the registered name
  (is (eq 'gtk:frame
          (gobject:symbol-for-gtype "GtkFrame")))
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
  ;; CSS information
  (is (string= "frame"
               (gtk:widget-class-css-name "GtkFrame")))
  (is (string=
"frame:dir(ltr)
"
               (gtk:style-context-to-string
                   (gtk:widget-style-context (make-instance 'gtk:frame))
                   :none)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkFrame" GTK-FRAME
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

(test frame-properties.1
  (let ((frame (make-instance 'gtk:frame
                              :child (make-instance 'gtk:button))))
    (is (typep (gtk:frame-child frame) 'gtk:button))
    (is-false (gtk:frame-label frame))
    (is-false (gtk:frame-label-widget frame))
    (is (= 0.0 (gtk:frame-label-xalign frame)))))

(test frame-properties.2
  (let ((frame (gtk:frame-new "label")))
    (is (string= "label" (gtk:frame-label frame)))
    (is (typep (gtk:frame-label-widget frame) 'gtk:label))
    (is (string= "label" (gtk:label-label (gtk:frame-label-widget frame))))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_frame_new

(test frame-new
  (is (typep (gtk:frame-new) 'gtk:frame))
  (is (typep (gtk:frame-new nil) 'gtk:frame))
  (is (typep (gtk:frame-new "label") 'gtk:frame)))

;;; --- 2023-3-18 --------------------------------------------------------------

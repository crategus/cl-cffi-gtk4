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
          (g:gtype (foreign-funcall "gtk_frame_get_type" :size))))
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

;;;     child
;;;     label
;;;     label-widget
;;;     label-xalign

;;; --- Functions --------------------------------------------------------------

;;;     gtk_frame_new
;;;     gtk_frame_set_label_align                          not implemented
;;;     gtk_frame_get_label_align                          not implemented

;;; 2022-11-11

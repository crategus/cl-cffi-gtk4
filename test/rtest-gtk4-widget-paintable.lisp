(in-package :gtk-test)

(def-suite gtk-widget-paintable :in gtk-suite)
(in-suite gtk-widget-paintable)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkWidgetPaintable

(test gtk-widget-paintable-class
  ;; Check type
  (is (g:type-is-object "GtkWidgetPaintable"))
  ;; Check registered name
  (is (eq 'gtk:widget-paintable
          (glib:symbol-for-gtype "GtkWidgetPaintable")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkWidgetPaintable")
          (g:gtype (cffi:foreign-funcall "gtk_widget_paintable_get_type"
                                         :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkWidgetPaintable")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkWidgetPaintable")))
  ;; Check interfaces
  (is (equal '("GdkPaintable")
             (glib-test:list-interfaces "GtkWidgetPaintable")))
  ;; Check class properties
  (is (equal '("widget")
             (glib-test:list-properties "GtkWidgetPaintable")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkWidgetPaintable")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkWidgetPaintable" GTK:WIDGET-PAINTABLE
                       (:SUPERCLASS G:OBJECT
                        :EXPORT T
                        :INTERFACES ("GdkPaintable")
                        :TYPE-INITIALIZER "gtk_widget_paintable_get_type")
                       ((WIDGET WIDGET-PAINTABLE-WIDGET
                         "widget" "GtkWidget" T T)))
             (gobject:get-gtype-definition "GtkWidgetPaintable"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-widget-paintable-properties
  (let* ((widget (make-instance 'gtk:label))
         (button (make-instance 'gtk:button))
         (paintable (gtk:widget-paintable-new widget)))
    (is (typep (gtk:widget-paintable-widget paintable) 'gtk:label))
    (is (typep (setf (gtk:widget-paintable-widget paintable) button) 'gtk:button))
    (is (typep (gtk:widget-paintable-widget paintable) 'gtk:button))

    ;; FIXME: We have to remove the widget from the paintable to avoid
    ;; an error with the memory management
    (is-false (setf (gtk:widget-paintable-widget paintable) nil))

    (is (= 1 (g:object-ref-count button)))
    (is (= 1 (g:object-ref-count widget)))
    (is (= 1 (g:object-ref-count paintable)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_widget_paintable_new

(test gtk-widget-paintable-new
  (let (paintable)
    (is (typep (setf paintable
                     (gtk:widget-paintable-new (make-instance 'gtk:button)))
               'gtk:widget-paintable))
    ;; FIXME: We have to remove the widget from the paintable to avoid
    ;; an error with the memory management
    (is-false (setf (gtk:widget-paintable-widget paintable) nil))
    (is (= 1 (g:object-ref-count paintable)))))

;;; 2024-11-2

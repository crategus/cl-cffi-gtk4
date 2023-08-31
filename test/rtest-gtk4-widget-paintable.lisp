(in-package :gtk-test)

(def-suite gtk-widget-paintable :in gtk-suite)
(in-suite gtk-widget-paintable)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkWidgetPaintable

(test gtk-widget-paintable-class
  ;; Type check
  (is (g:type-is-object "GtkWidgetPaintable"))
  ;; Check the registered name
  (is (eq 'gtk:widget-paintable
          (glib:symbol-for-gtype "GtkWidgetPaintable")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkWidgetPaintable")
          (g:gtype (cffi:foreign-funcall "gtk_widget_paintable_get_type"
                                         :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkWidgetPaintable")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkWidgetPaintable")))
  ;; Check the interfaces
  (is (equal '("GdkPaintable")
             (list-interfaces "GtkWidgetPaintable")))
  ;; Check the class properties
  (is (equal '("widget")
             (list-properties "GtkWidgetPaintable")))
  ;; Check the list of signals
  (is (equal '()
             (list-signals "GtkWidgetPaintable")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkWidgetPaintable"
                                             GTK-WIDGET-PAINTABLE
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES
                        ("GdkPaintable") :TYPE-INITIALIZER
                        "gtk_widget_paintable_get_type")
                       ((WIDGET GTK-WIDGET-PAINTABLE-WIDGET "widget"
                         "GtkWidget" T T)))
             (gobject:get-g-type-definition "GtkWidgetPaintable"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-widget-paintable-properties
  (let* ((widget (make-instance 'gtk:label))
         (button (make-instance 'gtk:button))
         (paintable (gtk:widget-paintable-new widget)))
    (is (typep (gtk:widget-paintable-widget paintable) 'gtk:label))
    (is (typep (setf (gtk:widget-paintable-widget paintable) button) 'gtk:button))
    (is (typep (gtk:widget-paintable-widget paintable) 'gtk:button))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_widget_paintable_new

(test gtk-widget-paintable-new
  (is (typep (gtk:widget-paintable-new (make-instance 'gtk:button))
             'gdk:paintable)))

;;; --- 2023-8-31 --------------------------------------------------------------

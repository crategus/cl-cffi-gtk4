(def-suite gtk-widget-paintable :in gtk-suite)
(in-suite gtk-widget-paintable)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkWidgetPaintable

(test gtk-widget-paintable-class
  ;; Type check
  (is (g:type-is-object "GtkWidgetPaintable"))
  ;; Check the registered name
  (is (eq 'gtk-widget-paintable
          (gobject:symbol-for-gtype "GtkWidgetPaintable")))
  ;; Check the type initializer
  (is (eq (gtype "GtkWidgetPaintable")
          (gtype (foreign-funcall "gtk_widget_paintable_get_type" g-size))))
  ;; Check the parent
  (is (eq (gtype "GObject")
          (g-type-parent "GtkWidgetPaintable")))
  ;; Check the children
  (is (equal '()
             (mapcar #'g-type-name (g-type-children "GtkWidgetPaintable"))))
  ;; Check the interfaces
  (is (equal '("GdkPaintable")
             (mapcar #'g-type-name (g-type-interfaces "GtkWidgetPaintable"))))
  ;; Check the class properties
  (is (equal '("widget")
             (list-class-property-names "GtkWidgetPaintable")))
  ;; Check the list of signals
  (is (equal '()
             (sort (mapcar #'g-signal-name
                           (g-signal-list-ids "GtkWidgetPaintable"))
                   #'string<)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkWidgetPaintable" GTK-WIDGET-PAINTABLE
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES
                        ("GdkPaintable") :TYPE-INITIALIZER
                        "gtk_widget_paintable_get_type")
                       ((WIDGET GTK-WIDGET-PAINTABLE-WIDGET "widget"
                         "GtkWidget" T T)))
             (get-g-type-definition "GtkWidgetPaintable"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-widget-paintable-properties
  (let* ((widget (make-instance 'gtk-label))
         (paintable (make-instance 'gtk-widget-paintable
                                   :widget widget)))
    (is (eq (gtype "GtkLabel")
            (g-type-from-instance (gtk-widget-paintable-widget paintable))))
    (is (eq (gtype "GtkButton")
            (g-type-from-instance (setf (gtk-widget-paintable-widget paintable)
                                        (make-instance 'gtk:button)))))
    (is (eq (gtype "GtkButton")
            (g-type-from-instance (gtk-widget-paintable-widget paintable))))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_widget_paintable_new

;;; 2022-7-10

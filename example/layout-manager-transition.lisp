;;;; Layout Manger Transition

(in-package :gtk4-example)

#|
static void
demo_child_init (DemoChild *self)
{
}

static void
demo_child_class_init (DemoChildClass *class)
{
  GtkWidgetClass *widget_class = GTK_WIDGET_CLASS (class);

  widget_class->snapshot = demo_child_snapshot;
  widget_class->measure = demo_child_measure;
}
|#


;; /* This is a trivial child widget just for demo purposes.
;;  * It draws a 32x32 square in fixed color.
;;  */
;; struct _DemoChild
;; {
;;   GtkWidget parent_instance;
;;   GdkRGBA color;
;; };
(gobject:define-gobject-subclass "DemoChild" demo-child
  (:superclass gtk:widget
   :export t
   :interfaces ())
  ((color
    demo-child-color
    "color" "GdkRGBA" t t)))

(gobject:define-vtable ("DemoChild" demo-child)
  (:skip parent-instance (:struct gobject::object-class))
  ;; Methods for the GtkWidget class
  (:skip show :pointer)
  (:skip hide :pointer)
  (:skip map :pointer)
  (:skip unmap :pointer)
  (:skip realize :pointer)
  (:skip unrealize :pointer)
  (:skip root :pointer)
  (:skip unroot :pointer)
  (:skip size-allocate :pointer)
  (:skip state-flags-changed :pointer)
  (:skip direction-changed :pointer)
  (:skip get-request-mode :pointer)
  (:skip measure :pointer)
  ;; Install a virtual measure function
  (measure (:void (widget (g:object demo-child))
                  (orientation gtk:orientation)
                  (for-size :int)
                  (minimum (:pointer :int))
                  (natural (:pointer :int))
                  (minimum-baseline (:pointer :int))
                  (natural-baseline (:pointer :int))))
  (:skip mnemonic-activate :pointer)
  (:skip grab-focus :pointer)
  (:skip focus :pointer)
  (:skip set-focus-child :pointer)
  (:skip move-focus :pointer)
  (:skip keynav-failed :pointer)
  (:skip query-tooltip :pointer)
  (:skip compute-expand :pointer)
  (:skip css-changed :pointer)
  (:skip system-settings-changed :pointer)
  ;; Install new virtual snapshot function
  (snapshot (:void (widget (g:object demo-child))
                   (snapshot (g:object gtk:snapshot))))
  (:skip contains :pointer))

;; static void
;; demo_child_snapshot (GtkWidget   *widget,
;;                      GtkSnapshot *snapshot)
;; {
;;   DemoChild *self = DEMO_CHILD (widget);
;;   int width, height;
;;
;;   width = gtk_widget_get_width (widget);
;;   height = gtk_widget_get_height (widget);
;;
;;   gtk_snapshot_append_color (snapshot, &self->color,
;;                              &GRAPHENE_RECT_INIT(0, 0, width, height));
;; }
(defmethod demo-child-snapshot-impl ((widget demo-child) snapshot)
  (let ((width (gtk:widget-width widget))
        (height (gtk:widget-height widget))
        (color (demo-child-color widget)))
    (format t "in DEMO-CHILD-SNAPSHOT-IMPL~%")
    (format t "    width : ~a~%" width)
    (format t "   height : ~a~%" height)
    (format t "    color : ~a~%" color)
    (graphene:with-rect (bounds 0 0 width height)
      (gtk:snapshot-append-color snapshot color bounds))))

;; static void
;; demo_child_measure (GtkWidget        *widget,
;;                     GtkOrientation    orientation,
;;                     int               for_size,
;;                     int              *minimum,
;;                     int              *natural,
;;                     int              *minimum_baseline,
;;                     int              *natural_baseline)
;; {
;;   *minimum = *natural = 32;
;; }
(defmethod demo-child-measure-impl ((widget demo-child) orientation for-size
                                    minimum
                                    natural
                                    minimum-baseline
                                    natural-baseline)
  (declare (ignore minimum-baseline natural-baseline))
  (format t "in DEMO-CHILD-MEASURE-IMPL~%")
;  (setf (cffi:mem-ref minimum :int) 32)
;  (setf (cffi:mem-ref natural :int) 32)
  )

;; GtkWidget *
;; demo_child_new (const char *color)
;; {
;;   DemoChild *self;

;;   self = g_object_new (DEMO_TYPE_CHILD,
;;                        "tooltip-text", color,
;;                        NULL);

;;   gdk_rgba_parse (&self->color, color);

;;   return GTK_WIDGET (self);
;; }
(defun demo-child-new (color)
  (make-instance 'demo-child
                 :tooltip-text color
                 :color (gdk:rgba-parse color)))

;;; ----------------------------------------------------------------------------

#|
GtkWidget *
do_layoutmanager (GtkWidget *parent)
{
  static GtkWidget *window = NULL;

  if (!window)
    {
      GtkWidget *widget;
      GtkWidget *child;
      const char *color[] = {
        "red", "orange", "yellow", "green",
        "blue", "grey", "magenta", "lime",
        "yellow", "firebrick", "aqua", "purple",
        "tomato", "pink", "thistle", "maroon"
      };
      int i;

      window = gtk_window_new ();
      gtk_window_set_title (GTK_WINDOW (window), "Layout Manager — Transition");
      gtk_window_set_default_size (GTK_WINDOW (window), 600, 600);
      g_object_add_weak_pointer (G_OBJECT (window), (gpointer *)&window);

      widget = demo_widget_new ();

      for (i = 0; i < 16; i++)
        {
          child = demo_child_new (color[i]);
          gtk_widget_set_margin_start (child, 4);
          gtk_widget_set_margin_end (child, 4);
          gtk_widget_set_margin_top (child, 4);
          gtk_widget_set_margin_bottom (child, 4);
          demo_widget_add_child (DEMO_WIDGET (widget), child);
        }

      gtk_window_set_child (GTK_WINDOW (window), widget);
    }

  if (!gtk_widget_get_visible (window))
    gtk_widget_set_visible (window, TRUE);
  else
    gtk_window_destroy (GTK_WINDOW (window));

  return window;

}
|#

;;; ----------------------------------------------------------------------------

#|



struct _DemoWidgetClass
{
  GtkWidgetClass parent_class;
};

G_DEFINE_TYPE (DemoWidget, demo_widget, GTK_TYPE_WIDGET)

/* The widget is controlling the transition by calling
 * demo_layout_set_position() in a tick callback.
 *
 * We take half a second to go from one layout to the other.
 */

#define DURATION (0.5 * G_TIME_SPAN_SECOND)

static gboolean
transition (GtkWidget     *widget,
            GdkFrameClock *frame_clock,
            gpointer       data)
{
  DemoWidget *self = DEMO_WIDGET (widget);
  DemoLayout *demo_layout = DEMO_LAYOUT (gtk_widget_get_layout_manager (widget));
  gint64 now = gdk_frame_clock_get_frame_time (frame_clock);

  gtk_widget_queue_allocate (widget);

  if (self->backward)
    demo_layout_set_position (demo_layout, 1.0 - (now - self->start_time) / DURATION);
  else
    demo_layout_set_position (demo_layout, (now - self->start_time) / DURATION);

  if (now - self->start_time >= DURATION)
    {
      self->backward = !self->backward;
      demo_layout_set_position (demo_layout, self->backward ? 1.0 : 0.0);
      /* keep things interesting by shuffling the positions */
      if (!self->backward)
        demo_layout_shuffle (demo_layout);
      self->tick_id = 0;

      return G_SOURCE_REMOVE;
    }

  return G_SOURCE_CONTINUE;
}

static void
clicked (GtkGestureClick *gesture,
         guint            n_press,
         double           x,
         double           y,
         gpointer         data)
{
  DemoWidget *self = data;
  GdkFrameClock *frame_clock;

  if (self->tick_id != 0)
    return;

  frame_clock = gtk_widget_get_frame_clock (GTK_WIDGET (self));
  self->start_time = gdk_frame_clock_get_frame_time (frame_clock);
  self->tick_id = gtk_widget_add_tick_callback (GTK_WIDGET (self), transition, NULL, NULL);
}

static void
demo_widget_init (DemoWidget *self)
{
  GtkGesture *gesture;

  gesture = gtk_gesture_click_new ();
  g_signal_connect (gesture, "pressed", G_CALLBACK (clicked), self);
  gtk_widget_add_controller (GTK_WIDGET (self), GTK_EVENT_CONTROLLER (gesture));
}

static void
demo_widget_dispose (GObject *object)
{
  GtkWidget *child;

  while ((child = gtk_widget_get_first_child (GTK_WIDGET (object))))
    gtk_widget_unparent (child);

  G_OBJECT_CLASS (demo_widget_parent_class)->dispose (object);
}

static void
demo_widget_class_init (DemoWidgetClass *class)
{
  GObjectClass *object_class = G_OBJECT_CLASS (class);
  GtkWidgetClass *widget_class = GTK_WIDGET_CLASS (class);

  object_class->dispose = demo_widget_dispose;

  /* here is where we use our custom layout manager */
  gtk_widget_class_set_layout_manager_type (widget_class, DEMO_TYPE_LAYOUT);
}
|#

;; struct _DemoWidget
;; {
;;   GtkWidget parent_instance;
;;
;;   gboolean backward; /* whether we go 0 -> 1 or 1 -> 0 */
;;   gint64 start_time; /* time the transition started */
;;   guint tick_id;     /* our tick cb */
;; };
(gobject:define-gobject-subclass "DemoWidget" demo-widget
  (:superclass gtk:widget
   :export t
   :interfaces ())
  ((backward                       ; whether we go 0 -> 1 or 1 ->0
    demo-widget-backward
    "backward" "gboolean" t t)
   (start                          ; time the transition started
    demo-widget-start
    "start" "guint64" t t)
   (tickid                         ; our tick callback
    demo-widget-tickid
    "tickid" "guint" t t)))

;; GtkWidget *
;; demo_widget_new (void)
;; {
;;  return g_object_new (DEMO_TYPE_WIDGET, NULL);
;; }
(defun demo-widget-new ()
  (make-instance 'demo-widget))

;; void
;; demo_widget_add_child (DemoWidget *self,
;;                        GtkWidget  *child)
;; {
;;   gtk_widget_set_parent (child, GTK_WIDGET (self));
;; }
(defun demo-widget-add-child (widget child)
  (gtk:widget-set-parent child widget))

;;; ----------------------------------------------------------------------------

(defun do-layout-manager-transition (&optional application)
  (let*  ((colors (list "red" "orange" "yellow" "green"
                        "blue" "grey" "magenta" "lime"
                        "yellow" "firebrick" "aqua" "purple"
                        "tomato" "pink" "thistle" "maroon"))
          (widget (demo-widget-new))
          (window (make-instance 'gtk:window
                                 :application application
                                 :title "Layout Manger Transitions"
                                 :child widget
                                 :default-width 600
                                 :default-height 600)))
    (gobject::install-vtable "DemoChild")
    (dolist (color colors)
      (demo-widget-add-child widget
                             (make-instance 'demo-child
                                            :tooltip-text color
                                            :color (gdk:rgba-parse color)
                                            :margin-start 4
                                            :margin-end 4
                                            :margin-top 4
                                            :margin-bottom 4)))
    (gtk:window-present window)))

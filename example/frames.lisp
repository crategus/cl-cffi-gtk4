
(in-package :gtk4-example)

#|

#define TIME_SPAN (3.0 * G_TIME_SPAN_SECOND)

static gboolean
change_color (GtkWidget     *widget,
              GdkFrameClock *frame_clock,
              gpointer       data)
{
  ColorWidget *color = (ColorWidget *)widget;
  gint64 time;

  time = gdk_frame_clock_get_frame_time (frame_clock);

  if (time >= color->time2)
    {
      color->time2 = time + TIME_SPAN;

      color->color1 = color->color2;
      color->color2.red = g_random_double_range (0, 1);
      color->color2.green = g_random_double_range (0, 1);
      color->color2.blue = g_random_double_range (0, 1);
      color->color2.alpha = 1;
    }

  color->t = 1 - (color->time2 - time) / TIME_SPAN;

  gtk_widget_queue_draw (widget);

  return G_SOURCE_CONTINUE;
}

static void
color_widget_snapshot (GtkWidget   *widget,
                       GtkSnapshot *snapshot)
{
  ColorWidget *color = (ColorWidget *)widget;
  float w, h;
  GdkRGBA c;

  w = gtk_widget_get_width (widget);
  h = gtk_widget_get_height (widget);

  c.red = (1 - color->t) * color->color1.red + color->t * color->color2.red;
  c.green = (1 - color->t) * color->color1.green + color->t * color->color2.green;
  c.blue = (1 - color->t) * color->color1.blue + color->t * color->color2.blue;
  c.alpha = 1;

  gtk_snapshot_append_color (snapshot, &c, &GRAPHENE_RECT_INIT (0, 0, w, h));
}

static void
color_widget_init (ColorWidget *color)
{
  gtk_widget_add_tick_callback (GTK_WIDGET (color), change_color, NULL, NULL);
  gtk_widget_set_hexpand (GTK_WIDGET (color), TRUE);
  gtk_widget_set_vexpand (GTK_WIDGET (color), TRUE);
}

static void
color_widget_class_init (ColorWidgetClass *class)
{
  GtkWidgetClass *widget_class = GTK_WIDGET_CLASS (class);

  widget_class->snapshot = color_widget_snapshot;
}

GtkWidget *
color_widget_new (void)
{
  return g_object_new (color_widget_get_type (), NULL);
}

static gboolean
update_fps_label (gpointer data)
{
  GtkWidget *label = GTK_WIDGET (data);
  GdkFrameClock *frame_clock;

  frame_clock = gtk_widget_get_frame_clock (label);

  if (frame_clock)
    {
      char *fps;

      fps = g_strdup_printf ("%.2f fps", gdk_frame_clock_get_fps (frame_clock));
      gtk_label_set_label (GTK_LABEL (label), fps);
      g_free (fps);
    }
  else
    gtk_label_set_label (GTK_LABEL (label), "");

  return G_SOURCE_CONTINUE;
}

static void
remove_id (gpointer data)
{
  guint id = GPOINTER_TO_UINT (data);

  g_source_remove (id);
}

GtkWidget *
do_frames (GtkWidget *do_widget)
{
  static GtkWidget *window = NULL;

  if (!window)
    {
      GtkBuilder *builder;
      GtkWidget *box;
      GtkWidget *label;
      guint id;

      builder = gtk_builder_new_from_resource ("/frames/frames.ui");
      window = GTK_WIDGET (gtk_builder_get_object (builder, "window"));
      g_object_add_weak_pointer (G_OBJECT (window), (gpointer *)&window);
      gtk_window_set_display (GTK_WINDOW (window),
                              gtk_widget_get_display (do_widget));

      label = GTK_WIDGET (gtk_builder_get_object (builder, "fps"));
      box = GTK_WIDGET (gtk_builder_get_object (builder, "box"));

      gtk_box_append (GTK_BOX (box), color_widget_new ());

      id = g_timeout_add (500, update_fps_label, label);
      g_object_set_data_full (G_OBJECT (label), "tick_cb",
                              GUINT_TO_POINTER (id), remove_id);
    }

  if (!gtk_widget_get_visible (window))
    gtk_widget_set_visible (window, TRUE);
  else
    gtk_window_destroy (GTK_WINDOW (window));

  return window;
}
|#

#|
typedef struct
{
  GtkWidget parent_instance;

  GdkRGBA color1;
  GdkRGBA color2;
  guint64 time2;
  float t;

  guint tick_cb;
} ColorWidget;

typedef struct
{
  GtkWidgetClass parent_class;
} ColorWidgetClass;

G_DEFINE_TYPE (ColorWidget, color_widget, GTK_TYPE_WIDGET)
|#

(gobject:define-gobject-subclass "ColorWidget" color-widget
  (:superclass gtk:widget
   :export t
   :interfaces ())
  ((color1
    color-widget-color1
    "color1" "GdkRGBA" t t)
   (color1
    color-widget-color2
    "color2" "GdkRGBA" t t)
   (time2
    color-widget-time2
    "time2" "guint64" t t)
   (time1
    color-widget-time1
    "time1" "gfloat" t t)
   (tick-cb
    color-widget-tick-cb
    "tick-cb" "guint" t t)))


(in-package :gtk4-example)

;; See https://www.embeddeduse.com/2019/08/26/qt-compare-two-floats/
(defun approx-equal (x y &optional (eps 1.0d-6))
  (or (< (abs (- x y)) eps)
      (< (abs (- x y)) (* eps (max (abs x) (abs y))))))

#|
enum
{
  PROP_TEXTURE = 1,
  PROP_FILTER,
  PROP_SCALE,
  PROP_ANGLE,
};
|#

#|
struct _ImageView
{
  GtkWidget parent_instance;

  GdkTexture *texture;
  float scale;
  float angle;
  GskScalingFilter filter;

  GtkWidget *menu;
  float start_scale;
  float start_angle;
};
|#
(gobject:define-gobject-subclass "ImageView" image-view
  (:superclass gtk:widget
   :export t
   :interfaces ())
  ((texture
    image-view-texture
    "texture" "GdkTexture" t t)
   (scale
    image-view-scale
    "scale" "gfloat" t t)
   (angle
    image-view-angle
    "angle" "gfloat" t t)
   (filter
    image-view-filter
    "filter" "GskScalingFilter" t t)
   (menu
    image-view-menu
    "menu" "GtkWidget" t t)
   (start-scale
    image-view-start-scale
    "start-scale" "gfloat" t t)
   (start-angle
    image-view-start-angle
    "start-angle" "gfloat" t t)))

(defmethod gobject:object-class-init :after
           ((subclass (eql (find-class 'image-view))) class data)
  (format t "in GOBJECT:OBJECT-CLASS-INIT for ~a~%" class))

(defmethod gobject:object-instance-init :after
           ((subclass (eql (find-class 'image-view))) instance data)
  (format t "in GOBJECT:OBJECT-INSTANCE-INIT for ~a~%" instance)

  (g:signal-connect instance "notify::scale"
                    (lambda (object pspec)
                      (declare (ignore pspec))
                      (format t "in NOTIFY::SCALE~%")
                      (gtk:widget-queue-resize object)))
  (g:signal-connect instance "notify::angle"
                    (lambda (object pspec)
                      (declare (ignore pspec))
                      (format t "in NOTIFY::ANGLE~%")
                      (gtk:widget-queue-resize object)))
  (g:signal-connect instance "notify::filter"
                    (lambda (object pspec)
                      (declare (ignore pspec))
                      (format t "in NOTIFY::FIlTER~%")
                      (gtk:widget-queue-resize object))))


(gobject:define-vtable ("ImageView" image-view)
  ;; Skip over vtable for GObject class
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
  ;; Install new virtual size-allocate method
  (size-allocate (:void (widget (g:object widget))
                        (width :int)
                        (height :int)
                        (baseline :int)))
  (:skip state-flags-changed :pointer)
  (:skip direction-changed :pointer)
  (:skip get-request-mode :pointer)
  ;; Install new virtual measure method
  (measure (:void (widget (g:object widget))
                  (orientation gtk:orientation)
                  (for-size :int)
                  (minimum :pointer)
                  (natural :pointer)
                  (minimum-baseline :pointer)
                  (natural-baseline :pointer)))
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
  ;; Install new virtual snapshot method
  (snapshot (:void (widget (g:object canvas-item))
                   (snapshot (g:object gtk:snapshot))))
  (:skip contains :pointer))

;; static void
;; image_view_size_allocate (GtkWidget *widget,
;;                           int        width,
;;                           int        height,
;;                           int        baseline)
;; {
;;   ImageView *self = IMAGE_VIEW (widget);
;;   gtk_popover_present (GTK_POPOVER (self->menu));
;; }
(defmethod image-view-size-allocate-impl
           ((widget image-view) width height baseline)
  (declare (ignore width height baseline))
  (format t "in IMAGE-VIEW-SIZE-ALLOCATE-IMPL for ~a~&" widget)
  ;; Since we are not using a layout manager (who would do this for us), we
  ;; need to allocate a size for our menu by calling gtk:popover-present.
  (when (image-view-menu widget)
    (gtk:popover-present (image-view-menu widget))))

;; static void
;; image_view_measure (GtkWidget      *widget,
;;                     GtkOrientation  orientation,
;;                     int             for_size,
;;                     int            *minimum,
;;                     int            *natural,
;;                     int            *minimum_baseline,
;;                     int            *natural_baseline)
;; {
;;   ImageView *self = IMAGE_VIEW (widget);
;;   int width, height;
;;   int size;
;;
;;   width = gdk_texture_get_width (self->texture);
;;   height = gdk_texture_get_height (self->texture);
;;
;;   if (G_APPROX_VALUE (self->angle, 90.f, FLT_EPSILON) ||
;;       G_APPROX_VALUE (self->angle, 270.f, FLT_EPSILON))
;;     {
;;       int s = width;
;;       width = height;
;;       height = s;
;;     }
;;
;;   if (orientation == GTK_ORIENTATION_HORIZONTAL)
;;     size = width;
;;   else
;;     size = height;
;;
;;   *minimum = *natural = (int) ceil (self->scale * size);
;; }
(defmethod image-view-measure-impl
           ((widget image-view)
            orientation for-size minimum natural min-baseline nat-baseline)
  (declare (ignore for-size min-baseline nat-baseline))
  (format t "in IMAGE-VIEW-MEASURE-IMPL for ~a~%" widget)
  (let ((width (gdk:texture-width (image-view-texture widget)))
        (height (gdk:texture-height (image-view-texture widget)))
        size)
    (when (or (approx-equal (image-view-angle widget) 90.0)
              (approx-equal (image-view-angle widget) 270.0))
      (rotatef width height))
    (if (eq :horizontel orientation)
        (setf size width)
        (setf size height))

    (format t "   size : ~a~%" (ceiling (* size (image-view-scale widget))))

    (setf (cffi:mem-ref minimum :int)
          (ceiling (* size (image-view-scale widget))))
    (setf (cffi:mem-ref natural :int)
          (ceiling (* size (image-view-scale widget))))))

;; static void
;; image_view_snapshot (GtkWidget   *widget,
;;                      GtkSnapshot *snapshot)
;; {
;;   ImageView *self = IMAGE_VIEW (widget);
;;   int x, y, width, height;
;;   double w, h, w2, h2;
;;
;;   width = gtk_widget_get_width (widget);
;;   height = gtk_widget_get_height (widget);
;;
;;   w2 = w = self->scale * gdk_texture_get_width (self->texture);
;;   h2 = h = self->scale * gdk_texture_get_height (self->texture);
;;
;;   if (G_APPROX_VALUE (self->angle, 90.f, FLT_EPSILON) ||
;;       G_APPROX_VALUE (self->angle, 270.f, FLT_EPSILON))
;;     {
;;       double s = w2;
;;       w2 = h2;
;;       h2 = s;
;;     }
;;
;;   x = (width - ceil (w2)) / 2;
;;   y = (height - ceil (h2)) / 2;
;;
;;   gtk_snapshot_push_clip (snapshot, &GRAPHENE_RECT_INIT (0, 0, width, height));
;;   gtk_snapshot_save (snapshot);

;;   gtk_snapshot_translate (snapshot, &GRAPHENE_POINT_INIT (x, y));
;;   gtk_snapshot_translate (snapshot, &GRAPHENE_POINT_INIT (w2 / 2, h2 / 2));
;;   gtk_snapshot_rotate (snapshot, self->angle);
;;   gtk_snapshot_translate (snapshot, &GRAPHENE_POINT_INIT (- w / 2, - h / 2));

;;   gtk_snapshot_append_scaled_texture (snapshot,
;;                                       self->texture,
;;                                       self->filter,
;;                                       &GRAPHENE_RECT_INIT (0, 0, w, h));
;;   gtk_snapshot_restore (snapshot);
;;   gtk_snapshot_pop (snapshot);
;; }
(defmethod image-view-snapshot-impl ((widget image-view) snapshot)
  (format t "in IMAGE-VIEW-SNAPSHOT-IMPL for ~a~%" widget)
  (let* ((width (gtk:widget-width widget))
         (height (gtk:widget-height widget))
         (scale (image-view-scale widget))
         (texture (image-view-texture widget))
         (w (* scale (gdk:texture-width texture)))
         (h (* scale (gdk:texture-height texture)))
         (w2 w)
         (h2 h))
    (when (or (approx-equal (image-view-angle widget) 90.0)
              (approx-equal (image-view-angle widget) 270.0))
      (rotatef w2 h2))
    (let ((x (/ (- width (ceiling w2)) 2))
          (y (/ (- height (ceiling h2)) 2)))
      (graphene:with-rect (bounds 0 0 width height)
        (gtk:snapshot-push-clip snapshot bounds)
        (gtk:snapshot-save snapshot)
        (graphene:with-point (point x y)
          (gtk:snapshot-translate snapshot point)
          (gtk:snapshot-translate snapshot
                                  (graphene:point-init point (/ w2 2) (/ h2 2)))
          (gtk:snapshot-rotate snapshot (image-view-angle widget))
          (gtk:snapshot-translate snapshot
                                  (graphene:point-init point (- (/ w 2))
                                                             (- (/ h 2))))
          (gtk:snapshot-append-scaled-texture
                  snapshot texture
                  (image-view-filter widget)
                  (graphene:rect-init bounds 0 0 w h)))
        (gtk:snapshot-restore snapshot)
        (gtk:snapshot-pop snapshot)))))

#|
static gboolean
query_tooltip (GtkWidget  *widget,
               int         x,
               int         y,
               gboolean    keyboard_mode,
               GtkTooltip *tooltip,
               gpointer    data)
{
  ImageView *self = IMAGE_VIEW (widget);
  GtkWidget *grid;
  GtkWidget *label;
  char *s, *s2;
  const char *filter[] = { "Linear", "Nearest", "Trilinear" };
  int precision, l;

  grid = gtk_grid_new ();
  gtk_grid_set_column_spacing (GTK_GRID (grid), 12);
  gtk_grid_set_row_spacing (GTK_GRID (grid), 6);
  label = gtk_label_new ("Texture");
  gtk_label_set_xalign (GTK_LABEL (label), 0);
  gtk_grid_attach (GTK_GRID (grid), label, 0, 0, 1, 1);
  s = g_strdup_printf ("%d\342\200\206\303\227\342\200\206%d",
                       gdk_texture_get_width (self->texture),
                       gdk_texture_get_height (self->texture));
  label = gtk_label_new (s);
  g_free (s);
  gtk_label_set_xalign (GTK_LABEL (label), 1);
  gtk_grid_attach (GTK_GRID (grid), label, 1, 0, 1, 1);

  label = gtk_label_new ("Rotation");
  gtk_label_set_xalign (GTK_LABEL (label), 0);
  gtk_grid_attach (GTK_GRID (grid), label, 0, 1, 1, 1);
  s = g_strdup_printf ("%.1f", self->angle);
  if (g_str_has_suffix (s, ".0"))
    s[strlen (s) - 2] = '\0';
  s2 = g_strconcat (s, "\302\260", NULL);
  label = gtk_label_new (s2);
  g_free (s2);
  g_free (s);
  gtk_label_set_xalign (GTK_LABEL (label), 1);
  gtk_grid_attach (GTK_GRID (grid), label, 1, 1, 1, 1);

  label = gtk_label_new ("Scale");
  gtk_label_set_xalign (GTK_LABEL (label), 0);
  gtk_grid_attach (GTK_GRID (grid), label, 0, 2, 1, 1);

  precision = 1;
  s = NULL;
  do {
    g_free (s);
    s = g_strdup_printf ("%.*f", precision, self->scale);
    l = strlen (s) - 1;
    while (s[l] == '0')
      l--;
    if (s[l] == '.')
      s[l] = '\0';
    precision++;
  } while (strcmp (s, "0") == 0);

  label = gtk_label_new (s);
  g_free (s);
  gtk_label_set_xalign (GTK_LABEL (label), 1);
  gtk_grid_attach (GTK_GRID (grid), label, 1, 2, 1, 1);

  label = gtk_label_new ("Filter");
  gtk_label_set_xalign (GTK_LABEL (label), 0);
  gtk_grid_attach (GTK_GRID (grid), label, 0, 3, 1, 1);
  label = gtk_label_new (filter[self->filter]);
  gtk_label_set_xalign (GTK_LABEL (label), 1);
  gtk_grid_attach (GTK_GRID (grid), label, 1, 3, 1, 1);

  gtk_tooltip_set_custom (tooltip, grid);

  return TRUE;
}

static void
image_view_init (ImageView *self)
{
  self->scale = 1.f;
  self->angle = 0.f;
  self->filter = GSK_SCALING_FILTER_LINEAR;
  gtk_widget_init_template (GTK_WIDGET (self));
}

static void
image_view_dispose (GObject *object)
{
  ImageView *self = IMAGE_VIEW (object);

  g_clear_object (&self->texture);

  gtk_widget_dispose_template (GTK_WIDGET (self), IMAGE_TYPE_VIEW);

  G_OBJECT_CLASS (image_view_parent_class)->dispose (object);
}


static void update_actions (ImageView *self);

static void
image_view_set_property (GObject      *object,
                         guint         prop_id,
                         const GValue *value,
                         GParamSpec   *pspec)
{
  ImageView *self = IMAGE_VIEW (object);

  switch (prop_id)
    {
    case PROP_TEXTURE:
      g_clear_object (&self->texture);
      self->texture = g_value_dup_object (value);
      self->scale = 1.f;
      self->angle = 0.f;
      self->filter = GSK_SCALING_FILTER_LINEAR;
      update_actions (self);
      gtk_widget_queue_resize (GTK_WIDGET (object));
      g_object_notify (object, "scale");
      g_object_notify (object, "angle");
      g_object_notify (object, "filter");
      break;

    case PROP_SCALE:
      self->scale = g_value_get_float (value);
      update_actions (self);
      gtk_widget_queue_resize (GTK_WIDGET (object));
      break;

    case PROP_ANGLE:
      self->angle = fmodf (g_value_get_float (value), 360.f);
      gtk_widget_queue_resize (GTK_WIDGET (object));
      break;

    case PROP_FILTER:
      self->filter = g_value_get_enum (value);
      gtk_widget_queue_resize (GTK_WIDGET (object));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void
pressed_cb (GtkGestureClick *gesture,
            guint            n_press,
            double           x,
            double           y,
            ImageView     *self)
{
  /* We are placing our menu at the point where
   * the click happened, before popping it up.
   */
  gtk_popover_set_pointing_to (GTK_POPOVER (self->menu),
                               &(const GdkRectangle){ x, y, 1, 1 });
  gtk_popover_popup (GTK_POPOVER (self->menu));
}

static void
update_actions (ImageView *self)
{
  gtk_widget_action_set_enabled (GTK_WIDGET (self), "zoom.in", self->scale < 1024.);
  gtk_widget_action_set_enabled (GTK_WIDGET (self), "zoom.out", self->scale > 1./1024.);
  gtk_widget_action_set_enabled (GTK_WIDGET (self), "zoom.reset", self->scale != 1.);
}

static void
zoom_cb (GtkWidget  *widget,
         const char *action_name,
         GVariant   *parameter)
{
  ImageView *self = IMAGE_VIEW (widget);
  float scale;

  if (g_str_equal (action_name, "zoom.in"))
    scale = MIN (1024., self->scale * M_SQRT2);
  else if (g_str_equal (action_name, "zoom.out"))
    scale = MAX (1./1024., self->scale / M_SQRT2);
  else if (g_str_equal (action_name, "zoom.reset"))
    scale = 1.0;
  else
    g_assert_not_reached ();

  g_object_set (widget, "scale", scale, NULL);
}

static void
rotate_cb (GtkWidget  *widget,
           const char *action_name,
           GVariant   *parameter)
{
  ImageView *self = IMAGE_VIEW (widget);
  int angle;

  g_variant_get (parameter, "i", &angle);

  g_object_set (widget, "angle", fmodf (self->angle + angle, 360.f), NULL);
}

static void
scale_begin_cb (GtkGesture       *gesture,
                GdkEventSequence *sequence,
            ImageView        *self)
{
  self->start_scale = self->scale;
}

static void
scale_changed_cb (GtkGestureZoom *gesture,
          double          scale,
          ImageView      *self)
{
  g_object_set (self, "scale", self->start_scale*scale, NULL);
}

#define RAD_TO_DEG(a) (180.0 * (a) / M_PI)

static void
rotate_begin_cb (GtkGesture       *gesture,
             GdkEventSequence *sequence,
             ImageView        *self)
{
  self->start_angle = self->angle;
}

static void
angle_changed_cb (GtkGestureRotate *gesture,
          double            angle,
          double            delta,
          ImageView        *self)
{
  double a;

  a = 90 * round (RAD_TO_DEG (delta) / 10);

  g_object_set (self, "angle", fmodf (self->start_angle + a, 360.f), NULL);
}

static void
image_view_class_init (ImageViewClass *class)
{
  GObjectClass *object_class = G_OBJECT_CLASS (class);
  GtkWidgetClass *widget_class = GTK_WIDGET_CLASS (class);

  object_class->dispose = image_view_dispose;
  object_class->set_property = image_view_set_property;
  object_class->get_property = image_view_get_property;

  widget_class->snapshot = image_view_snapshot;
  widget_class->measure = image_view_measure;
  widget_class->size_allocate = image_view_size_allocate;

  g_object_class_install_property (object_class, PROP_TEXTURE,
      g_param_spec_object ("texture", NULL, NULL,
                           GDK_TYPE_TEXTURE,
                           G_PARAM_READWRITE));

  g_object_class_install_property (object_class, PROP_SCALE,
      g_param_spec_float ("scale", NULL, NULL,
                          1./1024., 1024., 1.0,
                          G_PARAM_READWRITE));

  g_object_class_install_property (object_class, PROP_ANGLE,
      g_param_spec_float ("angle", NULL, NULL,
                          0.0, 360.0, 0.0,
                          G_PARAM_READWRITE));

  g_object_class_install_property (object_class, PROP_FILTER,
      g_param_spec_enum ("filter", NULL, NULL,
                         GSK_TYPE_SCALING_FILTER, GSK_SCALING_FILTER_LINEAR,
                         G_PARAM_READWRITE));

  /* These are the actions that we are using in the menu */
  gtk_widget_class_install_action (widget_class, "zoom.in", NULL, zoom_cb);
  gtk_widget_class_install_action (widget_class, "zoom.out", NULL, zoom_cb);
  gtk_widget_class_install_action (widget_class, "zoom.reset", NULL, zoom_cb);
  gtk_widget_class_install_action (widget_class, "rotate", "i", rotate_cb);

  gtk_widget_class_set_template_from_resource (widget_class, "/menu/imageview.ui");
  gtk_widget_class_bind_template_child (widget_class, ImageView, menu);
  gtk_widget_class_bind_template_callback (widget_class, pressed_cb);
  gtk_widget_class_bind_template_callback (widget_class, scale_changed_cb);
  gtk_widget_class_bind_template_callback (widget_class, scale_begin_cb);
  gtk_widget_class_bind_template_callback (widget_class, angle_changed_cb);
  gtk_widget_class_bind_template_callback (widget_class, rotate_begin_cb);
  gtk_widget_class_bind_template_callback (widget_class, query_tooltip);

  gtk_widget_class_set_accessible_role (widget_class, GTK_ACCESSIBLE_ROLE_IMG);
}
|#

#|
GtkWidget *
image_view_new (const char *resource)
{
  ImageView *self;
  GdkTexture *texture;

  texture = gdk_texture_new_from_resource (resource);

  self = g_object_new (IMAGE_TYPE_VIEW,
                       "texture", texture,
                       NULL);

  g_object_unref (texture);

  return GTK_WIDGET (self);
}
|#
(defun image-view-new-from-filename (filename)
  (let ((texture (gdk:texture-new-from-filename filename)))
    (make-instance 'image-view
                   :filter :linear
                   :texture texture
                   :scale 1.0
                   :start-scale 1.0
                   :angle 0.0
                   :start-angle 0.0)))

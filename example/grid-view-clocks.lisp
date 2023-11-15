;;;; Clocks - 2023-9-22
;;;;
;;;; This demo displays the time in different timezones.
;;;;
;;;; The goal is to show how to set up expressions that track changes in objects
;;;; and make them update widgets. For that, we create a clock object that
;;;; updates its time every second and then use various ways to display that
;;;; time.
;;;;
;;;; Typically, this will be done using GtkBuilder .ui files with the help of
;;;; the <binding> tag, but this demo shows the code that runs behind that.

(in-package :gtk)

;; This is our object. It's just a timezone
;;
;; Finally, we define the type. The important part is adding the
;; paintable interface, so GTK knows that this object can indeed
;; be drawn.
;;
;; Initialize the paintable interface. This way we turn our clocks
;; into objects that can be drawn. There are more functions to this
;; interface to define desired size, but this is enough.
#+nil
(defclass clock (gdk:paintable)
  (;; Initialize with the UTC timezone
   (timezone :initform local-time:+utc-zone+
             :accessor clock-timezone)
   ;; Initialize with the name of the UTC timezone
   (location :initform "UTC"
             :accessor clock-location))
  (:gname . "GtkClock")
  (:metaclass gobject:gobject-class))

(gobject:define-g-object-subclass "GtkClock" clock
  (:superclass g:object
   :export t
   :interfaces ("GdkPaintable"))
  ((timezone
    clock-timezone
    "timezone" "guint" t t)
   (location
    clock-location
    "location" "gchararray" t t)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'clock)
  (export 'clock-location)
  (export 'clock-timezone))

#+nil
(gobject:register-object-type-implementation "GtkClock"        ; name
                                             clock             ; class
                                             "GObject"         ; parent
                                             ("GdkPaintable")  ; interfaces
                                             nil)              ; properties

#+nil
(defmethod initialize-instance :after ((obj clock) &rest initargs)
  ;; Set the slot values from initargs
  (iter (for (slot value) on initargs by #'cddr)
        (cond ((eq slot :timezone)
               (setf (clock-timezone obj) value))
              ((eq slot :location)
               (setf (clock-location obj) value)))))

(defmethod gdk:paintable-snapshot-impl ((paintable clock) snapshot width height)
  (let ((black (gdk:rgba-new :red 0 :green 0 :blue 0 :alpha 1)))

    ;; save/restore is necessary so we can undo the transforms we start out with
    (gtk:snapshot-save snapshot)

    ;; First, we move the (0, 0) point to the center of the area so
    ;; we can draw everything relative to it.
    (graphene:with-graphene-point (point (/ width 2.0) (/ height 2))
      (gtk:snapshot-translate snapshot point))

    ;; Next we scale it, so that we can pretend that the clock is
    ;; 100px in size. That way, we don't need to do any complicated
    ;; math later. We use MIN() here so that we use the smaller
    ;; dimension for sizing. That way we don't overdraw but keep
    ;; the aspect ratio.
    (gtk:snapshot-scale snapshot (/ (min width height) 100.0)
                                 (/ (min width height) 100.0))

    ;; Now we have a circle with diameter 100px (and radius 50px) that
    ;; has its (0, 0) point at the center. Let's draw a simple clock into it.

    ;; First, draw a circle. This is a neat little trick to draw a circle
    ;; without requiring Cairo.
    ;; Improve the implementation of GskRoundedRect to avoid foreign objects
    (cffi:with-foreign-object (outline '(:struct gsk:rounded-rect))
      (graphene:with-graphene-rect (rect -50 -50 100 100)
        (gsk:rounded-rect-init-from-rect outline rect 50)
        (gtk:snapshot-append-border snapshot
                                    outline
                                    '(4 4 4 4)
                                    (list black black black black))))

    ;; Next, draw the hour hand.
    ;; We do this using transforms again: Instead of computing where the angle
    ;; points to, we just rotate everything and then draw the hand as if it
    ;; was :00. We don't even need to care about am/pm here because rotations
    ;; just work.
    (let* ((zone (gtk:clock-timezone paintable))
           ;; Create a timestamp with the actuell time
           (time (local-time:now))
           (hour (local-time:timestamp-hour time :timezone zone))
           (minute (local-time:timestamp-minute time :timezone zone))
           (second (local-time:timestamp-second time :timezone zone)))
      (gtk:snapshot-save snapshot)
      (gtk:snapshot-rotate snapshot (+ (* 30 hour) (* 0.5 minute)))
      (cffi:with-foreign-object (outline '(:struct gsk:rounded-rect))
        (graphene:with-graphene-rect (rect -2 -23 4 25)
          (gsk:rounded-rect-init-from-rect outline rect 2.0)
          (gtk:snapshot-push-rounded-clip snapshot outline)
          (gtk:snapshot-append-color snapshot
                                     black
                                     (gsk:rounded-rect-bounds outline))
          (gtk:snapshot-pop snapshot)
          (gtk:snapshot-restore snapshot)

          ;; And the same as above for the minute hand. Just make this one
          ;;; longer so people can tell the hands apart.
          (gtk:snapshot-save snapshot)
          (gtk:snapshot-rotate snapshot (* 6 minute))
          (graphene:rect-init rect -2 -43 4 45)
          (gsk:rounded-rect-init-from-rect outline rect 2)
          (gtk:snapshot-push-rounded-clip snapshot outline)
          (gtk:snapshot-append-color snapshot
                                     black
                                     (gsk:rounded-rect-bounds outline))
          (gtk:snapshot-pop snapshot)
          (gtk:snapshot-restore snapshot)

          ;; and finally, the second indicator.
          (gtk:snapshot-save snapshot)
          (gtk:snapshot-rotate snapshot (* 6 second))
          (graphene:rect-init rect -2 -43 4 10)
          (gsk:rounded-rect-init-from-rect outline rect 2)
          (gtk:snapshot-push-rounded-clip snapshot outline)
          (gtk:snapshot-append-color snapshot
                                     black
                                     (gsk:rounded-rect-bounds outline))
          (gtk:snapshot-pop snapshot)
          (gtk:snapshot-restore snapshot))))

  ;; And finally, don't forget to restore the initial save() that
  ;; we did for the initial transformations.
  (gtk:snapshot-restore snapshot)))

;; Our desired size is 100px. That sounds okay for an analog clock
(defmethod gdk:paintable-get-intrinsic-width-impl ((paintable clock))
  100)
(defmethod gdk:paintable-get-intrinsic-height-impl ((paintable clock))
  100)

#|

/* This is the list of all the ticking clocks */
static GSList *ticking_clocks = NULL;

/* This is the ID of the timeout source that is updating all
 * ticking clocks.
 */
static guint ticking_clock_id = 0;

/* Every second, this function is called to tell everybody that
 * the clocks are ticking.
 */
static gboolean
gtk_clock_tick (gpointer unused)
{
  GSList *l;

  for (l = ticking_clocks; l; l = l->next)
    {
      GtkClock *clock = l->data;

      /* We will now return a different value for the time property,
       * so notify about that.
       */
      g_object_notify_by_pspec (G_OBJECT (clock), properties[PROP_TIME]);

      /* We will also draw the hands of the clock differently.
       * So notify about that, too.
       */
      gdk_paintable_invalidate_contents (GDK_PAINTABLE (clock));
    }

  return G_SOURCE_CONTINUE;
}

static void
gtk_clock_stop_ticking (GtkClock *self)
{
  ticking_clocks = g_slist_remove (ticking_clocks, self);

  /* If no clock is remaining, stop running the tick updates */
  if (ticking_clocks == NULL && ticking_clock_id != 0)
    g_clear_handle_id (&ticking_clock_id, g_source_remove);
}

static void
gtk_clock_start_ticking (GtkClock *self)
{
  /* if no clock is ticking yet, start */
  if (ticking_clock_id == 0)
    ticking_clock_id = g_timeout_add_seconds (1, gtk_clock_tick, NULL);

  ticking_clocks = g_slist_prepend (ticking_clocks, self);
}

static void
gtk_clock_finalize (GObject *object)
{
  GtkClock *self = GTK_CLOCK (object);

  gtk_clock_stop_ticking (self);

  g_free (self->location);
  g_clear_pointer (&self->timezone, g_time_zone_unref);

  G_OBJECT_CLASS (gtk_clock_parent_class)->finalize (object);
}

static void
gtk_clock_class_init (GtkClockClass *klass)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (klass);

  gobject_class->get_property = gtk_clock_get_property;
  gobject_class->set_property = gtk_clock_set_property;
  gobject_class->finalize = gtk_clock_finalize;

  properties[PROP_LOCATION] =
    g_param_spec_string ("location", NULL, NULL, NULL, G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY);
  properties[PROP_TIME] =
    g_param_spec_boxed ("time", NULL, NULL, G_TYPE_DATE_TIME, G_PARAM_READABLE);
  properties[PROP_TIMEZONE] =
    g_param_spec_boxed ("timezone", NULL, NULL, G_TYPE_TIME_ZONE, G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY);

  g_object_class_install_properties (gobject_class, N_PROPS, properties);
}

static void
gtk_clock_init (GtkClock *self)
{
  gtk_clock_start_ticking (self);
}

static GtkClock *
gtk_clock_new (const char *location,
               GTimeZone  *_tz)
{
  GtkClock *result;

  result = g_object_new (GTK_TYPE_CLOCK,
                         "location", location,
                         "timezone", _tz,
                         NULL);

  g_clear_pointer (&_tz, g_time_zone_unref);

  return result;
}
|#

#|
static GListModel *
create_clocks_model (void)
{
  GListStore *result;
  GtkClock *clock;

  result = g_list_store_new (GTK_TYPE_CLOCK);

  /* local time */
  clock = gtk_clock_new ("local", NULL);
  g_list_store_append (result, clock);
  g_object_unref (clock);
  /* UTC time */
  clock = gtk_clock_new ("UTC", g_time_zone_new_utc ());
  g_list_store_append (result, clock);
  g_object_unref (clock);
  /* A bunch of timezones with GTK hackers */
  clock = gtk_clock_new ("San Francisco", g_time_zone_new ("America/Los_Angeles"));
  g_list_store_append (result, clock);
  g_object_unref (clock);
  clock = gtk_clock_new ("Xalapa", g_time_zone_new ("America/Mexico_City"));
  g_list_store_append (result, clock);
  g_object_unref (clock);
  clock = gtk_clock_new ("Boston", g_time_zone_new ("America/New_York"));
  g_list_store_append (result, clock);
  g_object_unref (clock);
  clock = gtk_clock_new ("London", g_time_zone_new ("Europe/London"));
  g_list_store_append (result, clock);
  g_object_unref (clock);
  clock = gtk_clock_new ("Berlin", g_time_zone_new ("Europe/Berlin"));
  g_list_store_append (result, clock);
  g_object_unref (clock);
  clock = gtk_clock_new ("Moscow", g_time_zone_new ("Europe/Moscow"));
  g_list_store_append (result, clock);
  g_object_unref (clock);
  clock = gtk_clock_new ("New Delhi", g_time_zone_new ("Asia/Kolkata"));
  g_list_store_append (result, clock);
  g_object_unref (clock);
  clock = gtk_clock_new ("Shanghai", g_time_zone_new ("Asia/Shanghai"));
  g_list_store_append (result, clock);
  g_object_unref (clock);

  return G_LIST_MODEL (result);
}
|#

(in-package :gtk4-example)

(defun create-clocks-model ()
  (let ((store (g:list-store-new "GtkClock")))
    store
))


#|
static char *
convert_time_to_string (GObject   *image,
                        GDateTime *time,
                        gpointer   unused)
{
  return g_date_time_format (time, "%x\n%X");
}
|#


#|
/* And this function is the crux for this whole demo.
 * It shows how to use expressions to set up bindings.
 */
static void
setup_listitem_cb (GtkListItemFactory *factory,
                   GtkListItem        *list_item)
{
  GtkWidget *box, *picture, *location_label, *time_label;
  GtkExpression *clock_expression, *expression;

  box = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
  gtk_list_item_set_child (list_item, box);

  /* First, we create an expression that gets us the clock from the listitem:
   * 1. Create an expression that gets the list item.
   * 2. Use that expression's "item" property to get the clock
   */
  expression = gtk_constant_expression_new (GTK_TYPE_LIST_ITEM, list_item);
  clock_expression = gtk_property_expression_new (GTK_TYPE_LIST_ITEM, expression, "item");

  /* Bind the clock's location to a label.
   * This is easy: We just get the "location" property of the clock.
   */
  expression = gtk_property_expression_new (GTK_TYPE_CLOCK,
                                            gtk_expression_ref (clock_expression),
                                            "location");
  /* Now create the label and bind the expression to it. */
  location_label = gtk_label_new (NULL);
  gtk_expression_bind (expression, location_label, "label", location_label);
  gtk_box_append (GTK_BOX (box), location_label);


  /* Here we bind the item itself to a GdkPicture.
   * This is simply done by using the clock expression itself.
   */
  expression = gtk_expression_ref (clock_expression);
  /* Now create the widget and bind the expression to it. */
  picture = gtk_picture_new ();
  gtk_expression_bind (expression, picture, "paintable", picture);
  gtk_box_append (GTK_BOX (box), picture);


  /* And finally, everything comes together.
   * We create a label for displaying the time as text.
   * For that, we need to transform the "GDateTime" of the
   * time property into a string so that the label can display it.
   */
  expression = gtk_property_expression_new (GTK_TYPE_CLOCK,
                                            gtk_expression_ref (clock_expression),
                                            "time");
  expression = gtk_cclosure_expression_new (G_TYPE_STRING,
                                            NULL,
                                            1, (GtkExpression *[1]) { expression },
                                            G_CALLBACK (convert_time_to_string),
                                            NULL, NULL);
  /* Now create the label and bind the expression to it. */
  time_label = gtk_label_new (NULL);
  gtk_expression_bind (expression, time_label, "label", time_label);
  gtk_box_append (GTK_BOX (box), time_label);

  gtk_expression_unref (clock_expression);
}
|#

(defun setup-listitem-cb (factory item)
  (declare (ignore factory item))
)


(defun do-grid-view-clocks (&optional application)
  (let* ((clock (make-instance 'gtk:clock
                               :timezone local-time:+utc-zone+
                               :location "UTC"))
         (image (gtk:image-new-from-paintable clock))
         (vbox (make-instance 'gtk:box :orientation :vertical
                                       :homogeneous t))
         ;; Create the factory that creates the listitems. Because we used
         ;; bindings above during setup, we only need to connect to the setup
         ;; signal. The bindings take care of the bind step.
         (factory (make-instance 'gtk:signal-list-item-factory))
         (model (gtk:no-selection-new (create-clocks-model)))
         (gridview (make-instance 'gtk:grid-view
                                  :model model
                                  :factory factory
                                  :hscroll-policy :natural
                                  :vscroll-policy :natural))
         ;; List widgets go into a scrolled window. Always.
         (scrolled (make-instance 'gtk:scrolled-window
                                  :child gridview))
         ;; This is the normal window setup code every demo does
         (window (make-instance 'gtk:window
                        :application application
                        :title "Clocks"
                        :child vbox
                        :default-width 600
                        :defalut-height 400)))

    (setf (gtk:widget-hexpand image) t)
    (setf (gtk:widget-vexpand image) t)

    (g:signal-connect factory "setup"
                      (lambda (factory item)
                        (format t "in SETUP for ~a ~a~%" factory item)
                        (setup-listitem-cb factory item)))

    (gtk:box-append vbox image)
    (gtk:box-append vbox scrolled)
    (gtk:window-present window)))

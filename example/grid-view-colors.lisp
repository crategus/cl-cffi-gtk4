;;;; Colors - 2023-11-25
;;;;
;;;; This demo displays a grid of colors.
;;;;
;;;; It is using a GtkGridView, and shows how to display and sort the data in
;;;; various ways. The controls for this are implemented using GtkDropDown.
;;;;
;;;; The dataset used here has up to 16 777 216 items.
;;;;
;;;; Note that this demo also functions as a performance test for some of the
;;;; list model machinery, and the biggest sizes here can lock up the
;;;; application for extended times when used with sorting.
;;;;
;;;; 2023-11-25

(in-package :gtk)

;(eval-when (:compile-toplevel :load-toplevel :execute)

(gobject:define-gobject-subclass "GtkColor" color
  (:superclass g:object
   :export t
   :interfaces ("GdkPaintable"))
  ((name
    color-name
    "name" "gchararray" t t)
   (rgba
    color-rgba
    "rgba" "GdkRGBA" t t)
   (hue
    color-hue
    "hue" "gint" t t)
   (saturation
    color-saturation
    "saturation" "gint" t t)
   (value
    color-value
    "value" "gint" t t)))
;)

(defmethod initialize-instance :after ((obj color) &key)
  (let* ((rgba (color-rgba obj))
         (red (gdk:rgba-red rgba))
         (green (gdk:rgba-green rgba))
         (blue (gdk:rgba-blue rgba)))
    (multiple-value-bind (hue saturation value)
        (gtk:rgb-to-hsv red green blue)
      (setf (color-hue obj) (round (* 360 hue)))
      (setf (color-saturation obj) (round (* 100 saturation)))
      (setf (color-value obj) (round (* 100 value))))))

(defmethod (setf color-rgba) :before (value (obj color))
  (let* ((red (gdk:rgba-red value))
         (green (gdk:rgba-green value))
         (blue (gdk:rgba-blue value)))
    (multiple-value-bind (hue saturation value)
        (gtk:rgb-to-hsv red green blue)
      (setf (color-hue obj) (round (* 360 hue)))
      (setf (color-saturation obj) (round (* 100 saturation)))
      (setf (color-value obj) (round (* 100 value))))))

(defmethod paintable-snapshot-impl ((paintable color) snapshot width height)
  (graphene:with-rect (bounds 0 0 width height)
    (gtk:snapshot-append-color snapshot (color-rgba paintable) bounds)))

(defmethod paintable-get-flags-impl ((paintable color))
  (list :static-contents :static-size))

(defmethod paintable-get-intrinsic-width ((paintable color))
  32)

(defmethod paintable-get-intrinsic-height ((paintable color))
  32)

(defun color-new (name red green blue)
  (make-instance 'color
                 :name name
                 :rgba (make-instance 'gdk:rgba
                                      :red red
                                      :green green
                                      :blue blue
                                      :alpha 1.0)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'color)
  (export 'color-new)
  (export 'color-name)
  (export 'color-rgba))

#|

static void
gtk_color_list_init (GtkColorList *self)
{
  GBytes *data;
  char **lines;
  guint i;

  self->colors = g_new0 (GtkColor *, N_COLORS);

  data = g_resources_lookup_data ("/listview_colors/color.names.txt", 0, NULL);
  lines = g_strsplit (g_bytes_get_data (data, NULL), "\n", 0);

  for (i = 0; lines[i]; i++)
    {
      const char *name;
      char **fields;
      int red, green, blue;
      guint pos;

      if (lines[i][0] == '#' || lines[i][0] == '\0')
        continue;

      fields = g_strsplit (lines[i], " ", 0);
      name = fields[1];
      red = atoi (fields[3]);
      green = atoi (fields[4]);
      blue = atoi (fields[5]);

      pos = ((red & 0xFF) << 16) | ((green & 0xFF) << 8) | blue;
      if (self->colors[pos] == NULL)
        self->colors[pos] = gtk_color_new (name, red / 255., green / 255., blue / 255.);

      g_strfreev (fields);
    }
  g_strfreev (lines);

  g_bytes_unref (data);
}
|#

#|
static char *
get_rgb_markup (gpointer this,
                GtkColor *color)
{
  if (!color)
    return NULL;

  return g_strdup_printf ("<b>R:</b> %d <b>G:</b> %d <b>B:</b> %d",
                          (int)(color->color.red * 255),
                          (int)(color->color.green * 255),
                          (int)(color->color.blue * 255));
}

static char *
get_hsv_markup (gpointer this,
                GtkColor *color)
{
  if (!color)
    return NULL;

  return g_strdup_printf ("<b>H:</b> %d <b>S:</b> %d <b>V:</b> %d",
                          color->h,
                          color->s,
                          color->v);
}
|#

#|
static void
setup_listitem_cb (GtkListItemFactory *factory,
                   GtkListItem        *list_item)
{
  GtkWidget *box, *picture, *name_label, *rgb_label, *hsv_label;
  GtkExpression *color_expression, *expression;
  GtkExpression *params[1];

  box = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
  gtk_list_item_set_child (list_item, box);

  expression = gtk_constant_expression_new (GTK_TYPE_LIST_ITEM, list_item);
  color_expression = gtk_property_expression_new (GTK_TYPE_LIST_ITEM, expression, "item");

  expression = gtk_property_expression_new (GTK_TYPE_COLOR,
                                            gtk_expression_ref (color_expression),
                                            "name");
  name_label = gtk_label_new (NULL);
  gtk_expression_bind (expression, name_label, "label", NULL);
  gtk_box_append (GTK_BOX (box), name_label);

  expression = gtk_expression_ref (color_expression);
  picture = gtk_picture_new ();
  gtk_expression_bind (expression, picture, "paintable", NULL);
  gtk_box_append (GTK_BOX (box), picture);

  params[0] = gtk_expression_ref (color_expression);
  expression = gtk_cclosure_expression_new (G_TYPE_STRING,
                                            NULL,
                                            1, params,
                                            (GCallback)get_rgb_markup,
                                            NULL, NULL);

  rgb_label = gtk_label_new (NULL);
  gtk_label_set_use_markup (GTK_LABEL (rgb_label), TRUE);
  gtk_expression_bind (expression, rgb_label, "label", NULL);
  gtk_box_append (GTK_BOX (box), rgb_label);

  params[0] = gtk_expression_ref (color_expression);
  expression = gtk_cclosure_expression_new (G_TYPE_STRING,
                                            NULL,
                                            1, params,
                                            (GCallback)get_hsv_markup,
                                            NULL, NULL);

  hsv_label = gtk_label_new (NULL);
  gtk_label_set_use_markup (GTK_LABEL (hsv_label), TRUE);
  gtk_expression_bind (expression, hsv_label, "label", NULL);
  gtk_box_append (GTK_BOX (box), hsv_label);

  gtk_expression_unref (color_expression);
}


static void
set_title (gpointer    item,
           const char *title)
{
  g_object_set_data (G_OBJECT (item), "title", (gpointer)title);
}

static char *
get_title (gpointer item)
{
  return g_strdup ((char *)g_object_get_data (G_OBJECT (item), "title"));
}


static gboolean
add_colors (GtkWidget     *widget,
            GdkFrameClock *clock,
            gpointer       data)
{
  GtkColorList *colors = data;
  guint limit;

  limit = GPOINTER_TO_UINT (g_object_get_data (data, "limit"));
  gtk_color_list_set_size (colors, MIN (limit, colors->size + MAX (1, limit / 4096)));

  if (colors->size >= limit)
    return G_SOURCE_REMOVE;
  else
    return G_SOURCE_CONTINUE;
}
|#

(gobject:define-gobject-subclass "GtkColorList" color-list
  (:superclass g:object
   :export t
   :interfaces ("GListModel"))
  ((:cl colors ; We do not register the slot for GTK
        :accessor color-list-colors)
   (size
    color-list-size
    "size" "guint" t t)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'color-list)
  (export 'color-list-size))

(defmethod initialize-instance :after ((obj color-list) &key)
  (format t "in INITIAlIZE-INSTANCE~%")
  (format t "   size : ~a~%" (color-list-size obj))
  (setf (color-list-colors obj)
        (make-array (* 256 256 256)
                    :initial-element nil)))

(defmethod (setf color-list-size) :before (value (obj color-list))
  (let ((oldsize (color-list-size obj)))
    (format t "in SETF COLOR-LIST-SIZE :after ~a~%" obj)
    (format t "    oldsize : ~a~%" oldsize)
    (format t "    newsize : ~a~%" value)
    (if (> value oldsize)
        (g:list-model-items-changed obj oldsize 0 (- value oldsize))
        (g:list-model-items-changed obj value (- oldsize value) 0))
    (g:object-notify obj "size")))

(defmethod gio:list-model-get-item-type-impl ((model color-list))
  (g:gtype "GtkColor"))

(defmethod gio:list-model-get-n-items-impl ((model color-list))
  (color-list-size model))

(defparameter *map*
  (make-array 24
              :initial-contents
              '(#xff0000 #x00ff00 #x0000ff
                #x7F0000 #x007F00 #x00007F
                #x3F0000 #x003F00 #x00003F
                #x1F0000 #x001F00 #x00001F
                #x0F0000 #x000F00 #x00000F
                #x070000 #x000700 #x000007
                #x030000 #x000300 #x000003
                #x010000 #x000100 #x000001)))

(defun position-to-color (position)
  (let ((result 0))
    (dotimes (i 24)
      (unless (= 0 (logand position (ash 1 i)))
        (setf result (logxor result (aref *map* i)))))
     result))

(defmethod gio:list-model-get-item-impl ((model color-list) position)
  (unless (>= position (color-list-size model))
    (let ((pos (position-to-color position)))
      (format t "   pos : ~a ~a~%" pos (aref (color-list-colors model) pos))
      (unless (aref (color-list-colors model) pos)
        (let ((red (logand (ash pos -16) #xff))
              (green (logand (ash pos -8) #xff))
              (blue (logand pos #xff)))
        (format t "Create a color at ~a~%" pos)
        (setf (aref (color-list-colors model) pos)
              (make-instance 'color
                             :name ""
                             :rgba (gdk:rgba-new :red (/ red 255.0)
                                                 :green (/ green 255.0)
                                                 :blue (/ blue 255.0))))
        (format t "   new color is ~a~%" (aref (color-list-colors model) pos))))
      (aref (color-list-colors model) pos))))

#|
GtkWidget *
do_listview_colors (GtkWidget *do_widget)
{
  if (window == NULL)
    {
      GtkMultiSelection *selection;
      GtkSortListModel *sort_model;
      GtkWidget *header, *overlay, *gridview, *sw, *box, *dropdown;
      GtkListItemFactory *factory;
      GListStore *factories;
      GtkSorter *sorter;
      GtkSorter *multi_sorter;
      GListStore *sorters;
      GtkExpression *expression;
      GtkWidget *button;
      GtkWidget *label;
      PangoAttrList *attrs;
      char *string;
      guint len;
      GtkWidget *selection_view;
      GListModel *selection_filter;
      GtkSelectionModel *no_selection;
      GtkWidget *grid;
      GtkWidget *selection_size_label;
      GtkWidget *selection_average_picture;
      GtkWidget *selection_info_toggle;
      GtkWidget *selection_info_revealer;
      GtkWidget *progress;
      GtkCssProvider *provider;





      sorters = g_list_store_new (GTK_TYPE_SORTER);

      /* An empty multisorter doesn't do any sorting and the sortmodel is
       * smart enough to know that.
       */
      sorter = GTK_SORTER (gtk_multi_sorter_new ());
      set_title (sorter, "Unsorted");
      g_list_store_append (sorters, sorter);
      g_object_unref (sorter);

      sorter = GTK_SORTER (gtk_string_sorter_new (gtk_property_expression_new (GTK_TYPE_COLOR, NULL, "name")));
      set_title (sorter, "Name");
      g_list_store_append (sorters, sorter);
      g_object_unref (sorter);

      multi_sorter = GTK_SORTER (gtk_multi_sorter_new ());

      sorter = GTK_SORTER (gtk_numeric_sorter_new (gtk_property_expression_new (GTK_TYPE_COLOR, NULL, "red")));
      gtk_numeric_sorter_set_sort_order (GTK_NUMERIC_SORTER (sorter), GTK_SORT_DESCENDING);
      set_title (sorter, "Red");
      g_list_store_append (sorters, sorter);
      gtk_multi_sorter_append (GTK_MULTI_SORTER (multi_sorter), sorter);

      sorter = GTK_SORTER (gtk_numeric_sorter_new (gtk_property_expression_new (GTK_TYPE_COLOR, NULL, "green")));
      gtk_numeric_sorter_set_sort_order (GTK_NUMERIC_SORTER (sorter), GTK_SORT_DESCENDING);
      set_title (sorter, "Green");
      g_list_store_append (sorters, sorter);
      gtk_multi_sorter_append (GTK_MULTI_SORTER (multi_sorter), sorter);

      sorter = GTK_SORTER (gtk_numeric_sorter_new (gtk_property_expression_new (GTK_TYPE_COLOR, NULL, "blue")));
      gtk_numeric_sorter_set_sort_order (GTK_NUMERIC_SORTER (sorter), GTK_SORT_DESCENDING);
      set_title (sorter, "Blue");
      g_list_store_append (sorters, sorter);
      gtk_multi_sorter_append (GTK_MULTI_SORTER (multi_sorter), sorter);

      set_title (multi_sorter, "RGB");
      g_list_store_append (sorters, multi_sorter);
      g_object_unref (multi_sorter);

      multi_sorter = GTK_SORTER (gtk_multi_sorter_new ());

      sorter = GTK_SORTER (gtk_numeric_sorter_new (gtk_property_expression_new (GTK_TYPE_COLOR, NULL, "hue")));
      gtk_numeric_sorter_set_sort_order (GTK_NUMERIC_SORTER (sorter), GTK_SORT_DESCENDING);
      set_title (sorter, "Hue");
      g_list_store_append (sorters, sorter);
      gtk_multi_sorter_append (GTK_MULTI_SORTER (multi_sorter), sorter);

      sorter = GTK_SORTER (gtk_numeric_sorter_new (gtk_property_expression_new (GTK_TYPE_COLOR, NULL, "saturation")));
      gtk_numeric_sorter_set_sort_order (GTK_NUMERIC_SORTER (sorter), GTK_SORT_DESCENDING);
      set_title (sorter, "Saturation");
      g_list_store_append (sorters, sorter);
      gtk_multi_sorter_append (GTK_MULTI_SORTER (multi_sorter), sorter);

      sorter = GTK_SORTER (gtk_numeric_sorter_new (gtk_property_expression_new (GTK_TYPE_COLOR, NULL, "value")));
      gtk_numeric_sorter_set_sort_order (GTK_NUMERIC_SORTER (sorter), GTK_SORT_DESCENDING);
      set_title (sorter, "Value");
      g_list_store_append (sorters, sorter);
      gtk_multi_sorter_append (GTK_MULTI_SORTER (multi_sorter), sorter);

      set_title (multi_sorter, "HSV");
      g_list_store_append (sorters, multi_sorter);
      g_object_unref (multi_sorter);

      expression = gtk_cclosure_expression_new (G_TYPE_STRING,
                                                NULL,
                                                0, NULL,
                                                (GCallback)get_title,
                                                NULL, NULL);

      dropdown = gtk_drop_down_new (G_LIST_MODEL (sorters), expression);
      box = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 10);
      gtk_box_append (GTK_BOX (box), gtk_label_new ("Sort by:"));
      gtk_box_append (GTK_BOX (box), dropdown);
      gtk_header_bar_pack_end (GTK_HEADER_BAR (header), box);

      g_object_bind_property (dropdown, "selected-item", sort_model, "sorter", G_BINDING_SYNC_CREATE);


      g_object_unref (selection);
    }
}
|#

(defun color-list-new (size)
  (make-instance 'color-list
                 :size size))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'color-list-new))

;;; ----------------------------------------------------------------------------

(in-package :gtk4-example)

#|
static void
update_progress_cb (GtkSortListModel *model,
                    GParamSpec       *pspec,
                    GtkProgressBar   *progress)
{
  guint total;
  guint pending;

  total = g_list_model_get_n_items (G_LIST_MODEL (model));
  total = MAX (total, 1); /* avoid div by 0 below */
  pending = gtk_sort_list_model_get_pending (model);

  gtk_widget_set_visible (GTK_WIDGET (progress), pending != 0);
  gtk_progress_bar_set_fraction (progress, (total - pending) / (double) total);
}
|#

(defun update-progress-cb (model pspec progress)
  (format t "in UPDATE-PROGRESS-CB : ~a, ~a, ~a~%" model pspec progress)
)

#|
static void
setup_selection_listitem_cb (GtkListItemFactory *factory,
                             GtkListItem        *list_item)
{
  GtkWidget *picture;
  GtkExpression *color_expression, *expression;

  expression = gtk_constant_expression_new (GTK_TYPE_LIST_ITEM, list_item);
  color_expression = gtk_property_expression_new (GTK_TYPE_LIST_ITEM, expression, "item");

  picture = gtk_picture_new ();
  gtk_widget_set_size_request (picture, 8, 8);
  gtk_expression_bind (color_expression, picture, "paintable", NULL);

  gtk_list_item_set_child (list_item, picture);
}
|#

(defun setup-selection-listitem-cb (factory listitem)
  (format t "in SETUP-SELECTION-LISTITEM-CB: ~a, ~a~%" factory listitem))

#|
GtkWidget *
create_color_grid (void)
{
  GtkWidget *gridview;
  GtkListItemFactory *factory;

  gridview = gtk_grid_view_new (NULL, NULL);
  gtk_scrollable_set_hscroll_policy (GTK_SCROLLABLE (gridview), GTK_SCROLL_NATURAL);
  gtk_scrollable_set_vscroll_policy (GTK_SCROLLABLE (gridview), GTK_SCROLL_NATURAL);

  factory = gtk_signal_list_item_factory_new ();
  g_signal_connect (factory, "setup", G_CALLBACK (setup_simple_listitem_cb), NULL);
  gtk_grid_view_set_factory (GTK_GRID_VIEW (gridview), factory);
  g_object_unref (factory);

  gtk_grid_view_set_max_columns (GTK_GRID_VIEW (gridview), 24);
  gtk_grid_view_set_enable_rubberband (GTK_GRID_VIEW (gridview), TRUE);

  return gridview;
}
|#

#|
static void
setup_simple_listitem_cb (GtkListItemFactory *factory,
                          GtkListItem        *list_item)
{
  GtkWidget *picture;
  GtkExpression *color_expression, *expression;

  expression = gtk_constant_expression_new (GTK_TYPE_LIST_ITEM, list_item);
  color_expression = gtk_property_expression_new (GTK_TYPE_LIST_ITEM, expression, "item");

  picture = gtk_picture_new ();
  gtk_widget_set_size_request (picture, 32, 32);
  gtk_expression_bind (color_expression, picture, "paintable", NULL);

  gtk_list_item_set_child (list_item, picture);
}
|#

(defun setup-simple-listitem-cb (factory listitem)
  (declare (ignore factory))
  (format t "in SETUP-SIMPLE-LISTITEM-CB for ~a~%" listitem)
  (let ((picture (make-instance 'gtk:picture
                                :width-request 32
                                :height-request 32))
        expression color-expression)
    (setf expression
          (gtk:constant-expression-new "GtkListItem" listitem))
    (setf color-expression
          (gtk:property-expression-new "GtkListItem" expression "item"))
    (gtk:expression-bind color-expression picture "paintable" nil)
    (setf (gtk:list-item-child listitem) picture)))

;; Replace this later with the correct implementation
(declaim (notinline setup-listitem-cb))
(defun setup-listitem-cb (factory listitem)
  (setup-simple-listitem-cb factory listitem))

(defun create-color-grid (model)
  (let* ((factory (gtk:signal-list-item-factory-new))
         (gridview (make-instance 'gtk:grid-view
                                  :factory factory
                                  :model model
                                  :max-columns 24
                                  :enable-rubberband t
                                  :hscroll-policy :natural
                                  :vscroll-policy :natural)))
    (g:signal-connect factory "setup" #'setup-simple-listitem-cb)
    gridview))

#|
static void
update_selection_count (GListModel *model,
                        guint       position,
                        guint       removed,
                        guint       added,
                        gpointer    data)
{
  char *text;
  text = g_strdup_printf ("%u", g_list_model_get_n_items (model));
  gtk_label_set_label (GTK_LABEL (data), text);
  g_free (text);
}
|#

(defun update-selection-count (model position removed added)
  (declare (ignore model position removed added))
  (format t "in UPDATE-SELECTION-COUNT~%"))

#|
static void
update_selection_average (GListModel *model,
                          guint       position,
                          guint       removed,
                          guint       added,
                          gpointer    data)
{
  guint n = g_list_model_get_n_items (model);
  GdkRGBA c = { 0, 0, 0, 1 };
  guint i;
  GtkColor *color;

  for (i = 0; i < n; i++)
    {
      color = g_list_model_get_item (model, i);

      c.red += color->color.red;
      c.green += color->color.green;
      c.blue += color->color.blue;

      g_object_unref (color);
    }

  color = gtk_color_new ("", c.red / n, c.green / n, c.blue / n);
  gtk_picture_set_paintable (GTK_PICTURE (data), GDK_PAINTABLE (color));
  g_object_unref (color);
}
|#

(defun update-selection-average (model position removed added)
  (declare (ignore model position removed added))
  (format t "in UPDATE-SELECTION-AVERAGE~%"))

#|
static void
refill (GtkWidget    *button,
        GtkColorList *colors)
{
  gtk_color_list_set_size (colors, 0);
  gtk_widget_add_tick_callback (button, add_colors, g_object_ref (colors), g_object_unref);
}
|#

(defun refill (button colors)
  (format t "in REFILL ~a, ~a~%" button colors))


(defun do-grid-view-colors (&optional application)
  (let* (
         (sort-model (make-instance 'gtk:sort-list-model
                                    ;; FIXME: a size of 0 causes an error
                                    :model (gtk:color-list-new 1)
                                    :sorter nil
                                    :incremental t))
         (selection (make-instance 'gtk:multi-selection
                                   :model sort-model))
         (selection-filter (make-instance 'gtk:selection-filter-model
                                          :model selection))
         (no-selection (gtk:no-selection-new selection-filter))

         (progress (make-instance 'gtk:progress-bar
                                  :hexpand t
                                  :valign :start))

         (grid (make-instance 'gtk:grid
                              :margin-start 10
                              :margin-end 10
                              :margin-top 10
                              :margin-bottom 10
                              :row-spacing 10
                              :column-spacing 10))
         (selection-info-revealer (make-instance 'gtk:revealer
                                                 :child grid))
         (selection-info-toggle (make-instance 'gtk:toggle-button
                                               :icon-name
                                               "emblem-important-symbolic"
                                               :tooltip-text
                                               "Show selection info"))

         (selection-size-label (gtk:label-new "4.096"))
         (selection-average-picture (gtk:picture-new))

         (selection-view (make-instance
                             'gtk:grid-view
                             :max-columns 200
                             :model no-selection
                             :factory
                             (make-instance 'gtk:signal-list-item-factory)))

         (gridview (create-color-grid selection))

         (box (make-instance 'gtk:box
                             :orientation :vertical))
         (overlay (make-instance 'gtk:overlay
                                 :child box))
         (header (make-instance 'gtk:header-bar))
         (window (make-instance 'gtk:window
                                :application application
                                :title "Colors"
                                :child overlay
                                :titlebar header
                                :default-width 600
                                :default-height 400))

         (label nil) (scrolled nil)
        )

    ;; Add CSS for application specific style information
    (let ((provider (gtk:css-provider-new)))
      (gtk:css-provider-load-from-string provider
                                       ".view.compact > child {padding: 1px;}")
      (gtk:style-context-add-provider-for-display (gdk:display-default)
                                                  provider
                                                  gtk:+priority-user+))

    (g:signal-connect sort-model "notify::pending" #'update-progress-cb)
    (gtk:overlay-add-overlay overlay progress)
    (gtk:box-append box selection-info-revealer)

    (gtk:grid-attach grid
                     (setf label
                           (make-instance 'gtk:label
                                          :label "Selection"
                                          :hexpand t))
                     0 0 5 1)
    (gtk:widget-add-css-class label "title-3")
    (gtk:grid-attach grid (gtk:label-new "Size:") 0 2 1 1)
    (gtk:grid-attach grid selection-size-label 1 2 1 1)
    (gtk:grid-attach grid (gtk:label-new "Average:") 2 2 1 1)
    (gtk:grid-attach grid selection-average-picture 3 2 1 1)
    (gtk:grid-attach grid (make-instance 'gtk:label
                                         :label ""
                                         :hexpand t) 4 2 1 1)

    (gtk:grid-attach grid
                     (setf scrolled
                           (make-instance 'gtk:scrolled-window
                                          :hexpand t
                                          :hscrollbar-policy :never
                                          :vscrollbar-policy :automatic))
                     0 1 5 1)
    (g:signal-connect (gtk:grid-view-factory selection-view)
                      "setup"
                      #'setup-selection-listitem-cb)
    (gtk:widget-add-css-class selection-view "compact")
    (setf (gtk:scrolled-window-child scrolled) selection-view)

    (gtk:box-append box
                    (setf scrolled
                          (make-instance 'gtk:scrolled-window
                                         :child gridview
                                         :hexpand t
                                         :vexpand t)))

    (g:signal-connect selection-filter "items-changed"
                      #'update-selection-count)
    (g:signal-connect selection-filter "items-changed"
                      #'update-selection-average)


    (gtk:header-bar-pack-start header selection-info-toggle)
    (g:object-bind-property selection-info-toggle
                            "active"
                            selection-info-revealer
                            "reveal-child"
                            :default)

    (let ((button (gtk:button-new-with-mnemonic "_Refill")))
      (g:signal-connect button "clicked"
                        (lambda (button)
                          (refill button
                                  (gtk:sort-list-model-model sort-model))))
      (gtk:header-bar-pack-start header button))


    (let* ((label (gtk:label-new "0 /"))
           (attrs (pango:attr-list-new))
           (str (format nil "~,,'.:d" 4096))
           (len (length str))
           (strings '("8" "64" "512" "4096"
                      "32768" "262144" "2097152" "16777216"))
           (dropdown (gtk:drop-down-new-from-strings strings))
           (factory (gtk:signal-list-item-factory-new)))

      (pango:attr-list-insert attrs
                              (pango:attr-font-features-new "tnum"))
;     FIXME: Causes a memory fault. What is the problem?
;     (setf (gtk:label-attributes label) attrs)
      (setf (gtk:label-width-chars label) (+ len 2))
      (setf (gtk:label-xalign label) 1)

       ;; Update the text of the label
       (g:signal-connect selection "items-changed"
                         (lambda (model position removed added)
                           (declare (ignore position removed added))
                           (let ((n (g:list-model-n-items model)))
                             (setf (gtk:label-label label)
                                   (format nil "~,,'.:d" n)))))

       (gtk:header-bar-pack-start header label)

#|
      dropdown = gtk_drop_down_new_from_strings  ((const char * const[]) { "8", "64", "512", "4096", "32768", "262144", "2097152", "16777216", NULL });

      g_signal_connect (dropdown, "notify::selected",
                        G_CALLBACK (limit_changed_cb),
                        gtk_sort_list_model_get_model (sort_model));
      g_signal_connect (dropdown, "notify::selected",
                        G_CALLBACK (limit_changed_cb2),
                        label);

      factory = gtk_signal_list_item_factory_new ();
      g_signal_connect (factory, "setup", G_CALLBACK (setup_number_item), NULL);
      g_signal_connect (factory, "bind", G_CALLBACK (bind_number_item), NULL);
      gtk_drop_down_set_factory (GTK_DROP_DOWN (dropdown), factory);
      g_object_unref (factory);
      gtk_drop_down_set_selected (GTK_DROP_DOWN (dropdown), 3); /* 4096 */
      gtk_header_bar_pack_start (GTK_HEADER_BAR (header), dropdown);
|#

#|
static void
limit_changed_cb (GtkDropDown  *dropdown,
                  GParamSpec   *pspec,
                  GtkColorList *colors)
{
  guint new_limit, old_limit;

  old_limit = GPOINTER_TO_UINT (g_object_get_data (G_OBJECT (colors), "limit"));
  new_limit = 1 << (3 * (gtk_drop_down_get_selected (dropdown) + 1));

  g_object_set_data (G_OBJECT (colors), "limit", GUINT_TO_POINTER (new_limit));

  if (old_limit == colors->size)
    gtk_color_list_set_size (colors, new_limit);
}
|#
      (g:signal-connect dropdown "notify::selected"
          (lambda (dropdown pspec)
            (declare (ignore pspec))
            (let* ((colors (gtk:sort-list-model-model sort-model))
                   (oldlimit (g:object-data colors "limit"))
                   (newlimit (ash 1
                                  (* 3
                                     (+ 1
                                        (gtk:drop-down-selected dropdown))))))
              (setf (g:object-data colors "limit") newlimit)
              (when (= oldlimit (gtk:color-list-size colors))
                (setf (gtk:color-list-size colors) newlimit)))))


#|
static void
limit_changed_cb2 (GtkDropDown  *dropdown,
                   GParamSpec   *pspec,
                   GtkLabel     *label)
{
  char *string;
  int len;
  guint limit;

  limit = 1 << (3 * (gtk_drop_down_get_selected (dropdown) + 1));

  string = g_strdup_printf ("%'u", limit);
  len = g_utf8_strlen (string, -1);
  g_free (string);

  gtk_label_set_width_chars (label, len + 2); /* for " /" */
}
|#

      (g:signal-connect dropdown "notify::selected"
          (lambda (dropdown pspec)
            (declare (ignore pspec))
            (let ((limit (ash 1
                              (* 3 (+ 1 (gtk:drop-down-selected dropdown))))))
              (setf (gtk:label-width-chars label)
                    (+ 2 (length (format nil "~,,'.:d" limit)))))))




      (g:signal-connect factory "setup"
          (lambda (factory item)
            (declare (ignore factory))
            (let ((label (make-instance 'gtk:label
                                        :label ""
                                        :xalign 1.0)))
              (pango:attr-list-insert attrs
                                      (pango:attr-font-features-new "tnum"))
; FIXME: Causes a memory fault
;              (setf (gtk:label-attributes label) attrs)
              (setf (gtk:list-item-child item) label))))

      (g:signal-connect factory "bind"
          (lambda (factory item)
            (declare (ignore factory))
            (let ((label (gtk:list-item-child item))
                  (limit (ash 1 (* 3 (+ 1 (gtk:list-item-position item))))))
              (setf (gtk:label-label label) (format nil "~,,'.:d" limit)))))

      (setf (gtk:drop-down-factory dropdown) factory)
      (setf (gtk:drop-down-selected dropdown) 3)
      (gtk:header-bar-pack-start header dropdown)
    )


#|
      factories = g_list_store_new (GTK_TYPE_LIST_ITEM_FACTORY);

      factory = gtk_signal_list_item_factory_new ();
      g_signal_connect (factory, "setup", G_CALLBACK (setup_simple_listitem_cb), NULL);
      set_title (factory, "Colors");
      g_list_store_append (factories, factory);

      factory = gtk_signal_list_item_factory_new ();
      g_signal_connect (factory, "setup", G_CALLBACK (setup_listitem_cb), NULL);
      set_title (factory, "Everything");
      g_list_store_append (factories, factory);

      expression = gtk_cclosure_expression_new (G_TYPE_STRING,
                                                NULL,
                                                0, NULL,
                                                (GCallback)get_title,
                                                NULL, NULL);

      dropdown = gtk_drop_down_new (G_LIST_MODEL (factories), expression);
      box = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 10);
      gtk_box_append (GTK_BOX (box), gtk_label_new ("Show:"));
      gtk_box_append (GTK_BOX (box), dropdown);
      gtk_header_bar_pack_end (GTK_HEADER_BAR (header), box);

      g_object_bind_property (dropdown, "selected-item", gridview, "factory", G_BINDING_SYNC_CREATE);
|#

    (let ((factories (g:list-store-new "GtkListItemFactory"))
          factory dropdown box expression)
      (setf factory (gtk:signal-list-item-factory-new))
      (setf (g:object-data factory "title") "Colors")
      (g:list-store-append factories factory)
      (g:signal-connect factory "setup" #'setup-simple-listitem-cb)

      (setf factory (gtk:signal-list-item-factory-new))
      (setf (g:object-data factory "title") "Evertything")
      (g:list-store-append factories factory)
      (g:signal-connect factory "setup" #'setup-listitem-cb)

      ;; Replacement for missing gtk_cclosure_expression_new
      (setf expression (gtk:constant-expression-new "gchararray" "Colors"))

      (setf dropdown (gtk:drop-down-new factories expression))

      (setf box (gtk:box-new :horizontal 10))
      (gtk:box-append box (gtk:label-new "Show"))
      (gtk:box-append box dropdown)
      (gtk:header-bar-pack-end header box)

      (g:object-bind-property dropdown "selected-item"
                              gridview "factory"
                              :sync-create)
    )

    (gtk:window-present window)
))

(in-package :gtk4-example)

#|
GtkWidget *
do_dropdown (GtkWidget *do_widget)
{
  static GtkWidget *window = NULL;
  GtkWidget *button, *box, *spin, *check, *hbox, *label, *entry;

  GListModel *model;

  GtkExpression *expression;
  GtkListItemFactory *factory;

  const char * const times[] = { "1 minute", "2 minutes", "5 minutes", "20 minutes", NULL };

  const char * const many_times[] = {
    "1 minute", "2 minutes", "5 minutes", "10 minutes", "15 minutes", "20 minutes",
    "25 minutes", "30 minutes", "35 minutes", "40 minutes", "45 minutes", "50 minutes",
    "55 minutes", "1 hour", "2 hours", "3 hours", "5 hours", "6 hours", "7 hours",
    "8 hours", "9 hours", "10 hours", "11 hours", "12 hours", NULL
  };

  const char * const device_titles[] = { "Digital Output", "Headphones", "Digital Output", "Analog Output", NULL };

  const char * const device_icons[] = {  "audio-card-symbolic", "audio-headphones-symbolic", "audio-card-symbolic", "audio-card-symbolic", NULL };

  const char * const device_descriptions[] = {
    "Built-in Audio", "Built-in audio", "Thinkpad Tunderbolt 3 Dock USB Audio", "Thinkpad Tunderbolt 3 Dock USB Audio", NULL
  };

  char *cwd;
  GFile *file;
  GListModel *dir;
  GtkStringList *strings;

  if (!window)
    {
      window = gtk_window_new ();
      gtk_window_set_display (GTK_WINDOW (window),
                              gtk_widget_get_display (do_widget));
      gtk_window_set_title (GTK_WINDOW (window), "Selections");
      gtk_window_set_resizable (GTK_WINDOW (window), FALSE);
      g_object_add_weak_pointer (G_OBJECT (window), (gpointer *)&window);

      hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 20);

      gtk_widget_set_margin_start (hbox, 20);
      gtk_widget_set_margin_end (hbox, 20);
      gtk_widget_set_margin_top (hbox, 20);
      gtk_widget_set_margin_bottom (hbox, 20);
      gtk_window_set_child (GTK_WINDOW (window), hbox);

      box = gtk_box_new (GTK_ORIENTATION_VERTICAL, 10);
      gtk_box_append (GTK_BOX (hbox), box);

      label = gtk_label_new ("Dropdowns");
      gtk_widget_add_css_class (label, "title-4");
      gtk_box_append (GTK_BOX (box), label);

      /* A basic dropdown */
      button = drop_down_new_from_strings (times, NULL, NULL);
      gtk_box_append (GTK_BOX (box), button);

      /* A dropdown using an expression to obtain strings */
      button = drop_down_new_from_strings (many_times, NULL, NULL);
      gtk_drop_down_set_enable_search (GTK_DROP_DOWN (button), TRUE);
      expression = gtk_cclosure_expression_new (G_TYPE_STRING, NULL,
                                                0, NULL,
                                                (GCallback)get_title,
                                                NULL, NULL);
      gtk_drop_down_set_expression (GTK_DROP_DOWN (button), expression);
      gtk_expression_unref (expression);
      gtk_box_append (GTK_BOX (box), button);

      button = gtk_drop_down_new (NULL, NULL);

      model = G_LIST_MODEL (pango_cairo_font_map_get_default ());
      gtk_drop_down_set_model (GTK_DROP_DOWN (button), model);
      gtk_drop_down_set_selected (GTK_DROP_DOWN (button), 0);

      expression = gtk_cclosure_expression_new (G_TYPE_STRING, NULL,
                                                0, NULL,
                                                (GCallback)get_family_name,
                                                NULL, NULL);
      gtk_drop_down_set_expression (GTK_DROP_DOWN (button), expression);
      gtk_expression_unref (expression);
      gtk_box_append (GTK_BOX (box), button);

      spin = gtk_spin_button_new_with_range (-1, g_list_model_get_n_items (G_LIST_MODEL (model)), 1);
      gtk_widget_set_halign (spin, GTK_ALIGN_START);
      gtk_widget_set_margin_start (spin, 20);
      g_object_bind_property  (button, "selected", spin, "value", G_BINDING_SYNC_CREATE | G_BINDING_BIDIRECTIONAL);
      gtk_box_append (GTK_BOX (box), spin);

      check = gtk_check_button_new_with_label ("Enable search");
      gtk_widget_set_margin_start (check, 20);
      g_object_bind_property  (button, "enable-search", check, "active", G_BINDING_SYNC_CREATE | G_BINDING_BIDIRECTIONAL);
      gtk_box_append (GTK_BOX (box), check);

      g_object_unref (model);

      /* A dropdown with a separate list factory */
      button = drop_down_new_from_strings (device_titles, device_icons, device_descriptions);
      gtk_box_append (GTK_BOX (box), button);

      gtk_box_append (GTK_BOX (hbox), gtk_separator_new (GTK_ORIENTATION_VERTICAL));

      box = gtk_box_new (GTK_ORIENTATION_VERTICAL, 10);
      gtk_box_append (GTK_BOX (hbox), box);

      label = gtk_label_new ("Suggestions");
      gtk_widget_add_css_class (label, "title-4");
      gtk_box_append (GTK_BOX (box), label);

      /* A basic suggestion entry */
      entry = suggestion_entry_new ();
      g_object_set (entry, "placeholder-text", "Words with T or G…", NULL);
      strings = gtk_string_list_new ((const char *[]){
                                     "GNOME",
                                     "gnominious",
                                     "Gnomonic projection",
                                     "total",
                                     "totally",
                                     "toto",
                                     "tottery",
                                     "totterer",
                                     "Totten trust",
                                     "totipotent",
                                     "totipotency",
                                     "totemism",
                                     "totem pole",
                                     "Totara",
                                     "totalizer",
                                     "totalizator",
                                     "totalitarianism",
                                     "total parenteral nutrition",
                                     "total hysterectomy",
                                     "total eclipse",
                                     "Totipresence",
                                     "Totipalmi",
                                     "Tomboy",
                                     "zombie",
                                     NULL});
      suggestion_entry_set_model (SUGGESTION_ENTRY (entry), G_LIST_MODEL (strings));
      g_object_unref (strings);

      gtk_box_append (GTK_BOX (box), entry);

      /* A suggestion entry using a custom model, and no filtering */
      entry = suggestion_entry_new ();

      cwd = g_get_current_dir ();
      file = g_file_new_for_path (cwd);
      dir = G_LIST_MODEL (gtk_directory_list_new ("standard::display-name,standard::content-type,standard::icon,standard::size", file));
      suggestion_entry_set_model (SUGGESTION_ENTRY (entry), dir);
      g_object_unref (dir);
      g_object_unref (file);
      g_free (cwd);

      expression = gtk_cclosure_expression_new (G_TYPE_STRING, NULL,
                                                0, NULL,
                                                (GCallback)get_file_name,
                                                NULL, NULL);
      suggestion_entry_set_expression (SUGGESTION_ENTRY (entry), expression);
      gtk_expression_unref (expression);

      factory = gtk_signal_list_item_factory_new ();
      g_signal_connect (factory, "setup", G_CALLBACK (setup_item), NULL);
      g_signal_connect (factory, "bind", G_CALLBACK (bind_item), NULL);

      suggestion_entry_set_factory (SUGGESTION_ENTRY (entry), factory);
      g_object_unref (factory);

      suggestion_entry_set_use_filter (SUGGESTION_ENTRY (entry), FALSE);
      suggestion_entry_set_show_arrow (SUGGESTION_ENTRY (entry), TRUE);

      gtk_box_append (GTK_BOX (box), entry);

      /* A suggestion entry with match highlighting */
      entry = suggestion_entry_new ();
      g_object_set (entry, "placeholder-text", "Destination", NULL);

      strings = gtk_string_list_new ((const char *[]){
                                     "app-mockups",
                                     "settings-mockups",
                                     "os-mockups",
                                     "software-mockups",
                                     "mocktails",
                                     NULL});
      suggestion_entry_set_model (SUGGESTION_ENTRY (entry), G_LIST_MODEL (strings));
      g_object_unref (strings);

      gtk_box_append (GTK_BOX (box), entry);

      suggestion_entry_set_match_func (SUGGESTION_ENTRY (entry), match_func, NULL, NULL);

      factory = gtk_signal_list_item_factory_new ();
      g_signal_connect (factory, "setup", G_CALLBACK (setup_highlight_item), NULL);
      g_signal_connect (factory, "bind", G_CALLBACK (bind_highlight_item), NULL);
      suggestion_entry_set_factory (SUGGESTION_ENTRY (entry), factory);
      g_object_unref (factory);

    }

  if (!gtk_widget_get_visible (window))
    gtk_widget_set_visible (window, TRUE);
  else
    gtk_window_destroy (GTK_WINDOW (window));

  return window;
}
|#




(defclass string-holder (g:object)
  ((title :initarg :title
          :initform nil
          :accessor string-holder-title)
   (icon :initarg :icon
         :initform nil
         :accessor string-holder-icon)
   (description :initarg :description
                :initform nil
                :accessor string-holder-description))
   (:gname . "StringHolder")
   (:metaclass gobject:gobject-class))

(gobject:register-object-type-implementation "StringHolder"     ; name
                                             string-holder      ; class
                                             "GObject"          ; parent
                                             nil                ; interfaces
                                             nil)               ; properties

#|
static GtkListItemFactory *
strings_factory_new (gpointer data, gboolean full)
{
  GtkListItemFactory *factory;

  factory = gtk_signal_list_item_factory_new ();
  if (full)
    g_signal_connect (factory, "setup", G_CALLBACK (strings_setup_item_full), data);
  else
    g_signal_connect (factory, "setup", G_CALLBACK (strings_setup_item_single_line), data);
  g_signal_connect (factory, "bind", G_CALLBACK (strings_bind_item), data);
  g_signal_connect (factory, "unbind", G_CALLBACK (strings_unbind_item), data);

  return factory;
}
|#

(defun selected-item-changed (dropdown pspec item)
  (declare (ignore dropdown pspec item))
  (format t "in SELECTED-ITEM-CHANGED~%")
  (let ((checkmark (g:object-data item "checkmark")))
  
    (if (eq (gtk:drop-down-selected-item dropdown) (gtk:list-item-item item))
        (setf (gtk:widget-opacity checkmark) 1.0)
        (setf (gtk:widget-opacity checkmark) 0.0))))


(defun strings-factory-new (dropdown full)
  (let ((factory (gtk:signal-list-item-factory-new)))
    (if full
        (g:signal-connect factory "setup"
            (lambda (factory item)
              (format t "in SETUP for FULL with ~a, ~a~%" factory item)))
        ;; STRINGS_SETUP_ITEM_SINGLE_LINE
        (g:signal-connect factory "setup"
            (lambda (factory item)
              (format t "in SETUP for ~a, ~a~%" factory item)

              (let* ((box (make-instance 'gtk:box
                                         :orientation :horizontal
                                         :spacing 10))
                     (image (make-instance 'gtk:image))
                     (title (make-instance 'gtk:label
                                           :xalign 0.0))
                     (checkmark (make-instance 'gtk:image
                                               :icon-name
                                               "object-select-symbolic")))
                (gtk:box-append box image)
                (gtk:box-append box title)
                (gtk:box-append box checkmark)

                (setf (g:object-data item "title") title)
                (setf (g:object-data item "image") image)
                (setf (g:object-data item "checkmark") checkmark)

                (setf (gtk:list-item-child item) box)
              ))))
    (g:signal-connect factory "bind"
        ;; STRINGS-BIND-ITEM
        (lambda (factory item)
          (format t "in BIND for ~a, ~a~%" factory item)
          
          (let* ((holder (gtk:list-item-item item))
                 (title (g:object-data item "title"))
                 (image (g:object-data item "image"))
                 (description (g:object-data item "description"))
                 (checkmark (g:object-data item "checkmark")))

            (setf (gtk:label-label title) (string-holder-title holder))

            (let ((popup (gtk:widget-ancestor title "GtkPopover")))
              (if (and popup (gtk:widget-is-ancestor popup dropdown))
                  (progn
                    (setf (gtk:widget-visible checkmark) t)
                    (g:signal-connect dropdown "notify::selected-item"
                        (lambda (dropdown pspec)
                           (selected-item-changed dropdown pspec item)))
                    (selected-item-changed dropdown nil item))
                  (setf (gtk:widget-visible checkmark) nil)))
          )))
    ;; TODO: Does not work as expected. Work out the correct code!
    (g:signal-connect factory "unbind"
        (lambda (factory item)
          (format t "in UNBIND for ~a, ~a~%" factory item)
          (let* ((signal-id (g:signal-lookup "notify" "GtkDropDown"))
                 (handler-id (g:signal-handler-find item signal-id)))
            (format t "   signal-id : ~a~%" signal-id)
            (format t "  handler-id : ~a~%" handler-id)
            (format t "  handlers : ~a~%" (gobject:object-signal-handlers factory))
            (format t "  handlers : ~a~%" (gobject:object-signal-handlers item))
            (g:signal-handler-disconnect factory handler-id)
          
          )))
    factory
))

#|
static GListModel *
strings_model_new (const char *const *titles,
                   const char *const *icons,
                   const char *const *descriptions)
{
  GListStore *store;
  int i;

  store = g_list_store_new (STRING_TYPE_HOLDER);
  for (i = 0; titles[i]; i++)
    {
      StringHolder *holder = string_holder_new (titles[i],
                                                icons ? icons[i] : NULL,
                                                descriptions ? descriptions[i] : NULL);
      g_list_store_append (store, holder);
      g_object_unref (holder);
    }

  return G_LIST_MODEL (store);
}
|#

(defun strings-model-new (titles icons descriptions)
  (let ((store (g:list-store-new "GObject")))
    (iter (for title in titles)

          (for icon-list first icons then (cdr icon-list))
          (for icon = (car icon-list))

          (for description-list first descriptions then (cdr description-list))
          (for description = (car description-list))

          (for holder = (make-instance 'string-holder
                                       :title title
                                       :icon icon
                                       :description description))
          (format t "title : ~a  icon: ~a  description: ~a~%"
                    title icon description)
          (g:list-store-append store holder))
    store))

#|
static GtkWidget *
drop_down_new_from_strings (const char *const *titles,
                            const char *const *icons,
                            const char *const *descriptions)
{
  GtkWidget *widget;
  GListModel *model;
  GtkListItemFactory *factory;
  GtkListItemFactory *list_factory;

  g_return_val_if_fail (titles != NULL, NULL);
  g_return_val_if_fail (icons == NULL || g_strv_length ((char **)icons) == g_strv_length ((char **)titles), NULL);
  g_return_val_if_fail (descriptions == NULL || g_strv_length ((char **)icons) == g_strv_length ((char **)descriptions), NULL);

  model = strings_model_new (titles, icons, descriptions);
  widget = g_object_new (GTK_TYPE_DROP_DOWN,
                         "model", model,
                         NULL);
  g_object_unref (model);

  factory = strings_factory_new (widget, FALSE);
  if (icons != NULL || descriptions != NULL)
    list_factory = strings_factory_new (widget, TRUE);
  else
    list_factory = NULL;

  g_object_set (widget,
                "factory", factory,
                "list-factory", list_factory,
                NULL);

  g_object_unref (factory);
  if (list_factory)
    g_object_unref (list_factory);

  return widget;
}
|#

(defun drop-down-new-from-strings (titles icons descriptions)

  (let* ((model (strings-model-new titles icons descriptions))
         (dropdown (make-instance 'gtk:drop-down
                                  :model model))
         (factory (strings-factory-new dropdown nil)))
    (setf (gtk:drop-down-factory dropdown) factory)
    dropdown
))

(defun do-drop-down (&optional application)
  (let* ((times (list "1 minute" "2 minutes" "5 minutes" "20 minutes"))

         (hbox (make-instance 'gtk:box
                              :orientation :horizontal
                              :margin-start 20
                              :margin-end 20
                              :margin-top 20
                              :margin-bottom 20
                              :spacing 20))
         (window (make-instance 'gtk:window
                                :application application
                                :child hbox
                                :title "Drop Down Widget"
                                :resizeable nil))
         (box (make-instance 'gtk:box
                             :orientation :vertical
                             :spacing 10))
        )
    (gtk:box-append hbox box)
    (let ((label (make-instance 'gtk:label
                                :label "DropDowns")))
      (gtk:widget-add-css-class label "title-4")
      (gtk:box-append box label)

      ;; A basic dropdown
      (let ((button (drop-down-new-from-strings times nil nil)))
        (gtk:box-append box button))
    )
    (setf (gtk:widget-visible window) t)
))

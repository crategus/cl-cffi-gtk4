;;;; Drop Down
;;;;
;;;; The <tt>gtk:drop-down</tt> widget is a widget that allows the user to
;;;; choose an item from a list of options. The <tt>gtk:drop-down</tt> widget
;;;; displays the selected choice.
;;;;
;;;; The options are given to the <tt>gtk:drop-down</tt> widget in the form of
;;;; a <tt>g:list-model</tt> object, and how the individual options are
;;;; represented is determined by a <tt>gtk:list-item-factory</tt> object. The
;;;; default factory displays simple strings.
;;;;
;;;; The <tt>gtk:drop-down</tt> widget knows how to obtain strings from the
;;;; items in a <tt>gtk:string-list</tt> object. For other models, you have to
;;;; provide an expression to find the strings via the
;;;; <tt>gtk:drop-down-expression</tt> function.
;;;;
;;;; The <tt>gtk:drop-down</tt> widget can optionally allow search in the popup,
;;;; which is useful if the list of options is long. To enable the search entry,
;;;; use the <tt>gtk:drop-down-enable-search</tt> function.
;;;;
;;;; 2024-4-6

(in-package :gtk4-example)

(gobject:define-gobject-subclass "StringHolder" string-holder
  (:superclass g:object
   :export t
   :interfaces nil)
  ((title
    string-holder-title
    "title" "gchararray" t t)
   (icon
    string-holder-icon
    "icon" "gchararray" t t)
   (description
    string-holder-description
    "description" "gchararray" t t)))

(defun string-holder-new (title icon description)
  (make-instance 'string-holder
                 :title title
                 :icon icon
                 :description description))

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
  (declare (ignore pspec))
  (format t "in SELECTED-ITEM-CHANGED~%")
  (let ((checkmark (g:object-data item "checkmark")))
    (if (eq (gtk:drop-down-selected-item dropdown) (gtk:list-item-item item))
        (setf (gtk:widget-opacity checkmark) 1.0)
        (setf (gtk:widget-opacity checkmark) 0.0))))


(defun strings-factory-new (dropdown full)
  (let ((factory (gtk:signal-list-item-factory-new)))
    (if full
        ;; STRINGS_SETUP_ITEM_FULL
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
                                           :label ""
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
    ;; STRINGS_BIND_ITEM
    (g:signal-connect factory "bind"
        (lambda (factory item)
          (format t "in BIND for ~a, ~a~%" factory item)

          (let* ((holder (gtk:list-item-item item))
                 (title (g:object-data item "title"))
;                 (image (g:object-data item "image"))
;                 (description (g:object-data item "description"))
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
    ;; STRINGS_UNBIND_ITEM
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
  (let* ((times '("1 minute" "2 minutes" "5 minutes" "20 minutes"))
         (minutes '("1 minute" "2 minutes" "5 minutes" "10 minutes" "15 minutes"
                    "20 minutes" "25 minutes" "30 minutes" "35 minutes"
                    "40 minutes" "45 minutes" "50 minutes" "55 minutes"))
         (hours '("1 hour" "2 hours" "3 hours" "5 hours" "6 hours" "7 hours"
                  "8 hours" "9 hours" "10 hours" "11 hours" "12 hours"))
         (titles '("Digital Output" "Headphones" "Digital Output"
                   "Analog Output"))
         (icons '("audio-card-symbolic" "audio-headphones-symbolic"
                  "audio-card-symbolic" "audio-card-symbolic"))
         (descriptions '("Built-in Audio"
                         "Built-in audio"
                         "Thinkpad Tunderbolt 3 Dock USB Audio"
                         "Thinkpad Tunderbolt 3 Dock USB Audio"))

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

      ;; A dropdown using an expression to obtain strings
      (let* ((minutes (gtk:string-list-new minutes))
             (hours (gtk:string-list-new hours))
             (store (g:list-store-new "GListModel"))
             (model (gtk:flatten-list-model-new store)))
        (g:list-store-append store minutes)
        (g:list-store-append store hours)
      )
    )
    (gtk:window-present window)
))

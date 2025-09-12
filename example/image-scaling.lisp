;;;; Image Scaling
;;;;
;;;; Last updated: 2025-08-31

(in-package :gtk4-example)

#|
static void
load_texture (GTask        *task,
              gpointer      source_object,
              gpointer      task_data,
              GCancellable *cable)
{
  GFile *file = task_data;
  GdkTexture *texture;
  GError *error = NULL;

  texture = gdk_texture_new_from_file (file, &error);

  if (texture)
    g_task_return_pointer (task, texture, g_object_unref);
  else
    g_task_return_error (task, error);
}

static void
set_wait_cursor (GtkWidget *widget)
{
  gtk_widget_set_cursor_from_name (GTK_WIDGET (gtk_widget_get_root (widget)), "wait");
}

static void
unset_wait_cursor (GtkWidget *widget)
{
  gtk_widget_set_cursor (GTK_WIDGET (gtk_widget_get_root (widget)), NULL);
}

static void
texture_loaded (GObject      *source,
                GAsyncResult *result,
                gpointer      data)
{
  GdkTexture *texture;
  GError *error = NULL;

  texture = g_task_propagate_pointer (G_TASK (result), &error);

  if (!texture)
    {
      g_print ("%s\n", error->message);
      g_error_free (error);
      return;
    }

  if (!window)
    {
      g_object_unref (texture);
      return;
    }

  unset_wait_cursor (GTK_WIDGET (data));

  g_object_set (G_OBJECT (data), "texture", texture, NULL);
}

static void
open_file_async (GFile     *file,
                 GtkWidget *demo)
{
  GTask *task;

  set_wait_cursor (demo);

  task = g_task_new (demo, cancellable, texture_loaded, demo);
  g_task_set_task_data (task, g_object_ref (file), g_object_unref);
  g_task_run_in_thread (task, load_texture);
  g_object_unref (task);
}

static void
open_portland_rose (GtkWidget *button,
                    GtkWidget *demo)
{
  GFile *file;

  file = g_file_new_for_uri ("resource:///transparent/portland-rose.jpg");
  open_file_async (file, demo);
  g_object_unref (file);
}

static void
open_large_image (GtkWidget *button,
                  GtkWidget *demo)
{
  GFile *file;

  file = g_file_new_for_uri ("resource:///org/gtk/Demo4/large-image.png");
  open_file_async (file, demo);
  g_object_unref (file);
}

static void
file_opened (GObject      *source,
             GAsyncResult *result,
             void         *data)
{
  GFile *file;
  GError *error = NULL;

  file = gtk_file_dialog_open_finish (GTK_FILE_DIALOG (source), result, &error);

  if (!file)
    {
      g_print ("%s\n", error->message);
      g_error_free (error);
      return;
    }

  open_file_async (file, data);

  g_object_unref (file);
}

static void
open_file (GtkWidget *picker,
           GtkWidget *demo)
{
  GtkWindow *parent = GTK_WINDOW (gtk_widget_get_root (picker));
  GtkFileDialog *dialog;
  GtkFileFilter *filter;
  GListStore *filters;

  dialog = gtk_file_dialog_new ();

  filter = gtk_file_filter_new ();
  gtk_file_filter_set_name (filter, "Images");
  gtk_file_filter_add_pixbuf_formats (filter);
  filters = g_list_store_new (GTK_TYPE_FILE_FILTER);
  g_list_store_append (filters, filter);
  g_object_unref (filter);

  gtk_file_dialog_set_filters (dialog, G_LIST_MODEL (filters));
  g_object_unref (filters);

  gtk_file_dialog_open (dialog, parent, NULL, file_opened, demo);

  g_object_unref (dialog);
}

static gboolean
transform_from (GBinding     *binding,
                const GValue *src,
                GValue       *dest,
                gpointer      user_data)
{
  float to;
  double from;

  to = g_value_get_float (src);
  from = log2 (to);
  g_value_set_double (dest, from);

  return TRUE;
}
|#

;; static gboolean
;; cancel_load (GtkWidget *widget,
;;              GVariant  *args,
;;              gpointer   data)
;; {
;;   unset_wait_cursor (widget);
;;   g_cancellable_cancel (G_CANCELLABLE (data));
;;   return TRUE;
;; }
(defun cancel-load (cancellable widget args)
  (declare (ignore args))
  ;; Unset cursor on root of widget
  (setf (gtk:widget-cursor (gtk:widget-root widget)) nil)
  (g:cancellable-cancel cancellable)
  t)

;;; ----------------------------------------------------------------------------

(defun open-file (button)
  (format t "in OPEN-FILE for ~a~%" button))

(defun open-portland-rose (button)
  (format t "in OPEN-PORTLAND-ROSE for ~a~%" button))

(defun open-large-image (button)
  (format t "in OPEN-LARGE-IMAGE for ~a~%" button))

;; static void
;; rotate (GtkWidget *button,
;;         GtkWidget *demo)
;; {
;;   float angle;
;;
;;   g_object_get (demo, "angle", &angle, NULL);
;;
;;   angle = fmodf (angle + 90.f, 360.f);
;;
;;   g_object_set (demo, "angle", angle, NULL);
;; }
(defun rotate (image)
  (format t "in ROTATE for ~a~%" image)
  (format t "   angle : ~a~%" (image-view-angle image))
  (format t "     new : ~a~%" (mod (+ (image-view-angle image) 90.0) 360))
  (setf (image-view-angle image)
        (mod (+ (image-view-angle image) 90.0) 360.0))
  ;; TODO: Setting the angle does not cause a notify signal. Is there a problem?
  ;; We emit the signal as a workaround.
  (g:object-notify image "angle"))

;; static gboolean
;; transform_to (GBinding     *binding,
;;               const GValue *src,
;;               GValue       *dest,
;;               gpointer      user_data)
;; {
;;   double from;
;;   float to;
;;
;;   from = g_value_get_double (src);
;;   to = (float) pow (2., from);
;;   g_value_set_float (dest, to);
;;
;;   return TRUE;
;; }
(defun transform-to (binding src dest)
  (format t "in TRANSFORM-TO for ~a (~a, ~a)~%" binding src dest)
  (let* ((from (g:value-get src))
         (to (expt 2 from)))
    (setf (g:value-get dest) to)
    t))

(defun transform-from (binding from to)
  (format t "in TRANSFORM-FROM for ~a (~a, ~a)~%" binding from to))

;;; ----------------------------------------------------------------------------

(defun do-image-scaling (&optional application)
  (let* (
         (path (glib-sys:sys-path "resource/portland-rose.jpg"))
         (image (image-view-new-from-filename path))

         (viewport (make-instance 'gtk:viewport
                                  :child image
                                  :scroll-to-focus nil))
         (scrolled (make-instance 'gtk:scrolled-window
                                  :child viewport
                                  :vexpand t))

         (box (make-instance 'gtk:box
                             :orientation :vertical))
         (box2 (make-instance 'gtk:box
                              :orientation :horizontal))

         (window (make-instance 'gtk:window
                                :child box
                                :application application
                                :title "Image Scaling"
                                :default-width 600
                                :default-height 400))

         (cancellable (g:cancellable-new))
         (controller (gtk:shortcut-controller-new))
        )

    ;; The lambda function is called when the window is destroyed.
    ;; TODO: Does this work as expected?
    (g:object-set-data-full window "cancellable"
                            (lambda ()
                              (g:cancellable-cancel cancellable)))

    ;; Configure shortcut controller
    (gtk:shortcut-controller-add-shortcut controller
            (gtk:shortcut-new (gtk:keyval-trigger-new #\Esc :no-modifier-mask)
                              (gtk:callback-action-new
                                      (lambda (widget args)
                                        (cancel-load cancellable widget args)))))
    (setf (gtk:shortcut-controller-scope controller) :global)
    (gtk:widget-add-controller window controller)

    (let ((button (gtk:button-new-from-icon-name "document-open-symbolic")))
      (setf (gtk:widget-tooltip-text button) "Open File")
      (g:signal-connect button "clicked" #'open-file)
      (gtk:box-append box2 button))

    (let ((path (glib-sys:sys-path "resource/portland-rose-thumbnail.png"))
          (button (gtk:button-new)))
      (setf (gtk:button-child button) (gtk:image-new-from-file path))
      (gtk:widget-add-css-class button "image-button")
      (setf (gtk:widget-tooltip-text button) "Portland Rose")
      (g:signal-connect button "clicked" #'open-portland-rose)
      (gtk:box-append box2 button))

    (let ((path (glib-sys:sys-path "resource/large-image-thumbnail.png"))
          (button (gtk:button-new)))
      (setf (gtk:button-child button) (gtk:image-new-from-file path))
      (gtk:widget-add-css-class button "image-button")
      (setf (gtk:widget-tooltip-text button) "Large image")
      (g:signal-connect button "clicked" #'open-large-image)
      (gtk:box-append box2 button))

    (let ((button (gtk:button-new-from-icon-name "object-rotate-right-symbolic")))
      (setf (gtk:widget-tooltip-text button) "Rotate")
      (g:signal-connect button "clicked"
                        (lambda (button)
                          (declare (ignore button))
                          (rotate image)))
      (gtk:box-append box2 button))

    (let ((scale (gtk:scale-new-with-range :horizontal -10.0 10.0 0.1)))
      (gtk:scale-add-mark scale 0.0 :top)
      (setf (gtk:widget-tooltip-text scale) "Zoom")
      ;; gtk_accessible_update_property (GTK_ACCESSIBLE (scale),
      ;;                                 GTK_ACCESSIBLE_PROPERTY_LABEL, "Zoom",
      ;;                                 -1);
      (setf (gtk:range-value scale) 0.0)
      (setf (gtk:widget-hexpand scale) t)

#|
      g_object_bind_property_full (gtk_range_get_adjustment (GTK_RANGE (scale)),
                                   "value",
                                   widget, "scale",
                                   G_BINDING_BIDIRECTIONAL,
                                   transform_to,
                                   transform_from,
                                   NULL, NULL);
|#

      (g:object-bind-property-full (gtk:range-adjustment scale)
                                   "value"
                                   image
                                   "scale"
                                   :bidirectional
                                   #'transform-to
                                   #'transform-from)


      (gtk:box-append box2 scale))

    (let* ((content '("Linear" "Nearest" "Trilinear"))
           (dropdown (gtk:drop-down-new-from-strings content)))
      (setf (gtk:widget-tooltip-text dropdown) "Filter")
      ;; gtk_accessible_update_property (GTK_ACCESSIBLE (dropdown),
      ;;                                 GTK_ACCESSIBLE_PROPERTY_LABEL, "Filter",
      ;;                                 -1);
      (g:object-bind-property dropdown "selected" image "filter" :default)
      (gtk:box-append box2 dropdown))

    ;; Present window
    (gtk:box-append box scrolled)
    (gtk:box-append box box2)
    (gtk:window-present window)
))

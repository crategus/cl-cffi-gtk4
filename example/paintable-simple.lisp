;;;; Simple Paintable
;;;;
;;;; GdkPaintable is an interface used by GTK for drawings of any sort that do
;;;; not require layouting or positioning.
;;;;
;;;; This demo code gives a simple example on how a paintable can be created.
;;;; Paintables can be used in many places inside GTK widgets, but the most
;;;; common usage is inside GtkImage and that's what we're going to do here.

(in-package :gtk4-example)

(trace gobject::create-gobject-from-pointer)
(trace gobject:type-from-instance)
(trace gobject::get-g-object-for-pointer)
(trace cffi:translate-from-foreign)
(trace cffi:convert-from-foreign)
(trace glib::allocate-stable-pointer)
(trace glib::get-stable-pointer-value)


#|

/* First, add the boilerplate for the object itself.
 * This part would normally go in the header.
 */
#define GTK_TYPE_NUCLEAR_ICON (gtk_nuclear_icon_get_type ())
G_DECLARE_FINAL_TYPE (GtkNuclearIcon, gtk_nuclear_icon, GTK, NUCLEAR_ICON, GObject)

/* Declare the struct. */
struct _GtkNuclearIcon
{
  GObject parent_instance;

  /* We store this rotation value here.
   * We are not doing with it here, but it will come in
   * very useful in the followup demos.
   */
  double rotation;
};

struct _GtkNuclearIconClass
{
  GObjectClass parent_class;
};
|#

(in-package :gdk)

(gobject::define-vtable ("GdkPaintable" paintable)
  (:skip parent-instance (:pointer (:struct g:type-interface)))
  ;; Methods of the interface
  (snapshot (:void (paintable (g:object paintable))
                   (snapshot (g:object paintable))
                   (width :double)
                   (height :double)))
  (get-current-image ((g:object paintable)
                      (paintable (g:object paintable))))
  (get-flags (paintable-flags (paintable (g:object paintable))))
  (get-intrinsic-width (:int (paintable (g:object paintable))))
  (get-intrinsic-height (:int (paintable (g:object paintable))))
  (get-intrinsic-aspect-ratio (:double (paintable (g:object paintable))))
)

(defclass nuclear-icon (paintable)
  ((rotation :initform 0.0d0
             :accessor nuclear-icon-rotation))
  (:gname . "GdkNuclearIcon")
  (:metaclass gobject:gobject-class))

(export 'nuclear-icon)

(gobject::register-object-type-implementation "GdkNuclearIcon"  ; name
                                              nuclear-icon      ; class
                                              "GObject"         ; parent
                                              ("GdkPaintable")  ; interfaces
                                              nil)              ; properties

#|
/* This is the function that draws the actual icon.
 * We make it a custom function and define it in the paintable.h header
 * so that it can be called from all the other demos, too.
 */
void
gtk_nuclear_snapshot (GtkSnapshot   *snapshot,
                      const GdkRGBA *foreground,
                      const GdkRGBA *background,
                      double         width,
                      double         height,
                      double         rotation)
{
#define RADIUS 0.3
  cairo_t *cr;
  double size;

  gtk_snapshot_append_color (snapshot,
                             background,
                             &GRAPHENE_RECT_INIT (0, 0, width, height));

  size = MIN (width, height);
  cr = gtk_snapshot_append_cairo (snapshot,
                                  &GRAPHENE_RECT_INIT ((width - size) / 2.0,
                                                       (height - size) / 2.0,
                                                       size, size));
  gdk_cairo_set_source_rgba (cr, foreground);
  cairo_translate (cr, width / 2.0, height / 2.0);
  cairo_scale (cr, size, size);
  cairo_rotate (cr, rotation);

  cairo_arc (cr, 0, 0, 0.1, - G_PI, G_PI);
  cairo_fill (cr);

  cairo_set_line_width (cr, RADIUS);
  cairo_set_dash (cr, (double[1]) { RADIUS * G_PI / 3 }, 1, 0.0);
  cairo_arc (cr, 0, 0, RADIUS, - G_PI, G_PI);
  cairo_stroke (cr);

  cairo_destroy (cr);
}
|#

(defun nuclear-snapshot (snapshot foreground background width height rotation)
  (format t "in NUCLEAR-SNAPSHOT~%")
  (let ((size (min width height))
        (radius 0.3)
        (cr nil))
    (graphene:with-graphene-rects ((rect1 0 0 width height)
                                   (rect2 (/ (- width size) 2.0)
                                          (/ (- height size) 2.0)
                                          size size))
      (gtk:snapshot-append-color snapshot background rect1)
      (setf cr (gtk:snapshot-append-cairo snapshot rect2))
      (cairo-set-source-rgba cr foreground)
      (cairo:translate cr (/ width 2.0) (/ height 2.0))
      (cairo:scale cr size size)
      (cairo:rotate cr rotation)

      (cairo:arc cr 0 0 0.1 (- pi) pi)
      (cairo:fill cr)

      (setf (cairo:line-width cr) radius)
      (setf (cairo:dash cr 0.0) (list (/ (* radius pi) 3)))
      (cairo:arc cr 0 0 radius (- pi) pi)
      (cairo:stroke cr)
      (cairo:destroy cr)
)))


#|
/* Here, we implement the functionality required by the GdkPaintable interface */
static void
gtk_nuclear_icon_snapshot (GdkPaintable *paintable,
                           GdkSnapshot  *snapshot,
                           double        width,
                           double        height)
{
  GtkNuclearIcon *nuclear = GTK_NUCLEAR_ICON (paintable);

  /* The snapshot function is the only function we need to implement.
   * It does the actual drawing of the paintable.
   */

  gtk_nuclear_snapshot (snapshot,
                        &(GdkRGBA) { 0, 0, 0, 1 }, /* black */
                        &(GdkRGBA) { 0.9, 0.75, 0.15, 1.0 }, /* yellow */
                        width, height,
                        nuclear->rotation);
}
|#

(defmethod paintable-snapshot-impl ((paintable nuclear-icon) snapshot width height)
  (format t "in NUCLEAR-ICON-SNAPSHOT for ~a~%" paintable)
  (nuclear-snapshot snapshot
                    (rgba-new :red 0 :green 0 :blue 0 :alpha 1)
                    (rgba-new :red 0.9 :green 0.75 :blue 0.15 :alpha 1)
                    width
                    height
                    (nuclear-icon-rotation paintable))
)

(trace paintable-snapshot-impl)
(trace PAINTABLE-SNAPSHOT-CALLBACK)

#|
static GdkPaintableFlags
gtk_nuclear_icon_get_flags (GdkPaintable *paintable)
{
  /* The flags are very useful to let GTK know that this image
   * is never going to change.
   * This allows many optimizations and should therefore always
   * be set.
   */
  return GDK_PAINTABLE_STATIC_CONTENTS | GDK_PAINTABLE_STATIC_SIZE;
}
|#

(defmethod paintable-get-flags-impl ((paintable nuclear-icon))
  (format t "in PAINTABLE-GET-FLAGS-IMPL for ~a~%" paintable)
  (list :static-contents :static-size))
(defmethod paintable-get-flags-impl ((paintable paintable))
  (format t "in PAINTABLE-GET-FLAGS-IMPL for ~a~%" paintable)
  (gdk:paintable-flags paintable))

(trace paintable-get-flags-impl)
(trace paintable-get-flags-callback)

(defmethod paintable-get-intrinsic-width-impl ((paintable nuclear-icon))
  (format t "in PAINTABLE-GET-INTRINSIC-WIDTH-IMPL for ~a~%" paintable)
  0)

(defmethod paintable-get-intrinsic-height-impl ((paintable nuclear-icon))
  (format t "in PAINTABLE-GET-INTRINSIC-HEIGHT-IMPL for ~a~%" paintable)
  0)

(defmethod paintable-get-intrinsic-aspect-ratio-impl ((paintable nuclear-icon))
  (format t "in PAINTABLE-GET-INTRINSIC-ASPECT-RATIO-IMPL for ~a~%" paintable)
  0.0d0)

(defmethod paintable-get-current-image-impl ((paintable nuclear-icon))
  (format t "in PAINTABLE-GET-CURRENT-IMAGE-IMPL for ~a~%" paintable)
  (gdk:paintable-current-image paintable))

#|
static void
gtk_nuclear_icon_paintable_init (GdkPaintableInterface *iface)
{
  iface->snapshot = gtk_nuclear_icon_snapshot;
  iface->get_flags = gtk_nuclear_icon_get_flags;
}



/* When defining the GType, we need to implement the GdkPaintable interface */
G_DEFINE_TYPE_WITH_CODE (GtkNuclearIcon, gtk_nuclear_icon, G_TYPE_OBJECT,
                         G_IMPLEMENT_INTERFACE (GDK_TYPE_PAINTABLE,
                                                gtk_nuclear_icon_paintable_init))

/* Here's the boilerplate for the GObject declaration.
 * We don't need to do anything special here, because we keep no
 * data of our own.
 */
static void
gtk_nuclear_icon_class_init (GtkNuclearIconClass *klass)
{
}

static void
gtk_nuclear_icon_init (GtkNuclearIcon *nuclear)
{
}

/* And finally, we add a simple constructor.
 * It is declared in the header so that the other examples
 * can use it.
 */
GdkPaintable *
gtk_nuclear_icon_new (double rotation)
{
  GtkNuclearIcon *nuclear;

  nuclear = g_object_new (GTK_TYPE_NUCLEAR_ICON, NULL);
  nuclear->rotation = rotation;

  return GDK_PAINTABLE (nuclear);
}

|#

(in-package :gtk4-example)

(defun do-paintable-simple (&optional application)
  (gobject::type-ensure "GdkNuclearIcon")
  (let* ((nuclear (make-instance 'gdk::nuclear-icon))
         (image (gtk:image-new-from-paintable nuclear))
         (window (make-instance 'gtk:window
                                :application application
                                :child image
                                :title "Nuclear Icon"
                                :default-width 300
                                :default-height 200)))
    (format t "Show the window~%")
    (gtk:window-present window)
))


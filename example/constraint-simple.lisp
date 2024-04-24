;;;; Simple Constraints
;;;;
;;;; The <tt>GtkConstraintLayout</tt> object provides a layout manager that uses
;;;; relations between widgets (also known as “constraints”) to compute the
;;;; position and size of each child.
;;;;
;;;; In addition to child widgets, the constraints can involve spacer objects
;;;; (also known as “guides”). This example has a guide between the two buttons
;;;; in the top row.
;;;;
;;;; Try resizing the window to see how the constraints react to update the
;;;; layout.
;;;;
;;;; 2024-4-20

(in-package :gtk4-example)

(gobject:define-g-object-subclass "SimpleGrid" simple-grid
  (:superclass gtk:widget
   :export t
   :interfaces ())
  ((button1
    simple-grid-button1
    "button1" "GtkWidget" t t)
   (button2
    simple-grid-button2
    "button2" "GtkWidget" t t)
   (button3
    simple-grid-button3
    "button3" "GtkWidget" t t)))

(defmethod initialize-instance :after
           ((class (eql 'simple-grid)) &key &allow-other-keys)
  (format t "in INITIALIZE-INSTANCE for gobject-class ~a~%" class))


(defmethod initialize-instance :after ((obj simple-grid) &key)
  (format t "in INITIALIZE-INSTANCE for ~a~%" obj)

  (setf (simple-grid-button1 obj)
        (make-instance 'gtk:button
                       :label "Button 1"
                       :parent obj))
  ;; FIXME: We get an critical error. The parent property is not writable,
  ;; but the C code uses this function to set a parent widget.
  (gtk:widget-set-parent (simple-grid-button1 obj) obj)


;  gtk_widget_class_set_layout_manager_type (widget_class, GTK_TYPE_CONSTRAINT_LAYOUT);

  ;; TODO: This call has to be placed in a class initializer, not the
  ;; instance initializer.
  (setf (gtk:widget-class-layout-manager-type "SimpleGrid")
        "GtkConstraintLayout")

;  GtkLayoutManager *manager = gtk_widget_get_layout_manager (GTK_WIDGET (self));
;  build_constraints (self, GTK_CONSTRAINT_LAYOUT (manager));

  (let ((layout (gtk:widget-layout-manager obj)))
    (format t "  layout-manager is ~a~%" layout)
  )
  )

#|
G_DECLARE_FINAL_TYPE (SimpleGrid, simple_grid, SIMPLE, GRID, GtkWidget)

struct _SimpleGrid
{
  GtkWidget parent_instance;

  GtkWidget *button1, *button2;
  GtkWidget *button3;
};

G_DEFINE_TYPE (SimpleGrid, simple_grid, GTK_TYPE_WIDGET)

static void
simple_grid_dispose (GObject *object)
{
  SimpleGrid *self = SIMPLE_GRID (object);

  g_clear_pointer (&self->button1, gtk_widget_unparent);
  g_clear_pointer (&self->button2, gtk_widget_unparent);
  g_clear_pointer (&self->button3, gtk_widget_unparent);

  G_OBJECT_CLASS (simple_grid_parent_class)->dispose (object);
}

static void
simple_grid_class_init (SimpleGridClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);
  GtkWidgetClass *widget_class = GTK_WIDGET_CLASS (klass);

  object_class->dispose = simple_grid_dispose;

  gtk_widget_class_set_layout_manager_type (widget_class, GTK_TYPE_CONSTRAINT_LAYOUT);
}

/* Layout:
 *
 *   +-------------------------------------+
 *   | +-----------++-------++-----------+ |
 *   | |  Child 1  || Space ||  Child 2  | |
 *   | +-----------++-------++-----------+ |
 *   | +---------------------------------+ |
 *   | |             Child 3             | |
 *   | +---------------------------------+ |
 *   +-------------------------------------+
 *
 * Constraints:
 *
 *   super.start = child1.start - 8
 *   child1.width = child2.width
 *   child1.end = space.start
 *   space.end = child2.start
 *   child2.end = super.end - 8
 *   super.start = child3.start - 8
 *   child3.end = super.end - 8
 *   super.top = child1.top - 8
 *   super.top = child2.top - 8
 *   child1.bottom = child3.top - 12
 *   child2.bottom = child3.top - 12
 *   child3.height = child1.height
 *   child3.height = child2.height
 *   child3.bottom = super.bottom - 8
 *
 * To add some flexibility, we make the space
 * stretchable:
 *
 *   space.width >= 10
 *   space.width = 100
 *   space.width <= 200
 */
static void
build_constraints (SimpleGrid          *self,
                   GtkConstraintLayout *manager)
{
  GtkConstraintGuide *guide;

  guide = gtk_constraint_guide_new ();
  gtk_constraint_guide_set_name (guide, "space");
  gtk_constraint_guide_set_min_size (guide, 10, 10);
  gtk_constraint_guide_set_nat_size (guide, 100, 10);
  gtk_constraint_guide_set_max_size (guide, 200, 20);
  gtk_constraint_guide_set_strength (guide, GTK_CONSTRAINT_STRENGTH_STRONG);
  gtk_constraint_layout_add_guide (manager, guide);

  gtk_constraint_layout_add_constraint (manager,
    gtk_constraint_new_constant (GTK_CONSTRAINT_TARGET (self->button1),
                        GTK_CONSTRAINT_ATTRIBUTE_WIDTH,
                        GTK_CONSTRAINT_RELATION_LE,
                        200.0,
                        GTK_CONSTRAINT_STRENGTH_REQUIRED));
  gtk_constraint_layout_add_constraint (manager,
    gtk_constraint_new (NULL,
                        GTK_CONSTRAINT_ATTRIBUTE_START,
                        GTK_CONSTRAINT_RELATION_EQ,
                        self->button1,
                        GTK_CONSTRAINT_ATTRIBUTE_START,
                        1.0,
                        -8.0,
                        GTK_CONSTRAINT_STRENGTH_REQUIRED));
  gtk_constraint_layout_add_constraint (manager,
    gtk_constraint_new (self->button1,
                        GTK_CONSTRAINT_ATTRIBUTE_WIDTH,
                        GTK_CONSTRAINT_RELATION_EQ,
                        self->button2,
                        GTK_CONSTRAINT_ATTRIBUTE_WIDTH,
                        1.0,
                        0.0,
                        GTK_CONSTRAINT_STRENGTH_REQUIRED));
  gtk_constraint_layout_add_constraint (manager,
    gtk_constraint_new (self->button1,
                        GTK_CONSTRAINT_ATTRIBUTE_END,
                        GTK_CONSTRAINT_RELATION_EQ,
                        guide,
                        GTK_CONSTRAINT_ATTRIBUTE_START,
                        1.0,
                        0.0,
                        GTK_CONSTRAINT_STRENGTH_REQUIRED));
  gtk_constraint_layout_add_constraint (manager,
    gtk_constraint_new (guide,
                        GTK_CONSTRAINT_ATTRIBUTE_END,
                        GTK_CONSTRAINT_RELATION_EQ,
                        self->button2,
                        GTK_CONSTRAINT_ATTRIBUTE_START,
                        1.0,
                        0.0,
                        GTK_CONSTRAINT_STRENGTH_REQUIRED));
  gtk_constraint_layout_add_constraint (manager,
    gtk_constraint_new (self->button2,
                        GTK_CONSTRAINT_ATTRIBUTE_END,
                        GTK_CONSTRAINT_RELATION_EQ,
                        NULL,
                        GTK_CONSTRAINT_ATTRIBUTE_END,
                        1.0,
                        -8.0,
                        GTK_CONSTRAINT_STRENGTH_REQUIRED));
  gtk_constraint_layout_add_constraint (manager,
    gtk_constraint_new (NULL,
                        GTK_CONSTRAINT_ATTRIBUTE_START,
                        GTK_CONSTRAINT_RELATION_EQ,
                        self->button3,
                        GTK_CONSTRAINT_ATTRIBUTE_START,
                        1.0,
                        -8.0,
                        GTK_CONSTRAINT_STRENGTH_REQUIRED));
  gtk_constraint_layout_add_constraint (manager,
    gtk_constraint_new (self->button3,
                        GTK_CONSTRAINT_ATTRIBUTE_END,
                        GTK_CONSTRAINT_RELATION_EQ,
                        NULL,
                        GTK_CONSTRAINT_ATTRIBUTE_END,
                        1.0,
                        -8.0,
                        GTK_CONSTRAINT_STRENGTH_REQUIRED));
  gtk_constraint_layout_add_constraint (manager,
    gtk_constraint_new (NULL,
                        GTK_CONSTRAINT_ATTRIBUTE_TOP,
                        GTK_CONSTRAINT_RELATION_EQ,
                        self->button1,
                        GTK_CONSTRAINT_ATTRIBUTE_TOP,
                        1.0,
                        -8.0,
                        GTK_CONSTRAINT_STRENGTH_REQUIRED));
  gtk_constraint_layout_add_constraint (manager,
    gtk_constraint_new (NULL,
                        GTK_CONSTRAINT_ATTRIBUTE_TOP,
                        GTK_CONSTRAINT_RELATION_EQ,
                        self->button2,
                        GTK_CONSTRAINT_ATTRIBUTE_TOP,
                        1.0,
                        -8.0,
                        GTK_CONSTRAINT_STRENGTH_REQUIRED));
  gtk_constraint_layout_add_constraint (manager,
    gtk_constraint_new (self->button1,
                        GTK_CONSTRAINT_ATTRIBUTE_BOTTOM,
                        GTK_CONSTRAINT_RELATION_EQ,
                        self->button3,
                        GTK_CONSTRAINT_ATTRIBUTE_TOP,
                        1.0,
                        -12.0,
                        GTK_CONSTRAINT_STRENGTH_REQUIRED));
  gtk_constraint_layout_add_constraint (manager,
    gtk_constraint_new (self->button2,
                        GTK_CONSTRAINT_ATTRIBUTE_BOTTOM,
                        GTK_CONSTRAINT_RELATION_EQ,
                        self->button3,
                        GTK_CONSTRAINT_ATTRIBUTE_TOP,
                        1.0,
                        -12.0,
                        GTK_CONSTRAINT_STRENGTH_REQUIRED));
  gtk_constraint_layout_add_constraint (manager,
    gtk_constraint_new (self->button3,
                        GTK_CONSTRAINT_ATTRIBUTE_HEIGHT,
                        GTK_CONSTRAINT_RELATION_EQ,
                        self->button1,
                        GTK_CONSTRAINT_ATTRIBUTE_HEIGHT,
                        1.0,
                        0.0,
                        GTK_CONSTRAINT_STRENGTH_REQUIRED));
  gtk_constraint_layout_add_constraint (manager,
    gtk_constraint_new (self->button3,
                        GTK_CONSTRAINT_ATTRIBUTE_HEIGHT,
                        GTK_CONSTRAINT_RELATION_EQ,
                        self->button2,
                        GTK_CONSTRAINT_ATTRIBUTE_HEIGHT,
                        1.0,
                        0.0,
                        GTK_CONSTRAINT_STRENGTH_REQUIRED));
  gtk_constraint_layout_add_constraint (manager,
    gtk_constraint_new (self->button3,
                        GTK_CONSTRAINT_ATTRIBUTE_BOTTOM,
                        GTK_CONSTRAINT_RELATION_EQ,
                        NULL,
                        GTK_CONSTRAINT_ATTRIBUTE_BOTTOM,
                        1.0,
                        -8.0,
                        GTK_CONSTRAINT_STRENGTH_REQUIRED));
}

static void
simple_grid_init (SimpleGrid *self)
{
  GtkWidget *widget = GTK_WIDGET (self);

  self->button1 = gtk_button_new_with_label ("Child 1");
  gtk_widget_set_parent (self->button1, widget);

  self->button2 = gtk_button_new_with_label ("Child 2");
  gtk_widget_set_parent (self->button2, widget);

  self->button3 = gtk_button_new_with_label ("Child 3");
  gtk_widget_set_parent (self->button3, widget);

  GtkLayoutManager *manager = gtk_widget_get_layout_manager (GTK_WIDGET (self));
  build_constraints (self, GTK_CONSTRAINT_LAYOUT (manager));
}

|#

(defun do-constraint-simple (&optional application)
  (let* ((box (make-instance 'gtk:box
                             :orientation :vertical
                             :spacing 12))
         (window (make-instance 'gtk:window
                                :title "Simple Constraints"
                                :application application
                                :child box
                                :default-width 260))
         (grid (make-instance 'simple-grid)))
    (gtk:box-append box grid)
    (gtk:window-present window)))

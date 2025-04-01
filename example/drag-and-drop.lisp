;;;; Drag and Drop
;;;;
;;;; This demo shows dragging colors and widgets. The items in this demo can be
;;;; moved, recolored and rotated. The demo also has an example for creating a
;;;; menu-like popover without using a menu model.
;;;;
;;;; 2025-2-11

(in-package :gtk4-example)

(defvar *n-items* 0)

(gobject:define-gobject-subclass "CanvasItem" canvas-item
  (:superclass gtk:widget
   :export t
   :interfaces ())
  ((parent
    canvas-item-parent
    "parent" "GtkWidget" t t)
   (fixed
    canvas-item-fixed
    "fixed" "GtkWidget" t t)
   (label
    canvas-item-label
    "label" "GtkWidget" t t)
   (hotspot
    canvas-item-hotspot
    "hotspot" "gdouble" t t)
   (angle
    canvas-item-angle
    "angle" "gdouble" t t)
   (editor
    canvas-item-editor
    "editor" "GtkWidget" t t)
   ;; FIXME: GtkStyleProvider is unknown when compiling. What is wrong?
   (provider
    canvas-item-provider
    "provider" "GObject" t t)
   (css-class
    canvas-item-css-class
    "css-class" "gchararray" t t)))

;; FIXME: Does not work. The method is not called during initialization.
(defmethod initialize-instance :after
           ((class (eql (find-class 'canvas-item))) &key)
  (format t "in NEW INITIALIZE-INSTANCE :after for ~a~%" class)
  (setf (gtk:widget-class-layout-manager-type "CanvasItem") "GtkBinLayout"))

;;; ----------------------------------------------------------------------------

(defun unstyle-item (item)
  (let* ((label (canvas-item-label item))
         (display (gtk:widget-display label))
         (provider (canvas-item-provider item))
         (css-class (canvas-item-css-class item)))
  (when provider
    (gtk:style-context-remove-provider-for-display display provider)
    (setf (canvas-item-provider item) nil))
  (when css-class
    (gtk:widget-remove-css-class label css-class)
    (setf (canvas-item-css-class item) nil))))

(defun set-color (item color)
  (let* ((str (gdk:rgba-to-string color))
         (name (gtk:widget-name (canvas-item-label item)))
         (css (format nil "#~a { background: ~a; }" name str))
         (provider (gtk:css-provider-new))
         (label (canvas-item-label item))
         (display (gtk:widget-display label)))
  (unstyle-item item)
  (gtk:css-provider-load-from-string provider css)
  (gtk:style-context-add-provider-for-display display provider 700)
  (setf (canvas-item-provider item) provider)))

(defun set-css (item css-class)
  (unstyle-item item)
  (gtk:widget-add-css-class (canvas-item-label item) css-class)
  (setf (canvas-item-css-class item) css-class))

(defun apply-transform (item)
  (let ((label (canvas-item-label item))
        (transform (gsk:transform-new)))
    (graphene:with-rect (bounds)
      (when (gtk:widget-compute-bounds label label bounds)
        (let* ((x (/ (graphene:rect-width bounds) 2.0))
               (y (/ (graphene:rect-height bounds) 2.0))
               (hotspot (sqrt (+ (* x x) (* y y)))))

          (setf (canvas-item-hotspot item) hotspot)

          (graphene:with-point (point hotspot hotspot)
            (setf transform (gsk:transform-translate transform point))
            (setf transform
                  (gsk:transform-rotate transform (canvas-item-angle item)))
            (graphene:point-init point (- x) (- y))
            (setf transform (gsk:transform-translate transform point))
            (setf (gtk:fixed-child-transform (canvas-item-fixed item) label)
                  transform)))))))

;;; ----------------------------------------------------------------------------

(defun canvas-item-init (item)

  (incf *n-items*)

  (setf (canvas-item-label item)
        (gtk:label-new (format nil "Item ~a" *n-items*)))
  (gtk:widget-add-css-class (canvas-item-label item) "canvasitem")

  (setf (canvas-item-fixed item) (gtk:fixed-new))
  (gtk:widget-set-parent (canvas-item-fixed item) item)
  (gtk:fixed-put (canvas-item-fixed item) (canvas-item-label item) 0 0)

  (gtk:widget-add-css-class (canvas-item-label item) "frame")

  (setf (gtk:widget-name (canvas-item-label item))
        (format nil "item~a" *n-items*))

  (set-color item (gdk:rgba-parse "orange"))
  (setf (canvas-item-angle item) 0)

  (let ((dest (gtk:drop-target-new nil :copy)))
    (setf (gtk:drop-target-gtypes dest) (list "gchararray" "GdkRGBA"))
    (gtk:widget-add-controller (canvas-item-label item) dest)
    (g:signal-connect dest "drop"
        (lambda (dest value x y)
          (declare (ignore x y))
          (let* ((label (gtk:event-controller-widget dest))
                 (item (gtk:widget-get-parent (gtk:widget-get-parent label))))
            (cond ((eq (g:gtype "gchararray") (g:value-type value))
                   (set-css item (g:value-get value)))
                  ((eq (g:gtype "GdkRGBA") (g:value-type value))
                   (set-color item (g:value-get value)))
                  (t t))
            gdk:+event-stop+))))
  item)

;; TODO: Implement this more comfortable
(defun canvas-item-class-init ()

;  object_class->dispose = canvas_item_dispose;
;  widget_class->map = canvas_item_map;

  (setf (gtk:widget-class-layout-manager-type "CanvasItem") "GtkBinLayout")
  (setf (gtk:widget-class-css-name "CanvasItem") "item"))

(defun canvas-item-is-editing (item)
  (canvas-item-editor item))

(defun canvas-item-stop-editing (item)
  (when (canvas-item-editor item)
    (let ((canvas (gtk:widget-get-parent (canvas-item-editor item)))
          (scale (gtk:widget-last-child (canvas-item-editor item))))
       (declare (ignorable scale))

      (let* ((signalid (g:signal-lookup "value-changed" "GtkScale"))
            (handlerid (g:signal-handler-find scale signalid)))
        (g:signal-handler-disconnect scale handlerid))

      (gtk:fixed-remove canvas (canvas-item-editor item))
      (setf (canvas-item-editor item) nil))))

(defun canvas-item-start-editing (item)
  (unless (canvas-item-editor item)
    (let* ((canvas (gtk:widget-get-parent item))
           (label (canvas-item-label item))
           (entry (make-instance 'gtk:entry
                                 :text (gtk:label-text label)
                                 :width-chars 12))
           (scale (gtk:scale-new-with-range :horizontal 0 360 1)))

      (setf (canvas-item-editor item)
            (make-instance 'gtk:box
                           :orientation :vertical
                           :spacing 6))

      (gtk:box-append (canvas-item-editor item) entry)

      (g:signal-connect entry "notify::text"
          (lambda (editable pspec)
            (declare (ignore pspec))
            (setf (gtk:label-text (canvas-item-label item))
                  (gtk:editable-text editable))
            (apply-transform item)))

      (g:signal-connect entry "activate"
          (lambda (entry)
            (declare (ignore entry))
            (canvas-item-stop-editing item)))

      (setf (gtk:scale-draw-value scale) nil)
      (setf (gtk:range-value scale) (mod (canvas-item-angle item) 360))

      (gtk:box-append (canvas-item-editor item) scale)

      (g:signal-connect scale "value-changed"
          (lambda (range)
            (setf (canvas-item-angle item) (gtk:range-value range))
            (apply-transform item)))

      (graphene:with-points ((point 0 0) out)
        (unless (gtk:widget-compute-point item canvas point out)
          (graphene:point-init out 0 0))
        (gtk:fixed-put canvas
                       (canvas-item-editor item)
                       (graphene:point-x out)
                       (+ (graphene:point-y out)
                          (* 2.1 (canvas-item-hotspot item))))
        (gtk:widget-grab-focus entry)))))

;;; ----------------------------------------------------------------------------

;; Implementation of COLOR-SWATCH subclass

(gobject:define-gobject-subclass "ColorSwatch" color-swatch
  (:superclass gtk:widget
   :export t
   :interfaces ())
  ((color
    color-swatch-color
    "color" "GdkRGBA" t t)))

(gobject:define-vtable ("ColorSwatch" color-swatch)
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
  (:skip size-allocate :pointer)
  (:skip state-flags-changed :pointer)
  (:skip direction-changed :pointer)
  (:skip get-request-mode :pointer)

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

  (snapshot (:void (widget (g:object canvas-item))
                   (snapshot (g:object gtk:snapshot))))

  (:skip contains :pointer)
)

(gobject::install-vtable "ColorSwatch")

#|
static GdkContentProvider *
color_swatch_drag_prepare (GtkDragSource  *source,
                           double          x,
                           double          y,
                           ColorSwatch    *swatch)
{
  return gdk_content_provider_new_typed (GDK_TYPE_RGBA, &swatch->color);
}
|#

(defun swatch-drag-prepare (source x y swatch)
  (declare (ignore source x y))
  (gobject:with-value (gvalue "GdkRGBA" (color-swatch-color swatch))
    (gdk:content-provider-new-for-value gvalue)))


#|
static void
color_swatch_init (ColorSwatch *swatch)
{
  GtkDragSource *source = gtk_drag_source_new ();
  g_signal_connect (source, "prepare", G_CALLBACK (color_swatch_drag_prepare), swatch);
  gtk_widget_add_controller (GTK_WIDGET (swatch), GTK_EVENT_CONTROLLER (source));
}
|#

(defun color-swatch-init (swatch)
  (let ((source (gtk:drag-source-new)))
    (g:signal-connect source "prepare"
            (lambda (source x y)
              (swatch-drag-prepare source x y swatch)))
    (gtk:widget-add-controller swatch source)
    swatch))

(defmethod color-swatch-snapshot-impl (swatch snapshot)
  (let ((w (gtk:widget-width swatch))
        (h (gtk:widget-height swatch))
        (color (color-swatch-color swatch)))
    (graphene:with-rect (bounds 0 0 w h)
      (gtk:snapshot-append-color snapshot color bounds))))

(defmethod color-swatch-measure-impl (swatch orientation
                                      for-size
                                      minimum-size natural-size
                                      minimum-baseline natural-baseline)
  (declare (ignore for-size minimum-baseline natural-baseline))
  (if (eq :horizontal orientation)
      (setf (cffi:mem-ref minimum-size :int) 48
            (cffi:mem-ref natural-size :int) 48)
      (setf (cffi:mem-ref minimum-size :int) 32
            (cffi:mem-ref natural-size :int) 32)))

(defun color-swatch-class-init ()
  (setf (gtk:widget-class-css-name "ColorSwatch") "colorswatch"))

(defun color-swatch-new (color)
  (color-swatch-init (make-instance 'color-swatch
                                    :color (gdk:rgba-parse color))))

;;; ----------------------------------------------------------------------------

(defun canvas-item-new ()
  (canvas-item-init (make-instance 'canvas-item)))

(defun css-button-new (css-class)
  (let ((button (make-instance 'gtk:image
                               :width-request 48
                               :height-request 32))
        (source (gtk:drag-source-new)))
    (gtk:widget-add-css-class button css-class)
    (setf (g:object-data button "css-class") css-class)

    ;; FIXME: This implementation crashes, when we cancel a drag and cancel
    ;; the drag for a second button. The C Gtk Demo has the same failure.
    (g:signal-connect source "prepare"
            (lambda (source x y)
              (declare (ignore x y))
              (let ((css-class (g:object-data button "css-class"))
                    (paintable (gtk:widget-paintable-new button)))
                (gtk:drag-source-set-icon source paintable 0 0)
                (gobject:with-value (gvalue "gchararray" css-class)
                  (gdk:content-provider-new-for-value gvalue)))))

    ;; Note: It is important to implement this handler. The code will crash,
    ;; when it is missing.
    (g:signal-connect source "drag-cancel"
            (lambda (source drag reason)
              (format t "in DRAG-CANCEL~%")
              (format t "   source : ~a~%" source)
              (format t "     drag : ~a~%" drag)
              (format t "   reason : ~a~%" reason)
              gdk:+event-stop+))

    (gtk:widget-add-controller button source)

    button))

(defun canvas-new ()
  (let ((canvas (make-instance 'gtk:fixed
                               :hexpand t
                               :vexpand t))
        (source (gtk:drag-source-new))
        (dest (gtk:drop-target-new "GtkWidget" :move))
        (gesture (gtk:gesture-click-new)))

    ;; Configure drag source
    (setf (gtk:drag-source-actions source) :move)
    (gtk:widget-add-controller canvas source)

    (g:signal-connect source "prepare"
        (lambda (source x y)
          (format t "in PREPARE signal handler~%")
          (let* ((canvas (gtk:event-controller-widget source))
                 (item (gtk:widget-pick canvas x y :default)))
            (setf item (gtk:widget-ancestor item "CanvasItem"))
            (when item
              (setf (g:object-data canvas "dragged-item") item)
              (graphene:with-points ((point x y) (out 0 0))
                (unless (gtk:widget-compute-point canvas item point out)
                  (graphene:point-init out x y))
                (setf (g:object-data canvas "hotspot")
                      (list (truncate (graphene:point-x out))
                            (truncate (graphene:point-y out))))
                (gobject:with-value (gvalue "GtkWidget" item)
                  (gdk:content-provider-new-for-value gvalue)))))))

    (g:signal-connect source "drag-begin"
        (lambda (source drag)
          (declare (ignore drag))
          (format t "in DRAG-BEGIN signal handler~%")
          (let* ((canvas (gtk:event-controller-widget source))
                 (item (g:object-data canvas "dragged-item"))
                 (hotspot (g:object-data canvas "hotspot"))
                 (paintable (gtk:widget-paintable-new (canvas-item-fixed item))))
            (gtk:drag-source-set-icon source paintable
                                             (first hotspot) (second hotspot))
            (setf (gtk:widget-opacity item) 0.3))))

    (g:signal-connect source "drag-cancel"
        (lambda (source drag reason)
          (declare (ignore source drag reason))
          (format t "in DRAG-CANCEL signal handler~%")
          gdk:+event-stop+))

    (g:signal-connect source "drag-end"
            (lambda (source drag delete)
              (declare (ignore drag delete))
              (format t "in DRAG-END signal handler~%")
              (let* ((canvas (gtk:event-controller-widget source))
                     (item (g:object-data canvas "dragged-item")))
                (setf (g:object-data canvas "dragged-item") nil)
                (setf (gtk:widget-opacity item) 1.0))))

    ;; Configure drag dest
    (gtk:widget-add-controller canvas dest)

    (g:signal-connect dest "drop"
        (lambda (target value x y)
          (declare (ignore target))
          (let* ((item (g:value-get value))
                 (canvas (gtk:widget-get-parent item))
                 (child (gtk:widget-last-child canvas)))

            (format t "   item : ~a~%" item)
            (format t " canvas : ~a~%" canvas)
            (format t "  child : ~a~%" child)

            (unless (eq item child)
              (gtk:widget-insert-after item canvas child))
            (gtk:fixed-move canvas item
                            (- x (canvas-item-hotspot item))
                            (- y (canvas-item-hotspot item)))
            gdk:+event-stop+)))

    ;; Configure gesture
    (setf (gtk:gesture-single-button gesture) 0)
    (gtk:widget-add-controller canvas gesture)

    (g:signal-connect gesture "pressed"
        (lambda (gesture n x y)
          (declare (ignore n))
          (format t "in PRESSED signal ~a ~a ~a~%" gesture x y)
          (let* ((widget (gtk:event-controller-widget gesture))
                 (child (gtk:widget-pick widget x y :default)))
            (setf child
                  (gtk:widget-ancestor child "CanvasItem"))
            (when (= gdk:+button-secondary+
                     (gtk:gesture-single-current-button gesture))
              (let* ((box (make-instance 'gtk:box
                                         :orientation :vertical))
                     (menu (make-instance 'gtk:popover-menu
                                          :child box
                                          :has-arrow nil))
                     item)
                (gtk:widget-set-parent menu widget)
                (setf (gtk:popover-pointing-to menu)
                      (gdk:rectangle-new :x (truncate x)
                                         :y (truncate y)
                                         :width 1
                                         :height 1))
                (setf item (make-instance 'gtk:button
                                          :label "New"
                                          :has-frame nil))
                (gtk:box-append box item)

                (g:signal-connect item "clicked"
                    (lambda (button)
                      (format t "in CLICKED signal ~a~%" button)
                      (let* ((popover (gtk:widget-ancestor button "GtkPopover"))
                             (rect (gtk:popover-pointing-to popover))
                             (item (canvas-item-new)))
                        (gtk:fixed-put canvas item
                                       (gdk:rectangle-x rect)
                                       (gdk:rectangle-y rect))
                        (apply-transform item)
                        (gtk:popover-popdown popover))))

                (gtk:box-append box (gtk:separator-new :horizontal))

                (setf item
                      (make-instance 'gtk:button
                                     :label "Edit"
                                     :has-frame nil
                                     :sensitive
                                     (and child (not (eq child widget)))))
                (gtk:box-append box item)

                (g:signal-connect item "clicked"
                    (lambda (button)
                      (let ((item child))
                        (when button
                          (gtk:popover-popdown
                              (gtk:widget-ancestor button "GtkPopver")))
                        (unless (canvas-item-is-editing item)
                          (canvas-item-start-editing item)))))

                (gtk:box-append box (gtk:separator-new :horizontal))

                (setf item
                      (make-instance 'gtk:button
                                     :label "Delete"
                                     :has-frame nil
                                     :sensitive
                                     (and child (not (eq child widget)))))
                (gtk:box-append box item)
                (g:signal-connect item "clicked"
                    (lambda (button)
                      (let ((canvas (gtk:widget-get-parent child)))
                        (gtk:fixed-remove canvas child)
                        (gtk:popover-popdown
                            (gtk:widget-ancestor button "GtkPopover")))))
                (gtk:popover-popup menu))))))

    (g:signal-connect gesture "released"
        (lambda (gesture n x y)
          (declare (ignore n))
          (let* ((widget (gtk:event-controller-widget gesture))
                 (child (gtk:widget-pick widget x y 0))
                 (item (gtk:widget-ancestor child "CanvasItem")))
            (when (and item
                       (= gdk:+button-primary+
                          (gtk:gesture-single-current-button gesture)))
              (if (canvas-item-is-editing item)
                  (canvas-item-stop-editing item)
                  (canvas-item-start-editing item))))))
    canvas))

(defun do-drag-and-drop (&optional application)
  (let* ((colors '("red"     "green"   "blue"    "magenta" "orange"
                   "yellow"  "brown"   "pink"    "cyan"    "bisque"
                   "gold"    "maroon"  "navy"    "orchid"  "olive"
                   "peru"    "salmon"  "silver"  "wheat"))
         (path (glib-sys:sys-path "resource/drag-and-drop.css"))
         (dialog (gtk:color-dialog-new))
         (button (gtk:color-dialog-button-new dialog))
         (box (gtk:box-new :vertical))
         (window (make-instance 'gtk:window
                                :title "Drag and Drop"
                                :application application
                                :child box
                                :default-width 640
                                :default-height 480)))
    (declare (ignorable button))
    (let ((provider (gtk:css-provider-new)))
      (gtk:css-provider-load-from-path provider path)
      (gtk:style-context-add-provider-for-display (gdk:display-default)
                                                  provider
                                                  gtk:+priority-user+))

    (let ((css ""))
      (dolist (color colors)
        (setf css
              (concatenate 'string
                           css
                           (format nil ".canvasitem.~a { background: ~a; }~%"
                                       color
                                       color))))
      ;; TODO: This should be done when initializing the class. Improve this!
      (canvas-item-class-init)
      (color-swatch-class-init)

      (let ((provider (gtk:css-provider-new)))
        (gtk:css-provider-load-from-string provider css)
        (gtk:style-context-add-provider-for-display (gdk:display-default)
                                                    provider
                                                    gtk:+priority-user+)))

      (let ((canvas (canvas-new))
            (box2 (gtk:box-new :horizontal)))
        (gtk:box-append box2 canvas)
        (gtk:box-append box box2)

        (let ((x 40) (y 40))
          (dotimes (i 4)
            (let ((item (canvas-item-new)))
              (gtk:fixed-put canvas item x y)
              (format t "created item : ~a ~a~%"
                        item
                        (gtk:widget-get-parent item))
              (apply-transform item)
              (setf x (+ x 100))
              (setf y (+ y 100))))))

    (let* ((box3 (gtk:box-new :horizontal))
           (scrolled (make-instance 'gtk:scrolled-window
                                    :child box3
                                    :hscroll-policy :automatic
                                    :vscroll-policy :never)))

      (gtk:box-append box (gtk:separator-new :horizontal))
      (gtk:box-append box scrolled)

      (gtk:widget-add-css-class box3 "linked")

      (gtk:box-append box3 (css-button-new "rainbow1"))
      (gtk:box-append box3 (css-button-new "rainbow2"))
      (gtk:box-append box3 (css-button-new "rainbow3"))

      (dolist (color colors)
        (gtk:box-append box3 (color-swatch-new color))))

    (gtk:window-present window)))

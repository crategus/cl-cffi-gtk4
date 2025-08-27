;;;; Drag and Drop
;;;;
;;;; This demo shows dragging colors and widgets. The items in this demo can be
;;;; moved, recolored and rotated. The demo also has an example for creating a
;;;; menu-like popover without using a menu model.
;;;;
;;;; 2025-08-24

(in-package :gtk4-example)

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
  ;; Install virtual measure method
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
  ;; Install virtual snapshot method
  (snapshot (:void (widget (g:object canvas-item))
                   (snapshot (g:object gtk:snapshot))))
  (:skip contains :pointer))

(defmethod gobject:object-class-init :after
           ((subclass (eql (find-class 'color-swatch))) class data)
  (setf (gtk:widget-class-css-name "ColorSwatch") "colorswatch"))

(defmethod color-swatch-measure-impl
           ((swatch color-swatch)
            orientation for-size min-size nat-size min-baseline nat-baseline)
  (declare (ignore for-size min-baseline nat-baseline))
  (if (eq :horizontal orientation)
      (setf (cffi:mem-ref min-size :int) 48
            (cffi:mem-ref nat-size :int) 48)
      (setf (cffi:mem-ref min-size :int) 32
            (cffi:mem-ref nat-size :int) 32)))

(defmethod color-swatch-snapshot-impl ((swatch color-swatch) snapshot)
  (let ((width (gtk:widget-width swatch))
        (height (gtk:widget-height swatch))
        (color (color-swatch-color swatch)))
    (graphene:with-rect (bounds 0 0 width height)
      (gtk:snapshot-append-color snapshot color bounds))))

(defun color-swatch-init (swatch)
  (let ((source (gtk:drag-source-new)))
    (g:signal-connect source "prepare"
            (lambda (source x y)
              (declare (ignore source x y))
              (g:with-value (gvalue "GdkRGBA" (color-swatch-color swatch))
                (gdk:content-provider-new-for-value gvalue))))
    (gtk:widget-add-controller swatch source)
    swatch))

(defun color-swatch-new (color)
  (color-swatch-init (make-instance 'color-swatch
                                    :color (gdk:rgba-parse color))))

;;; ----------------------------------------------------------------------------

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

(defmethod gobject:object-class-init :after
           ((subclass (eql (find-class 'canvas-item))) class data)
  (setf (gtk:widget-class-css-name "CanvasItem") "item")
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
  (when (and css-class (< 0 (length css-class)))
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
  (gtk:widget-add-css-class (canvas-item-label item) "frame")
  (setf (canvas-item-fixed item) (gtk:fixed-new))
  (gtk:widget-set-parent (canvas-item-fixed item) item)
  (gtk:fixed-put (canvas-item-fixed item) (canvas-item-label item) 0 0)
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

(defun canvas-item-is-editing (item)
  (canvas-item-editor item))

(defun canvas-item-stop-editing (item)
  (when (canvas-item-editor item)
    (let* ((canvas (gtk:widget-get-parent (canvas-item-editor item)))
           (scale (gtk:widget-last-child (canvas-item-editor item)))
           (signalid (g:signal-lookup "value-changed" "GtkScale"))
           (handlerid (g:signal-handler-find scale signalid)))
      (g:signal-handler-disconnect scale handlerid)
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

(defun canvas-item-new ()
  (canvas-item-init (make-instance 'canvas-item)))

(defun css-button-new (css-class)
  (let ((button (make-instance 'gtk:image
                               :width-request 48
                               :height-request 32))
        (source (gtk:drag-source-new)))
    (gtk:widget-add-css-class button css-class)
    (setf (g:object-data button "css-class") css-class)
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
              (declare (ignore source drag reason))
              gdk:+event-stop+))
    (gtk:widget-add-controller button source)
    button))

;;; ----------------------------------------------------------------------------

(defun canvas-new ()
  (let ((canvas (make-instance 'gtk:fixed
                               :hexpand t
                               :vexpand t))
        (source (gtk:drag-source-new))
        (dest (gtk:drop-target-new "GtkWidget" :move))
        (gesture (gtk:gesture-click-new)))
    ;; Configure Canvas as drag source
    (setf (gtk:drag-source-actions source) :move)
    (gtk:widget-add-controller canvas source)
    (g:signal-connect source "prepare"
        (lambda (source x y)
          (let* ((canvas (gtk:event-controller-widget source))
                 (pick (gtk:widget-pick canvas x y :default))
                 (item (gtk:widget-ancestor pick "CanvasItem")))
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
          (let* ((canvas (gtk:event-controller-widget source))
                 (item (g:object-data canvas "dragged-item"))
                 (hotspot (g:object-data canvas "hotspot"))
                 (paintable (gtk:widget-paintable-new (canvas-item-fixed item))))
            (gtk:drag-source-set-icon source paintable
                                             (first hotspot)
                                             (second hotspot))
            (setf (gtk:widget-opacity item) 0.3))))
    (g:signal-connect source "drag-cancel"
        (lambda (source drag reason)
          (declare (ignore source drag reason))
          gdk:+event-stop+))
    (g:signal-connect source "drag-end"
         (lambda (source drag delete)
           (declare (ignore drag delete))
           (let* ((canvas (gtk:event-controller-widget source))
                  (item (g:object-data canvas "dragged-item")))
             (setf (g:object-data canvas "dragged-item") nil)
             (setf (gtk:widget-opacity item) 1.0))))
    ;; Configure Canvas as a drag destination
    (gtk:widget-add-controller canvas dest)
    (g:signal-connect dest "drop"
        (lambda (target value x y)
          (declare (ignore target))
          (let* ((item (g:value-get value))
                 (canvas (gtk:widget-get-parent item))
                 (child (gtk:widget-last-child canvas)))
            (unless (eq item child)
              (gtk:widget-insert-after item canvas child))
            (gtk:fixed-move canvas item
                            (- x (canvas-item-hotspot item))
                            (- y (canvas-item-hotspot item)))
            gdk:+event-stop+)))
    ;; Configure gesture
    (setf (gtk:gesture-single-button gesture) 0)
    (gtk:widget-add-controller canvas gesture)
    ;; Configure Popover for right click on Canvas or CanvasItem
    ;; TODO: We get the following warning for a right click. What is wrong?!
    ;; Gtk-CRITICAL: gtk_stack_set_visible_child_name:
    ;; assertion 'GTK_IS_STACK (stack)' failed
    (g:signal-connect gesture "pressed"
        (lambda (gesture n x y)
          (declare (ignore n))
          (when (= gdk:+button-secondary+
                   (gtk:gesture-single-current-button gesture))
            (let* ((canvas (gtk:event-controller-widget gesture))
                   (pick (gtk:widget-pick canvas x y :default))
                   (item  (gtk:widget-ancestor pick "CanvasItem"))
                   (box (make-instance 'gtk:box
                                       :orientation :vertical))
                   ;; Put the menu in a scrolled window
                   (scrolled (make-instance 'gtk:scrolled-window
                                            :child box
                                            :propagate-natural-height t
                                            :propagate-natural-width t))
                   (menu (make-instance 'gtk:popover-menu
                                        :child scrolled
                                        :has-arrow nil))
                   menuitem)
              ;; Menu Button "New"
              (gtk:widget-set-parent menu canvas)
              (setf (gtk:popover-pointing-to menu)
                    (gdk:rectangle-new :x (truncate x)
                                       :y (truncate y)
                                       :width 1
                                       :height 1))
              (setf menuitem (make-instance 'gtk:button
                                            :label "New"
                                            :has-frame nil))
              (gtk:box-append box menuitem)
              (g:signal-connect menuitem "clicked"
                  (lambda (button)
                    (let* ((popover (gtk:widget-ancestor button "GtkPopover"))
                           (rect (gtk:popover-pointing-to popover))
                           (item (canvas-item-new)))
                      (gtk:fixed-put canvas item
                                     (gdk:rectangle-x rect)
                                     (gdk:rectangle-y rect))
                      (apply-transform item)
                      (gtk:popover-popdown popover))))
              ;; Add a horizontal line
              (gtk:box-append box (gtk:separator-new :horizontal))
              ;; Menu Button "Edit"
              (setf menuitem
                    (make-instance 'gtk:button
                                   :label "Edit"
                                   :has-frame nil
                                   :sensitive
                                   (and item (not (eq item canvas)))))
              (gtk:box-append box menuitem)
              (g:signal-connect menuitem "clicked"
                  (lambda (button)
                    (when button
                      (gtk:popover-popdown
                          (gtk:widget-ancestor button "GtkPopover")))
                    (unless (canvas-item-is-editing item)
                      (canvas-item-start-editing item))))
              ;; Add a horizontal line
              (gtk:box-append box (gtk:separator-new :horizontal))
              ;; Menu Button "Delete"
              (setf menuitem
                    (make-instance 'gtk:button
                                   :label "Delete"
                                   :has-frame nil
                                   :sensitive
                                   (and item (not (eq item canvas)))))
              (gtk:box-append box menuitem)
              (g:signal-connect menuitem "clicked"
                  (lambda (button)
                    (let ((canvas (gtk:widget-get-parent item)))
                      (gtk:fixed-remove canvas item)
                      (gtk:popover-popdown
                          (gtk:widget-ancestor button "GtkPopover")))))
              (gtk:popover-popup menu)))))
    ;; Pointer is released on a CanvasItem, start or stop editing the CanvasItem
    (g:signal-connect gesture "released"
        (lambda (gesture n x y)
          (declare (ignore n))
          (let* ((canvas (gtk:event-controller-widget gesture))
                 (pick (gtk:widget-pick canvas x y 0))
                 (item (gtk:widget-ancestor pick "CanvasItem")))
            (when (and item
                       (= gdk:+button-primary+
                          (gtk:gesture-single-current-button gesture)))
              (if (canvas-item-is-editing item)
                  (canvas-item-stop-editing item)
                  (canvas-item-start-editing item))))))
    canvas))

;;; ----------------------------------------------------------------------------

(defun do-drag-and-drop (&optional application)
  (let* ((colors '("olive"   "orange" "yellow"  "brown"   "pink"
                   "cyan"    "bisque" "gold"    "maroon"  "orchid"))
         (path (glib-sys:sys-path "resource/drag-and-drop.css"))
         (box (gtk:box-new :vertical))
         (window (make-instance 'gtk:window
                                :title "Drag and Drop"
                                :application application
                                :child box
                                :default-width 620
                                :default-height 480))
         (provider (gtk:css-provider-new))
         (css ""))
    ;; Load CSS from resources and add style provider
    (gtk:css-provider-load-from-path provider path)
    (gtk:widget-add-provider window provider)
    ;; Create CSS data for the items
    (dolist (color colors)
      (setf css
            (concatenate 'string
                         css
                         (format nil ".canvasitem.~a { background: ~a; }~%"
                                     color
                                     color))))
    ;; Add a second style provider with CSS data
    (setf provider (gtk:css-provider-new))
    (gtk:widget-add-provider window provider)
    ;; Create canvas and add items
    (let ((canvas (canvas-new))
          (x (* 1/10 (gtk:window-default-width window)))
          (y (* 1/10 (gtk:window-default-height window)))
          (width (* 7/10 (gtk:window-default-width window)))
          (height (* 6/10 (gtk:window-default-height window))))
      ;; Put the canvas in vertical box
      (gtk:box-append box canvas)
      (dotimes (i 4)
        (let ((item (canvas-item-new)))
          ;; Put 4 items at random positions in the canvas
          (gtk:fixed-put canvas item (+ x (random width)) (+ y (random height)))
          (apply-transform item))))
    ;; Create a scrollable horizontal box for color buttons
    (let* ((hbox (gtk:box-new :horizontal))
           (scrolled (make-instance 'gtk:scrolled-window
                                    :child hbox
                                    :hscroll-policy :automatic
                                    :vscroll-policy :never)))
      (gtk:box-append box (gtk:separator-new :horizontal))
      (gtk:box-append box scrolled)
      (gtk:widget-add-css-class hbox "linked")
      ;; Append three special color buttons
      (gtk:box-append hbox (css-button-new "rainbow1"))
      (gtk:box-append hbox (css-button-new "rainbow2"))
      (gtk:box-append hbox (css-button-new "rainbow3"))
      ;; Append more color buttons
      (dolist (color colors)
        (gtk:box-append hbox (color-swatch-new color))))
    ;; Present window
    (gtk:window-present window)))

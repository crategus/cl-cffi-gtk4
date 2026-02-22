;;;; Paint
;;;;
;;;; 2026-02-21

(in-package :gtk4-example)

(defvar pad-actions
        (list (list :button 1 -1 "Black" "pad.black")
              (list :button 2 -1 "Pink" "pad.pink")
              (list :button 3 -1 "Green" "pad.green")
              (list :button 4 -1 "Red" "pad.red")
              (list :button 5 -1 "Purple" "pad.purple")
              (list :button 6 -1 "Orange" "pad.oragne")
              (list :strip -1 -1 "Brush size" "pad.brush-size")
              (list :dial  -1 -1 "Brush size" "pad.change-brush-size")))

(gobject:define-gobject-subclass "DrawingArea" drawing-area
  (:superclass gtk:widget
   :export t
   :interfaces ())
  ((color
    drawing-area-color
    "color" "GdkRGBA" t t)
   (color-set
    drawing-area-color-set
    "color-set" "gboolean" t t)
   (context
    drawing-area-context
    "context" "gpointer" t t)
   (controller
    drawing-area-controller
    "controller" "GObject" t t)
   (gesture
    drawing-area-gesture
    "gesture" "GObject" t t)
   (surface
    drawing-area-surface
    "surface" "gpointer" t t)
   (size
    drawing-area-size
    "size" "gdouble" t t)))

(gobject:define-vtable ("DrawingArea" drawing-area)
  ;; Skip over vtable for GObject class
  (:skip parent-instance (:struct gobject:object-class))
  ;; Methods for the GtkWidget class
  (:skip show :pointer)
  (:skip hide :pointer)
  ;; Install virtual map and unmap methods
  (map (:void (widget (g:object drawing-area))) :chained :after)
  (unmap (:void (widget (g:object drawing-area))) :chained :before)
  (:skip realize :pointer)
  (:skip unrealize :pointer)
  ;; Install virtual root and unroot methods
  (root (:void (widget (g:object drawing-area))) :chained :after)
  (unroot (:void (widget (g:object drawing-area))) :chained :before)
  ;; Install virtual size-allocate method
  (size-allocate (:void (widget (g:object drawing-area))
                        (width :int)
                        (height :int)
                        (baseline :int)) :chained :before)
  (:skip state-flags-changed :pointer)
  (:skip direction-changed :pointer)
  (:skip get-request-mode :pointer)
  (:skip measure :pointer)
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
  (snapshot (:void (widget (g:object drawing-area))
                   (snapshot (g:object gtk:snapshot))))
  (:skip contains :pointer))

(defun drawing-area-ensure-surface (area width height)
  (let ((surface (drawing-area-surface area)))
    (unless (and surface
                 (not (cffi:null-pointer-p surface))
                 (= width (cairo:image-surface-width surface))
                 (= height (cairo:image-surface-height surface)))
      (let ((surface1 (cairo:image-surface-create :argb32 width height)))
        (when (and surface (not (cffi:null-pointer-p surface)))
          (let ((context (cairo:create surface1)))
            (cairo:set-source-surface context surface 0 0)
            (cairo:paint context)
            (cairo:surface-destroy surface)
            (cairo:destroy (drawing-area-context area))
            (cairo:destroy context)))
        (setf (drawing-area-surface area) surface1)
        (setf (drawing-area-context area) (cairo:create surface1))))))

(defmethod drawing-area-size-allocate-impl
           ((area drawing-area) width height baseline)
  (drawing-area-ensure-surface area width height))

(defmethod drawing-area-map-impl ((area drawing-area))
  (let ((width (gtk:widget-width area))
        (height (gtk:widget-height area)))
    (drawing-area-ensure-surface area width height)))

(defmethod drawing-area-unmap-impl ((area drawing-area))
  (cairo:destroy (drawing-area-context area))
  (cairo:surface-destroy (drawing-area-surface area)))

(defmethod drawing-area-root-impl ((area drawing-area))
  (let* ((toplevel (gtk:widget-root area))
         (actiongroup (g:simple-action-group-new))
         (controller (gtk:pad-controller-new actiongroup)))
    (setf (drawing-area-controller area) controller)
    (dolist (padaction pad-actions)
      (cond ((eq :button (first padaction))
             (let ((action (g:simple-action-new (fifth padaction))))
               (setf (g:object-data action "color") (fourth padaction))
               (g:signal-connect action "activate"
                       (lambda (action param)
                         (declare (ignore action param))
                         (let* ((color (g:object-data action "color"))
                                (rgba (gdk:rgba-parse color)))
                           (setf (drawing-area-color area) rgba))))
               (g:action-map-add-action actiongroup action)))
            ((eq :strip (first padaction))
             (let* ((state (g:variant-new-double 0))
                    (action (g:simple-action-new-stateful (fifth padaction)
                                                          "d"
                                                          state)))
               (g:signal-connect action "activate"
                       (lambda (action param)
                         (declare (ignore action))
                         (let ((value (g:variant-double param)))
                           (setf (drawing-area-size area) value))))
               (g:action-map-add-action actiongroup action)
               (g:variant-unref state)))
            ((eq :dial (first padaction))
             (let* ((state (g:variant-new-double 0))
                    (action (g:simple-action-new-stateful (fifth padaction)
                                                          "d"
                                                          state)))
               (g:signal-connect action "activate"
                       (lambda (action param)
                         (declare (ignore action))
                         (let ((value (g:variant-double param)))
                           (setf (drawing-area-size area)
                                 (+ (drawing-area-size area) (/ value 120.0)))
                           (g:variant-unref value))))
               (g:action-map-add-action actiongroup action)
               (g:variant-unref state)))
            (t
             (warn "Unknown action type~%"))))
    (gtk:pad-controller-set-action-entries controller pad-actions)
    (gtk:widget-add-controller toplevel controller)))

(defmethod drawing-area-unroot-impl ((area drawing-area))
  (let ((toplevel (gtk:widget-root area))
        (controller (drawing-area-controller area)))
    (when (and controller
               (not (cffi:null-pointer-p controller)))
      (gtk:widget-remove-controller toplevel controller)
      (setf (drawing-area-controller area) (cffi:null-pointer)))))

(defmethod drawing-area-snapshot-impl ((area drawing-area) snapshot)
  (let ((width (gtk:widget-width area))
        (height (gtk:widget-height area)))
    (graphene:with-rect (bounds 0 0 width height)
      (let ((context (gtk:snapshot-append-cairo snapshot bounds)))
        (cairo:set-source-rgb context 1 1 1)
        (cairo:paint context)
        (cairo:set-source-surface context (drawing-area-surface area) 0 0)
        (cairo:paint context)
        (cairo:set-source-rgb context 0.6 0.6 0.6)
        (cairo:rectangle context 0 0 width height)
        (cairo:stroke context)
        (cairo:destroy context)))))

;;; ----------------------------------------------------------------------------

(defun drawing-area-apply-stroke (area tool x y pressure)
  (let ((context (drawing-area-context area))
        (color (drawing-area-color area)))
    (if (and tool
             (eq :eraser (gdk:device-tool-tool-type tool)))
        (setf (cairo:line-width context)
              (* 4 pressure (drawing-area-size area))
              (cairo:operator context)
              :dest-out)
        (setf (cairo:line-width context)
              (* 4 pressure (drawing-area-size area))
              (cairo:operator context)
              :saturate))
    (cairo:set-source-rgba context
                           (gdk:rgba-red color)
                           (gdk:rgba-green color)
                           (gdk:rgba-blue color)
                           (* pressure (gdk:rgba-alpha color)))
    (cairo:line-to context x y)
    (cairo:stroke context)
    (cairo:move-to context x y)))

(defun stylus-gesture-down (gesture x y area)
  (declare (ignore gesture))
  (cairo:new-path (drawing-area-context area)))

(defun stylus-gesture-motion (gesture x y area)
  (let ((tool (gtk:gesture-stylus-device-tool gesture))
        (backlog (gtk:gesture-stylus-backlog gesture)))
    (if nil ;backlog
        ;; FIXME: This code slows down the application. What is the reason?
        (dolist (event backlog)
          (when (and (member :x (gdk:time-coord-flags event))
                     (member :y (gdk:time-coord-flags event)))
            (let (pressure)
              (setf pressure
                    (if (member :pressure (gdk:time-coord-flags event))
                        (aref (gdk:time-coord-axes event) 5) ; for :pressure
                        1))
              (drawing-area-apply-stroke area tool
                      (aref (gdk:time-coord-axes event) 1) ; for :x
                      (aref (gdk:time-coord-axes event) 2) ; for :y
                      pressure))))
        (let ((pressure (or (gtk:gesture-stylus-axis gesture :pressure) 1)))
          (drawing-area-apply-stroke area tool x y pressure)))
    (gtk:widget-queue-draw area)))

(defmethod initialize-instance :after ((area drawing-area) &key)
  (let ((gesture (gtk:gesture-stylus-new)))
    (g:signal-connect gesture "down"
            (lambda (gesture x y)
              (stylus-gesture-down gesture x y area)))
    (g:signal-connect gesture "motion"
            (lambda (gesture x y)
              (stylus-gesture-motion gesture x y area)))
    (gtk:widget-add-controller area gesture)
    (setf (drawing-area-color area) (gdk:rgba-parse "black"))
    (setf (drawing-area-size area) 1)
    (setf (drawing-area-gesture area) gesture)))

(defun drawing-area-clear (button area)
  (declare (ignore button))
  (let ((width (gtk:widget-width area))
        (height (gtk:widget-height area)))
    (cairo:destroy (drawing-area-context area))
    (cairo:surface-destroy (drawing-area-surface area))
    (setf (drawing-area-surface area) (cffi:null-pointer))
    (setf (drawing-area-context area) (cffi:null-pointer))
    (drawing-area-ensure-surface area width height)
    (gtk:widget-queue-draw area)))

;;; ----------------------------------------------------------------------------

(defun do-drawing-area-paint (&optional application)
  (let* ((headerbar (make-instance 'gtk:header-bar))
         (area (make-instance 'drawing-area))
         (window (make-instance 'gtk:window
                                :title "Paint"
                                :child area
                                :application application
                                :titlebar headerbar)))
    ;; Add color dialog button
    (let ((button (make-instance 'gtk:color-dialog-button
                                 :dialog (make-instance 'gtk:color-dialog)))
          (black (gdk:rgba-parse "black")))
      (g:signal-connect button "notify::rgba"
                        (lambda (button pspec)
                          (declare (ignore pspec))
                          (let ((color (gtk:color-dialog-button-rgba button)))
                            (setf (drawing-area-color area) color))))
      ;; TODO: We cannot implement a new signal "color-set" for the
      ;; drawing-area widget. A g:signal-new function is not implemented!
      ;; g_signal_connect (draw_area, "color-set",
      ;;                   G_CALLBACK (drawing_area_color_set), button);
      (setf (gtk:color-dialog-button-rgba button) black)
      (gtk:header-bar-pack-end headerbar button))
    ;; Add clear button
    (let ((button (gtk:button-new-from-icon-name "view-refresh-symbolic")))
      (g:signal-connect button "clicked"
                        (lambda (button)
                          (drawing-area-clear button area)))
      (gtk:header-bar-pack-end headerbar button))
    ;; Add stylus only button
    (let ((button (gtk:check-button-new-with-label "Stylus only")))
      (g:object-bind-property button "active"
                              (drawing-area-gesture area) "stylus-only"
                              :sync-create)
      (gtk:header-bar-pack-start headerbar button))
    ;; Present window
    (gtk:window-present window)))

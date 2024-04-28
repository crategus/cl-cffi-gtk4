;;;; Interactive Constraints
;;;;
;;;; This example shows how constraints can be updated during user interaction.
;;;; The vertical edge between the buttons can be dragged with the mouse.
;;;;
;;;; 2024-4-28

(in-package :gtk4-example)

(gobject:define-g-object-subclass "InteractiveGrid" interactive-grid
  (:superclass gtk:widget
   :export t
   :interfaces ())
  ((button1
    interactive-grid-button1
    "button1" "GtkWidget" t t)
   (button2
    interactive-grid-button2
    "button2" "GtkWidget" t t)
   (button3
    interactive-grid-button3
    "button3" "GtkWidget" t t)
   (guide
    interactive-grid-guide
    "guide" "GtkConstraintGuide" t t)
   (constraint
    interactive-grid-constraint
    "contraint" "GtkConstraint" t t)))

(defun build-constraint-interactive (widget layout)
  (let ((guide (setf (interactive-grid-guide widget)
                     (gtk:constraint-guide-new))))
    (gtk:constraint-layout-add-guide layout guide)
    (gtk:constraint-layout-add-constraint layout
        (gtk:constraint-new-constant guide
                                     :width
                                     :eq
                                     0.0
                                     :required))
    (gtk:constraint-layout-add-constraint layout
        (gtk:constraint-new nil
                            :start
                            :eq
                            (interactive-grid-button1 widget)
                            :start
                            1.0
                            -8.0
                            :required))
    (gtk:constraint-layout-add-constraint layout
        (gtk:constraint-new (interactive-grid-button1 widget)
                            :end
                            :eq
                            guide
                            :start
                            1.0
                            0.0
                            :required))
    (gtk:constraint-layout-add-constraint layout
        (gtk:constraint-new (interactive-grid-button2 widget)
                            :start
                            :eq
                            guide
                            :end
                            1.0
                            0.0
                            :required))
    (gtk:constraint-layout-add-constraint layout
        (gtk:constraint-new (interactive-grid-button2 widget)
                            :end
                            :eq
                            nil
                            :end
                            1.0
                            -8.0
                            :required))
    (gtk:constraint-layout-add-constraint layout
        (gtk:constraint-new nil
                            :start
                            :eq
                            (interactive-grid-button3 widget)
                            :start
                            1.0
                            -8.0
                            :required))
    (gtk:constraint-layout-add-constraint layout
        (gtk:constraint-new (interactive-grid-button3 widget)
                            :end
                            :eq
                            guide
                            :start
                            1.0
                            0.0
                            :required))
    (gtk:constraint-layout-add-constraint layout
        (gtk:constraint-new nil
                            :top
                            :eq
                            (interactive-grid-button1 widget)
                            :top
                            1.0
                            -8.0
                            :required))
    (gtk:constraint-layout-add-constraint layout
        (gtk:constraint-new (interactive-grid-button2 widget)
                            :top
                            :eq
                            (interactive-grid-button1 widget)
                            :bottom
                            1.0
                            0.0
                            :required))
    (gtk:constraint-layout-add-constraint layout
         (gtk:constraint-new (interactive-grid-button3 widget)
                             :top
                             :eq
                             (interactive-grid-button2 widget)
                             :bottom
                             1.0
                             0.0
                             :required))
    (gtk:constraint-layout-add-constraint layout
        (gtk:constraint-new (interactive-grid-button3 widget)
                            :bottom
                            :eq
                            nil
                            :bottom
                            1.0
                            -8.0
                            :required))))

(defmethod initialize-instance :after ((obj interactive-grid) &key)
  ;; Make button1
  (setf (interactive-grid-button1 obj)
        (make-instance 'gtk:button
                       :label "Child 1"))
  ;; Property parent is not writeable, but the function call works
  (gtk:widget-set-parent (interactive-grid-button1 obj) obj)
  ;; Make button2
  (setf (interactive-grid-button2 obj)
        (make-instance 'gtk:button
                       :label "Child 2"))
  (gtk:widget-set-parent (interactive-grid-button2 obj) obj)
  ;; Make button3
  (setf (interactive-grid-button3 obj)
        (make-instance 'gtk:button
                       :label "Child 3"))
  (gtk:widget-set-parent (interactive-grid-button3 obj) obj)
  ;; TODO: This call has to be placed in a class initializer, not the
  ;; instance initializer
  (setf (gtk:widget-class-layout-manager-type "InteractiveGrid")
        "GtkConstraintLayout")
  ;; Set a new layout manager
  (setf (gtk:widget-layout-manager obj) (gtk:constraint-layout-new))
  ;; Create constraints
  (build-constraint-interactive obj (gtk:widget-layout-manager obj))
  ;; Install an event drag event handler
  (let ((drag (gtk:gesture-drag-new)))
    (gtk:widget-add-controller obj drag)
    (g:signal-connect drag "drag-update"
        (lambda (gesture xoffset yoffset)
          (declare (ignore gesture yoffset))
          (let ((layout (gtk:widget-layout-manager obj))
                (x (gtk:gesture-drag-start-point drag)))
            (when (interactive-grid-constraint obj)
              (gtk:constraint-layout-remove-constraint
                      layout
                      (interactive-grid-constraint obj)))
            (setf (interactive-grid-constraint obj)
                  (gtk:constraint-new-constant (interactive-grid-guide obj)
                                               :left
                                               :eq
                                               (+ xoffset x)
                                               :required))
            (gtk:constraint-layout-add-constraint
                    layout
                    (interactive-grid-constraint obj))
            (gtk:widget-queue-allocate obj))))))

(defun do-constraint-interactive (&optional application)
  (let* ((box (make-instance 'gtk:box
                             :orientation :vertical
                             :spacing 12))
         (window (make-instance 'gtk:window
                                :title "Interactive Constraints"
                                :application application
                                :child box
                                :default-width 260))
         (grid (make-instance 'interactive-grid
                              :vexpand t
                              :hexpand t)))
    (gtk:box-append box grid)
    (gtk:window-present window)))

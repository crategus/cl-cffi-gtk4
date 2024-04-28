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
;;;; 2024-4-28

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

;; Layout:
;;
;;   +-------------------------------------+
;;   | +-----------++-------++-----------+ |
;;   | |  Child 1  || Space ||  Child 2  | |
;;   | +-----------++-------++-----------+ |
;;   | +---------------------------------+ |
;;   | |             Child 3             | |
;;   | +---------------------------------+ |
;;   +-------------------------------------+
;;
;; Constraints:
;;
;;   super.start = child1.start - 8
;;   child1.width = child2.width
;;   child1.end = space.start
;;   space.end = child2.start
;;   child2.end = super.end - 8
;;   super.start = child3.start - 8
;;   child3.end = super.end - 8
;;   super.top = child1.top - 8
;;   super.top = child2.top - 8
;;   child1.bottom = child3.top - 12
;;   child2.bottom = child3.top - 12
;;   child3.height = child1.height
;;   child3.height = child2.height
;;   child3.bottom = super.bottom - 8
;;
;; To add some flexibility, we make the space
;; stretchable:
;;
;;   space.width >= 10
;;   space.width = 100
;;   space.width <= 200

(defun build-constraints (object manager)
  (let ((guide (make-instance 'gtk:constraint-guide
                              :name "space"
                              :min-width 10
                              :min-height 10
                              :nat-width 100
                              :nat-height 10
                              :max-width 200
                              :max-height 20
                              :strength :strong)))
    (gtk:constraint-layout-add-guide manager guide)
    (gtk:constraint-layout-add-constraint manager
        (gtk:constraint-new-constant (simple-grid-button1 object)
                                     :width
                                     :le
                                     200
                                     :required))
    (gtk:constraint-layout-add-constraint manager
        (gtk:constraint-new (cffi:null-pointer)
                            :start
                            :eq
                            (simple-grid-button1 object)
                            :start
                             1.0
                            -8.0
                            :required))
    (gtk:constraint-layout-add-constraint manager
        (gtk:constraint-new (simple-grid-button1 object)
                            :width
                            :eq
                            (simple-grid-button2 object)
                            :width
                            1.0
                            0.0
                            :required))
    (gtk:constraint-layout-add-constraint manager
        (gtk:constraint-new (simple-grid-button1 object)
                            :end
                            :eq
                            guide
                            :start
                            1.0
                            0.0
                            :required))
    (gtk:constraint-layout-add-constraint manager
        (gtk:constraint-new guide
                            :end
                            :eq
                            (simple-grid-button2 object)
                            :start
                            1.0
                            0.0
                            :required))
    (gtk:constraint-layout-add-constraint manager
        (gtk:constraint-new (simple-grid-button2 object)
                            :end
                            :eq
                            nil
                            :end
                             1.0
                            -8.0
                            :required))
    (gtk:constraint-layout-add-constraint manager
        (gtk:constraint-new nil
                            :start
                            :eq
                            (simple-grid-button3 object)
                            :start
                             1.0
                            -8.0
                            :required))
    (gtk:constraint-layout-add-constraint manager
        (gtk:constraint-new (simple-grid-button3 object)
                            :end
                            :eq
                            nil
                            :end
                             1.0
                            -8.0
                            :required))
    (gtk:constraint-layout-add-constraint manager
        (gtk:constraint-new nil
                            :top
                            :eq
                            (simple-grid-button1 object)
                            :top
                             1.0
                            -8.0
                            :required))
    (gtk:constraint-layout-add-constraint manager
        (gtk:constraint-new nil
                            :top
                            :eq
                            (simple-grid-button2 object)
                            :top
                             1.0
                            -8.0
                            :required))
    (gtk:constraint-layout-add-constraint manager
        (gtk:constraint-new (simple-grid-button1 object)
                            :bottom
                            :eq
                            (simple-grid-button3 object)
                            :top
                              1.0
                            -12.0
                            :required))
    (gtk:constraint-layout-add-constraint manager
        (gtk:constraint-new (simple-grid-button2 object)
                            :bottom
                            :eq
                            (simple-grid-button3 object)
                            :top
                              1.0
                            -12.0
                            :required))
    (gtk:constraint-layout-add-constraint manager
        (gtk:constraint-new (simple-grid-button3 object)
                            :height
                            :eq
                            (simple-grid-button1 object)
                            :height
                            1.0
                            0.0
                            :required))
    (gtk:constraint-layout-add-constraint manager
        (gtk:constraint-new (simple-grid-button3 object)
                            :height
                            :eq
                            (simple-grid-button2 object)
                            :height
                            1.0
                            0.0
                            :required))
    (gtk:constraint-layout-add-constraint manager
        (gtk:constraint-new (simple-grid-button3 object)
                            :bottom
                            :eq
                            nil
                            :bottom
                             1.0
                            -8.0
                            :required))))

(defmethod initialize-instance :after ((obj simple-grid) &key)
  ;; Make button1
  (setf (simple-grid-button1 obj)
        (make-instance 'gtk:button
                       :label "Child 1"))
  ;; Property parent is not writeable, but the function call works
  (gtk:widget-set-parent (simple-grid-button1 obj) obj)
  ;; Make button2
  (setf (simple-grid-button2 obj)
        (make-instance 'gtk:button
                       :label "Child 2"))
  (gtk:widget-set-parent (simple-grid-button2 obj) obj)
  ;; Make button3
  (setf (simple-grid-button3 obj)
        (make-instance 'gtk:button
                       :label "Child 3"))
  (gtk:widget-set-parent (simple-grid-button3 obj) obj)
  ;; TODO: This call has to be placed in a class initializer, not the
  ;; instance initializer.
  (setf (gtk:widget-class-layout-manager-type "SimpleGrid")
        "GtkConstraintLayout")
  ;; Set a new layout manager
  (setf (gtk:widget-layout-manager obj) (gtk:constraint-layout-new))
  ;; Create constraints
  (build-constraints obj (gtk:widget-layout-manager obj)))

(defun do-constraint-simple (&optional application)
  (let* ((box (make-instance 'gtk:box
                             :orientation :vertical
                             :spacing 12))
         (window (make-instance 'gtk:window
                                :title "Simple Constraints"
                                :application application
                                :child box
                                :default-width 260))
         (grid (make-instance 'simple-grid
                              :vexpand t
                              :hexpand t)))
    (gtk:box-append box grid)
    (gtk:window-present window)))

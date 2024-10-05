;;;; World Clocks
;;;;
;;;; This demo displays the time in different timezones.
;;;;
;;;; The goal is to show how to set up expressions that track changes in objects
;;;; and make them update widgets. For that, we create a clock object that
;;;; updates its time every second and then use various ways to display that
;;;; time.
;;;;
;;;; Typically, this will be done using GtkBuilder .ui files with the help of
;;;; the <binding> tag, but this demo shows the code that runs behind that.
;;;;
;;;; This example uses the local-time libraray for date and time manipulation.
;;;;
;;;; Last version: 2024-5-5

(in-package :gtk)

;; This is our object. It is just a timezone
;;
;; Finally, we define the type. The important part is adding the
;; paintable interface, so GTK knows that this object can indeed
;; be drawn.
;;
;; Initialize the paintable interface. This way we turn our clocks
;; into objects that can be drawn. There are more functions to this
;; interface to define desired size, but this is enough.
(gobject:define-gobject-subclass "GtkClock" clock
  (:superclass g:object
   :export t
   :interfaces ("GdkPaintable"))
  ((:cl
    timezone
    :accessor clock-timezone
    :initform local-time:+utc-zone+)
   (time
    clock-time
    "time" "gchararray" t t)
   (location
    clock-location
    "location" "gchararray" t t)))

(defmethod initialize-instance :after ((obj clock) &key)
  (let ((location (clock-location obj)))
    (local-time:reread-timezone-repository)
    (if (> (length location) 0)
      ;; Set the timezone from the location
      (setf (clock-timezone obj)
            (local-time:find-timezone-by-location-name location))
      ;; Set the location to UTC, that is the default timezone
      (setf (clock-location obj) "UTC"))
      ;; Set the string with the actual time for the clock
      (setf (clock-time obj)
            (local-time:format-timestring nil
                                          (local-time:now)
                                          :format '(:hour ":" :min)
                                          :timezone (clock-timezone obj)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'clock)
  (export 'clock-location)
  (export 'clock-timezone))

(defmethod gdk:paintable-snapshot-impl ((paintable clock) snapshot width height)
  (let ((black (gdk:rgba-new :red 0 :green 0 :blue 0 :alpha 1)))
    ;; save/restore is necessary so we can undo the transforms we start out with
    (gtk:snapshot-save snapshot)
    ;; First, we move the (0, 0) point to the center of the area so
    ;; we can draw everything relative to it.
    (graphene:with-point (point (/ width 2.0) (/ height 2))
      (gtk:snapshot-translate snapshot point))
    ;; Next we scale it, so that we can pretend that the clock is
    ;; 100px in size. That way, we don't need to do any complicated
    ;; math later. We use MIN() here so that we use the smaller
    ;; dimension for sizing. That way we don't overdraw but keep
    ;; the aspect ratio.
    (gtk:snapshot-scale snapshot (/ (min width height) 100.0)
                                 (/ (min width height) 100.0))
    ;; Now we have a circle with diameter 100px (and radius 50px) that
    ;; has its (0, 0) point at the center. Let's draw a simple clock into it.

    ;; First, draw a circle. This is a neat little trick to draw a circle
    ;; without requiring Cairo.
    ;; TODO:Improve the implementation of GskRoundedRect to avoid foreign
    ;; objects
    (cffi:with-foreign-object (outline '(:struct gsk:rounded-rect))
      (graphene:with-rect (rect -50 -50 100 100)
        (gsk:rounded-rect-init-from-rect outline rect 50)
        (gtk:snapshot-append-border snapshot
                                    outline
                                    '(4 4 4 4)
                                    (list black black black black))))
    ;; Next, draw the hour hand.
    ;; We do this using transforms again: Instead of computing where the angle
    ;; points to, we just rotate everything and then draw the hand as if it
    ;; was :00. We don't even need to care about am/pm here because rotations
    ;; just work.
    (let* ((zone (gtk:clock-timezone paintable))
           ;; Create a timestamp with the actuell time
           (time (local-time:now))
           (hour (local-time:timestamp-hour time :timezone zone))
           (minute (local-time:timestamp-minute time :timezone zone))
           (second (local-time:timestamp-second time :timezone zone)))
      (gtk:snapshot-save snapshot)
      (gtk:snapshot-rotate snapshot (+ (* 30 hour) (* 0.5 minute)))
      (cffi:with-foreign-object (outline '(:struct gsk:rounded-rect))
        (graphene:with-rect (rect -2 -23 4 25)
          (gsk:rounded-rect-init-from-rect outline rect 2.0)
          (gtk:snapshot-push-rounded-clip snapshot outline)
          (gtk:snapshot-append-color snapshot
                                     black
                                     (gsk:rounded-rect-bounds outline))
          (gtk:snapshot-pop snapshot)
          (gtk:snapshot-restore snapshot)
          ;; And the same as above for the minute hand. Just make this one
          ;;; longer so people can tell the hands apart.
          (gtk:snapshot-save snapshot)
          (gtk:snapshot-rotate snapshot (* 6 minute))
          (graphene:rect-init rect -2 -43 4 45)
          (gsk:rounded-rect-init-from-rect outline rect 2)
          (gtk:snapshot-push-rounded-clip snapshot outline)
          (gtk:snapshot-append-color snapshot
                                     black
                                     (gsk:rounded-rect-bounds outline))
          (gtk:snapshot-pop snapshot)
          (gtk:snapshot-restore snapshot)
          ;; and finally, the second indicator.
          (gtk:snapshot-save snapshot)
          (gtk:snapshot-rotate snapshot (* 6 second))
          (graphene:rect-init rect -2 -43 4 10)
          (gsk:rounded-rect-init-from-rect outline rect 2)
          (gtk:snapshot-push-rounded-clip snapshot outline)
          (gtk:snapshot-append-color snapshot
                                     black
                                     (gsk:rounded-rect-bounds outline))
          (gtk:snapshot-pop snapshot)
          (gtk:snapshot-restore snapshot))))
  ;; And finally, don't forget to restore the initial save() that
  ;; we did for the initial transformations.
  (gtk:snapshot-restore snapshot)))

(defmethod gdk:paintable-get-flags-impl ((paintable clock))
  '())

;; Our desired size is 100px. That sounds okay for an analog clock
(defmethod gdk:paintable-get-intrinsic-width-impl ((paintable clock))
  100)

(defmethod gdk:paintable-get-intrinsic-height-impl ((paintable clock))
  100)

(let (;; This is the list of all the ticking clocks
      (clocks nil)
      ;; This is the ID of the timeout source that is updating all
      ;; ticking clocks.
      (ticking-clock-id 0))

  (defun clock-tick ()
    (dolist (clock clocks)
      ;; Update the time of the clock
      (setf (clock-time clock)
            (local-time:format-timestring nil
                                          (local-time:now)
                                          :format '(:hour ":"
                                                    (:min 2) ":"
                                                    (:sec 2))
                                          :timezone (clock-timezone clock)))
      (g:object-notify clock "time")
      ;; We will also draw the hands of the clock differently.
      ;; So notify about that, too.
      (gdk:paintable-invalidate-contents clock))
    t)

  (defun clock-start-ticking (clock)
    (when (= 0 ticking-clock-id)
       (format t "Start ticking clocks~%")
       ;; if no clock is ticking yet, start
       (setf ticking-clock-id
             (g:timeout-add-seconds 1 #'clock-tick)))
    (setf clocks (cons clock clocks)))

  (defun clock-stop-ticking (clock)
    (remove clock clocks)
    (when (= 0 (length clocks))
      (format t "Stop ticking clocks~%")
      ;; If no clock is remaining, stop running the tick updates
      (g:source-remove ticking-clock-id)
      (setf ticking-clock-id 0)))

  (defun clock-stop-ticking-all ()
    (dolist (clock clocks)
      (clock-stop-ticking clock))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'clock-start-ticking)
  (export 'clock-stop-ticking-all))

(in-package :gtk4-example)

(defun create-clocks-model ()
  (let ((store (g:list-store-new "GtkClock"))
        ;; A bunch of timezones with GTK hackers
        (timezones '("UTC"
                     "Europe/London"
                     "Europe/Berlin"
                     "America/Los_Angeles"
                     "America/Mexico_City"
                     "America/New_York"
                     "Asia/Kolkata"
                     "Asia/Shanghai")))
    (dolist (timezone timezones)
      (let ((clock (make-instance 'gtk:clock :location timezone)))
        (gtk:clock-start-ticking clock)
        (g:list-store-append store clock)))
    store))

;; And this function is the crux for this whole demo. It shows how to use
;; expressions to set up bindings.
(defun clocks-setup-listitem-cb (factory item)
  (declare (ignore factory))
  (let ((box (make-instance 'gtk:box
                            :orientation :vertical))
        (label-location (make-instance 'gtk:label))
        (label-time (make-instance 'gtk:label))
        (picture (make-instance 'gtk:picture
                                :halign :center
                                :valign :center))
        expression clock-expression)
    (setf (gtk:list-item-child item) box)
    ;; First, we create an expression that gets us the clock from the listitem:
    ;;  1. Create an expression that gets the list item.
    ;;  2. Use that expression's "item" property to get the clock
    (setf expression
          (gtk:constant-expression-new "GtkListItem" item))
    (setf clock-expression
          (gtk:property-expression-new "GtkListItem" expression "item"))
    ;; Bind the clock's location to a label.
    ;; This is easy: We just get the "location" property of the clock.
    (setf expression
          (gtk:property-expression-new "GtkClock"
                                       (gtk:expression-ref clock-expression)
                                       "location"))
    ;; Now create the label and bind the expression to it.
    (gtk:expression-bind expression label-location "label" label-location)
    (gtk:box-append box label-location)
    ;; Here we bind the item itself to a GdkPicture.
    ;; This is simply done by using the clock expression itself.
    (setf expression (gtk:expression-ref clock-expression))
    ;; Now create the widget and bind the expression to it.
    (gtk:expression-bind expression picture "paintable" picture)
    (gtk:box-append box picture)
    ;; And finally, everything comes together.
    ;; We create a label for displaying the time as text.
    ;; For that, we need to transform the "GDateTime" of the
    ;; time property into a string so that the label can display it.
    (setf expression
          (gtk:property-expression-new "GtkClock"
                                       (gtk:expression-ref clock-expression)
                                       "time"))
    ;; Now create the label and bind the expression to it.
    (gtk:expression-bind expression label-time "label" label-time)
    (gtk:box-append box label-time)))

(defun do-grid-view-clocks (&optional application)
  (let* ((vbox (make-instance 'gtk:box
                              :orientation :vertical))
         ;; Create the factory that creates the listitems. Because we used
         ;; bindings above during setup, we only need to connect to the setup
         ;; signal. The bindings take care of the bind step.
         (factory (make-instance 'gtk:signal-list-item-factory))
         (model (gtk:no-selection-new (create-clocks-model)))
         (gridview (make-instance 'gtk:grid-view
                                  :model model
                                  :factory factory
                                  :hscroll-policy :natural
                                  :vscroll-policy :natural))
         ;; List widgets go into a scrolled window. Always.
         (scrolled (make-instance 'gtk:scrolled-window
                                  :child gridview
                                  :vexpand t))
         ;; This is the normal window setup code every demo does
         (window (make-instance 'gtk:window
                        :application application
                        :title "World Clocks"
                        :child vbox
                        :default-width 600
                        :default-height 540)))
    (g:signal-connect window "close-request"
                      (lambda (window)
                        (declare (ignore window))
                        (gtk:clock-stop-ticking-all)))
    (g:signal-connect factory "setup"
                      (lambda (factory item)
                        (clocks-setup-listitem-cb factory item)))
    ;; Pack and present window
    (gtk:box-append vbox scrolled)
    (gtk:window-present window)))

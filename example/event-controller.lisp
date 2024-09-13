;;;; Event Controller - 2022-8-22

(in-package :gtk4-example)

(defun print-controller-details (controller)

  (format t "  Details for the Event Controller :~%")
  (format t "        name : ~a~%"
            (gtk:event-controller-name controller))
  (format t "       limit : ~a~%"
            (gtk:event-controller-propagation-limit controller))
  (format t "       phase : ~a~%"
            (gtk:event-controller-propagation-phase controller))
  (format t "      widget : ~a~%"
            (gtk:event-controller-widget controller))
  (format t "       event : ~a~%"
            (gtk:event-controller-current-event controller))
  (format t "      device : ~a~%"
            (gtk:event-controller-current-event-device controller))
  (format t "       state : ~a~%"
            (gtk:event-controller-current-event-state controller))
  (format t "        time : ~a~%"
            (gtk:event-controller-current-event-time controller)))

(defparameter event-controller-ui
"<interface>

  <object class='GtkBox' id='infobox'>

    <property name='orientation'>vertical</property>

    <child>
      <object class='GtkBox'>
        <property name='orientation'>horizontal</property>
        <child>
          <object class='GtkLabel' id='textcontroller'>
            <property name='label'>Controller :</property>
            <property name='halign'>GTK_ALIGN_END</property>
          </object>
        </child>
        <child>
          <object class='GtkLabel' id='labelcontroller'>
            <property name='label'>...</property>
            <property name='halign'>GTK_ALIGN_START</property>
          </object>
        </child>
      </object>
    </child>

    <child>
      <object class='GtkBox'>
        <property name='orientation'>horizontal</property>
        <child>
          <object class='GtkLabel' id='textname'>
            <property name='label'>name :</property>
            <property name='halign'>GTK_ALIGN_END</property>
          </object>
        </child>
        <child>
          <object class='GtkLabel' id='labelname'>
            <property name='label'>...</property>
            <property name='halign'>GTK_ALIGN_START</property>
          </object>
        </child>
      </object>
    </child>

  </object>

  <object class='GtkSizeGroup'>
    <property name='mode'>GTK_SIZE_GROUP_VERTICAL</property>
    <widgets>
      <widget name='textcontroller'/>
      <widget name='textname'/>
    </widgets>
  </object>

  <object class='GtkSizeGroup'>
    <property name='mode'>GTK_SIZE_GROUP_VERTICAL</property>
    <widgets>
      <widget name='labelcontroller'/>
      <widget name='labelname'/>
    </widgets>
  </object>

<interface>")

(defun do-event-controller (&optional application)
  (let* ((builder (gtk:builder-new-from-string event-controller-ui))
         (infobox (gtk:builder-object builder "infobox"))

         (area (make-instance 'gtk:drawing-area
                               :content-width 300
                               :content-height 300))
         (frame1 (make-instance 'gtk:frame
                                :child area))

         (entry (make-instance 'gtk:text))
         (frame2 (make-instance 'gtk:frame
                                :child entry))

         (vbox (make-instance 'gtk:box
                              :orientation :vertical))
         (hbox (make-instance 'gtk:box
                              :orientation :horizontal))

         (window (make-instance 'gtk:window
                                :application application
                                :child hbox
                                :title "Event Controller"))

         (keycontroller (make-instance 'gtk:event-controller-key
                                       :name "GtkEventControllerKey"))
         (motioncontroller (make-instance 'gtk:event-controller-motion
                                          :name "GtkEventControllerMotion")))

    (gtk:box-append vbox (make-instance 'gtk:label
                                        :label "Drawing area"))
    (gtk:box-append vbox frame1)
    (gtk:box-append vbox (make-instance 'gtk:label
                                        :label "Text entry"))
    (gtk:box-append vbox frame2)

    (gtk:box-append hbox vbox)
    (gtk:box-append hbox infobox)

    ;; Add the motion controller to the drawing area
    (gtk:widget-add-controller area motioncontroller)

    (g:signal-connect motioncontroller "enter"
        (lambda (controller x y)
          (setf (gtk:label-label (gtk:builder-object builder "labelcontroller"))
                (format nil "~a" controller))
          (setf (gtk:label-label (gtk:builder-object builder "labelname"))
                (format nil "~a" (gtk:event-controller-name controller)))

          (format t "~%in Signal ENTER~%")
          (format t "  controller : ~a~%" controller)
          (format t "       (x,y) : (~a, ~a)~%" x y)))

    (g:signal-connect motioncontroller "leave"
        (lambda (controller)
          (format t "~%in Signal LEAVE~%")
          (format t "  controller : ~a~%" controller)))

    (g:signal-connect motioncontroller "motion"
        (lambda (controller x y)
          (format t "~%in Signal MOTION~%")
          (format t "  controller : ~a~%" controller)
          (format t "       (x,y) : (~a, ~a)~%" x y)))

    ;; Add the key event controller to the text widget
    (gtk:widget-add-controller entry keycontroller)

    ;; Handle the signals for the key event controller
    (g:signal-connect keycontroller "key-pressed"
        (lambda (controller keyval keycode state)
          (format t "~%in Signal KEY-PRESSED~%")
          (format t "  controller : ~a~%" controller)
          (format t "      keyval : ~a~%" keyval)
          (format t "     keycode : ~a~%" keycode)
          (format t "       state : ~a~%" state)

          (print-controller-details controller)

;        (format t "Details for the Event : ~%")
;        (let ((event (gtk-event-controller-current-event controller)))
;          (format t "      keyval : ~a~%" (gdk-key-event-keyval event))
;          (format t "     keycode : ~a~%" (gdk-key-event-keycode event))
;          (format t "      layout : ~a~%" (gdk-key-event-layout event))
;          (format t "       level : ~a~%" (gdk-key-event-level event))
;          (format t "       match : ~a~%"
;                    (multiple-value-list (gdk-key-event-match event))))

    ))
    (g:signal-connect keycontroller "key-released"
        (lambda (controller keyval keycode state)
          (format t "~%in Signal KEY-RELEASED~%")
          (format t "  controller : ~a~%" controller)
          (format t "      keyval : ~a~%" keyval)
          (format t "     keycode : ~a~%" keycode)
          (format t "       state : ~a~%" state)
          (print-controller-details controller)
    ))



  ;; Show the window
  (gtk:window-present window)))

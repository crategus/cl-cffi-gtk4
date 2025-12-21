;;;; Emblemed Icon
;;;;
;;;; This demo shows how <tt>GdkPaintable</tt> can be used to overlay an emblem
;;;; on top of an icon. The emblem can either be another icon, or an arbitrary
;;;; paintable.
;;;;
;;;; 2025-12-21

(in-package :gtk4-example)

(gobject:define-gobject-subclass "EmblemedIcon" emblemed-icon
  (:superclass g:object
   :export t
   :interfaces ("GdkPaintable"))
  ((:cl icon
    :initform nil
    :accessor emblemed-icon-icon)
   (:cl emblem
    :initform nil
    :accessor emblemed-icon-emblem)
   (:cl flags
    :initform '()
    :accessor emblemed-icon-flags)))

(defmethod initialize-instance :after ((obj emblemed-icon) &rest initargs)
  ;; Set FLAGS property
  (setf (emblemed-icon-flags obj) '(:static-size :static-contents))
  ;; Set slot values from initargs
  (iter (for (slot value) on initargs by #'cddr)
        (cond ((eq slot :icon)
               (setf (emblemed-icon-icon obj) value))
              ((eq slot :emblem)
               (setf (emblemed-icon-emblem obj) value)))))

(defmethod gdk:paintable-snapshot-impl
           ((paintable emblemed-icon) snapshot width height)
  (graphene:with-point (point (* 0.5 width) 0)
    (gdk:paintable-snapshot (emblemed-icon-icon paintable)
                            snapshot
                            width
                            height)
    (gtk:snapshot-save snapshot)
    (gtk:snapshot-translate snapshot point)
    (gdk:paintable-snapshot (emblemed-icon-emblem paintable)
                            snapshot
                            (* 0.5 width)
                            (* 0.5 height))
    (gtk:snapshot-restore snapshot)))

(defmethod gdk:paintable-get-flags-impl ((paintable emblemed-icon))
  (emblemed-icon-flags paintable))

(defun emblemed-icon-new-with-paintable (icon-name emblem)
  (let* ((theme (gtk:icon-theme-for-display (gdk:display-default)))
         (icon (gtk:icon-theme-lookup-icon theme
                                           icon-name
                                           nil
                                           128 1
                                           :ltr
                                           :none))
         (emblemed-icon (make-instance 'emblemed-icon
                                       :icon icon
                                       :emblem emblem)))
    (when (not (member :static-contents
                       (gdk:paintable-flags emblem)))
      (setf (emblemed-icon-flags emblemed-icon) '(:static-size))
      (g:signal-connect emblem "invalidate-contents"
                        (lambda (paintable)
                          (declare (ignore paintable))
                          (gdk:paintable-invalidate-contents emblemed-icon))))
   emblemed-icon))

(defun emblemed-icon-new (icon-name emblem-name)
  (let* ((theme (gtk:icon-theme-for-display (gdk:display-default)))
         (emblem (gtk:icon-theme-lookup-icon theme
                                             emblem-name
                                             nil
                                             128 1
                                             :ltr
                                             :none)))
    (emblemed-icon-new-with-paintable icon-name emblem)))

(defun do-paintable-emblem (&optional application)
  (let* ((box (make-instance 'gtk:box
                            :orientation :horizontal
                            :margin-top 12
                            :margin-bottom 12
                            :margin-start 12
                            :margin-end 12
                            :spacing 12))
         (window (make-instance 'gtk:window
                                :application application
                                :child box
                                :title "Emblemed Icon"
                                :default-width 300
                                :default-height 200)))
    ;; First icon with emblem from theme
    (let* ((icon (emblemed-icon-new "folder" "emblem-important"))
           (image (gtk:image-new-from-paintable icon)))
      (setf (gtk:image-pixel-size image) 128)
      (setf (gtk:widget-hexpand image) t)
      (setf (gtk:widget-vexpand image) t)
      (gtk:box-append box image))
    ;; Second icon with animated emblem
    (let* ((emblem (make-instance 'gdk:nuclear-animation
                                  :drawbackground nil))
           (icon (emblemed-icon-new-with-paintable "drive-multidisk" emblem))
           (image (gtk:image-new-from-paintable icon)))
      (setf (gtk:image-pixel-size image) 128)
      (setf (gtk:widget-hexpand image) t)
      (setf (gtk:widget-vexpand image) t)
      (gtk:box-append box image))
    (gtk:window-present window)))

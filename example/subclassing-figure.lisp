;;;; Example GtkFigure subclass
;;;;
;;;; The GtkFigure subclass displays a picture with a caption, that is at the
;;;; bottom by default, but can also be placed at the top, the left or the right
;;;; position. GtkFigure is derived from GtkBox to display the picture and the
;;;; caption in a box. We define three slots to specify the behavior of the
;;;; widget.
;;;;
;;;; 2023-10-29

;; TODO: Add a widget to change the position of the caption.

(in-package :gtk)

(gobject:define-g-object-subclass "GtkFigure" figure
  (:superclass box
   :export t
   :interfaces ())
  ((picture
    figure-picture
    "picture" "GtkPicture" t t)
   (caption
    figure-caption
    "caption" "gchararray" t t)
   (position
    figure-position
    "position" "GtkPositionType" t t)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'figure)
  (export 'figure-picture)
  (export 'figure-caption)
  (export 'figure-position))

(defmethod initialize-instance :after ((obj figure) &key)
  ;; Do we have a caption, if not we set an appropriate value
  (unless (figure-caption obj)
    (let ((picture (figure-picture obj)))
      (if (and picture (picture-file picture))
          ;; Set a caption with the filename
          (setf (figure-caption obj)
                (make-instance 'label
                               :label (g:file-get-parse-name
                                          (picture-file picture))))
          ;; Set an empty caption
          (setf (figure-caption obj) (make-instance 'label)))))
  ;; Do we have a picture, if not we set an appropriate value
  (unless (figure-picture obj)
    ;; Set an empty picture
    (setf (figure-picture obj) (make-instance 'picture)))
  ;; Set the position of the caption in the vbox
  (cond ((eq :bottom (figure-position obj))
         (setf (gtk:orientable-orientation obj) :vertical)
         (box-append obj (figure-picture obj))
         (box-append obj (figure-caption obj)))
        ((eq :top (figure-position obj))
         (setf (gtk:orientable-orientation obj) :vertical)
         (box-append obj (figure-caption obj))
         (box-append obj (figure-picture obj)))
        ((eq :left (figure-position obj))
         (setf (gtk:orientable-orientation obj) :horizontal)
         (box-append obj (figure-caption obj))
         (box-append obj (figure-picture obj)))
        ((eq :right (figure-position obj))
         (setf (gtk:orientable-orientation obj) :horizontal)
         (box-append obj (figure-picture obj))
         (box-append obj (figure-caption obj)))
        (t
         (warn "INITIALIZE-INSTANCE: Unknown position."))))

(in-package :gtk4-example)

(defun do-subclassing-figure (&optional application)
  (let* ((filename (sys-path "resource/ducky.png"))
         (figure (make-instance 'gtk:figure
                                ;; Initialize slots of GtkFigure
                                :picture
                                (gtk:picture-new-for-filename filename)
                                :caption (gtk:label-new "ducky.png")
                                :position :right
                                ;; Intialize inherited slots from GtkBox
                                :margin-top 12
                                :margin-bottom 12
                                :margin-start 12
                                :margin-end 12
                                :spacing 12))
         (window (make-instance 'gtk:window
                                :application application
                                :child figure
                                :title "Subclassing"
                                :default-width 300
                                :default-height 200)))
    (gtk:window-present window)))

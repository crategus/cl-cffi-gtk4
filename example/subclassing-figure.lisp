;;;; Example GtkFigure subclass
;;;;
;;;; 2023-10-29

(in-package :gtk)

;;; ----------------------------------------------------------------------------

;; A very simple example of a subclass of GtkBox. GtkVBox is a GtkBox, that is
;; vertically oriented by default. We do not define any new slots, but the 
;; GtkVBOX subclass inherits all slots from the GtkBox class.

(defclass vbox (box)
  ()
  (:gname . "GtkVBox")
  (:metaclass gobject:gobject-class))

;; Register the new GtkVBox type
(gobject:register-object-type-implementation "GtkVBox"  ; name
                                             vbox       ; class
                                             "GtkBox"   ; parent
                                             ()         ; interfaces
                                             nil)       ; properties

;; Set the :vertical property in the INITIALIZE-INSTANCE method
(defmethod initialize-instance :after ((obj vbox) &key)
  (setf (orientable-orientation obj) :vertical))

;;; ----------------------------------------------------------------------------

;; The GtkFigure subclass displays a picture with a caption, that is at the 
;; bottom by default, but can also be placed at the top. GtkFigure is derived 
;; from GtkVBox to display the picture and the caption in a vertical box.
;; We define three new slots to specify the behavior of the new widget.

(defclass figure (vbox)
  ((picture :initform nil
            :accessor figure-picture)
   (caption :initform nil
            :accessor figure-caption)
   (position :initform :bottom
             :accessor figure-position))
  (:gname . "GtkFigure")
  (:metaclass gobject:gobject-class))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'figure)
  (export 'figure-picture)
  (export 'figure-caption)
  (export 'figure-position))

(gobject:register-object-type-implementation "GtkFigure"  ; name
                                             figure       ; class
                                             "GtkVBox"    ; parent
                                             ()           ; interfaces
                                             nil)         ; properties

(defmethod initialize-instance :after ((obj figure) &rest initargs)
  ;; Set the slot values from initargs
  (iter (for (slot value) on initargs by #'cddr)
        (cond ((eq slot :caption)
               (setf (figure-caption obj) value))
              ((eq slot :picture)
               (setf (figure-picture obj) value))
              ((eq slot :position)
               (cond ((eq :bottom value)
                      (setf (figure-position obj) :bottom))
                     ((eq :top value)
                      (setf (figure-position obj) :top))
                     (t
                      (warn "INITIALIZE-INSTANCE: Unknown position."))))))
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
         (box-append obj (figure-picture obj))
         (box-append obj (figure-caption obj)))
        ((eq :top (figure-position obj))
         (box-append obj (figure-caption obj))
         (box-append obj (figure-picture obj)))
        (t
         (warn "INITIALIZE-INSTANCE: Unknown position."))))

(in-package :gtk4-example)

(defun do-subclassing-figure (&optional application)
  (let* ((filename (sys-path "resource/ducky.png"))
         (figure (make-instance 'gtk:figure
                                ;; Initialize slots of GtkFigure
                                :picture 
                                (gtk:picture-new-for-filename filename)
                                :position :top
                                ;; Intialize inherited slots from GtkVBox
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
    (format t "Infos about gtk:figure~%")
    (format t "    picture : ~a~%" (gtk:figure-picture figure))
    (format t "    label : ~a~%" (gtk:figure-caption figure))
    (format t "    position : ~a~%" (gtk:figure-position figure))
    (gtk:window-present window)))

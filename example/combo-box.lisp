;;;; Combo Box
;;;;
;;;; The <tt>gtk:combo-box</tt> widget allows the user to choose from a list of
;;;; valid choices. The <tt>gtk:combo-box</tt> widget displays the selected
;;;; choice. When activated, the <tt>gtk:combo-box</tt> widget displays a popup
;;;; which allows the user to make a new choice. The style in which the selected
;;;; value is displayed, and the style of the popup is determined by the current
;;;; theme. It may be similar to a Windows style combo box.
;;;;
;;;; The <tt>gtk:combo-box</tt> widget uses the model-view pattern. The list of
;;;; valid choices is specified in the form of a tree model, and the display of
;;;; the choices can be adapted to the data in the model by using cell
;;;; renderers, as you would in a tree view. This is possible since the
;;;; <tt>gtk:combo-box</tt> class implements the <tt>gtk:cell-layout</tt>
;;;; interface. The tree model holding the valid choices is not restricted to a
;;;; flat list, it can be a real tree, and the popup will reflect the tree
;;;; structure.
;;;;
;;;; 2024-4-7

(in-package :gtk4-example)

(defparameter *icon-list*
              '(("gchararray" "gchararray")
                ("dialog-warning" "Warning")
                ("process-stop" "Stop")
                ("document-new" "New")
                ("edit-clear" "Clear")
                ("" "separator")
                ("document-open" "Open")))

(defun create-and-fill-list-store-combo-box (data)
  (flet ((mklist (obj) (if (listp obj) obj (list obj))))
    (let ((model (apply #'gtk:list-store-new (mklist (first data)))))
      (dolist (entry (rest data))
        (let ((iter (gtk:list-store-append model)))
          (apply #'gtk:list-store-set model iter (mklist entry))))
      model)))

(defun set-sensitivity (layout cell model iter)
  (declare (ignore layout))
  (let* ((path (gtk:tree-model-path model iter))
         (pathstr (gtk:tree-path-to-string path)))
    (if (string= "1" pathstr)
        (setf (gtk:cell-renderer-sensitive cell) nil)
        (setf (gtk:cell-renderer-sensitive cell) t))))

(defun do-combo-box (&optional application)
  (let* ((hbox (make-instance 'gtk:box
                              :orientation :horizontal
                              :margin-top 24
                              :margin-bottom 24
                              :margin-start 24
                              :margin-end 24
                              :spacing 24))
         (window (make-instance 'gtk:window
                               :title "Combo Box"
                               :child hbox
                               :application application))
         (vbox1 (make-instance 'gtk:box
                               :orientation :vertical
                               :spacing 12))
         (vbox2 (make-instance 'gtk:box
                               :orientation :vertical
                               :spacing 12))
         (label (make-instance 'gtk:label
                               :label "label"
                               :halign :start
                               :valign :center))
         (model (create-and-fill-list-store-combo-box *icon-list*))
         (combo (make-instance 'gtk:combo-box
                               :model model)))
    ;; Setup Cell Renderer for icon
    (let ((renderer (gtk:cell-renderer-pixbuf-new)))
      (setf (gtk:cell-renderer-xpad renderer) 6) ; More space between cells
      (gtk:cell-layout-pack-start combo renderer :expand nil)
      (gtk:cell-layout-set-attributes combo renderer "icon-name" 0)
      (gtk:cell-layout-set-cell-data-func combo renderer #'set-sensitivity))
    ;; Setup Cell Renderer for icon-name
    (let ((renderer (gtk:cell-renderer-text-new)))
      (gtk:cell-layout-pack-start combo renderer)
      (gtk:cell-layout-set-attributes combo renderer "text" 1)
      (gtk:cell-layout-set-cell-data-func combo renderer #'set-sensitivity))
    ;; Setup a Row Separator Function
    (gtk:combo-box-set-row-separator-func combo
        (lambda (object iter)
          (let ((value (gtk:tree-model-value object iter 1)))
            (string= value "separator"))))
    ;; Combo box selection has changed
    (g:signal-connect combo "changed"
        (lambda (object)
          (let ((value (gtk:combo-box-active-id object)))
            (gtk:label-set-markup label
                                  (format nil "<tt>~a</tt>" value)))))
    (setf (gtk:combo-box-id-column combo) 1)
    (setf (gtk:combo-box-active combo) 0)
    ;; Pack and show widgets
    (gtk:box-append vbox1
                    (make-instance 'gtk:label
                                   :use-markup t
                                   :xalign 0
                                   :label "<b>Select item</b>"))
    (gtk:box-append vbox1 combo)
    (gtk:box-append vbox2
                    (make-instance 'gtk:label
                                   :use-markup t
                                   :xaling 0
                                   :label "<b>Activated item</b>"))
    (gtk:box-append vbox2 label)
    (gtk:box-append hbox vbox1)
    (gtk:box-append hbox vbox2)
    (gtk:window-present window)))

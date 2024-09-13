;;;; Grid Buildable
;;;;
;;;; Every child in a <tt>gtk:grid</tt> widget has access to a custom
;;;; <tt>gtk:buildable</tt> element, called <tt>layout</tt>. It can by used to
;;;; specify a position in the grid and optionally spans. All properties that
;;;; can be used in the <tt>layout</tt> element are implemented by the
;;;; <tt>gtk:grid-layout-child</tt> class.
;;;;
;;;; This example organizes the first two buttons side-by-side in one cell each.
;;;; The third button is in the last column but spans across two rows. This is
;;;; defined by the row-span property. The last button is located in the second
;;;; row and spans across two columns, which is defined by the column-span
;;;; property.
;;;;
;;;; 2024-8-6

(in-package :gtk4-example)

(defun do-grid-buildable (&optional application)
  (let* ((path (glib-sys:sys-path "resource/grid-buildable.ui"))
         (builder (gtk:builder-new-from-file path))
         (grid (gtk:builder-object builder "mygrid"))
         (window (make-instance 'gtk:window
                                :title "Grid Buildable"
                                :application application
                                :child grid)))
    (gtk:window-present window)))

;;;; Cursors
;;;;
;;;; Demonstrates a useful set of available cursors. The cursors shown here are
;;;; the ones defined by CSS, which we assume to be available. The example shows
;;;; creating cursors by name or from an image, with or without a fallback.
;;;;
;;;; 2026-02-22

(in-package :gtk4-example)

;; FIXME: The CURSOR-CALLBACK function crashes with a memory fault after it is
;; called multiple times. What is the problem?
(defun cursor-callback (cursor size scale width height xhotspot yhotspot)
  (let* ((scaled (ceiling (* size scale)))
         (path "/images/cursors/gtk_logo_cursor.svg")
         (pixbuf (gdk:pixbuf-new-from-resource-at-scale path scaled scaled t)))
    (setf (cffi:mem-ref width :int) (truncate size))
    (setf (cffi:mem-ref height :int) (truncate size))
    (setf (cffi:mem-ref xhotspot :int) (truncate (* 18 (/ size 32))))
    (setf (cffi:mem-ref yhotspot :int) (truncate (* 2 (/ size 32))))
    (when pixbuf
      (gdk:texture-new-for-pixbuf pixbuf))))

(defun do-cursors (&optional application)
  (let* ((path (glib-sys:sys-path "resource/cursors.ui"))
         (builder (gtk:builder-new-from-file path))
         (window (gtk:builder-object builder "window"))
         (logo-callback (gtk:builder-object builder "logo_callback"))
         (cursor (gdk:cursor-new-from-callback #'cursor-callback))
         (provider (gtk:css-provider-new)))

    (format t "  cursor (ref-count) : ~a~%" (g:object-ref-count cursor))

    (gtk:css-provider-load-from-string provider
                                       ".cursorbg {
                                          background: linear-gradient(to bottom
                                          right, white 0%, white 50%, black 50%,
                                          black 100%); }")
    (gtk:widget-add-provider window provider)
    (setf (gtk:widget-cursor logo-callback) cursor)
    (setf (gtk:window-application window) application)
    (gtk:window-present window)))

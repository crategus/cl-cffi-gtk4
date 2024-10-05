;;;; Clipboard
;;;;
;;;; <tt>GdkClipboard</tt> is used for clipboard handling. This demo shows how
;;;; to copy and paste text, images, colors or files to and from the clipboard.
;;;;
;;;; You can also use Drag-And-Drop to copy the data from the source to the
;;;; target.
;;;;
;;;; Last version: 2024-5-14

;; TODO: Improve the example: We get an error when dragging a NIL file.

(in-package :gtk4-example)

(defun update-copy-button (source)
  (let ((setting nil)
        (button (g:object-data source "copy-button"))
        (child (gtk:stack-visible-child source))
        (name (gtk:stack-visible-child-name source)))
    (cond ((string= name "Text")
           (setf setting (> (length (gtk:editable-text child)) 0)))
          ((string= name "File")
           (setf setting (g:object-data child "file")))
          ((or (string= name "Image")
               (string= name "Color"))
           (setf setting t))
          (t
           (setf setting nil)))
    (setf (gtk:widget-sensitive button) setting)))

(defun update-paste-button (clipboard button)
  (let ((setting nil)
        (formats (gdk:clipboard-formats clipboard)))
    (when (or (gdk:content-formats-contain-gtype formats "GFile")
              (gdk:content-formats-contain-gtype formats "GdkRGBA")
              (gdk:content-formats-contain-gtype formats "GdkMemoryTexture")
              (gdk:content-formats-contain-gtype formats "gchararray"))
      (setf setting t))
    (setf (gtk:widget-sensitive button) setting)))

(defun file-open-response-cb (source result button)
  (let ((file (gtk:file-dialog-open-finish source result)))
    (when file
      (setf (g:object-data button "file") file)
      (setf (gtk:label-label (gtk:button-child button))
            (g:file-get-parse-name file))
      (update-copy-button (gtk:widget-ancestor button "GtkStack")))))

(defun file-open-cb (button)
  #-windows
  (let ((dialog (gtk:file-dialog-new)))
    (gtk:file-dialog-open dialog
                          (gtk:widget-ancestor button "GtkWindow")
                          nil
                          (lambda (source result)
                            (file-open-response-cb source result button))))
  #+windows
  (let ((dialog (gtk:alert-dialog-new "File Open not implemented for Windows")))
    (gtk:alert-dialog-show dialog (gtk:widget-root button))))

(defun copy-button-clicked (source)
  (let ((clipboard (gtk:widget-clipboard source))
        (child (gtk:stack-visible-child source))
        (name (gtk:stack-visible-child-name source)))
    (cond ((string= name "Text")
           (gdk:clipboard-set-text clipboard (gtk:editable-text child)))
          ((string= name "Color")
           (let ((color (gtk:color-dialog-button-rgba child)))
             (gdk:clipboard-set clipboard "GdkRGBA" color)))
          ((string= name "Image")
           (iter (for child1 first (gtk:widget-first-child child)
                             then (gtk:widget-next-sibling child1))
                 (while child1)
                 (when (gtk:toggle-button-active child1)
                   (let* ((image (gtk:widget-first-child child1))
                          (paintable (gtk:image-paintable image))
                          (gtype (g:type-from-instance paintable)))
                     (gdk:clipboard-set clipboard gtype paintable))
                   (finish))))
          ((string= name "File")
           (let ((file (g:object-data child "file")))
             (when file
               (gdk:clipboard-set clipboard "GFile" file))))
          (t
           (format t "Requested type is not implemented.~%")))))

(defun present-value (target gvalue)
  (let ((value (g:value-get gvalue)))
    (cond ((g:value-holds gvalue "gchararray")
           (setf (gtk:stack-visible-child-name target) "Text")
           (setf (gtk:label-label (gtk:stack-visible-child target)) value))
          ((g:value-holds gvalue "GdkRGBA")
           (setf (gtk:stack-visible-child-name target) "Color")
           (let ((child (gtk:widget-first-child
                            (gtk:stack-visible-child target))))
             (setf (g:object-property child "rgba") value)))
          ((g:value-holds gvalue "GdkTexture")
           (setf (gtk:stack-visible-child-name target) "Image")
           (let ((child (gtk:stack-visible-child target)))
             (setf (g:object-property child "paintable") value)))
          ((g:value-holds gvalue "GFile")
           (setf (gtk:stack-visible-child-name target) "File")
           (let ((child (gtk:stack-visible-child target)))
             (setf (g:object-data child "file") value)
             (setf (gtk:label-label child)
                   (g:file-get-parse-name value))))
          (t
           (format t "Requested type is not implemented.")))))

(defun paste-received (source result target)
  (let ((gvalue (gdk:clipboard-read-value-finish source result)))
    (present-value target gvalue)))

(defun paste-button-clicked (target button)
  (let* ((clipboard (gtk:widget-clipboard target))
         (formats (gdk:clipboard-formats clipboard)))
    (cond ((gdk:content-formats-contain-gtype formats "GFile")
           (gdk:clipboard-read-value-async clipboard "GFile" 0 nil
                   (lambda (source result)
                     (paste-received source result target))))
          ((gdk:content-formats-contain-gtype formats "gchararray")
           (gdk:clipboard-read-value-async clipboard "gchararray" 0 nil
                   (lambda (source result)
                     (paste-received source result target))))
          ((gdk:content-formats-contain-gtype formats "GdkRGBA")
           (gdk:clipboard-read-value-async clipboard "GdkRGBA" 0 nil
                   (lambda (source result)
                     (paste-received source result target))))
          ;; TODO: Improve the implementation of
          ;; gdk:content-formats-contain-gtype. Subclasses should be
          ;; recognized as well. E. G. GdkTexture should recognize
          ;; GdkMemoryTexture
          ((gdk:content-formats-contain-gtype formats "GdkMemoryTexture")
           (gdk:clipboard-read-value-async clipboard "GdkTexture" 0 nil
                   (lambda (source result)
                     (paste-received source result target))))
          (t
           (format t "No content found.~%")))
    (setf (gtk:widget-sensitive button) nil)))

(defun on-drop-cb (target gvalue x y)
  (declare (ignore x y))
  (present-value target gvalue)
  t)

(defun drag-prepare-cb (source x y)
  (declare (ignore x y))
  (let ((button (gtk:event-controller-widget source)))
    ;; TODO:GValue is a foreign type and less well implemented than other
    ;; GBoxed types. Consider to improve the implementation of GValue.
    (cffi:with-foreign-object (gvalue '(:struct g:value))
      (cond ((g:type-is-a (g:type-from-instance button) "GtkToggleButton")
             (let* ((image (gtk:widget-first-child button))
                    (paintable (gtk:image-paintable image)))
               (g:value-set gvalue paintable "GdkTexture")))
            (t
             (let ((file (g:object-data button "file")))
               (g:value-set gvalue file "GFile"))))
      (gdk:content-provider-new-for-value gvalue))))

(defun do-clipboard (&optional (application nil))
  (let* ((path (glib-sys:sys-path "resource/clipboard.ui"))
         (builder (gtk:builder-new-from-file path))
         (window (gtk:builder-object builder "window"))
         (copy (gtk:builder-object builder "copy_button"))
         (paste (gtk:builder-object builder "paste_button"))
         (file (gtk:builder-object builder "source_file"))
         (source (gtk:builder-object builder "source_stack"))
         (target (gtk:builder-object builder "dest_stack"))
         (text (gtk:builder-object builder "source_text"))
         (drop (gtk:builder-object builder "drop_target")))

    (setf (g:object-data source "copy-button") copy)
    (setf (g:object-data target "paste-button") paste)
    (g:signal-connect source "notify::visible-child"
                      (lambda (object param)
                        (declare (ignore object param))
                        (update-copy-button source)))
    (g:signal-connect text "notify::text"
                      (lambda (object param)
                        (declare (ignore object param))
                        (update-copy-button source)))
    (g:signal-connect (gtk:widget-clipboard paste) "changed"
                      (lambda (clipboard)
                        (update-paste-button clipboard paste)))
    (g:signal-connect copy "clicked"
                      (lambda (button)
                        (declare (ignore button))
                        (copy-button-clicked source)))
    (g:signal-connect paste "clicked"
                      (lambda (button)
                        (paste-button-clicked target button)))
    (g:signal-connect file "clicked" #'file-open-cb)

    (g:signal-connect drop "drop"
                      (lambda (drop value x y)
                        (declare (ignore drop))
                        (on-drop-cb target value x y)))

    (g:signal-connect (gtk:builder-object builder "drag_source_rose")
                      "prepare"
                      #'drag-prepare-cb)
    (g:signal-connect (gtk:builder-object builder "drag_source_floppy")
                      "prepare"
                      #'drag-prepare-cb)
    (g:signal-connect (gtk:builder-object builder "drag_source_logo")
                      "prepare"
                      #'drag-prepare-cb)
    (g:signal-connect (gtk:builder-object builder "drag_source_file")
                      "prepare"
                      #'drag-prepare-cb)

    (setf (gtk:widget-sensitive paste) nil)
    (setf (gtk:window-application window) application)
    ;; Show the window
    (gtk:window-present window)))

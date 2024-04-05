(in-package :gtk4-example)

(defun file-opened (source result)
  (format t "in FILE-OPENED~%")
  (let ((file (gtk:file-dialog-open-finish source result)))

    (format t "File opened : ~a~%" file)
))

(defun create-file-dialog-open (parent)
  (let ((dialog (gtk:file-dialog-new))
        (cancellable (g:cancellable-new)))

    (g:timeout-add-seconds 20
                           (lambda ()
                             (format t "Cancel cancellable~%")
                             (g:cancellable-cancel cancellable)
                             glib:+source-remove+))

    (format t "Start Dialog Open~%")
    (gtk:file-dialog-open dialog parent cancellable #'file-opened)
;    (gtk:file-dialog-open dialog parent (cffi:null-pointer) (cffi:null-pointer))
))

(defun file-opened-multiple (source result)
  (format t "in FILE-OPENED-MULTIPLE~%")
  (let ((filelst (gtk:file-dialog-open-multiple-finish source result)))

    (format t "File opened : ~a~%" filelst)
))

(defun create-file-dialog-open-multiple (parent)
  (let ((dialog (gtk:file-dialog-new))
        (cancellable (g:cancellable-new)))

    (g:timeout-add-seconds 20
                           (lambda ()
                             (format t "Cancel cancellable~%")
                             (g:cancellable-cancel cancellable)
                             glib:+source-remove+))

    (format t "Start Dialog Open~%")
    (gtk:file-dialog-open-multiple dialog
                                   parent cancellable #'file-opened-multiple)

))

(defun file-saved (source result)
  (format t "in FILE-SAVED~%")
  (let ((file (gtk:file-dialog-save-finish source result)))

    (format t "File saved : ~a~%" file)
))

(defun create-file-dialog-save (parent)
  (let ((dialog (gtk:file-dialog-new))
        (cancellable (g:cancellable-new)))

    (g:timeout-add-seconds 20
                           (lambda ()
                             (format t "Cancel cancellable~%")
                             (g:cancellable-cancel cancellable)
                             glib:+source-remove+))

    (format t "Start Dialog Save~%")
    (gtk:file-dialog-save dialog parent cancellable #'file-saved)
))

(defun folder-selected (source result)
  (format t "in FOLDER SELECTED~%")
  (let ((file (gtk:file-dialog-select-folder-finish source result)))

    (format t "Folder selected : ~a~%" file)
))

(defun create-file-dialog-select-folder (parent)
  (let ((dialog (gtk:file-dialog-new))
        (cancellable (g:cancellable-new)))

    (g:timeout-add-seconds 20
                           (lambda ()
                             (format t "Cancel cancellable~%")
                             (g:cancellable-cancel cancellable)
                             glib:+source-remove+))

    (format t "Start Select Folder~%")
    (gtk:file-dialog-select-folder dialog parent cancellable #'folder-selected)
))

(defun multiple-folders-selected (source result)
  (format t "in MULTIPLE FOLDERS SELECTED~%")
  (let ((file (gtk:file-dialog-select-multiple-folders-finish source result)))

    (format t "Folders selected : ~a~%" file)
))

(defun create-file-dialog-select-multiple-folders (parent)
  (let ((dialog (gtk:file-dialog-new))
        (cancellable (g:cancellable-new)))

    (g:timeout-add-seconds 20
                           (lambda ()
                             (format t "Cancel cancellable~%")
                             (g:cancellable-cancel cancellable)
                             glib:+source-remove+))

    (format t "Start Select Multiple Folders~%")
    (gtk:file-dialog-select-multiple-folders dialog
                                             parent
                                             cancellable
                                             #'multiple-folders-selected)
))


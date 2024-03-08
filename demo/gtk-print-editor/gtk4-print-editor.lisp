;;; ----------------------------------------------------------------------------
;;; gtk4-print-editor.lisp
;;;
;;; Copyright (C) 2023 - 2024 Dieter Kaiser
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;; ----------------------------------------------------------------------------

;; TODO: Improve this example: Store the print settings. Do more error handling.

(defpackage :gtk4-print-editor
  (:use :iterate :common-lisp)
  (:export :gtk4-print-editor))

(in-package :gtk4-print-editor)

(defparameter *ui-info*
"<interface>
  <menu id='menubar'>
    <submenu>
      <attribute name='label'>_File</attribute>
      <section>
        <item>
          <attribute name='label'>_New</attribute>
          <attribute name='action'>app.new</attribute>
        </item>
        <item>
          <attribute name='label'>_Open</attribute>
          <attribute name='action'>app.open</attribute>
        </item>
        <item>
          <attribute name='label'>_Save</attribute>
          <attribute name='action'>app.save</attribute>
        </item>
        <item>
          <attribute name='label'>Save _As...</attribute>
          <attribute name='action'>app.save-as</attribute>
        </item>
      </section>
      <section>
        <item>
          <attribute name='label'>Page Setup</attribute>
          <attribute name='action'>app.page-setup</attribute>
        </item>
        <item>
          <attribute name='label'>Preview</attribute>
          <attribute name='action'>app.preview</attribute>
        </item>
        <item>
          <attribute name='label'>Print</attribute>
          <attribute name='action'>app.print</attribute>
        </item>
      </section>
      <section>
        <item>
          <attribute name='label'>_Quit</attribute>
          <attribute name='action'>app.quit</attribute>
        </item>
      </section>
    </submenu>
    <submenu>
      <attribute name='label'>_Help</attribute>
      <section>
        <item>
          <attribute name='label'>_About Print Editor</attribute>
          <attribute name='action'>app.about</attribute>
        </item>
      </section>
    </submenu>
  </menu>
</interface>")

;; Improve this code. Make an application class which contains the globals

(defvar *filename* nil)
(defvar *file-changed* nil)
(defvar *buffer* nil)
(defvar *window* nil)
(defvar *statusbar* nil)
(defvar *pagesetup* nil)
(defvar *settings* nil)

(defvar *active-prints* nil)

;; Get the absolute filename of a file for a ASDF loadable package
(defun sys-path (filename &optional (package :gtk4-print-editor))
  (let ((system-path (asdf:system-source-directory package)))
    (princ-to-string (merge-pathnames filename system-path))))

(defun update-title (window)
  (if *filename*
      (setf (gtk:window-title window)
            (format nil "~a - ~a" "Print Editor" (file-namestring *filename*)))
      (setf (gtk:window-title window)
            (format nil "~a - ~a" "Print Editor" "Untitled"))))

(defun update-statusbar (statusbar)
  (let* ((iter (gtk:text-buffer-iter-at-mark
                   *buffer*
                   (gtk:text-buffer-get-insert *buffer*)))
         (row (gtk:text-iter-line iter))
         (col (gtk:text-iter-line-offset iter))
         (msg (format nil "Row ~a, Col ~a~a"
                          row col
                          (if *file-changed* " - Modfied" ""))))
    (gtk:statusbar-pop statusbar 0)
    (gtk:statusbar-push statusbar 0 msg)))

(defun clear-buffer (buffer)
  (multiple-value-bind (start end)
      (gtk:text-buffer-bounds buffer)
    (gtk:text-buffer-delete buffer start end)))

(defun load-file-buffer (buffer filename)
  (with-open-file (stream filename)
    (clear-buffer buffer)
    (do ((line (read-line stream nil)
               (read-line stream nil)))
        ((null line))
      (gtk:text-buffer-insert buffer :cursor line)
      ;; We add an extra newline at the end of the file. Improve this!?
      (gtk:text-buffer-insert buffer :cursor (format nil "~%"))))
  (setf *file-changed* nil)
  (update-title *window*)
  (update-statusbar *statusbar*))

(defun save-file-buffer (buffer filename)
  (with-open-file (stream filename :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create)
    (write-sequence (gtk:text-buffer-text buffer) stream))
    (setf *file-changed* nil)
    (update-statusbar *statusbar*))

(defstruct print-data
  text
  layout
  pagebreaks
  fontbutton
  font)

(defun begin-print (operation context data)
  (format t "in BEGIN-PRINT~%")
  (let* ((width (gtk:print-context-width context))
         (height (gtk:print-context-height context))
         (layout (setf (print-data-layout data)
                       (gtk:print-context-create-pango-layout context)))
         (desc (pango:font-description-from-string (print-data-font data)))
         (pageheight 0)
         (pagebreaks nil)
         (numlines 0))
    (setf (pango:layout-font-description layout) desc)
    (setf (pango:layout-width layout)
          (truncate (* width pango:+scale+)))
    (setf (pango:layout-text layout) (print-data-text data))
    (setf numlines (pango:layout-line-count layout))

    (dotimes (line numlines)
      (let* ((layoutline (pango:layout-line layout line))
             (lineheight (/ (pango:layout-line-height layoutline) 1024.0)))
        (if (> (+ pageheight lineheight) height)
            (progn
              (push line pagebreaks)
              (setf pageheight 0))
            (setf pageheight (+ pageheight lineheight)))))

    (push nil pagebreaks) ; Last element is nil
    (setf (print-data-pagebreaks data) (reverse pagebreaks))
    (setf (gtk:print-operation-n-pages operation) (length pagebreaks))))

(defun draw-page (operation context page-nr data)
  (declare (ignore operation))
  (format t "in DRAW-PAGE for page ~a~%" page-nr)
  (let* ((start (if (= 0 page-nr)
                    0
                    (elt (print-data-pagebreaks data) (1- page-nr))))
         (pagebreak (elt (print-data-pagebreaks data) page-nr))
         (end (if pagebreak
                  pagebreak
                  (pango:layout-line-count (print-data-layout data))))
         (cr (gtk:print-context-cairo-context context)))
    (cairo:set-source-rgb cr 0 0 0)
    (let ((iter (pango:layout-iter (print-data-layout data)))
          (startpos 0))
      (iter (for i from 0)
            (while (and (< i end) (pango:layout-iter-next-line iter)))
            (when (>= i start)
              (let ((line (pango:layout-iter-line iter))
                    (baseline (pango:layout-iter-baseline iter)))
                (pango:with-rectangle (ink)
                  (pango:layout-iter-line-extents iter ink nil)
                  (let ((x (pango:rectangle-x ink))
                        (y (pango:rectangle-y ink)))
                    (when (= i start)
                      (setf startpos (/ y 1024)))
                      (cairo:move-to cr (/ x 1024)
                                        (- (/ baseline 1024) startpos))
                      (pango:cairo-show-layout-line cr line)))))))))

(defun create-custom-widget (operation data)
  (format t "in CREAGE-CUSTOM-WIDGET~%")
  (let ((vbox (make-instance 'gtk:box
                             :orientation :vertical
                             :margin-top 12
                             :margin-start 12))
        (hbox (make-instance 'gtk:box
                              :orientation :horizontal
                              :spacing 6))
        (label (make-instance 'gtk:label
                              :label "Font:")))
    (setf (gtk:print-operation-custom-tab-label operation) "Other")
    (gtk:box-append hbox label)
    (setf (print-data-fontbutton data)
          (gtk:font-button-new-with-font (print-data-font data)))
    (gtk:box-append hbox (print-data-fontbutton data))
    (gtk:box-append vbox hbox)
    (gtk:widget-show vbox)
    vbox))

(defun custom-widget-apply (operation widget data)
  (declare (ignore operation widget))
  (format t "in CUSTOM-WIDGET-APPLY~%")
  (setf (print-data-font data)
        (gtk:font-chooser-font (print-data-fontbutton data))))


(defun print-done (operation result data)
  (declare (ignore operation result data))
  (format t "in PRINT-DONE~%"))

(defun end-print (operation context data)
  (declare (ignore operation context data))
  (format t "in END-PRINT~%"))

(defun print-or-preview (action printaction)
  (declare (ignore action))
  (let ((data (make-instance 'print-data))
        (print (gtk:print-operation-new)))

    (setf (print-data-text data) (gtk:text-buffer-text *buffer*))
    (setf (print-data-font data) "Sans 12")

    (setf (gtk:print-operation-track-print-status print) t)
    (when *settings*
      (setf (gtk:print-operation-print-settings print) *settings*))
    (when *pagesetup*
      (setf (gtk:print-operation-default-page-setup print) *pagesetup*))
    (g:signal-connect print "begin-print"
                      (lambda (operation context)
                        (begin-print operation context data)))
    (g:signal-connect print "end-print"
                      (lambda (operation context)
                        (end-print operation context data)))
    (g:signal-connect print "draw-page"
                      (lambda (operation context page-nr)
                        (draw-page operation context page-nr data)))
    (g:signal-connect print "create-custom-widget"
                      (lambda (operation)
                        (create-custom-widget operation data)))
    (g:signal-connect print "custom-widget-apply"
                      (lambda (operation widget)
                        (custom-widget-apply operation widget data)))
    (g:signal-connect print "done"
                      (lambda (operation result)
                        (print-done operation result data)))
    (setf (gtk:print-operation-export-filename print)
          (sys-path "test.pdf"))
    (when (eq :apply (gtk:print-operation-run print printaction *window*))
      (setf *settings* (gtk:print-operation-print-settings print)))))

(defun activate-print (action parameter)
  (declare (ignore parameter))
  (print-or-preview action :print-dialog))

(defun activate-preview (action parameter)
  (declare (ignore parameter))
  (print-or-preview action :preview))

(defun activate-page-setup (action parameter)
  (declare (ignore action parameter))
  (setf *pagesetup*
        (gtk:print-run-page-setup-dialog *window* *pagesetup* *settings*)))

(defun activate-save-as (action parameter)
  (declare (ignore action parameter))
  (format t "in ACTIVATE-SAVE-AS~%")
  (let ((dialog (gtk:file-chooser-dialog-new "Select file"
                                             *window*
                                             :save
                                             "_Cancel" :cancel
                                             "_Save" :ok)))
    (gtk:dialog-set-default-response dialog :ok)
    (setf (gtk:window-modal dialog) t)
    (g:signal-connect dialog "response"
                      (lambda (dialog response)
                        (format t "in RESPONSE ~a~%" response)
                        (when (= -5 response) ; for :ok
                          (setf *filename*
                                (gtk:file-chooser-namestring dialog))
                          (format t "save to filename : ~a~%" *filename*)
                          (save-file-buffer *buffer* *filename*))
                        (gtk:window-destroy dialog)))
    (gtk:widget-show dialog)))

(defun activate-save (action parameter)
  (if *filename*
      (save-file-buffer *buffer* *filename*)
      (activate-save-as action parameter)))

(defun activate-open (action parameter)
  (declare (ignore action parameter))
  (format t "in ACTIVATE-OPEN~%")
  (let ((dialog (gtk:file-chooser-dialog-new "Select file"
                                             *window*
                                             :open
                                             "_Cancel" :cancel
                                             "_Open" :ok)))
    (gtk:dialog-set-default-response dialog :ok)
    (setf (gtk:window-modal dialog) t)
    (g:signal-connect dialog "response"
                      (lambda (dialog response)
                        (format t "in RESPONSE ~a~%" response)
                        (when (= -5 response) ; for :ok
                          (setf *filename*
                                (gtk:file-chooser-namestring dialog))
                          (format t "new filename : ~a~%" *filename*)
                          (load-file-buffer *buffer* *filename*))
                        (gtk:window-destroy dialog)))
    (gtk:widget-show dialog)))

(defun activate-new (action parameter)
  (declare (ignore action parameter))
  (setf (gtk:text-buffer-text *buffer*) "")
  (setf *filename* nil)
  (setf *file-changed* nil)
  (update-statusbar *statusbar*)
  (update-title *window*))

(defun activate-about (action parameter)
  (declare (ignore action parameter))
  (gtk:show-about-dialog *window*
                         :program-name "GTK4 Print Editor"
                         :version "0.9"
                         :copyright "(c) Dieter Kaiser"
                         :website
                         "github.com/crategus/cl-cffi-gtk"
                         :website-label "Project web site"
                         :license-type :mit-x11
                         :authors '("Dieter Kaiser")
                         :documenters '("Dieter Kaiser")
                         :artists '("None")
                         :logo-icon-name
                         "applications-development"
                         :wrap-license t))

(defun activate-quit (action parameter application)
  (declare (ignore action parameter))
  (let ((windows (gtk:application-windows application)))
    (dolist (window windows)
      (gtk:window-destroy window))))

(defun print-editor-startup (application)
  (format t "in STARTUP~%")
  (let ((accels (list "app.new" "<Control>n"
                      "app.quit" "<Control>q"
                      "app.save" "<Control>s"
                      "app.about" "<Control>a"))
        (builder (gtk:builder-new)))
    (gtk:builder-add-from-string builder *ui-info*)
    (setf (gtk:application-menubar application)
          (gtk:builder-object builder "menubar"))
    (loop for (name accel) on accels by #'cddr
          do (setf (gtk:application-accels-for-action application name)
                   accel))))

(defun print-editor-activate (application)
  (format t "in ACTIVATE~%")
  (let* ((box (make-instance 'gtk:box
                             :orientation :vertical))
         (window (make-instance 'gtk:application-window
                                :application application
                                :child box
                                :icon-name "text-editor"
                                :default-height 400
                                :default-width 600
                                :show-menubar t))
         (textview (make-instance 'gtk:text-view))
         (scrolled (make-instance 'gtk:scrolled-window
                                  :child textview
                                  :hscroll-policy :automatic
                                  :vscroll-policy :automatic
                                  :has-frame t
                                  :vexpand t))
         (statusbar (make-instance 'gtk:statusbar)))
    ;; Store window in a global variable
    (setf *window* window)
    ;; Store statusbar in a global varialbe
    (setf *statusbar* statusbar)
    ;; The text buffer is a global variable. Can we improve this?
    (setf *buffer* (gtk:text-view-buffer textview))
    (g:signal-connect *buffer* "changed"
                      (lambda (buffer)
                        (declare (ignore buffer))
                        (setf *file-changed* t)
                        (update-statusbar statusbar)))
    (g:signal-connect *buffer* "mark-set"
                      (lambda (buffer location mark)
                        (declare (ignore buffer location mark))
                        (update-statusbar statusbar)))
    (gtk:box-append box scrolled)
    (gtk:box-append box statusbar)
    (gtk:widget-grab-focus textview)
    (setf *file-changed* nil)
    (update-title window)
    (update-statusbar statusbar)
    (gtk:window-present window)))

(defun print-editor-open (application files n-files hint)
  (declare (ignore files hint))
  (format t "in OPEN~%")
  (when (> n-files 1)
    (format t "Can only open a single file.~%"))
  (print-editor-activate application)
  (load-file-buffer *buffer* (sys-path "gtk4-print-editor.lisp")))

(defun gtk4-print-editor (&rest argv)
  (let* ((app (make-instance 'gtk:application
                             :application-id "com.crategus.gtk4-print-editor"
                             :flags :handles-open))
         (entries (list (list "new" #'activate-new)
                        (list "open" #'activate-open)
                        (list "save" #'activate-save)
                        (list "save-as" #'activate-save-as)
                        ;; We use a lambda function to pass user data
                        (list "quit" #'(lambda (action parameter)
                                         (activate-quit action parameter app)))
                        (list "about" #'activate-about)
                        (list "page-setup" #'activate-page-setup)
                        (list "preview" #'activate-preview)
                        (list "print" #'activate-print)))
         (argv (cons "gtk4-print-editor"
                     (if argv argv (uiop:command-line-arguments))))
         (path-settings (sys-path "print-settings.ini"))
         (settings (gtk:print-settings-new-from-file path-settings))
         (path-pagesetup (sys-path "page-setup.ini"))
         (pagesetup (gtk:page-setup-new-from-file path-pagesetup)))
    (setf *settings* settings)
    (unless *settings*
      (format t "~&No settings available. Create a new print settings object.")
      (setf *settings* (gtk:print-settings-new)))
    (setf *pagesetup* pagesetup)
    (unless *pagesetup*
      (format t "~&No page setup available. Create a new page setup object.")
      (setf *pagesetup* (gtk:page-setup-new)))
    (g:action-map-add-action-entries app entries)
    (g:signal-connect app "startup" #'print-editor-startup)
    (g:signal-connect app "activate" #'print-editor-activate)
    (g:signal-connect app "open" #'print-editor-open)
    (g:application-run app argv)
    (unless (gtk:print-settings-to-file *settings* path-settings)
      (format t "Failed to save print settings."))
    (unless (gtk:page-setup-to-file *pagesetup* path-pagesetup)
      (format t "Failed to save page setup."))))

;;; --- End of file gtk4.print-editor.lisp -------------------------------------

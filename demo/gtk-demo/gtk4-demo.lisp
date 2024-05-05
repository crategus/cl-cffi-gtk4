;;;; GTK 4 Demo
;;;;
;;;; GTK Demo is a collection of useful examples to demonstrate GTK widgets
;;;; and features. It is a useful example in itself.
;;;;
;;;; You can select examples in the sidebar or search for them by typing a
;;;; search term. Double-clicking or hitting the "Run" button will run the
;;;; demo. The source code and other resources used in the demo are shown in
;;;; this area.
;;;;
;;;; You can also use the GTK Inspector, available from the menu on the top
;;;; right, to poke at the running demos, and see how they are put together.
;;;;
;;;; Last version: 2024-5-5

(in-package :gtk4-demo)

;; Globals
(defvar *infoview* nil)
(defvar *sourceview* nil)
(defvar *currentfile* nil)
(defvar *notebook* nil)
(defvar *selection* nil)
(defvar *searchneedle* nil)

;;; ----------------------------------------------------------------------------

(gobject:define-g-object-subclass "GtkDemo" gtk-demo
  (:superclass g:object
   :export t
   :interfaces ())
  ((title
    gtk-demo-title
    "title" "gchararray" t t)
   (ftype
    gtk-demo-ftype
    "ftype" "gchararray" t t)
   (name
    gtk-demo-name
    "name" "gchararray" t t)
   (package
    gtk-demo-package
    "package" "gchararray" t t)
   (filename
    gtk-demo-filename
    "filename" "gchararray" t t)
   (resources
    gtk-demo-resources
    "resources" "GStrv" t t)
   (keywords
    gtk-demo-keywords
    "keywords" "GStrv" t t)
   (children
    gtk-demo-children
    "children" "GListStore" t t)))

;;; ----------------------------------------------------------------------------

;; Create the list store as the model for the data

(defun mklist (obj)
  (if (listp obj)
      obj
      (list obj)))

(defun create-and-fill-list-store (data &optional (parent nil))
  (let ((model (g:list-store-new "GtkDemo")))
    (dolist (entry (mklist data))
      (cond ((or (atom entry) (every #'atom entry))
             (destructuring-bind (title &optional ftype name package file
                                        &rest args)
                 (mklist entry)
               (let* (;; Extract keywords from args
                      (keywords (mapcar #'string-upcase
                                        (rest (member :keywords args))))
                      ;; More keywords from the title
                      (keywords2 (mapcar #'string-upcase
                                         (split-sequence:split-sequence #\space
                                                                        title)))
                      ;; Extract resources from args
                      (resources (if keywords
                                     (butlast args (1+ (length keywords)))
                                     args))
                      ;; Create GtkDemo object
                      (item (make-instance 'gtk-demo
                                           :title title
                                           :ftype ftype
                                           :name name
                                           :package package
                                           :filename file
                                           :resources resources
                                           :keywords
                                           (append keywords keywords2)
                                           :children nil)))
                 (setf parent item)
                 (g:list-store-append model item))))
            ((some #'listp entry)
             (let ((model (create-and-fill-list-store entry parent)))
               (setf (gtk-demo-children parent) model)))))
  model))

(defun find-demo-by-name (data name &optional result)
  (dolist (entry (mklist data))
    (cond ((or (atom entry) (every #'atom entry))
           (destructuring-bind (title
                                &optional ftype name1 package file
                                &rest args)
               (mklist entry)
             (declare (ignore args))
             (when (string-equal name name1)
               (setf result (make-instance 'gtk-demo
                                           :title title
                                           :ftype ftype
                                           :name name
                                           :package package
                                           :filename file
                                           :children nil))
               (return))))
          ((some #'listp entry)
           (setf result (find-demo-by-name entry name result)))))
    result)

(defun dump-list-store (model &optional (prefix ""))
  (dotimes (i (g:list-store-n-items model))
    (let ((object (g:list-model-object model i)))
      (if (gtk-demo-name object)
          (format t "~a~a~%" prefix (string-upcase (gtk-demo-name object)))
          (format t "~%~a~a~%" prefix (gtk-demo-title object)))
    (when (gtk-demo-children object)
      (let ((model (gtk-demo-children object)))
      (dump-list-store model (concatenate 'string "  " prefix)))))))

;;; ----------------------------------------------------------------------------

;; Load selected source file and the resources in notebook tabs

;; TODO: Complete the implementation and rewrite the SELECTION-CB and
;; ADD-DATA-TABS functions more elegantly

(defun sys-path (filename &optional (package :gtk4-demo))
  (let ((system-path (asdf:system-source-directory package)))
    (princ-to-string (merge-pathnames filename system-path))))

(defun clear-buffer (buffer)
  (multiple-value-bind (start end)
      (gtk:text-buffer-bounds buffer)
    (gtk:text-buffer-delete buffer start end)))

(defun load-file (filename)
  (let ((sourcebuffer (gtk:text-buffer-new))
        (infobuffer (gtk:text-buffer-new)))
    (gtk:text-buffer-create-tag infobuffer "title"
                                :size (* 12 pango:+scale+)
                                :weight 700
                                :pixels-below-lines 3)
    (with-open-file (stream filename)
      ;; Read the info-header of the file
      (clear-buffer infobuffer)
      (let ((firstline t)
            (iter (gtk:text-buffer-start-iter infobuffer)))
        (do ((line (read-line stream nil)
                   (read-line stream nil)))
            ((or (null line)
                 (not (>= (length line) 4))
                 (not (string= line ";;;;" :start1 0 :end1 4))))
          (cond (firstline
                 (setf firstline nil)
                 (gtk:text-buffer-insert-with-tags infobuffer
                                                   iter
                                                   (string-left-trim "; " line)
                                                   "title")
;                 (gtk:text-buffer-insert infobuffer iter (format nil "~%"))
                 )
                (t
                 (setf line (string-left-trim "; " line))
                 (gtk:text-buffer-insert-markup infobuffer
                                                iter
                                                line)
                 (gtk:text-buffer-insert-markup infobuffer iter " ")))
          (when (string= "" line)
            (gtk:text-buffer-insert infobuffer iter (format nil "~%")))))
      ;; Read the source code of the file
      (clear-buffer sourcebuffer)
      (do ((line (read-line stream nil)
                 (read-line stream nil)))
          ((null line))
        (gtk:text-buffer-insert sourcebuffer :cursor line)
        (gtk:text-buffer-insert sourcebuffer :cursor (format nil "~%")))
      (setf (gtk:text-view-buffer *sourceview*) sourcebuffer)
      (setf (gtk:text-view-buffer *infoview*) infobuffer))))

(defun load-file-buffer (buffer filename)
  (with-open-file (stream filename)
    (clear-buffer buffer)
    (do ((line (read-line stream nil)
               (read-line stream nil)))
        ((null line))
      (gtk:text-buffer-insert buffer :cursor line)
      (gtk:text-buffer-insert buffer :cursor (format nil "~%")))))

(defun add-data-tabs (demo)
  (let ((resources (gtk-demo-resources demo)))
    ;; Remove existing data tabs from the notebook
    (dotimes (i (- (gtk:notebook-n-pages *notebook*) 2))
      (gtk:notebook-remove-page *notebook* 2))
    (dolist (resource resources)
      (let ((file (sys-path resource (gtk-demo-package demo))))
        (cond ((member (pathname-type file)
                       '("css" "ui" "xml" "lisp" "asd" "txt" "ini")
                       :test #'string-equal)
               (let* ((buffer (gtk:text-buffer-new))
                      (view (make-instance 'gtk:text-view
                                           :buffer buffer
                                           :left-margin 12
                                           :right-margin 12
                                           :top-margin 12
                                           :bottom-margin 12
                                           :pixels-above-lines 2
                                           :pixels-below-lines 2
                                           :monospace t))
                      (scrolled (make-instance 'gtk:scrolled-window
                                               :child view))
                      (label (make-instance 'gtk:label
                                            :label (file-namestring file))))
                 (load-file-buffer buffer file)
                 (gtk:notebook-add-page *notebook* scrolled label)))
              (t
               ;; Add code to display images and videos
               (format t " not implemented for type ~a~%"
                         (pathname-type file))))))))

(defun selection-cb (window selection)
  (let* ((row (gtk:single-selection-selected-item selection))
         (demo (if row (gtk:tree-list-row-item row))))
    (when (and demo (gtk-demo-filename demo))
      (load-file (sys-path (gtk-demo-filename demo) (gtk-demo-package demo)))
      (add-data-tabs demo)
      (setf (gtk:window-title window) (gtk-demo-title demo)))))

;;; ----------------------------------------------------------------------------

;; Run a demo

(defun window-draw-func (title drawfunc application
                         &optional (width 600) (height 600))
  (let* ((area (make-instance 'gtk:drawing-area))
         (window (make-instance 'gtk:window
                                 :application application
                                 :child area
                                 :title title
                                 :default-width width
                                 :default-height height)))
    ;; Set the drawing function
    (gtk:drawing-area-set-draw-func area
        (lambda (widget cr width height)
          (declare (ignore widget))
          (funcall drawfunc cr width height)))
    ;; Show the window.
    (setf (gtk:widget-visible window) t)))

(defun gtk-demo-run (demo application)
  (let* ((title (gtk-demo-title demo))
         (functype (read-from-string (gtk-demo-ftype demo)))
         (funcname (string-upcase (gtk-demo-name demo)))
         (package  (string-upcase (gtk-demo-package demo)))
         (func (find-symbol funcname (find-package package))))
    (cond (;; Example as a window for application
           (eq functype :window)
           (funcall func application))
          (;; Example as a parent to the active application window
           (eq functype :dialog)
           (funcall func (gtk:application-active-window application)))
          (;; Example called from a draw handler
           (eq functype :drawfunc)
           (window-draw-func title func application))
          (t (format t "NO function found.")))))

;;; ----------------------------------------------------------------------------

;; Callback functions for actions

(defun activate-about (action parameter)
  (declare (ignore action parameter))
  (let ((version (asdf:component-version (asdf:find-system :gtk4-demo))))
    (gtk:show-about-dialog nil
                           :modal t
                           :program-name "GTK4 Lisp Demo"
                           :version version
                           :copyright "(c) Dieter Kaiser"
                           :website "github.com/crategus/cl-cffi-gtk4"
                           :website-label "Project web site"
                           :license "MIT"
                           :authors '("Dieter Kaiser")
                           :documenters '("Dieter Kaiser")
                           :artists '("None")
                           :logo-icon-name "applications-development"
                           :wrap-license t)))

(defun activate-quit (application)
  (let ((windows (gtk:application-windows application)))
    (dolist (window windows)
      (gtk:window-destroy window))))

(defun activate-inspector (action parameter)
  (declare (ignore action parameter))
  (gtk:window-set-interactive-debugging t))

(defun activate-run (application action parameter)
  (declare (ignore action parameter))
  (let* ((row (gtk:single-selection-selected-item *selection*))
         (demo (gtk:tree-list-row-item row)))
    (when (gtk-demo-name demo)
      (gtk-demo-run demo application))))

(defun activate-cb (listview position application)
  (let* ((model (gtk:list-view-model listview))
         (row (g:list-model-object model position))
         (demo (gtk:tree-list-row-item row)))
    (when (gtk-demo-name demo)
      (gtk-demo-run demo application))))

;;; ----------------------------------------------------------------------------

;; Functions for searching examples

(defun clear-search (searchbar filter)
  (unless (gtk:search-bar-search-mode-enabled searchbar)
    (let ((entry (gtk:search-bar-child searchbar)))
      (setf *searchneedle* nil)
      (setf (gtk:editable-text entry) "")
      (gtk:filter-changed filter :different))))

(defun demo-search-changed-cb (entry filter)
  (let ((text (gtk:editable-text entry)))
    (if (= 0 (length text))
        (setf *searchneedle* nil)
        (setf *searchneedle*
              (mapcar #'string-upcase
                      (split-sequence:split-sequence #\space text))))
    (gtk:filter-changed filter :different)))

(defun demo-filter-by-name (row)
  (let* ((parent (gtk:tree-list-row-parent row)))
    ;; Does the parent row match
    (when parent
      (let* ((demo (gtk:tree-list-row-item parent))
             (keywords (gtk-demo-keywords demo)))
        (when (subsetp *searchneedle* keywords :test #'search)
          ;; Return T for all children of the parent
          (return-from demo-filter-by-name t))))
    ;; No match for the parent, check the row itself
    (let* ((demo (gtk:tree-list-row-item row))
           (keywords (gtk-demo-keywords demo)))
      (subsetp *searchneedle* keywords :test #'search))))

;;; ----------------------------------------------------------------------------

;; Callback functions for application handler

(defun get-child-model (object)
  (when (gtk-demo-children object)
    ;; We have to increase the reference counter
    ;; Can we improve the implementation at another place?
    (gobject:object-ref (gtk-demo-children object))))

(defun activate (application)
  ;; Register the resources
  (gio:with-g-resources (resource (sys-path "gtk4-demo.gresource"))
    (let* ((builder (gtk:builder-new-from-resource "/ui/main.ui"))
           (window (gtk:builder-object builder "window"))
           (listview (gtk:builder-object builder "listview"))
           (searchbar (gtk:builder-object builder "searchbar"))
           (action (g:simple-action-new "run" nil))
           (listmodel (create-and-fill-list-store *gtk-demos*))
           (treemodel (gtk:tree-list-model-new listmodel
                                               nil
                                               t
                                               #'get-child-model))
           (filter (gtk:custom-filter-new #'demo-filter-by-name))
           (filtermodel (gtk:filter-list-model-new treemodel filter))
           (selection (gtk:single-selection-new filtermodel))
           (searchentry (gtk:builder-object builder "search-entry")))
      (gtk:application-add-window application window)
      (g:signal-connect window "close-request"
                        (lambda (window)
                          (declare (ignore window))
                          (activate-quit application)))
      (g:signal-connect action "activate"
                        (lambda (action parameter)
                          (activate-run application action parameter)))
      (g:action-map-add-action window action)
      (setf *notebook* (gtk:builder-object builder "notebook"))
      (setf *infoview* (gtk:builder-object builder "info-textview"))
      (setf *sourceview* (gtk:builder-object builder "source-textview"))
      (setf *selection* selection)
      (g:signal-connect listview "activate"
                        (lambda (listview position)
                          (activate-cb listview position application)))
      (g:signal-connect searchbar "notify::search-mode-enabled"
                        (lambda (searchbar pspec)
                          (declare (ignore pspec))
                          (clear-search searchbar filter)))
      (g:signal-connect searchentry "search-changed"
                        (lambda (entry)
                          (demo-search-changed-cb entry filter)))
      (g:signal-connect selection "notify::selected-item"
                        (lambda (selection pspec)
                          (declare (ignore pspec))
                          (selection-cb window selection)))
      (setf (gtk:list-view-model listview) selection)
      (selection-cb window selection))))

(defun command-line (application cmdline)
  (let ((options (g:application-command-line-options-dict cmdline))
        (status 0))
    ;; Activate the application
    (g:application-activate application)
    ;; Check for options LIST and RUN
    (cond ((g:variant-dict-contains options "list")
           (dump-list-store (create-and-fill-list-store *gtk-demos*))
           (activate-quit application))
          ((g:variant-dict-contains options "run")
           (let* ((variant (g:variant-dict-lookup-value options "run" "s"))
                  (name (g:variant-string variant))
                  (demo (find-demo-by-name *gtk-demos* name))
                  (functype (when demo
                                  (read-from-string (gtk-demo-ftype demo)))))
             (cond ((and demo (eq :window functype))
                    (gtk-demo-run demo application)
                    (let ((window (gtk:application-active-window application)))
                      (g:signal-connect window "close-request"
                                        (lambda (window)
                                          (declare (ignore window))
                                          (activate-quit application)))))
                   ((and demo (eq :dialog functype))
                    (let ((window (make-instance 'gtk:application-window
                                                 :application application
                                                 :title (gtk-demo-title demo)
                                                 :default-width 600
                                                 :default-height 400)))
                     (setf (gtk:widget-visible window) t)
                     (gtk-demo-run demo application)
                     (g:signal-connect window "close-request"
                                       (lambda (window)
                                         (declare (ignore window))
                                         (activate-quit application)))))
                   ((and demo (eq :drawfunc functype))
                    (gtk-demo-run demo application)
                    (let ((window (gtk:application-active-window application)))
                      (g:signal-connect window "close-request"
                                        (lambda (window)
                                          (declare (ignore window))
                                          (activate-quit application)))))
                   (t
                    (format t "Example ~a not found~%" (string-upcase name))
                    (g:application-quit application)))))
           (t
            ;; Show the application window
            (gtk:window-present (gtk:application-active-window application))))
    ;; Option AUTOQUIT
    (when (g:variant-dict-contains options "autoquit")
      (g:timeout-add-seconds 3
                             (lambda ()
                               (activate-quit application)
                               glib:+source-remove+)))
    status))

(defun local-options (application options)
  (declare (ignore application))
  (let ((status -1))
    (when (g:variant-dict-contains options "version")
      (format t "GTK 4 Demo Version ~a~%"
                (asdf:component-version (asdf:find-system :gtk4-demo)))
      (setf status 0))
    status))

;;; ----------------------------------------------------------------------------

(defun gtk4-demo (&rest argv)
  (let* (;; Create the application
         (app (make-instance 'gtk:application
                             :application-id "com.crategus.gtk4-demo"
                             :flags :handles-command-line))
         ;; Get the command line arguments
         (argv (cons "gtk4-demo" (or argv (uiop:command-line-arguments))))
         ;; Define action entries
         (entries (list (list "about" #'activate-about)
                        (list "quit"
                              (lambda (action parameter)
                                (declare (ignore action parameter))
                                (activate-quit app)))
                        (list "inspector" #'activate-inspector)))
         ;; Define options
         (options (list (list "version"                     ; long name
                              #\v                           ; short name
                              :none                         ; flags
                              :none                         ; arg
                              nil                           ; arg data
                              "Show program version"        ; description
                              nil)                          ; arg description
                        (list "run" #\r
                              :none
                              :string
                              nil
                              "Run Example"
                              "EXAMPLE")
                         (list "list" #\l
                               :none
                               :none
                               nil
                               "List examples"
                               nil)
                         (list "autoquit" #\a
                               :none
                               :none
                               nil
                               "Quit after a delay"
                               nil))))
    ;; Add actions entries for the application
    (g:action-map-add-action-entries app entries)
    ;; Add accelerators for the application
    (setf (gtk:application-accels-for-action app "app.about") "F1")
    (setf (gtk:application-accels-for-action app "app.quit") "<Control>q")
    (setf (gtk:application-accels-for-action app "app.inspector") "<Control>d")
    ;; Add comand line options
    (g:application-add-main-option-entries app options)
    ;; Connect signal handlers for the application
    (g:signal-connect app "activate" #'activate)
    (g:signal-connect app "command-line" #'command-line)
    (g:signal-connect app "handle-local-options" #'local-options)
    ;; Run the application
    (g:application-run app argv)))

;;; --- End of file gtk4-demo.lisp ---------------------------------------------

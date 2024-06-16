;;;; Application Sunny
;;;;
;;;; 2024-6-2

(in-package :gtk4-application)

(defparameter *sunny-menus*
"<interface>
  <menu id='menubar'>
    <submenu>
      <attribute name='label' translatable='yes'>Sunny</attribute>
      <section>
        <item>
          <attribute name='label' translatable='yes'>_New Window</attribute>
          <attribute name='action'>app.new</attribute>
        </item>
        <item>
          <attribute name='label' translatable='yes'>_About Sunny</attribute>
          <attribute name='action'>app.about</attribute>
        </item>
        <item>
          <attribute name='label' translatable='yes'>_Quit</attribute>
          <attribute name='action'>app.quit</attribute>
          <attribute name='accel'>&lt;Primary&gt;q</attribute>
        </item>
      </section>
    </submenu>
  </menu>
</interface>")

(gobject:define-g-object-subclass "Sunny" sunny
  (:superclass gtk:application
   :export t
   :interfaces ())
  nil)

(defun new-sunny-window (application filename)
  (let* ((textview (make-instance 'gtk:text-view
                                  :monospace t))
         (scrolled (make-instance 'gtk:scrolled-window
                                  :child textview
                                  :hexpand t
                                  :vexpand t))
         (overlay (make-instance 'gtk:overlay
                                 :child scrolled))
         (window (make-instance 'gtk:application-window
                                :application application
                                :title "Sunny"
                                :child overlay
                                :show-menubar t
                                :icon-name "sunny"
                                :default-width 480
                                :default-height 320)))
    ;; Load the file into the buffer
    (when (and filename (probe-file filename))
      (let ((buffer (gtk:text-view-buffer textview)))
        (gtk:text-buffer-load-file buffer filename)))
    ;; Present the window
    (gtk:window-present window)))

(defun sunny-action-new (application action parameter)
  (declare (ignore action parameter))
  (g:application-activate application))

(defun sunny-action-about (application action parameter)
  (declare (ignore action parameter))
  (let ((parent (gtk:application-active-window application)))
    (gtk:show-about-dialog parent
                           :modal t
                           :program-name "Sunny"
                           :title "About Sunny"
                           :logo-icon-name "sunny"
                           :comments "A cheap Bloatpad clone.")))

(defun sunny-action-quit (application action parameter)
  (declare (ignore action parameter))
  (let ((windows (gtk:application-windows application)))
    (dolist (window windows)
      (gtk:window-destroy window))))

(defun sunny (&rest argv)
  (let ((argv (cons "sunny" (or argv (uiop:command-line-arguments))))
        ;; Create the application
        (app (make-instance 'sunny
                            :application-id "com.crategus.sunny"
                            :flags :handles-open)))
    ;; Connect signal "startup" to the application
    (g:signal-connect app "startup"
        (lambda (application)
          (let ((actions (list (list "about"
                                     #'(lambda (action parameter)
                                         (sunny-action-about application
                                                             action
                                                             parameter)))
                               (list "quit"
                                     #'(lambda (action parameter)
                                         (sunny-action-quit application
                                                            action
                                                            parameter)))
                               (list "new"
                                     #'(lambda (action parameter)
                                         (sunny-action-new application
                                                           action
                                                           parameter)))))
                (builder (make-instance 'gtk:builder)))
          ;; Set the application name
          (unless (g:application-name)
            (setf (g:application-name) "Sunny"))
          ;; Add the actions
          (g:action-map-add-action-entries application actions)
          ;; The application shows the menu
          (setf (gtk:settings-gtk-shell-shows-app-menu (gtk:settings-default))
                nil)
          ;; Read the menus from a string
          (gtk:builder-add-from-string builder *sunny-menus*)
          ;; Set the application menu
          (setf (gtk:application-menubar application)
                (gtk:builder-object builder "menubar"))
          ;; Set accelerators
          (setf (gtk:application-accels-for-action application "app.new")
                "<Control>n")
          ;; TODO: The accelerator from the UI definition does not work.
          ;; but with the following definition it does
          (setf (gtk:application-accels-for-action application "app.quit")
                "<Control>q")
          (setf (gtk:application-accels-for-action application "app.about")
                "F1"))))
    ;; Connect signal "open" to the application
    (g:signal-connect app "open"
        (lambda (application files n-files hint)
          (declare (ignore hint))
          (dotimes (i n-files)
            (let* ((file (cffi:mem-aref files '(g:object g:file) i))
                   (filename (g:file-get-parse-name file)))
              (new-sunny-window application filename)))))
    ;; Connect signal "activate" to the application
    (g:signal-connect app "activate"
                          (lambda (application)
                            (new-sunny-window application nil)))
    ;; Run the application
    (g:application-run app argv)))

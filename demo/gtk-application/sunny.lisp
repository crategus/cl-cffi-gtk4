;;;; Example Sunny - 2023-8-6

;; FIXME: The keyboard accelerators do not work as expected. What is wrong?

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

(defun clear-buffer (buffer)
  (multiple-value-bind (start end)
      (gtk:text-buffer-bounds buffer)
    (gtk:text-buffer-delete buffer start end)))

(defun load-file-into-buffer (buffer filename)
  (with-open-file (stream filename)
    (clear-buffer buffer)
    (do ((line (read-line stream nil)
               (read-line stream nil)))
        ((null line))
      (gtk:text-buffer-insert buffer line)
      (gtk:text-buffer-insert buffer (format nil "~%")))))

(defclass sunny (gtk:application)
  ()
  (:metaclass gobject:gobject-class))

(defun new-sunny-window (application filename)
  (let* ((textview (make-instance 'gtk:text-view))
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
    (when filename
      (let ((buffer (gtk:text-view-buffer textview)))
        (load-file-into-buffer buffer (sys-path filename))))
    ;; Show the window
    (gtk:window-present window)))

(defun sunny-action-new (application action parameter)
  (declare (ignore action parameter))
  (g:application-activate application))

(defun sunny-action-about (application action parameter)
  (declare (ignore application action parameter))
  (gtk:show-about-dialog nil
                         :modal t
                         :program-name "Sunny"
                         :title "About Sunny"
                         :logo-icon-name "sunny"
                         :comments "A cheap Bloatpad clone."))

(defun sunny-action-quit (application action parameter)
  (declare (ignore action parameter))
  (let ((windows (gtk:application-windows application)))
    (dolist (window windows)
      (gtk:window-destroy window))))

(defun sunny (&rest argv)
  (let ((argv (cons "sunny" (if argv argv (uiop:command-line-arguments))))
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
                                                             parameter))
                                     nil nil nil)
                               (list "quit"
                                     #'(lambda (action parameter)
                                         (sunny-action-quit application
                                                            action
                                                            parameter))
                                     nil nil nil)
                               (list "new"
                                     #'(lambda (action parameter)
                                         (sunny-action-new application
                                                           action
                                                           parameter))
                                     nil nil)))
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
          ;; Set a second accelarator by hand
          ;; FIXME: Both accelarators for "new" and "quit" does not work
          ;; What is wrong?
          (setf (gtk:application-accels-for-action application "app::new")
                '("<Control>n"))
          )))
    ;; Connect signal "open" to the application
    (g:signal-connect app "open"
        (lambda (application files n-files hint)
          (declare (ignore hint))
          (dotimes (i n-files)
            (let* ((file (cffi:mem-aref files '(g:object g:file) i))
                   (filename (g:file-basename file)))
              (new-sunny-window application filename)))))
    ;; Connect signal "activate" to the application
    (g:signal-connect app "activate"
                          (lambda (application)
                            (new-sunny-window application nil)))
    ;; Run the application
    (g:application-run app argv)))

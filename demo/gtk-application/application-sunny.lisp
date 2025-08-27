;;;; Application Sunny
;;;;
;;;; This example allows several filenames to be passed in on the command line.
;;;; The files are then opened and displayed in their own text view window.
;;;;
;;;; The application is created using the :handles-open value for the FLAGS
;;;; flags for the FLAGS property of the GTK:APPLICATION object. If no command
;;;; line options are present, the ACTIVATE signal will be emitted after the
;;;; STARTUP signal, creating a new empty window.
;;;;
;;;; However, if one or more file names are passed on the command line, the OPEN
;;;; signal handler will be executed. A window is created for each file, and the
;;;; file is loaded into the text buffer of a text view to display.
;;;;
;;;; This application defines three actions: "New", "About" and "Quit". These
;;;; actions are added to the action map of the application using the
;;;; g:action-map-add-action-entries function. The corresponding keyboard
;;;; shortcuts "Ctrl+N", "F1" and "Ctrl+Q" are installed using the
;;;; gtk:application-accels-for-action function.
;;;;
;;;; This example introduces subclassing using the
;;;; gobject:define-gobject-subclass macro to define the SUNNY subclass of the
;;;; GtkApplication class. The SUNNY subclass inherits all of the properties and
;;;; signals of its parent class. In this example, no new properties or
;;;; functionality are added to the subclass. Therefore, subclassing could have
;;;; been omitted.
;;;;
;;;; Last updated: 2025-08-27

(in-package :gtk4-application)

(gobject:define-gobject-subclass "Sunny" sunny
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
                                :icon-name "gnome-weather"
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
                           :logo-icon-name "gnome-weather"
                           :comments "A cheap Bloatpad clone.")))

(defun sunny-action-quit (application action parameter)
  (declare (ignore action parameter))
  (let ((windows (gtk:application-windows application)))
    (dolist (window windows)
      (gtk:window-destroy window))))

(defun sunny (&rest argv)
  (let ((menus "<interface>
                  <menu id='menubar'>
                    <submenu>
                      <attribute name='label'>Sunny</attribute>
                      <section>
                        <item>
                          <attribute name='label'>_New Window</attribute>
                          <attribute name='action'>app.new</attribute>
                        </item>
                        <item>
                          <attribute name='label'>_About Sunny</attribute>
                          <attribute name='action'>app.about</attribute>
                        </item>
                        <item>
                          <attribute name='label'>_Quit</attribute>
                          <attribute name='action'>app.quit</attribute>
                          <attribute name='accel'>&lt;Primary&gt;q</attribute>
                        </item>
                      </section>
                    </submenu>
                  </menu>
                </interface>")
        (argv (cons "sunny" (or argv (uiop:command-line-arguments))))
        ;; Create the application
        (app (make-instance 'sunny
                            :application-id "com.crategus.sunny"
                            :flags :handles-open)))
    ;; Connect signal "startup" to the application
    (g:signal-connect app "startup"
        (lambda (application)
          (let ((actions `(("about"
                            ,(lambda (action param)
                               (sunny-action-about application action param)))
                           ("quit"
                            ,(lambda (action param)
                               (sunny-action-quit application action param)))
                           ("new"
                            ,(lambda (action param)
                               (sunny-action-new application action param)))))
                (builder (make-instance 'gtk:builder)))
          ;; Set application name
          (unless (g:application-name)
            (setf (g:application-name) "Sunny"))
          ;; Add the actions
          (g:action-map-add-action-entries application actions)
          ;; The application shows the menu
          (setf (gtk:settings-gtk-shell-shows-app-menu (gtk:settings-default))
                nil)
          ;; Read menus from a string
          (gtk:builder-add-from-string builder menus)
          ;; Set the application menu
          (setf (gtk:application-menubar application)
                (gtk:builder-object builder "menubar"))
          ;; Set accelerators
          (setf (gtk:application-accels-for-action application "app.new")
                "<Control>n")
          ;; TODO: The accelerator from the UI definition does not work.
          ;; but with the following definitions it does
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

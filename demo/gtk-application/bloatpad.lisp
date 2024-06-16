;;;; Application Bloatpad
;;;;
;;;; Last version: 2024-6-2

;; TODO: Improve the example

(in-package :gtk4-application)

(defparameter *menu*
"<?xml version='1.0' encoding='UTF-8'?>
<interface>
  <menu id='menubar'>
    <submenu>
      <attribute name='label' translatable='yes'>_Application</attribute>
      <section>
        <item>
          <attribute name='label' translatable='yes'>_New Window</attribute>
          <attribute name='action'>app.new</attribute>
        </item>
      </section>
      <section>
        <item>
          <attribute name='label' translatable='yes'>_About Bloatpad</attribute>
          <attribute name='hidden-when'>macos-menubar</attribute>
          <attribute name='action'>app.about</attribute>
        </item>
      </section>
      <section>
        <item>
          <attribute name='label' translatable='yes'>_Quit</attribute>
          <attribute name='hidden-when'>macos-menubar</attribute>
          <attribute name='action'>app.quit</attribute>
        </item>
      </section>
    </submenu>

    <submenu>
      <attribute name='label' translatable='yes'>_Edit</attribute>
      <section>
        <item>
          <attribute name='label' translatable='yes'>_Copy</attribute>
          <attribute name='action'>win.copy</attribute>
        </item>
        <item>
          <attribute name='label' translatable='yes'>_Paste</attribute>
          <attribute name='action'>win.paste</attribute>
        </item>
      </section>
      <section>
        <item>
          <!-- action should never be missing (so always shown) -->
          <attribute name='label'>Clear (always shown)</attribute>
          <attribute name='action'>win.clear</attribute>
          <attribute name='hidden-when'>action-missing</attribute>
        </item>
        <item>
          <attribute name='label'>Clear (hidden when no text)</attribute>
          <attribute name='hidden-when'>action-disabled</attribute>
          <attribute name='action'>win.clear</attribute>
        </item>
        <item>
          <attribute name='label'>Spell check (does nothing, hides)</attribute>
          <attribute name='hidden-when'>action-missing</attribute>
          <attribute name='action'>win.spell-check</attribute>
        </item>
      </section>
      <section>
        <item>
          <attribute name='label' translatable='yes'>Accelerators...</attribute>
          <attribute name='action'>app.edit-accels</attribute>
        </item>
      </section>
      <section>
        <item>
          <attribute name='label' translatable='yes'>Shortcuts...</attribute>
          <attribute name='action'>win.show-help-overlay</attribute>
        </item>
      </section>
    </submenu>
    <submenu>
      <attribute name='label' translatable='yes'>_View</attribute>
      <section>
        <item>
          <attribute name='label' translatable='yes'>_Fullscreen</attribute>
          <attribute name='action'>win.fullscreen</attribute>
        </item>
        <item>
          <attribute name='label' translatable='yes'>_Look Busy</attribute>
          <attribute name='action'>win.busy</attribute>
        </item>
      </section>
    </submenu>
    <submenu id='icon-menu'>
      <attribute name='label' translatable='yes'>_Icons</attribute>
    </submenu>
    <submenu id='time-menu'>
      <attribute name='label' translatable='yes'>Time</attribute>
      <attribute name='submenu-action'>win.time-active</attribute>
    </submenu>
  </menu>
</interface>")


(gobject:define-g-object-subclass "Bloatpad" bloatpad
  (:superclass gtk:application
   :export t
   :interfaces ())
  ((inhibit
    bloatpad-inhibit
    "inhibit" "guint" t t)
   (time
    bloatpad-timemenu
    "time" "GMenu" t t)
   (timeout
    bloatpad-timeout
    "timeout" "guint" t t)))


(defun text-buffer-changed-cb (window buffer)
  (let ((application (gtk:window-application window))
        (count (gtk:text-buffer-char-count buffer)))
    (if (> count 0)
        (when (= 0 (bloatpad-inhibit application))
          (format t "inhibit application~%")
          (setf (bloatpad-inhibit application)
                (gtk:application-inhibit
                                    application
                                    (gtk:application-active-window application)
                                    :logout
                                    "Bloatpad cannot save, you cannot logout")))
        (when (> (bloatpad-inhibit application) 0)
          (format t "uninhibit application~%")
          (gtk:application-uninhibit application (bloatpad-inhibit application))
          (setf (bloatpad-inhibit application) 0)))
    (setf (g:action-enabled (g:action-map-lookup-action window "clear"))
          (> count 0))
    (if (> count 0)
        (let ((action (g:simple-action-new "spell-check" nil)))
          (g:action-map-add-action window action))
        (g:action-map-remove-action window "spell-check"))
    (let ((line-count-old (g:object-data buffer "line-count"))
          (line-count (gtk:text-buffer-line-count buffer)))
      (unless line-count-old (setf line-count-old 0))
      (setf (g:object-data buffer "line-count") line-count)
      (when (and (< line-count-old 3) (= 3 line-count))
        (format t "Create and send a notification~%")
        (let ((notification (g:notification-new "Three lines of text")))
          (g:notification-set-body notification "Keep up the good work.")
          (g:notification-add-button notification "Start over" "app.clear-all")
          (g:application-send-notification application
                                           "three-lines"
                                           notification))))))

(defun new-bloatpad-window (application filename)
  (let* ((grid (make-instance 'gtk:grid))
         (window (make-instance 'gtk:application-window
                                :application application
                                :title "Bloatpad"
                                :child grid
                                :show-menubar t
                                :default-width 640
                                :default-height 480))
         (view (make-instance 'gtk:text-view
                              :monospace t))
         (scrolled (make-instance 'gtk:scrolled-window
                                  :child view
                                  :has-frame t
                                  :hexpand t
                                  :vexpand t))
         ;; Toolbar is a horizontal box
         (toolbar (make-instance 'gtk:box
                                 :orientation :horizontal)))

    ;; Add action "copy" to the application window
    (let ((action (g:simple-action-new "copy" nil)))
      (g:action-map-add-action window action)
      (g:signal-connect action "activate"
          (lambda (action parameter)
            (declare (ignore action parameter))
            (gtk:text-buffer-copy-clipboard (gtk:text-view-buffer view)
                                            (gtk:widget-clipboard view)))))
    ;; Add action "paste" to the application window
    (let ((action (g:simple-action-new "paste" nil)))
      (g:action-map-add-action window action)
      (g:signal-connect action "activate"
          (lambda (action parameter)
            (declare (ignore action parameter))
            (gtk:text-buffer-paste-clipboard (gtk:text-view-buffer view)
                                             (gtk:widget-clipboard view)
                                             :editable t))))

    ;; Add action "fullscreen" to the application window
    (let ((action (g:simple-action-new-stateful "fullscreen"
                                                nil
                                                (g:variant-new-boolean nil))))
      (g:action-map-add-action window action)
      (g:signal-connect action "activate"
         (lambda (action parameter)
           (declare (ignore parameter))
           (let* ((state (g:action-state action))
                  (value (g:variant-boolean state)))
             (g:action-change-state action
                                    (g:variant-new-boolean (not value))))))
      (g:signal-connect action "change-state"
         (lambda (action parameter)
           (if (g:variant-boolean parameter)
               (gtk:window-fullscreen window)
               (gtk:window-unfullscreen window))
           (setf (g:action-state action) parameter))))

    ;; Add action "busy" to the application window
    (let ((action (g:simple-action-new-stateful "busy"
                                                nil
                                                (g:variant-new-boolean nil))))
      (g:action-map-add-action window action)
      (g:signal-connect action "activate"
         (lambda (action parameter)
           (declare (ignore parameter))
           (let* ((state (g:action-state action))
                  (value (g:variant-boolean state)))
             (g:action-change-state action
                                    (g:variant-new-boolean (not value))))))
      (g:signal-connect action "change-state"
         (lambda (action parameter)
           (if (g:variant-boolean parameter)
               (let ((cursor (gdk:cursor-new-from-name "wait" nil)))
                 (setf (gtk:widget-cursor window) cursor)
                 (g:application-mark-busy application))
               (progn
                 (setf (gtk:widget-cursor window) nil)
                 (g:application-unmark-busy application)))
           (setf (g:action-state action) parameter))))

    ;; Add action "justify" to the application window
    (let ((action (g:simple-action-new-stateful "justify"
                                                (g:variant-type-new "s")
                                                (g:variant-new-string "left"))))
      (g:action-map-add-action window action)
      (g:signal-connect action "activate"
         (lambda (action parameter)
           (g:action-change-state action parameter)))
      (g:signal-connect action "change-state"
         (lambda (action parameter)
           (let ((str (g:variant-string parameter)))
             (cond ((string= str "left")
                    (setf (gtk:text-view-justification view) :left))
                   ((string= str "center")
                    (setf (gtk:text-view-justification view) :center))
                   ((string= str "right")
                    (setf (gtk:text-view-justification view) :right))
                   (t
                    (error "Action justify with unkown parameter called")))
             (setf (g:action-state action) parameter)))))

    ;; Add action "clear" to the application window
    (let ((action (g:simple-action-new "clear" nil)))
      (g:action-map-add-action window action)
      (g:signal-connect action "activate"
         (lambda (action parameter)
           (declare (ignore action parameter))
             (setf (gtk:text-buffer-text (gtk:text-view-buffer view)) ""))))

    (gtk:widget-add-css-class toolbar "toolbar")

    ;; Left justify toggle tool button for the toolbar
    (let ((button (make-instance 'gtk:toggle-button
                                 :icon-name "format-justify-left")))
      (gtk:actionable-set-detailed-action-name button "win.justify::left")
      (gtk:box-append toolbar button))
    ;; Center justify toggle tool button for the toolbar
    (let ((button (make-instance 'gtk:toggle-button
                                 :icon-name "format-justify-center")))
      (gtk:actionable-set-detailed-action-name button "win.justify::center")
      (gtk:box-append toolbar button))
    ;; Right justify toggle tool button for the toolbar
    (let ((button (make-instance 'gtk:toggle-button
                                 :icon-name "format-justify-right")))
      (gtk:actionable-set-detailed-action-name button "win.justify::right")
      (gtk:box-append toolbar button))

    ;; Invisible separator which shift the next tool item to the right
    (let ((button (make-instance 'gtk:separator :orientation :vertical)))
      (setf (gtk:widget-hexpand button) t)
      (gtk:box-append toolbar button))

    ;; Toggle button for fullscreen
    (let ((button (make-instance 'gtk:toggle-button
                                 :icon-name "view-fullscreen")))
      (gtk:actionable-set-detailed-action-name button "win.fullscreen")
      (g:signal-connect window "notify::fullscreened"
          (lambda (object pspec)
            (declare (ignore pspec))
              (if (gtk:window-is-fullscreen object)
                  (setf (gtk:button-icon-name button) "view-restore")
                  (setf (gtk:button-icon-name button) "view-fullscreen"))))
      (gtk:box-append toolbar button))

    ;; Load file into the buffer
    (when (and filename (probe-file filename))
      (let ((buffer (gtk:text-view-buffer view)))
        (format t "    buffer : ~a~%" buffer)
        (gtk:text-buffer-load-file buffer filename)))
    ;; Connect a signal handler for the text buffer
    (g:signal-connect (gtk:text-view-buffer view) "changed"
        (lambda (buffer)
          (text-buffer-changed-cb window buffer)))
    (text-buffer-changed-cb window (gtk:text-view-buffer view))

    ;; Place widgets in grid and present window
    (gtk:grid-attach grid toolbar 0 0 1 1)
    (gtk:grid-attach grid scrolled 0 1 1 1)
    (gtk:window-present window)))

(defun action-about (application action parameter)
  (declare (ignore action parameter))
  (let ((window (gtk:application-active-window application)))
    (gtk:show-about-dialog window
                           :modal t
                           :program-name "Bloatpad"
                           :version
                           (asdf:component-version
                               (asdf:find-system :gtk4-application))
                           :copyright "(c) Dieter Kaiser"
                           :website "github.com/crategus/cl-cffi-gtk4"
                           :website-label "Project web site"
                           :license-type :mit-x11
                           :authors '("Dieter Kaiser")
                           :documenters '("Dieter Kaiser")
                           :artists '("None")
                           :logo-icon-name "applications-development"
                           :wrap-license t)))

(defun action-edit-accels (application action parameter)
  (format t "in action EDIT-ACCELS~%")
  (format t "  application : ~a~%" application)
  (format t "       action : ~a~%" action)
  (format t "    parameter : ~a~%" parameter)
  (let ((dialog (make-instance 'gtk:dialog
                               :title "Edit Accelerators"
                               :border-width 6
                               :width-request 240
                               :application application))
        (combo (make-instance 'gtk:combo-box-text))
        (entry (make-instance 'gtk:entry
                              :margin-bottom 12))
        (actions (gtk:application-list-action-descriptions application)))

    (format t "      actions : ~a~%" actions)

    ;; Fill the combo box with the name of the actions
    (dolist (action (sort actions #'string<))
      (gtk:combo-box-text-append combo action action))

    (g:signal-connect combo "changed"
        (lambda (combo)
          (format t "in signal CHANGED~%")
          (format t "   combo : ~a~%" combo)
          (let ((action (gtk:combo-box-active-id combo)))
            (format t "  action : ~a~%" action)
            (when action
              (let ((accels (gtk:application-accels-for-action application
                                                               action)))
                (format t "  accels : ~a~%" accels)
                (setf (gtk:editable-text entry)
                      (string-right-trim ","
                                         (format nil "~{~a,~}" accels))))))))

    ;; Select the first item in the combo box
    (setf (gtk:combo-box-active combo) 0)

    ;; Add buttons to the dialog
    (gtk:dialog-add-button dialog "Close" :close)
    (gtk:dialog-add-button dialog "Set" :apply)

    (g:signal-connect dialog "response"
        (lambda (dialog id)
          (format t "in signal RESPONSE~%")
          (format t "  dialog : ~a~%" dialog)
          (format t "      id : ~a~%" id)

          (if (= (cffi:foreign-enum-value 'gtk:response-type :close) id)
              (gtk:window-destroy dialog)
              (let ((action (gtk:combo-box-active-id combo)))
                (when action
                  (let ((accels (gtk:editable-text entry)))
                    (setf (gtk:application-accels-for-action application action)
                          (split-sequence:split-sequence ","
                                                         accels
                                                         :test #'string=))))))))
    ;; Pack and show the widgets
    (let ((area (gtk:dialog-content-area dialog)))
      (gtk:box-append area
                         (make-instance 'gtk:label
                                        :xalign 0.0
                                        :margin-top 9
                                        :margin-bottom 6
                                        :use-markup t
                                        :label "<b>Detailed Action Name</b>"))
      (gtk:box-append area combo)
      (gtk:box-append area
                         (make-instance 'gtk:label
                                        :xalign 0.0
                                        :margin-top 9
                                        :margin-bottom 6
                                        :use-markup t
                                        :label "<b>Accelerators</b>"))
      (gtk:box-append area entry))
    (gtk:window-present dialog)))

(defun update-time (application)
  (g:menu-remove-all (bloatpad-timemenu application))
  (multiple-value-bind (second minute hour)
      (get-decoded-time)
    (let ((time (format nil "~2,'0d:~2,'0d:~2,'0d" hour minute second)))
      (g:menu-append (bloatpad-timemenu application) time nil)))
  g:+source-continue+)

(defun action-time-active (application action state)
  (format t "in action TIME-ACTIVE~%")
  (format t "  application : ~a~%" application)
  (format t "       action : ~a~%" action)
  (format t "        state : ~a~%" state)
  (if (g:variant-boolean state)
      (when (= 0 (bloatpad-timeout application))
        (setf (bloatpad-timeout application)
              (g:timeout-add 1000
                             (lambda ()
                               (update-time application))))
        (update-time application))
      (when (> 0 (bloatpad-timeout application))
        (g:source-remove (bloatpad-timeout application))
        (setf (bloatpad-timeout application) 0))))

(defun bloatpad-startup (application)
  (format t "Application in STARTUP.~%")
  ;; Add the accelerators
  (let ((accels '(("app.new" ("<Primary>n" "<Primary>t"))
                  ("app.quit" "<Primary>q")
                  ("win.copy" "<Primary>c")
                  ("win.paste" "<Primary>p")
                  ("win.justify::left" "<Primary>l")
                  ("win.justify::center" "<Primary>m")
                  ("win.justify::right" "<Primary>r")
                  ("win.fullscreen" "F11"))))
    (iter (for (name accel) in accels)
          (setf (gtk:application-accels-for-action application name) accel)))

  ;; Add action "new" to the application
  (let ((action (g:simple-action-new "new" nil)))
    ;; Connect a handler to the signal "activate"
    (g:signal-connect action "activate"
       (lambda (action parameter)
         (declare (ignore action parameter))
         (g:application-activate application)))
    ;; Add the action to the action map of the application
    (g:action-map-add-action application action))

  ;; Add action "about" to the application
  (let ((action (g:simple-action-new "about" nil)))
    ;; Connect a handler to the signal "activate"
    (g:signal-connect action "activate"
       (lambda (action parameter)
         (action-about application action parameter)))
    ;; Add the action to the action map of the application
    (g:action-map-add-action application action))

  ;; Add action "quit" to the application
  (let ((action (g:simple-action-new "quit" nil)))
    ;; Connect a handler to the signal activate
    (g:signal-connect action "activate"
       (lambda (action parameter)
         (declare (ignore action parameter))
         ;; Destroy all windows of the application. When the last window is
         ;; destroyed, the application is shutdown. The C example calls the
         ;; g_application_quit function. In the Lisp library this will not
         ;; close the open windows and the windows do not react.
         (dolist (window (gtk:application-windows application))
           (gtk:window-destroy window))))
    ;; Add the action to action map of the application
    (g:action-map-add-action application action))

  ;; Add action "edit-accels" to the application
  (let ((action (g:simple-action-new "edit-accels" nil)))
    (g:signal-connect action "activate"
       (lambda (action parameter)
         (action-edit-accels application action parameter)))
    (g:action-map-add-action application action))

  ;; Add action "time-active" to the application
  (let ((action (g:simple-action-new-stateful "time-active"
                                              nil
                                              (g:variant-new-boolean nil))))
    (g:signal-connect action "activate"
       (lambda (action parameter)
         (format t "in action activate for TIME-ACTIVE~%")
         (action-time-active application action parameter)))
    (g:action-map-add-action application action))

  ;; Add action "clear-all" to the application
  (let ((action (g:simple-action-new "clear-all" nil)))
    (g:signal-connect action "activate"
       (lambda (action parameter)
         (declare (ignore action parameter))
         (format t "in action CLEAR-ALL~%")
         (let ((windows (gtk:application-windows application)))
           (dolist (window windows)
             (g:action-group-activate-action window "clear" nil)))))
    (g:action-map-add-action application action))

  ;; Intitialize the application menu and the menubar
  (let ((builder (make-instance 'gtk:builder)))
    ;; Read the menus from a string
    (gtk:builder-add-from-string builder *menu*)
    ;; Set the application menu
;    (setf (gtk:application-app-menu application)
;          (gtk:builder-object builder "app-menu"))
    ;; Set the menubar
    (setf (gtk:application-menubar application)
          (gtk:builder-object builder "menubar"))
    ;; Populate the icon menu with icons
    ;; The C example uses the function gtk:application-menu-by-id to get
    ;; the menu from automatic loaded resources. This does not work in the
    ;; Lisp example. TODO: Implement automatic loading of resources.
    (let ((menu (gtk:builder-object builder "icon-menu")))
;      (let* ((file (g:file-new-for-path (glib-sys:sys-path "color-select.png")))
;             (icon (g:file-icon-new file))
;             (item (g:menu-item-new "File Icon" nil)))

;        (format t "file : ~a~%" file)
;        (format t "icon : ~a~%" icon)

;        (g:menu-item-set-icon item icon)
;        (g:menu-append-item menu item))

;      (let ((icon (gdk:pixbuf-new-from-resource
;                            "/com/crategus/application/preferences-system.png"))
;            (item (g:menu-item-new "Pixbuf" nil)))
;        (g:menu-item-set-icon item icon)
;        (g:menu-append-item menu item))
      (let ((icon (g:themed-icon-new "edit-find"))
            (item (g:menu-item-new "Themed Icon" nil)))
        (g:menu-item-set-icon item icon)
        (g:menu-append-item menu item))
      (let ((icon (g:themed-icon-new "weather-severe-alert-symbolic"))
            (item (g:menu-item-new "Symbolic Icon" nil)))
        (g:menu-item-set-icon item icon)
        (g:menu-append-item menu item)))
    ;; TIME-MENU
    (let ((menu (gtk:builder-object builder "time-menu")))
      (setf (bloatpad-timemenu application) menu)
      (format t "  bloatpad-timemenu : ~a~%" (bloatpad-timemenu application))))
  ;; Start the timer for the time menu
  (action-time-active application "time-active" (g:variant-new-boolean t)))

(defun bloatpad-open (application files n-files hint)
  (declare (ignore hint))
  (format t "Application in OPEN~%")
  ;; Executed when the application is opened
  (dotimes (i n-files)
    (let* ((file (cffi:mem-aref files '(g:object g:file) i))
           (filename (g:file-basename file)))
      (format t "  filename : ~a~%" filename)
      (new-bloatpad-window application filename))))

(defun bloatpad-activate (application)
  (format t "Application in ACTIVATE~%")
  ;; Create a new application window
  (new-bloatpad-window application nil))

(defun bloatpad-shutdown (application)
  ;; Executed when the application is shut down
  (format t "Application in SHUTDOWN~%")
  (unless (= 0 (bloatpad-timeout application))
    (g:source-remove (bloatpad-timeout application))
    (setf (bloatpad-timeout application) 0)))

(defmethod initialize-instance :after
    ((app bloatpad) &key &allow-other-keys)
  (g:signal-connect app "startup" #'bloatpad-startup)
  (g:signal-connect app "open" #'bloatpad-open)
  (g:signal-connect app "activate" #'bloatpad-activate)
  (g:signal-connect app "shutdown" #'bloatpad-shutdown))

(defun bloatpad (&rest argv)
  (g:with-resource (resource (glib-sys:sys-path "bloatpad.gresource"
                                                "gtk4-application"))
    (let ((argv (cons "bloatpad" (or argv (uiop:command-line-arguments))))
          ;; Create an instance of the application Bloat Pad
          (bloatpad (make-instance 'bloatpad
                                   :application-id "com.crategus.bloatpad"
                                   :flags :handles-open
                                   :register-session t)))

      (format t "Start BLOATPAD with argv : ~a~%" argv)
      (format t "~a~%"
                (glib-sys:sys-path "bloatpad.gresource" "gtk4-application"))

      (unless (string= "Bloatpad" (g:application-name))
        (setf (g:application-name) "Bloatpad"))

      ;; Run the application
      (g:application-run bloatpad argv))))

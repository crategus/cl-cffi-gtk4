(in-package :gtk-test)

(def-suite gtk-window :in gtk-suite)
(in-suite gtk-window)

(defvar *verbose-gtk-window* nil)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkWindow

(test gtk-window-class
  ;; Check type
  (is (g:type-is-object "GtkWindow"))
  ;; Check registered name
  (is (eq 'gtk:window
          (glib:symbol-for-gtype "GtkWindow")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkWindow")
          (g:gtype (cffi:foreign-funcall "gtk_window_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkWindow")))
  ;; Check children
  (is (equal '("GtkAboutDialog" "GtkApplicationWindow" "GtkAssistant"
               "GtkDialog" "GtkShortcutsWindow")
             (glib-test:list-children "GtkWindow")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget" "GtkNative"
               "GtkShortcutManager" "GtkRoot")
             (glib-test:list-interfaces "GtkWindow")))
  ;; Check class properties
  (is (equal '("application" "child" "decorated" "default-height"
               "default-widget" "default-width" "deletable"
               "destroy-with-parent" "display" "focus-visible"
               "focus-widget" "fullscreened" "handle-menubar-accel"
               "hide-on-close" "icon-name" "is-active" "maximized"
               "mnemonics-visible" "modal" "resizable" "startup-id" "suspended"
               "title" "titlebar" "transient-for")
             (glib-test:list-properties "GtkWindow")))
  ;; Check signals
  (is (equal '("activate-default" "activate-focus" "close-request"
               "enable-debugging" "keys-changed")
             (glib-test:list-signals "GtkWindow")))
  ;; Check CSS name
  (is (string= "window"
               (gtk:widget-class-css-name "GtkWindow")))
  ;; Check accessible role
  (is (eq :application (gtk:widget-class-accessible-role "GtkWindow")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkWindow" GTK:WINDOW
                       (:SUPERCLASS GTK:WIDGET
                        :EXPORT T
                        :INTERFACES
                        ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
                         "GtkNative" "GtkRoot" "GtkShortcutManager")
                        :TYPE-INITIALIZER "gtk_window_get_type")
                       ((APPLICATION WINDOW-APPLICATION
                         "application" "GtkApplication" T T)
                        (CHILD WINDOW-CHILD "child" "GtkWidget" T T)
                        (DECORATED WINDOW-DECORATED "decorated" "gboolean" T T)
                        (DEFAULT-HEIGHT WINDOW-DEFAULT-HEIGHT
                         "default-height" "gint" T T)
                        (DEFAULT-WIDGET WINDOW-DEFAULT-WIDGET
                         "default-widget" "GtkWidget" T T)
                        (DEFAULT-WIDTH WINDOW-DEFAULT-WIDTH
                         "default-width" "gint" T T)
                        (DELETABLE WINDOW-DELETABLE "deletable" "gboolean" T T)
                        (DESTROY-WITH-PARENT WINDOW-DESTROY-WITH-PARENT
                         "destroy-with-parent" "gboolean" T T)
                        (DISPLAY WINDOW-DISPLAY "display" "GdkDisplay" T T)
                        (FOCUS-VISIBLE WINDOW-FOCUS-VISIBLE
                         "focus-visible" "gboolean" T T)
                        (FOCUS-WIDGET WINDOW-FOCUS-WIDGET
                         "focus-widget" "GtkWidget" T T)
                        (FULLSCREENED WINDOW-FULLSCREENED
                         "fullscreened" "gboolean" T T)
                        (HANDLE-MENUBAR-ACCEL WINDOW-HANDLE-MENUBAR-ACCEL
                         "handle-menubar-accel" "gboolean" T T)
                        (HIDE-ON-CLOSE WINDOW-HIDE-ON-CLOSE
                         "hide-on-close" "gboolean" T T)
                        (ICON-NAME WINDOW-ICON-NAME
                         "icon-name" "gchararray" T T)
                        (IS-ACTIVE WINDOW-IS-ACTIVE
                         "is-active" "gboolean" T NIL)
                        (MAXIMIZED WINDOW-MAXIMIZED "maximized" "gboolean" T T)
                        (MNEMONICS-VISIBLE WINDOW-MNEMONICS-VISIBLE
                         "mnemonics-visible" "gboolean" T T)
                        (MODAL WINDOW-MODAL "modal" "gboolean" T T)
                        (RESIZABLE WINDOW-RESIZABLE "resizable" "gboolean" T T)
                        (STARTUP-ID WINDOW-STARTUP-ID
                         "startup-id" "gchararray" NIL T)
                        (SUSPENDED WINDOW-SUSPENDED
                         "suspended" "gboolean" T NIL)
                        (TITLE WINDOW-TITLE "title" "gchararray" T T)
                        (TITLEBAR WINDOW-TITLEBAR "titlebar" "GtkWidget" T T)
                        (TRANSIENT-FOR WINDOW-TRANSIENT-FOR
                         "transient-for" "GtkWindow" T T)))
             (gobject:get-gtype-definition "GtkWindow"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-window-properties
  (let ((window (make-instance 'gtk:window)))
    (is-false (gtk:window-application window))
    (is-false (gtk:window-child window))
    (is-true  (gtk:window-decorated window))
    (is (= 0  (gtk:window-default-height window)))
    (is-false (gtk:window-default-widget window))
    (is (= 0  (gtk:window-default-width window)))
    (is-true  (gtk:window-deletable window))
    (is-false (gtk:window-destroy-with-parent window))
    (is (eq (gdk:display-default) (gtk:window-display window)))
    (is-true  (gtk:window-focus-visible window))
    (is-false (gtk:window-focus-widget window))
    (is-false (gtk:window-fullscreened window))
    (is-true  (gtk:window-handle-menubar-accel window))
    (is-false (gtk:window-hide-on-close window))
    (is-false (gtk:window-icon-name window))
    (is-false (gtk:window-is-active window))
    (is-false (gtk:window-maximized window))
    (is-false (gtk:window-mnemonics-visible window))
    (is-false (gtk:window-modal window))
    (is-true  (gtk:window-resizable window))
    (signals (error) (gtk:window-startup-id window)) ; not readable
    #+gtk-4-12
    (is-false (gtk:window-suspended window))
    (is-false (gtk:window-title window))
    (is-false (gtk:window-titlebar window))
    (is-false (gtk:window-transient-for window))
    (is-false (gtk:window-destroy window))))

;;;     gtk:window-focus-widget

;; TODO: Does not work as expected. Improve the test.

#+nil
(test gtk-window-focus-widget
  (let* ((button1 (gtk:button-new))
         (button2 (make-instance 'gtk:button :focusable t))
         (window (make-instance 'gtk:window :child button1
                                            :focus-widget button2)))

    (is (= 2 (g:object-ref-count button1)))
    (is (= 2 (g:object-ref-count button2)))

    (is-false (gtk:window-focus-widget window))

;    (is (eq button2 (setf (gtk:window-focus-widget window) button2)))
;    (is (eq button2 (gtk:window-focus-widget window)))

    (is (= 1 (g:object-ref-count button2)))

    ;; Remove BUTTON from the window and destroy WINDOW
    (is-false (setf (gtk:window-child window) nil))
;    (is-false (setf (gtk:window-focus-widget window) nil))
    (is-false (gtk:window-destroy window))
    (is (= 1 (g:object-ref-count window)))
    (is (= 1 (g:object-ref-count button1)))
    (is (= 1 (g:object-ref-count button2)))))

;;; --- Signals ----------------------------------------------------------------

;;;     activate-default

(test gtk-window-activate-default-signal
  (let* ((name "activate-default")
         (gtype (g:gtype "GtkWindow"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    (is (equal '(:RUN-LAST :ACTION)
               (g:signal-query-signal-flags query)))
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     activate-focus

(test gtk-window-activate-focus-signal
  (let* ((name "activate-focus")
         (gtype (g:gtype "GtkWindow"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    (is (string= "activate-focus" (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    (is (equal '(:RUN-LAST :ACTION)
               (g:signal-query-signal-flags query)))
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     close-request

(test gtk-window-close-request-signal
  (let* ((name "close-request")
         (gtype (g:gtype "GtkWindow"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    (is (string= "close-request" (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    (is (equal '(:RUN-LAST)
               (g:signal-query-signal-flags query)))
    (is (eq (g:gtype "gboolean") (g:signal-query-return-type query)))
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     enable-debugging

(test gtk-window-enable-debugging-signal
  (let* ((name "enable-debugging")
         (gtype (g:gtype "GtkWindow"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    (is (string= "enable-debugging" (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    (is (equal '(:RUN-LAST :ACTION)
               (g:signal-query-signal-flags query)))
    (is (eq (g:gtype "gboolean") (g:signal-query-return-type query)))
    (is (equal '("gboolean")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     keys-changed

(test gtk-window-keys-changed-signal
  (let* ((name "keys-changed")
         (gtype (g:gtype "GtkWindow"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    (is (string= "keys-changed" (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    (is (equal '(:RUN-FIRST :DEPRECATED)
               (g:signal-query-signal-flags query)))
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_window_new

(test gtk-window-new
  (let ((window nil))
     ;; Create with constructor function
    (is (typep (setf window (gtk:window-new)) 'gtk:window))
    (is (= 2 (g:object-ref-count window)))
    (is-false (gtk:window-destroy window))
    ;; Create with make instance
    (is (typep (setf window (make-instance 'gtk:window)) 'gtk:window))
    (is (= 2 (g:object-ref-count window)))
    (is-false (gtk:window-destroy window))
    ;; Create with object new
    (is (typep (setf window (g:object-new "GtkWindow")) 'gtk:window))
    (is (= 2 (g:object-ref-count window)))
    (is-false (gtk:window-destroy window))
    (is (= 1 (g:object-ref-count window)))))

;;;     gtk_window_close
;;;     gtk_window_destroy

#+nil
(test gtk-window-close/destroy.1
  (let ((application (make-instance 'gtk:application
                                    :application-id "com.crategus.test"
                                    :flags :default-flags
                                    :register-session nil)))
    ;; Connect signal "activate"
    (g:signal-connect application "activate"
        (lambda (app)
;          (g:application-hold app)
          (when *verbose-gtk-window*
            (format t "~&Signal handler ACTIVATE~%"))
          (let ((window (gtk:application-window-new app)))
            ;; Connect signal "close-request"
            (g:signal-connect window "close-request"
                (lambda (window)
                  (declare (ignore window))
                  (when *verbose-gtk-window*
                    (format t "~&Signal handler CLOSE-REQUEST~%"))
                  ;; Stop the propagation of the event
                  gdk:+event-stop+))

            (is (typep window 'gtk:window))
            (is (eq app (gtk:window-application window)))
            (when *verbose-gtk-window*
              (format t "~&Emit 'close-request' signal~%"))
            (is-true (g:signal-emit window "close-request"))
            (when *verbose-gtk-window*
              (format t "~&Back from signal handler~%"))
;            (g:application-release app)
            ;; Destroy the window, this shuts down the application
            (is-false (gtk:window-destroy window)))))
    ;; Run the application
    (g:application-run application nil)))

#+nil
(test gtk-window-close/destroy.2
  (let ((application (make-instance 'gtk:application
                                    :application-id "com.crategus.test"
                                    :flags :default-flags
                                    :register-session nil)))
    ;; Connect signal "activate"
    (g:signal-connect application "activate"
        (lambda (app)
          (g:application-hold app)
          (when *verbose-gtk-window*
            (format t "~&Signal handler ACTIVATE~%"))
          (let ((window (gtk:application-window-new app)))
            ;; Connect signal "close-request"
            (g:signal-connect window "close-request"
                (lambda (window)
                  (declare (ignore window))
                  (when *verbose-gtk-window*
                    (format t "~&Signal handler CLOSE-REQUEST~%"))
                  ;; Stop the propagation of the event
                  gdk:+event-stop+))
            (is (typep window 'gtk:window))
            (is (eq app (gtk:window-application window)))
            (when *verbose-gtk-window*
              (format t "~&Call function GTK-WINDOW-CLOSE~%"))
            (is-false (gtk:window-close window))
            (when *verbose-gtk-window*
              (format t "~&Back from signal handler~%"))
            ;; Destroy the window, this shuts down the application
            (is-false (gtk:window-destroy window)))
          (g:application-release app)))
    ;; Run the application
    (g:application-run application nil)))

;;;     gtk_window_fullscreen
;;;     gtk_window_fullscreen_on_monitor
;;;     gtk_window_is_fullscreen
;;;     gtk_window_unfullscreen

(test gtk-window-fullscreen
  (let ((window (gtk:window-new)))
    (is-false (gtk:window-is-fullscreen window))
    (is-false (gtk:window-fullscreen window))
    (is-true (gtk:window-is-fullscreen window))
    (is-false (gtk:window-unfullscreen window))
    (is-false (gtk:window-is-fullscreen window))
    (is-false (gtk:window-destroy window))))

;;;     gtk_window_get_default_size
;;;     gtk_window_set_default_size

(test gtk-window-default-size
  (let ((window (make-instance 'gtk:window)))
    (is (equal '(0 0)
               (multiple-value-list (gtk:window-default-size window))))
    (is (equal '(100 200)
               (multiple-value-list (setf (gtk:window-default-size window)
                                          '(100 200)))))
    (is (equal '(100 200)
               (multiple-value-list (gtk:window-default-size window))))
    (is-false (gtk:window-destroy window))))

;;;     gtk_window_get_focus                                not exported
;;;     gtk_window_set_focus                                not exported

;; Implemented has the gtk:window-focus-widget accessor

;;;     gtk_window_get_group
;;;     gtk_window_has_group

(test gtk-window-has-group
  (let ((gtk-init:*gtk-warn-deprecated* nil)
        (window (make-instance 'gtk:window))
        (group (gtk:window-group-new)))
    (is (= 2 (g:object-ref-count window)))
    ;; Not in a group
    (is-false (gtk:window-has-group window))
    ;; But in the default group
    (is (typep (gtk:window-group window) 'gtk:window-group))
    (is (member window
                (gtk:window-group-list-windows (gtk:window-group window))
                :test #'eq))
    ;; Add window to a group
    (is-false (gtk:window-group-add-window group window))
    (is (= 2 (g:object-ref-count window)))
    (is-true (gtk:window-has-group window))
    (is (eq group (gtk:window-group window)))
    (is (member window (gtk:window-group-list-windows group) :test #'eq))
    (is-false (gtk:window-group-remove-window group window))
    (is-false (gtk:window-destroy window))))

;;;     gtk_window_maximize
;;;     gtk_window_unmaximize
;;;     gtk_window_is_maximized

(test gtk-window-maximize
  (let ((window (gtk:window-new)))
    (is-false (gtk:window-is-maximized window))
    (is-false (gtk:window-maximize window))
    (is-true (gtk:window-is-maximized window))
    (is-false (gtk:window-destroy window))))

;;;     gtk_window_minimize
;;;     gtk_window_unminimize

(test gtk-window-minimize
  (let ((window (gtk:window-new)))
    (is-false (gtk:window-minimize window))
    (is-false (gtk:window-unminimize window))
    (is-false (gtk:window-destroy window))))

;;;     gtk_window_present

(test gtk-window-present
  (let ((window (make-instance 'gtk:window)))
    (is-false (gtk:window-present window))
    (is-false (gtk:window-destroy window))))

;;;     gtk_window_present_with_time

(test gtk-window-present-with-time
  (let ((gtk-init:*gtk-warn-deprecated* nil)
        (window (make-instance 'gtk:window)))
    (is-false (gtk:window-present-with-time window gdk:+current-time+))
    (is-false (gtk:window-destroy window))))

;;;     gtk_window_get_default_icon_name
;;;     gtk_window_set_default_icon_name

(test gtk-window-default-icon-name
  (is-false (gtk:window-default-icon-name))
  (is (string= "applications-utilities"
               (setf (gtk:window-default-icon-name) "applications-utilities")))
  (is (string= "applications-utilities"
               (gtk:window-default-icon-name)))
  (is-false (setf (gtk:window-default-icon-name) nil)))

;;;     gtk_window_get_toplevels

(test gtk-window-toplevels
  ; Destroy already existing windows
  (dolist (window (gtk:window-list-toplevels))
    (gtk:window-destroy window))
  (is-false (gtk:window-list-toplevels))
  (let ((window1 (make-instance 'gtk:window))
        (window2 (make-instance 'gtk:application-window))
        (window3 (make-instance 'gtk:about-dialog)))
    (is (typep (gtk:window-toplevels) 'g:list-store))
    (is (= 3 (g:list-model-n-items (gtk:window-toplevels))))
    (is (eq window1 (g:list-model-object (gtk:window-toplevels) 0)))
    (is (eq window2 (g:list-model-object (gtk:window-toplevels) 1)))
    (is (eq window3 (g:list-model-object (gtk:window-toplevels) 2)))


    (is-false (gtk:window-destroy window1))
    (is-false (gtk:window-destroy window2))
    (is-false (gtk:window-destroy window3))

    (is-false (gtk:window-list-toplevels))
    ))

;;;     gtk_window_list_toplevels

;; TODO: This test generates warnings for deprecated windows.

(test gtk-window-list-toplevels
  ;; Destroy already existing windows
  (dolist (window (gtk:window-list-toplevels))
    (gtk:window-destroy window))
  (is-false (gtk:window-list-toplevels))
  (let ((window1 (make-instance 'gtk:window))
        (window2 (make-instance 'gtk:application-window))
        (window3 (make-instance 'gtk:about-dialog)))
    (is (= 3 (length (gtk:window-list-toplevels))))
    (is (eq window1 (third (gtk:window-list-toplevels))))
    (is (eq window2 (second (gtk:window-list-toplevels))))
    (is (eq window3 (first (gtk:window-list-toplevels))))

    (is-false (gtk:window-destroy window1))
    (is-false (gtk:window-destroy window2))
    (is-false (gtk:window-destroy window3))))

;;;     gtk_window_set_auto_startup_notification
;;;     gtk_window_set_interactive_debugging

;;;     gtk_window_is_suspended                            Since 4.12

(test gtk-window-is-suspended
  (let ((window (gtk:window-new)))
    (is-false (gtk:window-is-suspended window))
    (is-false (gtk:window-destroy window))))

;;; 2024-10-8

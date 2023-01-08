(in-package :gtk-test)

(def-suite gtk-window :in gtk-suite)
(in-suite gtk-window)

(defvar *verbose-gtk-window* nil)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkWindow

(test window-class
  ;; Type check
  (is (g:type-is-object "GtkWindow"))
  ;; Check the registered name
  (is (eq 'gtk:window
          (gobject:symbol-for-gtype "GtkWindow")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkWindow")
          (g:gtype (foreign-funcall "gtk_window_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkWindow")))
  ;; Check the children
  (is (equal '("GtkAboutDialog" "GtkApplicationWindow" "GtkAssistant"
               "GtkDialog" "GtkShortcutsWindow")
             (list-children "GtkWindow")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget" "GtkNative"
               "GtkShortcutManager" "GtkRoot")
             (list-interfaces "GtkWindow")))
  ;; Check the class properties
  (is (equal '("application" "child" "decorated" "default-height"
               "default-widget" "default-width" "deletable"
               "destroy-with-parent" "display" "focus-visible"
               "focus-widget" "fullscreened" "handle-menubar-accel"
               "hide-on-close" "icon-name" "is-active" "maximized"
               "mnemonics-visible" "modal" "resizable" "startup-id" "title"
               "titlebar" "transient-for")
             (list-properties "GtkWindow")))
  ;; Check the list of signals
  (is (equal '("activate-default" "activate-focus" "close-request"
               "enable-debugging" "keys-changed")
             (list-signals "GtkWindow")))
  ;; CSS information
  (is (string= "window"
               (gtk:widget-class-css-name "GtkWindow")))
  (is (string=
"[window.background:dir(ltr)]
"
               (gtk:style-context-to-string
                   (gtk:widget-style-context (make-instance 'gtk:window))
                   :none)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkWindow" GTK-WINDOW
                       (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                        ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
                         "GtkNative" "GtkRoot" "GtkShortcutManager")
                        :TYPE-INITIALIZER "gtk_window_get_type")
                       ((APPLICATION GTK-WINDOW-APPLICATION "application"
                         "GtkApplication" T T)
                        (CHILD GTK-WINDOW-CHILD "child" "GtkWidget" T T)
                        (DECORATED GTK-WINDOW-DECORATED "decorated" "gboolean"
                         T T)
                        (DEFAULT-HEIGHT GTK-WINDOW-DEFAULT-HEIGHT
                         "default-height" "gint" T T)
                        (DEFAULT-WIDGET GTK-WINDOW-DEFAULT-WIDGET
                         "default-widget" "GtkWidget" T T)
                        (DEFAULT-WIDTH GTK-WINDOW-DEFAULT-WIDTH "default-width"
                         "gint" T T)
                        (DELETABLE GTK-WINDOW-DELETABLE "deletable" "gboolean"
                         T T)
                        (DESTROY-WITH-PARENT GTK-WINDOW-DESTROY-WITH-PARENT
                         "destroy-with-parent" "gboolean" T T)
                        (DISPLAY GTK-WINDOW-DISPLAY "display" "GdkDisplay" T T)
                        (FOCUS-VISIBLE GTK-WINDOW-FOCUS-VISIBLE "focus-visible"
                         "gboolean" T T)
                        (FOCUS-WIDGET GTK-WINDOW-FOCUS-WIDGET "focus-widget"
                         "GtkWidget" T T)
                        (FULLSCREENED GTK-WINDOW-FULLSCREENED "fullscreened"
                         "gboolean" T T)
                        (HANDLE-MENUBAR-ACCEL GTK-WINDOW-HANDLE-MENUBAR-ACCEL
                         "handle-menubar-accel" "gboolean" T T)
                        (HIDE-ON-CLOSE GTK-WINDOW-HIDE-ON-CLOSE "hide-on-close"
                         "gboolean" T T)
                        (ICON-NAME GTK-WINDOW-ICON-NAME "icon-name"
                         "gchararray" T T)
                        (IS-ACTIVE GTK-WINDOW-IS-ACTIVE "is-active" "gboolean"
                         T NIL)
                        (MAXIMIZED GTK-WINDOW-MAXIMIZED "maximized" "gboolean"
                         T T)
                        (MNEMONICS-VISIBLE GTK-WINDOW-MNEMONICS-VISIBLE
                         "mnemonics-visible" "gboolean" T T)
                        (MODAL GTK-WINDOW-MODAL "modal" "gboolean" T T)
                        (RESIZABLE GTK-WINDOW-RESIZABLE "resizable" "gboolean"
                         T T)
                        (STARTUP-ID GTK-WINDOW-STARTUP-ID "startup-id"
                         "gchararray" NIL T)
                        (TITLE GTK-WINDOW-TITLE "title" "gchararray" T T)
                        (TITLEBAR GTK-WINDOW-TITLEBAR "titlebar" "GtkWidget"
                         T T)
                        (TRANSIENT-FOR GTK-WINDOW-TRANSIENT-FOR "transient-for"
                         "GtkWindow" T T)))
             (gobject:get-g-type-definition "GtkWindow"))))

;;; --- Properties -------------------------------------------------------------

(test window-properties
  (let ((window (make-instance 'gtk:window)))
    (is-false (gtk:window-application window))
    (is-false (gtk:window-child window))
    (is-true  (gtk:window-decorated window))
    (is (= 0  (gtk:window-default-height window)))
    (is-false (gtk:window-default-widget window))
    (is (= 0  (gtk:window-default-width window)))
    (is-true  (gtk:window-deletable window))
    (is-false (gtk:window-destroy-with-parent window))
    (is (typep (gtk:window-display window) 'gdk:display))
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
    (is-false (gtk:window-title window))
    (is-false (gtk:window-titlebar window))
    (is-false (gtk:window-transient-for window))))

;;; --- Signals ----------------------------------------------------------------

;;;     activate-default
;;;     activate-focus
;;;     close-request
;;;     enable-debugging
;;;     keys-changed

(test window-signals
  ;; Query info for "activate-default"
  (let ((query (g:signal-query (g:signal-lookup "activate-default"
                                                "GtkWindow"))))
    (is (string= "activate-default" (g:signal-query-signal-name query)))
    (is (string= "GtkWindow" (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-LAST :ACTION)
               (g:signal-query-signal-flags query)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query)))

  ;; Query info for "activate-focus"
  (let ((query (g:signal-query (g:signal-lookup "activate-focus" "GtkWindow"))))
    (is (string= "activate-focus" (g:signal-query-signal-name query)))
    (is (string= "GtkWindow" (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-LAST :ACTION)
               (g:signal-query-signal-flags query)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query)))

  ;; Query info for "enable-debugging"
  (let ((query (g:signal-query (g:signal-lookup "enable-debugging"
                                                "GtkWindow"))))
    (is (string= "enable-debugging" (g:signal-query-signal-name query)))
    (is (string= "GtkWindow" (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-LAST :ACTION)
               (g:signal-query-signal-flags query)))
    (is (string= "gboolean" (g:type-name (g:signal-query-return-type query))))
    (is (equal '("gboolean")
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query)))

  ;; Query info for "keys-changed"
  (let ((query (g:signal-query (g:signal-lookup "keys-changed" "GtkWindow"))))
    (is (string= "keys-changed" (g:signal-query-signal-name query)))
    (is (string= "GtkWindow" (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-FIRST)
               (g:signal-query-signal-flags query)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query)))

  ;; Query info for "close-request"
  (let ((query (g:signal-query (g:signal-lookup "close-request" "GtkWindow"))))
    (is (string= "close-request" (g:signal-query-signal-name query)))
    (is (string= "GtkWindow" (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-LAST)
               (g:signal-query-signal-flags query)))
    (is (string= "gboolean" (g:type-name (g:signal-query-return-type query))))
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_window_new

(test window-new
  (is (typep (gtk:window-new) 'gtk:window)))

;;;     gtk_window_close
;;;     gtk_window_destroy

;; TODO: This does not work as expected. The tests are not performed.

#+nil
(test window-close/destroy
  (let ((application (make-instance 'gtk:application
                                    :application-id "com.crategus.test"
                                    :flags :none
                                    :register-session nil)))
    ;; Connect signal "activate"
    (g:signal-connect application "activate"
        (lambda (app)
          (g-application-hold app)
          (when *verbose-gtk-window*
            (format t "~&Signal handler ACTIVATE~%"))

          (let ((window (gtk:application-window-new app)))

            ;; Connect signal "close-request"
            (g:signal-connect window "close-request"
                (lambda (window)
                  (when *verbose-gtk-window*
                    (format t "~&Signal handler CLOSE-REQUEST~%"))
                  (is-false (gtk:window-destroy window))))

            (is (typep window 'gtk:window))
            (is (eq app (gtk:window-application window)))
            (when *verbose-gtk-window*
              (format t "~&Call function GTK-WINDOW-CLOSE~%"))
;            (is-false (gtk:window-close window))
            (is-true (g:signal-emit window "close-request"))
;            (is-false (gtk:window-destroy window))
            ;; Remove the window, this shut down the application
            )
          (g-application-release app)))
    ;; Run the application
    (g-application-run application nil)))

;;;     gtk_window_fullscreen
;;;     gtk_window_fullscreen_on_monitor

;;;     gtk_window_get_default_size
;;;     gtk_window_set_default_size

(test window-default-size
  (let ((window (make-instance 'gtk:window)))
    (is (equal '(0 0)
               (multiple-value-list (gtk:window-default-size window))))
    (is (equal '(100 200)
               (multiple-value-list (setf (gtk:window-default-size window)
                                          '(100 200)))))
    (is (equal '(100 200)
               (multiple-value-list (gtk:window-default-size window))))))

;;;     gtk_window_get_focus
;;;     gtk_window_set_focus
;;;     gtk_window_get_group
;;;     gtk_window_has_group
;;;     gtk_window_is_active
;;;     gtk_window_is_fullscreen
;;;     gtk_window_is_maximized
;;;     gtk_window_maximize
;;;     gtk_window_minimize
;;;     gtk_window_present
;;;     gtk_window_present_with_time
;;;     gtk_window_unfullscreen
;;;     gtk_window_unmaximize
;;;     gtk_window_unminimize
;;;     gtk_window_get_default_icon_name
;;;     gtk_window_set_default_icon_name
;;;     gtk_window_get_toplevels

;;;     gtk_window_list_toplevels

(test window-list-toplevels
  (is (every (lambda (obj) (typep obj 'gtk:window))
             (gtk:window-list-toplevels))))

;;;     gtk_window_set_auto_startup_notification
;;;     gtk_window_set_interactive_debugging

;;; 2022-8-28

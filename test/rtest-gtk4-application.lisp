(in-package :gtk-test)

(def-suite gtk-application :in gtk-application-support)
(in-suite gtk-application)

(defvar *verbose-gtk-application* nil)

;;;     GtkApplicationInhibitFlags

(test gtk-application-inhibit-flags
  ;; Check type
  (is (g:type-is-flags "GtkApplicationInhibitFlags"))
  ;; Check registered name
  (is (eq 'gtk:application-inhibit-flags
          (glib:symbol-for-gtype "GtkApplicationInhibitFlags")))
  ;; Check names
  (is (equal '("GTK_APPLICATION_INHIBIT_LOGOUT" "GTK_APPLICATION_INHIBIT_SWITCH"
               "GTK_APPLICATION_INHIBIT_SUSPEND" "GTK_APPLICATION_INHIBIT_IDLE")
             (glib-test:list-flags-item-names "GtkApplicationInhibitFlags")))
  ;; Check values
  (is (equal '(1 2 4 8)
             (glib-test:list-flags-item-values "GtkApplicationInhibitFlags")))
  ;; Check nick names
  (is (equal '("logout" "switch" "suspend" "idle")
             (glib-test:list-flags-item-nicks "GtkApplicationInhibitFlags")))
  ;; Check flags definition
  (is (equal '(GOBJECT:DEFINE-GFLAGS "GtkApplicationInhibitFlags"
                                     GTK:APPLICATION-INHIBIT-FLAGS
                       (:EXPORT T
                        :TYPE-INITIALIZER "gtk_application_inhibit_flags_get_type")
                       (:LOGOUT 1)
                       (:SWITCH 2)
                       (:SUSPEND 4)
                       (:IDLE 8))
             (gobject:get-gtype-definition "GtkApplicationInhibitFlags"))))

;;;     GtkApplication

(test gtk-application-class
  ;; Check type
  (is (g:type-is-object "GtkApplication"))
  ;; Check registered name
  (is (eq 'gtk:application
          (glib:symbol-for-gtype "GtkApplication")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkApplication")
          (g:gtype (cffi:foreign-funcall "gtk_application_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GApplication")
          (g:type-parent "GtkApplication")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkApplication")))
  ;; Check interfaces
  (is (equal '("GActionGroup" "GActionMap")
             (glib-test:list-interfaces "GtkApplication")))
  ;; Check properties
  (is (equal '("active-window" "menubar" "register-session"
               "screensaver-active")
             (glib-test:list-properties "GtkApplication")))
  ;; Check signals
  (is (equal '("query-end" "window-added" "window-removed")
             (glib-test:list-signals "GtkApplication")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkApplication" GTK:APPLICATION
                       (:SUPERCLASS G:APPLICATION
                        :EXPORT T
                        :INTERFACES ("GActionGroup" "GActionMap")
                        :TYPE-INITIALIZER "gtk_application_get_type")
                       ((ACTIVE-WINDOW APPLICATION-ACTIVE-WINDOW
                         "active-window" "GtkWindow" T NIL)
                        (MENUBAR APPLICATION-MENUBAR
                         "menubar" "GMenuModel" T T)
                        (REGISTER-SESSION APPLICATION-REGISTER-SESSION
                         "register-session" "gboolean" T T)
                        (SCREENSAVER-ACTIVE APPLICATION-SCREENSAVER-ACTIVE
                         "screensaver-active" "gboolean" T NIL)))
             (gobject:get-gtype-definition "GtkApplication"))))

;;; --- Signals ----------------------------------------------------------------

;;;     query-end

(test gtk-application-query-end-signal
  (let* ((name "query-end")
         (gtype (g:gtype "GtkApplication"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     window-added

(test gtk-application-window-added-signal
  (let* ((name "window-added")
         (gtype (g:gtype "GtkApplication"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("GtkWindow")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     window-removed

(test gtk-application-window-removed-signal
  (let* ((name "window-removed")
         (gtype (g:gtype "GtkApplication"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("GtkWindow")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Properties and Accessors -----------------------------------------------

(test gtk-application-properties
  (let ((application (make-instance 'gtk:application)))
    (is-false (gtk:application-active-window application))
    (is-false (gtk:application-menubar application))
    (is-false (gtk:application-register-session application))
    (is-false (gtk:application-screensaver-active application))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_application_new

(test gtk-application-new.1
  ;; Create with constructor function
  (is (typep (gtk:application-new nil nil) 'gtk:application))
  (is (= 1 (g:object-ref-count (gtk:application-new nil nil))))
  ;; Create with MAKE-INSTANCE
  (is (typep (make-instance 'gtk:application) 'gtk:application))
  (is (= 1 (g:object-ref-count (make-instance 'gtk:application))))
  ;; Create with G:OBJECT-NEW
  (is (typep (g:object-new "GtkApplication") 'gtk:application))
  (is (= 1 (g:object-ref-count (g:object-new "GtkApplication")))))

(test gtk-application-new.2
  (let ((application (gtk:application-new "com.crategus.test" '(:handles-open))))
    (is (typep application 'gtk:application))
    (is (= 1 (g:object-ref-count application)))
    (is-true (g:application-id-is-valid "com.crategus.test"))
    (is (string= "com.crategus.test"
                 (g:application-application-id application)))
    (is (equal '(:handles-open) (g:application-flags application)))))

(test gtk-application-new.3
  (let ((application (gtk:application-new nil '(:handles-open))))
    (is (typep application 'gtk:application))
    (is (= 1 (g:object-ref-count application)))
    (is-false (g:application-application-id application))
    (is (equal '(:handles-open) (g:application-flags application)))))

;;;     gtk_application_add_window
;;;     gtk_application_remove_window
;;;     gtk_application_get_window_by_id
;;;     gtk_application_get_windows

(test gtk-application-add/remove-window
  (let ((message nil)
        (application (make-instance 'gtk:application)))
    ;; Connect signal "window-added"
    (g:signal-connect application "window-added"
                      (lambda (app window)
                        (declare (ignore app window))
                        (when *verbose-gtk-application*
                          (format t "~&  Signal WINDOW-ADDED emitted~%"))
                        (push "window-added" message)))
    ;; Connect signal "window-removed"
    (g:signal-connect application "window-removed"
                      (lambda (app window)
                        (declare (ignore app window))
                        (when *verbose-gtk-application*
                          (format t "~&  Signal WINDOW-REMOVED emitted~%"))
                        (push "window-removed" message)))
    ;; Connect signal "activate"
    (g:signal-connect application "activate"
                      (lambda (app)
                        (g:application-hold app)
                        (when *verbose-gtk-application*
                          (format t "~&  Signal ACTIVATE emitted~%"))
                        (push "activate" message)
                        ;; Code for emitting window-added and window-removed
                        (let ((window-id 0)
                              (window (make-instance 'gtk:application-window)))
                          ;; Add window to the application
                          (gtk:application-add-window app window)
                          ;; Get the window ID
                          (is (= 1 (setf window-id
                                         (gtk:application-window-id window))))
                          ;; Get the window by ID
                          (is (eq window
                                  (gtk:application-window-by-id app
                                                                window-id)))
                          ;; Check list of windows
                          (is-true (member window
                                           (gtk:application-windows app)
                                           :test #'eq))
                          ;; Remove window from the application
                          (gtk:application-remove-window app window))
                        (g:application-release app)))
    ;; Connect signal "shutdown"
    (g:signal-connect application "shutdown"
                      (lambda (app)
                        (declare (ignore app))
                        (when *verbose-gtk-application*
                          (format t "~&  Signal SHUTDOWN emitted~%"))
                        ;; Destroy all toplevel windows
                        (dolist (window (gtk:window-list-toplevels))
                          (gtk:window-destroy window))
                        (push "shutdown" message)))
    ;; Run the application
    (g:application-run application nil)
    ;; Check messages from the signal handlers
    (is (equal '("shutdown" "window-removed" "window-added" "activate")
               message))))

;;;     gtk_application_get_accels_for_action
;;;     gtk_application_set_accels_for_action

(test gtk-application-accels-for-action
  (let ((application (make-instance 'gtk:application)))
    (is-false (gtk:application-accels-for-action application "win::close"))
    (is (equal '("<Control>q" "<Shift><Alt>F1" "<Control>z")
               (setf (gtk:application-accels-for-action application "win::close")
                     '("<Control>q" "<Shift><Alt>F1" "<Control>z"))))
    (is (equal '("<Control>q" "<Shift><Alt>F1" "<Control>z")
               (gtk:application-accels-for-action application "win::close")))))

;;;     gtk_application_get_actions_for_accel

(test gtk-application-actions-for-accel
  (let ((application (make-instance 'gtk:application)))
    (is (equal '("<Control>q" "<Shift><Alt>F1" "<Control>z")
               (setf (gtk:application-accels-for-action application "win::close")
                     '("<Control>q" "<Shift><Alt>F1" "<Control>z"))))
    (is (equal '("win::close")
               (gtk:application-actions-for-accel application "<Control>q")))
    (is (equal '("<Control>q")
               (setf (gtk:application-accels-for-action application "win::save")
                     '("<Control>q"))))
    (is (equal '("win::close" "win::save")
               (gtk:application-actions-for-accel application "<Control>q")))))

;;;     gtk_application_get_menu_by_id

;; See application-resources.lisp for an example

;;;     gtk_application_inhibit
;;;     gtk_application_uninhibit

;; See application-inhibit.lisp for an example

;;;     gtk_application_list_action_descriptions

(test gtk-application-list-action-descriptions
  (let ((application (make-instance 'gtk:application)))
    (is (equal '()
               (gtk:application-list-action-descriptions application)))
    (is (equal '("<Control>q" "<Shift><Alt>F1" "<Control>z")
               (setf (gtk:application-accels-for-action application "win::close")
                     '("<Control>q" "<Shift><Alt>F1" "<Control>z"))))
    (is (equal '("win::close")
               (gtk:application-list-action-descriptions application)))))

;;;     gtk_application_uninhibit

;;; 2024-10-7

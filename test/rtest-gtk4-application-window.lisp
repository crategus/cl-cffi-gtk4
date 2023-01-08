(in-package :gtk-test)

(def-suite gtk-application-window :in gtk-suite)
(in-suite gtk-application-window)

;;; --- GtkApplicationWindow ---------------------------------------------------

(test application-window-class
  ;; Type check
  (is (g:type-is-object "GtkApplicationWindow"))
  ;; Check the registered name
  (is (eq 'gtk:application-window
          (gobject:symbol-for-gtype "GtkApplicationWindow")))
  ;; Check the parent
  (is (eq (g:gtype "GtkWindow") (g:type-parent "GtkApplicationWindow")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkApplicationWindow")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget" "GtkNative"
               "GtkShortcutManager" "GtkRoot" "GActionGroup" "GActionMap")
             (list-interfaces "GtkApplicationWindow")))
  ;; Check the class properties
  (is (equal '("show-menubar")
             (list-properties "GtkApplicationWindow")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkApplicationWindow")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkApplicationWindow" GTK-APPLICATION-WINDOW
                       (:SUPERCLASS GTK-WINDOW :EXPORT T :INTERFACES
                        ("GActionGroup" "GActionMap" "GtkAccessible"
                         "GtkBuildable" "GtkConstraintTarget" "GtkNative"
                         "GtkRoot" "GtkShortcutManager")
                        :TYPE-INITIALIZER "gtk_application_window_get_type")
                       ((SHOW-MENUBAR GTK-APPLICATION-WINDOW-SHOW-MENUBAR
                         "show-menubar" "gboolean" T T)))
             (gobject:get-g-type-definition "GtkApplicationWindow"))))

;;; --- Properties and Accessors -----------------------------------------------

;;; --- gtk-application-window-show-menubar ------------------------------------

(test application-window-show-menubar
  (let ((window (make-instance 'gtk:application-window)))
    ;; Default value is false
    (is-false  (gtk:application-window-show-menubar window))
    ;; Set show-menubar property to nil
    (is-true (setf (gtk:application-window-show-menubar window) t))
    (is-true (gtk:application-window-show-menubar window))))

;;; --- Functions --------------------------------------------------------------

;;; --- gtk_application_window_new ---------------------------------------------

;; TODO: This does not work as expected. The tests are not performed.
;; We need a running application to check the function
(test application-window-new
  (let ((application (make-instance 'gtk:application
                                    :application-id "com.crategus.test"
                                    :flags :none
                                    :register-session nil)))
    ;; Connect signal "activate"
    (g:signal-connect application "activate"
        (lambda (app)
          (g:application-hold app)
          (let ((window (gtk:application-window-new app)))
            (is (typep window 'gtk:application-window))
            (is (eq app (gtk:window-application window)))
            (is (= 1 (gtk:application-window-id window)))
            ;; Remove the window, this shut down the application
            (is-false (gtk:application-remove-window app window)))
          (g:application-release app)))
    ;; Run the application
    (g:application-run application nil)))

;;; --- gtk_application_window_id ----------------------------------------------

(test application-window-id
  (let ((window (make-instance 'gtk:application-window)))
    ;; Zero if the window is not added to a GtkApplication
    (is (= 0 (gtk:application-window-id window)))))

;;; --- gtk_application_window_set_help_overlay --------------------------------
;;; --- gtk_application_window_get_help_overlay --------------------------------

(test application-window-help-overlay
  (let ((window (make-instance 'gtk:application-window))
        (help-overlay (make-instance 'gtk:shortcuts-window)))
    ;; Default value is nil
    (is-false (gtk:application-window-help-overlay window))
    ;; Set a GtkShortcutsWindow
    (is (eq help-overlay
            (setf (gtk:application-window-help-overlay window) help-overlay)))
    ;; Retrieve the GtkShortcutsWindow
    (is (eq help-overlay
            (gtk:application-window-help-overlay window)))
    (is (typep (gtk:application-window-help-overlay window)
               'gtk:shortcuts-window))))

;;; 2022-1-21

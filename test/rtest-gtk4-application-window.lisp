(in-package :gtk-test)

(def-suite gtk-application-window :in gtk-suite)
(in-suite gtk-application-window)

;;; --- GtkApplicationWindow ---------------------------------------------------

(test gtk-application-window-class
  ;; Check type
  (is (g:type-is-object "GtkApplicationWindow"))
  ;; Check registered name
  (is (eq 'gtk:application-window
          (glib:symbol-for-gtype "GtkApplicationWindow")))
  ;; Check parent
  (is (eq (g:gtype "GtkWindow") (g:type-parent "GtkApplicationWindow")))
  ;; Check children
  (is (equal '()
             (gtk-test:list-children "GtkApplicationWindow")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget" "GtkNative"
               "GtkShortcutManager" "GtkRoot" "GActionGroup" "GActionMap")
             (gtk-test:list-interfaces "GtkApplicationWindow")))
  ;; Check class properties
  (is (equal '("show-menubar")
             (gtk-test:list-properties "GtkApplicationWindow")))
  ;; Check signals
  (is (equal '()
             (gtk-test:list-signals "GtkApplicationWindow")))
  ;; Check CSS name
  (is (string= "window"
               (gtk:widget-class-css-name "GtkApplicationWindow")))
  ;; Check accessible role
  (is (eq :application
          (gtk:widget-class-accessible-role "GtkApplicationWindow")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkApplicationWindow"
                                             GTK-APPLICATION-WINDOW
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

(test gtk-application-window-show-menubar
  (let ((window (make-instance 'gtk:application-window)))
    ;; Default value is false
    (is-false  (gtk:application-window-show-menubar window))
    ;; Set show-menubar property to nil
    (is-true (setf (gtk:application-window-show-menubar window) t))
    (is-true (gtk:application-window-show-menubar window))))

;;; --- Functions --------------------------------------------------------------

;;; --- gtk_application_window_new ---------------------------------------------

;; TODO: This test can cause a memory fault. Check this?!

#+nil
(test gtk-application-window-new
  (let ((message nil)
        (application (make-instance 'gtk:application
                                    :application-id "com.crategus.test"
                                    :flags :default-flags
                                    :register-session nil)))
    ;; Connect signal "activate"
    (g:signal-connect application "activate"
        (lambda (app)
          (g:application-hold app)
          (let ((window (gtk:application-window-new app)))
            (setf message "in activate")
            (is (typep window 'gtk:application-window))
            (is (eq app (gtk:window-application window)))
            (is (= 1 (gtk:application-window-id window)))
            ;; Remove the window, this shuts down the application
            (is-false (gtk:application-remove-window app window)))
          (g:application-release app)))
    ;; Run the application
    (g:application-run application nil)
    ;; Check if the "activate" signal is executed
    (is (string= "in activate" message))))

;;; --- gtk_application_window_id ----------------------------------------------

(test gtk-application-window-id
  (let ((window (make-instance 'gtk:application-window)))
    ;; Zero if the window is not added to a GtkApplication
    (is (= 0 (gtk:application-window-id window)))))

;;; --- gtk_application_window_set_help_overlay --------------------------------
;;; --- gtk_application_window_get_help_overlay --------------------------------

(test gtk-application-window-help-overlay
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

;;; 2024-7-3

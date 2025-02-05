(in-package :gtk-test)

(def-suite gdk-app-launch-context :in gdk-suite)
(in-suite gdk-app-launch-context)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkAppLaunchContext

(test gdk-app-launch-context-class
  ;; Check type
  (is (g:type-is-object "GdkAppLaunchContext"))
  ;; Check registered name
  (is (eq 'gdk:app-launch-context
          (glib:symbol-for-gtype "GdkAppLaunchContext")))
  ;; Check type initializer
  (is (eq (g:gtype "GdkAppLaunchContext")
          (g:gtype (cffi:foreign-funcall "gdk_app_launch_context_get_type"
                                         :size))))
  ;; Check parent
  (is (eq (g:gtype "GAppLaunchContext")
          (g:type-parent "GdkAppLaunchContext")))
  ;; Check children
  #-windows
  (if *first-run-gtk-test*
      (equal '()
             (glib-test:list-children "GdkAppLaunchContext"))
      (is (member "GdkWaylandAppLaunchContext"
                  (glib-test:list-children "GdkAppLaunchContext")
                  :test #'string=)))
  #+windows
  (is (equal '()
             (glib-test:list-children "GdkAppLaunchContext")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GdkAppLaunchContext")))
  ;; Check class properties
  (is (equal '("display")
             (glib-test:list-properties "GdkAppLaunchContext")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GdkAppLaunchContext")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GdkAppLaunchContext" GDK:APP-LAUNCH-CONTEXT
                      (:SUPERCLASS G:APP-LAUNCH-CONTEXT
                       :EXPORT T
                       :INTERFACES NIL
                       :TYPE-INITIALIZER "gdk_app_launch_context_get_type")
                      ((DISPLAY APP-LAUNCH-CONTEXT-DISPLAY
                        "display" "GdkDisplay" T NIL)))
             (gobject:get-gtype-definition "GdkAppLaunchContext"))))

;;; --- Properties -------------------------------------------------------------

;;;     display

(test gdk-app-launch-context-properties
  (when *first-run-testsuite*
    (glib-test:with-check-memory (context :strong 1)
      (let ((display (gdk:display-default)))
        (is (typep (setf context
                         (gdk:display-app-launch-context display))
                   'gdk:app-launch-context))
        (is (eq display (gdk:app-launch-context-display context)))))))

;;; --- Functions --------------------------------------------------------------

;;;     gdk_app_launch_context_set_desktop

(test gdk-app-launch-context-set-desktop
  (glib-test:with-check-memory (context)
    (is (typep (setf context
                     (gdk:display-app-launch-context (gdk:display-default)))
               'gdk:app-launch-context))
    (is-false (gdk:app-launch-context-set-desktop context -1))))

;;;     gdk_app_launch_context_set_timestamp

(test gdk-app-launch-context-set-timestamp
  (glib-test:with-check-memory (context)
    (is (typep (setf context
                     (gdk:display-app-launch-context (gdk:display-default)))
               'gdk:app-launch-context))
    (is-false (gdk:app-launch-context-set-timestamp context
                                                    gdk:+current-time+))))

;;;     gdk_app_launch_context_set_icon

(test gdk-app-launch-context-set-icon
  (glib-test:with-check-memory (context icon)
    (is (typep (setf context
                     (gdk:display-app-launch-context (gdk:display-default)))
               'gdk:app-launch-context))
    (is (typep (setf icon (g:themed-icon-new "list-add")) 'g:themed-icon))
    (is-false (gdk:app-launch-context-set-icon context icon))
    (is-false (gdk:app-launch-context-set-icon context nil))))

;;;     gdk_app_launch_context_set_icon_name

(test gdk-app-launch-context-set-icon-name
  (glib-test:with-check-memory (context)
    (is (typep (setf context
                     (gdk:display-app-launch-context (gdk:display-default)))
               'gdk:app-launch-context))
    (is-false (gdk:app-launch-context-set-icon-name context "list-add"))
    (is-false (gdk:app-launch-context-set-icon-name context
                                                    (cffi:null-pointer)))))

;;; 2025-1-1

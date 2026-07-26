(in-package :gtk-test)

(def-suite gdk-display-manager :in gdk-suite)
(in-suite gdk-display-manager)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkDisplayManager

(test gdk-display-manager-class
  ;; Check type
  (is (g:type-is-object "GdkDisplayManager"))
  ;; Check registered name
  (is (eq 'gdk:display-manager
          (glib:symbol-for-gtype "GdkDisplayManager")))
  ;; Check type initializer
  (is (eq (g:gtype "GdkDisplayManager")
          (g:gtype (cffi:foreign-funcall "gdk_display_manager_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GdkDisplayManager")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GdkDisplayManager")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GdkDisplayManager")))
  ;; Check properties
  (is (equal '("default-display")
             (glib-test:list-properties "GdkDisplayManager")))
  ;; Check signals
  (is (equal '("display-opened")
             (glib-test:list-signals "GdkDisplayManager")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GdkDisplayManager" GDK:DISPLAY-MANAGER
                      (:SUPERCLASS G:OBJECT
                       :EXPORT T
                       :INTERFACES NIL
                       :TYPE-INITIALIZER "gdk_display_manager_get_type")
                      ((DEFAULT-DISPLAY DISPLAY-MANAGER-DEFAULT-DISPLAY
                        "default-display" "GdkDisplay" T T)))
             (gobject:get-gtype-definition "GdkDisplayManager"))))

;;; --- Signals ----------------------------------------------------------------

;;;     display-opened

(test gdk-display-manager-display-opened-signal
  (let* ((name "display-opened")
         (gtype (g:gtype "GdkDisplayManager"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    ;; Check parameter types
    (is (equal '("GdkDisplay")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Properties -------------------------------------------------------------

;;;     default-display

(test gdk-display-manager-properties
  (glib-test:with-check-memory ((manager 2) (display 2) :strong 2)
      ;; Get singleton display manager
      (is (typep (setf manager
                       (gdk:display-manager-get)) 'gdk:display-manager))
      ;; Get default display
      (is (typep (setf display
                       (gdk:display-manager-default-display manager)) 'gdk:display))
      ;; Remove default display
      (is-false (setf (gdk:display-manager-default-display manager) nil))
      ;; Restore default display
      (is (eq display
              (setf (gdk:display-manager-default-display manager) display)))))

;;; --- Functions --------------------------------------------------------------

;;;     gdk_display_manager_get

(test gdk-display-manager-get
  (glib-test:with-check-memory ((manager 2) :strong 2)
    ;; Get singleton default manager
    (is (typep (setf manager (gdk:display-manager-get)) 'gdk:display-manager))
    ;; Get default display
    (is (eq (gdk:display-default)
        (gdk:display-manager-default-display (gdk:display-manager-get))))))

;;;     gdk_display_manager_list_displays

(test gdk-display-manager-list-displays.1
  (glib-test:with-check-memory ((manager 2) :strong 2)
    ;; Get singleton display manager
    (setf manager (gdk:display-manager-get))
    ;; Get list of displays
    (is (every (lambda (x) (typep x 'gdk:display))
               (gdk:display-manager-list-displays manager)))))

#+crategus
(test gdk-display-manager-list-displays.2
  (glib-test:with-check-memory ((manager 2) :strong 2)
    ;; Get singleton display manager
    (setf manager (gdk:display-manager-get))
    ;; Get list of names for the displays
    (is (equal '("wayland-0")
               (mapcar #'gdk:display-name
                       (gdk:display-manager-list-displays manager))))))

;;;     gdk_display_manager_open_display

;; Ensure that this test only runs once
(defvar *first-run-gdk-display-manager-open-display* nil)

#-windows
(test gdk-display-manager-open-display
  (unless *first-run-gdk-display-manager-open-display*
    (setf *first-run-gdk-display-manager-open-display* t)
    (glib-test:with-check-memory ((manager 2) (display 2) :strong 2)
      (let ((name (uiop:getenv "DISPLAY")) msg handler)
        ;; Get singleton display manager
        (setf manager (gdk:display-manager-get))
        (setf handler
              (g:signal-connect manager "display-opened"
                                (lambda (manager display)
                                  (is (eq manager (gdk:display-manager-get)))
                                  (is (typep display 'gdk:display))
                                  (push "display-opened" msg))))
        ;; Open display
        (is (typep (setf display
                        (gdk:display-manager-open-display manager name)) 'gdk:display))
        (is (equal '("display-opened") msg))
        ;; Close display
        (is-false (gdk:display-close display))
        ;; Remove signal handler
        (is-false (g:signal-handler-disconnect manager handler))))))

;;;     gdk_set_allowed_backends

;;; 2026-07-26

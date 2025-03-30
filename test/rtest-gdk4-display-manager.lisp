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
          (g:gtype (cffi:foreign-funcall "gdk_display_manager_get_type"
                                         :size))))
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
  (glib-test:with-check-memory (:strong 2)
    (let ((manager (gdk:display-manager-get)))
      (is (typep (gdk:display-manager-default-display manager) 'gdk:display)))))

;;; --- Functions --------------------------------------------------------------

;;;     gdk_display_manager_get

(test gdk-display-manager-get
  (glib-test:with-check-memory ((manager 2) :strong 2)
    (is (typep (setf manager (gdk:display-manager-get)) 'gdk:display-manager))
    (is (= 2 (g:object-ref-count manager)))
    (is (eq (gdk:display-default)
        (gdk:display-manager-default-display (gdk:display-manager-get))))))

;;;     gdk_display_manager_list_displays

(test gdk-display-manager-list-displays
  (glib-test:with-check-memory ((manager 2) :strong 2)
    (setf manager (gdk:display-manager-get))
    (is (= 2 (g:object-ref-count manager)))
    (is (every (lambda (x) (typep x 'gdk:display))
               (gdk:display-manager-list-displays manager)))))

;;;     gdk_display_manager_open_display

#-windows
(test gdk-display-manager-open-display
  (glib-test:with-check-memory ((manager 2) :strong 2)
  (let ((name (uiop:getenv "DISPLAY")))
    (setf manager (gdk:display-manager-get))
    (is (= 2 (g:object-ref-count manager)))
    (is (typep (gdk:display-manager-open-display manager name) 'gdk:display)))))

#+nil ;; Test no longer works for Windows?
(test gdk-display-manager-open-display
  (glib-test:with-check-memory ((manager 2) :strong 2)
    (setf manager (gdk:display-manager-get))
    (is (= 2 (g:object-ref-count manager)))
    (is (typep (gdk:display-manager-open-display manager "1\\WinSta0\\Default")
               'gdk:display))))

;;;     gdk_set_allowed_backends

;;; 2025-3-28

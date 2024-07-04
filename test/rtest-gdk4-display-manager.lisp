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
             (gtk-test:list-children "GdkDisplayManager")))
  ;; Check interfaces
  (is (equal '()
             (gtk-test:list-interfaces "GdkDisplayManager")))
  ;; Check properties
  (is (equal '("default-display")
             (gtk-test:list-properties "GdkDisplayManager")))
  ;; Check signals
  (is (equal '("display-opened")
             (gtk-test:list-signals "GdkDisplayManager")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GdkDisplayManager"
                                             GDK-DISPLAY-MANAGER
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gdk_display_manager_get_type")
                       ((DEFAULT-DISPLAY GDK-DISPLAY-MANAGER-DEFAULT-DISPLAY
                         "default-display" "GdkDisplay" T T)))
             (gobject:get-g-type-definition "GdkDisplayManager"))))

;;; --- Properties -------------------------------------------------------------

;;;     default-display

(test gdk-default-display-properties
  (let ((manager (gdk:display-manager-get)))
    (is (typep (gdk:display-manager-default-display manager) 'gdk:display))))

;;; --- Signals ----------------------------------------------------------------

;;;     display-opened

;;; --- Functions --------------------------------------------------------------

;;;     gdk_display_manager_get

(test gdk-display-manager-get
  (is (typep (gdk:display-manager-get) 'gdk:display-manager))
  (is (eq (gdk:display-default)
      (gdk:display-manager-default-display (gdk:display-manager-get)))))

;;;     gdk_display_manager_list_displays

(test gdk-display-manager-list-displays
  (let ((manager (gdk:display-manager-get)))
    (is (every (lambda (x) (typep x 'gdk:display))
               (gdk:display-manager-list-displays manager)))))

;;;     gdk_display_manager_open_display

#-windows
(test gdk-display-manager-open-display
  (let ((name (uiop:getenv "DISPLAY"))
        (manager (gdk:display-manager-get)))
    (is (typep (gdk:display-manager-open-display manager name) 'gdk:display))))

#+windows
(test gdk-display-manager-open-display
  (let ((manager (gdk:display-manager-get)))
    (is (typep (gdk:display-manager-open-display manager "1\\WinSta0\\Default")
               'gdk:display))))

;;;     gdk_set_allowed_backends

;;; 2024-7-4

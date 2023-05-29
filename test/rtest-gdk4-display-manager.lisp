(in-package :gtk-test)

(def-suite gdk-display-manager :in gdk-suite)
(in-suite gdk-display-manager)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkDisplayManager

(test gdk-display-manager-class
  ;; Type check
  (is (g:type-is-object "GdkDisplayManager"))
  ;; Check the registered name
  (is (eq 'gdk:display-manager
          (glib:symbol-for-gtype "GdkDisplayManager")))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkDisplayManager")
          (g:gtype (cffi:foreign-funcall "gdk_display_manager_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GdkDisplayManager")))
  ;; Check the children
  (is (equal '()
             (list-children "GdkDisplayManager")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GdkDisplayManager")))
  ;; Check the properties
  (is (equal '("default-display")
             (list-properties "GdkDisplayManager")))
  ;; Check the signals
  (is (equal '("display-opened")
             (list-signals "GdkDisplayManager")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GdkDisplayManager" GDK-DISPLAY-MANAGER
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
  (let ((manager (gdk:display-manager-get)))
    (is (typep (gdk:display-manager-open-display manager "wayland-0")
               'gdk:display))
    (is-false (gdk:display-manager-open-display manager "xxx"))))

#+windows
(test gdk-display-manager-open-display
  (let ((manager (gdk:display-manager-get)))
    (is (typep (gdk:display-manager-open-display manager "1\\WinSta0\\Default")
               'gdk:display))
    (is-false (gdk:display-manager-open-display manager "xxx"))))

;;;     gdk_set_allowed_backends

;;; --- 2023-5-29 --------------------------------------------------------------

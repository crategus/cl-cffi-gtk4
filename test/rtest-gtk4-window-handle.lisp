(in-package :gtk-test)

(def-suite gtk-window-handle :in gtk-suite)
(in-suite gtk-window-handle)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkWindowHandle

(test gtk-window-handle-class
  ;; Type check
  (is (g:type-is-object "GtkWindowHandle"))
  ;; Check the registered name
  (is (eq 'gtk:window-handle
          (glib:symbol-for-gtype "GtkWindowHandle")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkWindowHandle")
          (g:gtype (cffi:foreign-funcall "gtk_window_handle_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkWindowHandle")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkWindowHandle")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (list-interfaces "GtkWindowHandle")))
  ;; Check the properties
  (is (equal '("child")
             (list-properties "GtkWindowHandle")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkWindowHandle")))
  ;; CSS name
  (is (string= "windowhandle"
               (gtk:widget-class-css-name "GtkWindowHandle")))
  ;; Accessible role
  (is (eq :group (gtk:widget-class-accessible-role "GtkWindowHandle")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkWindowHandle" GTK-WINDOW-HANDLE
                               (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                                ("GtkAccessible" "GtkBuildable"
                                 "GtkConstraintTarget")
                                :TYPE-INITIALIZER "gtk_window_handle_get_type")
                               ((CHILD GTK-WINDOW-HANDLE-CHILD "child"
                                 "GtkWidget" T T)))
             (gobject:get-g-type-definition "GtkWindowHandle"))))

;;; --- Properties -------------------------------------------------------------

;;;     child

(test gtk-window-handle-properties
  (let ((handle (make-instance 'gtk:window-handle)))
    (is-false (gtk:window-handle-child handle))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_window_handle_new

(test gtk-window-handle-new
  (is (typep (gtk:window-handle-new) 'gtk:window-handle)))

;;; --- 2023-7-23 --------------------------------------------------------------

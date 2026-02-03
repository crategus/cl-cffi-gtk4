(in-package :gtk-test)

(def-suite gtk-window-handle :in gtk-miscellaneous)
(in-suite gtk-window-handle)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkWindowHandle

(test gtk-window-handle-class
  ;; Check type
  (is (g:type-is-object "GtkWindowHandle"))
  ;; Check registered name
  (is (eq 'gtk:window-handle
          (glib:symbol-for-gtype "GtkWindowHandle")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkWindowHandle")
          (g:gtype (cffi:foreign-funcall "gtk_window_handle_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkWindowHandle")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkWindowHandle")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (glib-test:list-interfaces "GtkWindowHandle")))
  ;; Check properties
  (is (equal '("child")
             (glib-test:list-properties "GtkWindowHandle")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkWindowHandle")))
  ;; Check CSS name
  (is (string= "windowhandle"
               (gtk:widget-class-css-name "GtkWindowHandle")))
  ;; Check accessible role
  (is (eq :generic (gtk:widget-class-accessible-role "GtkWindowHandle")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkWindowHandle" GTK:WINDOW-HANDLE
                      (:SUPERCLASS GTK:WIDGET
                       :EXPORT T
                       :INTERFACES
                       ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
                       :TYPE-INITIALIZER "gtk_window_handle_get_type")
                      ((CHILD WINDOW-HANDLE-CHILD "child" "GtkWidget" T T)))
             (gobject:get-gtype-definition "GtkWindowHandle"))))

;;; --- Properties -------------------------------------------------------------

;;;     child

(test gtk-window-handle-properties
  (glib-test:with-check-memory (handle)
    (is (typep (setf handle
                     (make-instance 'gtk:window-handle)) 'gtk:window-handle))
    (is-false (gtk:window-handle-child handle))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_window_handle_new

(test gtk-window-handle-new
  (glib-test:with-check-memory (handle)
    (is (typep (setf handle (gtk:window-handle-new)) 'gtk:window-handle))))

;;; 2026-01-31

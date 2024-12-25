(in-package :gtk-test)

(def-suite gtk-root :in gtk-windows)
(in-suite gtk-root)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkRoot

(test gtk-root-interface
  ;; Check type
  (is (g:type-is-interface "GtkRoot"))
  ;; Check registered name
  (is (eq 'gtk:root
          (glib:symbol-for-gtype "GtkRoot")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkRoot")
          (g:gtype (cffi:foreign-funcall "gtk_root_get_type" :size))))
  ;; Check interface prerequisites
  (is (equal '("GtkNative" "GtkWidget")
             (glib-test:list-interface-prerequisites "GtkRoot")))
  ;; Check interface properties
  (is (equal '()
             (glib-test:list-interface-properties "GtkRoot")))
  ;; Check interface signals
  (is (equal '()
             (glib-test:list-signals "GtkRoot")))
  ;; Check interface definition
  (is (equal '(GOBJECT:DEFINE-GINTERFACE "GtkRoot" GTK:ROOT
                       (:EXPORT T
                        :TYPE-INITIALIZER "gtk_root_get_type"))
             (gobject:get-gtype-definition "GtkRoot"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_root_get_display

(test gtk-root-display
  (glib-test:with-check-memory (window :strong 1)
    (is (typep (setf window (make-instance 'gtk:window)) 'gtk:window))
    (is (typep (gtk:root-display window) 'gdk:display))
    ;; Root display is the default display
    (is (eq (gdk:display-default) (gtk:root-display window)))
    (is-false (gtk:window-destroy window))))

;;;     gtk_root_get_focus
;;;     gtk_root_set_focus

(test gtk-root-focus
  (glib-test:with-check-memory (button window)
    (is (typep (setf button (make-instance 'gtk:button)) 'gtk:button))
    (is (typep (setf window
                     (make-instance 'gtk:window :child button)) 'gtk:window))
    ;; WINDOW is root widget for BUTTON
    (is (eq window (gtk:widget-root button)))
    ;; No focus widget
    (is-false (gtk:root-focus window))
    ;; Set BUTTON as focus widget
    (is (eq button (setf (gtk:root-focus window) button)))
    (is (eq button (gtk:root-focus window)))
    ;; Check focus widget
    (is-true (gtk:widget-is-focus button))
    ;; Unset focus widget
    (is-false (setf (gtk:root-focus window) nil))
    (is-false (gtk:root-focus window))
    ;; Remove child and destroy window
    (is-false (setf (gtk:window-child window) nil))
    (is-false (gtk:window-destroy window))))

;;; 2024-12-23

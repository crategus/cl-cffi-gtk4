(in-package :gtk-test)

(def-suite gtk-root :in gtk-suite)
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
  (let ((window (make-instance 'gtk:window)))
    (is (typep (gtk:root-display window) 'gdk:display))
    ;; Root display is the default display
    (is (eq (gdk:display-default) (gtk:root-display window)))))

;;;     gtk_root_get_focus
;;;     gtk_root_set_focus

(test gtk-root-focus
  (let* ((button (make-instance 'gtk:button))
         (window (make-instance 'gtk:window
                                :child button)))
    ;; WINDOW is root widget for BUTTON
    (is (eq window (gtk:widget-root button)))
    ;; No focus widget
    (is-false (gtk:root-focus window))
    ;; Set focus on BUTTON
    (is (eq button (setf (gtk:root-focus window) button)))
    (is (eq button (gtk:root-focus window)))
    ;; Unset focus
    (is-false (setf (gtk:root-focus window) nil))
    (is-false (gtk:root-focus window))))

;;; 2024-4-10

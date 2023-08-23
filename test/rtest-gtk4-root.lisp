(in-package :gtk-test)

(def-suite gtk-root :in gtk-suite)
(in-suite gtk-root)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkRoot

(test gtk-root-interface
  ;; Type check
  (is (g:type-is-interface "GtkRoot"))
  ;; Check the registered name
  (is (eq 'gtk:root
          (glib:symbol-for-gtype "GtkRoot")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkRoot")
          (g:gtype (cffi:foreign-funcall "gtk_root_get_type" :size))))
  ;; Check the interface prerequisites
  (is (equal '("GtkNative" "GtkWidget")
             (list-interface-prerequisites "GtkRoot")))
  ;; Check the interface properties
  (is (equal '()
             (list-interface-properties "GtkRoot")))
  ;; Check the interface signals
  (is (equal '()
             (list-signals "GtkRoot")))
  ;; Get the interface definition
  (is (equal '(GOBJECT:DEFINE-G-INTERFACE "GtkRoot"
                                  GTK-ROOT
                                  (:EXPORT T
                                   :TYPE-INITIALIZER "gtk_root_get_type"))
             (gobject:get-g-type-definition "GtkRoot"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_root_get_display

(test gtk-root-display
  (let ((window (make-instance 'gtk:window)))
    (is (typep (gtk:root-display window) 'gdk:display))))

;;;     gtk_root_get_focus
;;;     gtk_root_set_focus

;; Might cause an unexpected error

(test gtk-root-focus.1
  (let* ((button (make-instance 'gtk:button))
         (window (make-instance 'gtk:window
                                :child button)))
    (is-false (gtk:root-focus window))
    ;; Set the focus on the button
    (is (eq button (setf (gtk:root-focus window) button)))
    (is (eq button (gtk:root-focus window)))
    ;; Unset the focus
    (is-false (setf (gtk:root-focus window) nil))
    (is-false (gtk:root-focus window))))

(test gtk-root-focus.2
  (let* ((button (make-instance 'gtk:button))
         (window (make-instance 'gtk:window
                                :child button)))
    (is-false (gtk:root-focus window))
    ;; Set the focus on the button
    (is-true (gtk:widget-grab-focus button))
    (is (eq button (gtk:root-focus window)))))

;;; --- 2023-8-19 --------------------------------------------------------------

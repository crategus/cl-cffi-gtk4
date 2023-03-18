(in-package :gtk-test)

(def-suite gtk-root :in gtk-suite)
(in-suite gtk-root)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkRoot

(test root-interface
  ;; Type check
  (is (g:type-is-interface "GtkRoot"))
  ;; Check the registered name
  (is (eq 'gtk:root
          (gobject:symbol-for-gtype "GtkRoot")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkRoot")
          (g:gtype (cffi:foreign-funcall "gtk_root_get_type" :size))))
  ;; Get the names of the interface properties.
  (is (equal '()
             (list-interface-properties "GtkRoot")))
  ;; Get the interface definition
  (is (equal '(DEFINE-G-INTERFACE "GtkRoot"
                                  GTK-ROOT
                                  (:EXPORT T
                                   :TYPE-INITIALIZER "gtk_root_get_type"))
             (gobject:get-g-type-definition "GtkRoot"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_root_get_display

(test root-display
  (let ((window (make-instance 'gtk:window)))
    (is (typep (gtk:root-display window) 'gdk:display))))

;;;     gtk_root_get_focus
;;;     gtk_root_set_focus

;; Might cause an unexpected error

#+nil
(test root-focus
  (let* ((button (make-instance 'gtk:button))
         (window (make-instance 'gtk:window
                                :child button)))
    (is-false (gtk:root-focus window))
    (is (eq button (setf (gtk:root-focus window) button)))
    (is (eq button (gtk:root-focus window)))))

;;; --- 2023-3-18 --------------------------------------------------------------

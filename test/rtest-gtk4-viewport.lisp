(in-package :gtk-test)

(def-suite gtk-viewport :in gtk-suite)
(in-suite gtk-viewport)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkViewport

(test gtk-viewport-class
  ;; Type check
  (is (g:type-is-object "GtkViewport"))
  ;; Check the registered name
  (is (eq 'gtk:viewport
          (glib:symbol-for-gtype "GtkViewport")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkViewport")
          (g:gtype (cffi:foreign-funcall "gtk_viewport_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkViewport")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkViewport")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkScrollable")
             (list-interfaces "GtkViewport")))
  ;; Check the properties
  (is (equal '("child" "hadjustment" "hscroll-policy" "scroll-to-focus"
               "vadjustment" "vscroll-policy")
             (list-properties "GtkViewport")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkViewport")))
  ;; CSS name
  (is (string= "viewport"
               (gtk:widget-class-css-name "GtkViewport")))
  ;; CSS classes
  (is (equal '()
             (gtk:widget-css-classes (make-instance 'gtk:viewport))))
  ;; Accessible role
  (is (eq :group (gtk:widget-class-accessible-role "GtkViewport")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkViewport" GTK-VIEWPORT
                               (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                                ("GtkAccessible" "GtkBuildable"
                                 "GtkConstraintTarget" "GtkScrollable")
                                :TYPE-INITIALIZER "gtk_viewport_get_type")
                               ((CHILD GTK-VIEWPORT-CHILD "child" "GtkWidget" T
                                 T)
                                (SCROLL-TO-FOCUS GTK-VIEWPORT-SCROLL-TO-FOCUS
                                 "scroll-to-focus" "gboolean" T T)))
             (gobject:get-g-type-definition "GtkViewport"))))

;;; --- Properties -------------------------------------------------------------

;;;     child
;;;     scroll-to-focus

(test gtk-viewport-properties
  (let ((viewport (make-instance 'gtk:viewport)))
    (is-false (gtk:viewport-child viewport))
    (is-true (gtk:viewport-scroll-to-focus viewport))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_viewport_new

(test gtk-viewport-new
  (is (typep (gtk:viewport-new) 'gtk:viewport))
  (is (typep (gtk:viewport-new (make-instance 'gtk:adjustment)) 'gtk:viewport))
  (is (typep (gtk:viewport-new (make-instance 'gtk:adjustment)
                               (make-instance 'gtk:adjustment)) 'gtk:viewport)))

;;; --- 2023-11-1 --------------------------------------------------------------

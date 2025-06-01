(in-package :gtk-test)

(def-suite gtk-viewport :in gtk-scrolling)
(in-suite gtk-viewport)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkViewport

(test gtk-viewport-class
  ;; Check type
  (is (g:type-is-object "GtkViewport"))
  ;; Check registered name
  (is (eq 'gtk:viewport
          (glib:symbol-for-gtype "GtkViewport")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkViewport")
          (g:gtype (cffi:foreign-funcall "gtk_viewport_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkViewport")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkViewport")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkScrollable")
             (glib-test:list-interfaces "GtkViewport")))
  ;; Check properties
  (is (equal '("child" "hadjustment" "hscroll-policy" "scroll-to-focus"
               "vadjustment" "vscroll-policy")
             (glib-test:list-properties "GtkViewport")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkViewport")))
  ;; Check CSS name
  (is (string= "viewport"
               (gtk:widget-class-css-name "GtkViewport")))
  ;; Check accessible role
  (is (eq :generic (gtk:widget-class-accessible-role "GtkViewport")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkViewport" GTK:VIEWPORT
                      (:SUPERCLASS GTK:WIDGET
                       :EXPORT T
                       :INTERFACES
                       ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
                        "GtkScrollable")
                       :TYPE-INITIALIZER "gtk_viewport_get_type")
                      ((CHILD VIEWPORT-CHILD "child" "GtkWidget" T T)
                       (SCROLL-TO-FOCUS VIEWPORT-SCROLL-TO-FOCUS
                        "scroll-to-focus" "gboolean" T T)))
             (gobject:get-gtype-definition "GtkViewport"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-viewport-properties
  (glib-test:with-check-memory (viewport)
    (is (typep (setf viewport (make-instance 'gtk:viewport)) 'gtk:viewport))
    (is-false (gtk:viewport-child viewport))
    (is-true (gtk:viewport-scroll-to-focus viewport))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_viewport_new

(test gtk-viewport-new
  (glib-test:with-check-memory (viewport)
    (is (typep (setf viewport (gtk:viewport-new)) 'gtk:viewport))
    (is-false (setf (gtk:scrollable-hadjustment viewport) nil))
    (is-false (setf (gtk:scrollable-vadjustment viewport) nil))
    (is (typep (setf viewport
                     (gtk:viewport-new (make-instance 'gtk:adjustment)))
               'gtk:viewport))
    (is-false (setf (gtk:scrollable-hadjustment viewport) nil))
    (is-false (setf (gtk:scrollable-vadjustment viewport) nil))
    (is (typep (setf viewport
                     (gtk:viewport-new (make-instance 'gtk:adjustment)
                                       (make-instance 'gtk:adjustment)))
               'gtk:viewport))
    (is-false (setf (gtk:scrollable-hadjustment viewport) nil))
    (is-false (setf (gtk:scrollable-vadjustment viewport) nil))))

;;;     gtk_viewport_scroll_to                              Since 4.12

(test gtk-view-port-scroll-to
  (glib-test:with-check-memory (viewport area)
    (is (typep (setf viewport (gtk:viewport-new)) 'gtk:viewport))
    (is (typep (setf area (gtk:drawing-area-new)) 'gtk:drawing-area))
    (is (typep (setf (gtk:viewport-child viewport) area) 'gtk:drawing-area))
    (is-false (gtk:viewport-scroll-to viewport area (gtk:scroll-info-new)))

    (is-false (setf (gtk:viewport-child viewport) nil))))

;;; 2025-05-06
